---
title: Python Duplicate Image Finder (Part 2)
menu: blog
categories: [C, SSE, Projects]
description: Python Duplicate Image Finder (Part 2)
---
Welcome to the next part of the series of blog post on the [Python Duplicate
Image Finder](/projects/python-duplicate-image-finder/) project. This post will
go much more in-depth about performance analysis of the assembly code emitted by
[gcc](http://gcc.gnu.org/) and my usage of the
[SSE](http://en.wikipedia.org/wiki/Streaming_SIMD_Extensions) intrinsics. It
also goes over the importance of choosing the right data type and its impact on
performance and memory usage.


## Detailed Nehalem CPU specifications

Basically before I can go into detail about the performance and its implication
on several new re-write of the basic [Sum Absolute
Difference](http://en.wikipedia.org/wiki/Sum_of_absolute_differences) inner
loop, I need to go over some details about the Nehalem CPU specification. The
reason why I am going over the Nehalem CPU specification is because its
basically what is powering my dual x5650 Xeon processor in my system.

Now the large majority of these information came from the excellent [Software
optimization resources](http://www.agner.org/optimize/) manuals by Agner. If
you ever get into assembly and optimization I would highly recommend this
resource, the author has really gone into depth over the many facet of software
optimizations.

To start, the dual [x5650](http://ark.intel.com/Product.aspx?id=47922) Xeons
processor that I picked up is a hex-core processor with
[hyper-threading](http://en.wikipedia.org/wiki/Hyper-threading) technology for
a total of 12 threads per socket. It operates at 2.66 GHz with [Intel Turbo
Boost](http://en.wikipedia.org/wiki/Intel_Turbo_Boost) technology which can
boost the CPU clock frequency up to 3.06 GHz. These CPU also supports most of
the pre-[AVX](http://en.wikipedia.org/wiki/Advanced_Vector_Extensions)
[SIMD](http://en.wikipedia.org/wiki/SIMD) instruction set all the way from
[MMX](http://en.wikipedia.org/wiki/MMX_%28instruction_set%29) to the latest
non-AVX instruction set which is [SSE4.2](http://en.wikipedia.org/wiki/SSE4).

There is also this quite nice [SVG
diagram](/img/blog/2011/07/python-duplicate-image-finder_part-2/Intel_Nehalem_arch.svg)
of the Intel Nehalem microarchitecture that I got from the Wikipedia article on
the [Nehalem
microarchitecture](http://en.wikipedia.org/wiki/Nehalem_%28microarchitecture%29).


### Instruction fetching

The first stage of processing the instructions is to actually fetch the instruction
from the L1 Instruction cache, this stage can be a bottleneck because its among some of
the least improved stage in the Nehalem.

* Fetches 16 bytes or 6 instruction per clock cycle, whichever one is the smallest.
* Average instruction length should be less than 4 bytes long on average.


### Instruction decoding

Second stage is decoding the instruction fetched, and in some case depending on
the composition of the instruction this stage can be a bottleneck.  The
reasoning is because some instructions can break down into more than 1
[μops](http://en.wikipedia.org/wiki/Micro-operation).

* 4 decoders, can process up to 4 instruction per clock cycle
* First decoder can decode an instruction that generates up to 4 μops in a cycle.
    * Other 3 decoder can only decode an instruction that generates one μops.
* Using a 4-1-1-1 pattern for instructions will max out the decoding at 7 μops per cycle.

Basically the CPU is more
[RISC](http://en.wikipedia.org/wiki/Reduced_instruction_set_computer) like in
the core but still needs to support the x86's
[CISC](http://en.wikipedia.org/wiki/Complex_instruction_set_computing)
instruction set, thus to deal with this instructions are broke down into
multiple μops for execution by the CPU. The Nehalem is more effective at this
than some of the previous CPU in that the majority of the instruction breaks
down into one μops thanks to fused μops.


### Loopback buffer

This CPU also has a loopback buffer which caches all of the μops up to a
certain limit to help speed up the execution of loops.  However there are
several size restriction on how large a piece of code can be to be able to use
the loopback buffer, otherwise the loopback buffer is not used.

* Less than 256 bytes (8 block of 32 bytes)
* Less than 28 (possibly fused) μops

There also is an one clock delay in the loopback buffer execution, such as 4xN
μops taking N + 1 clock cycle to execute. It is also highly recommended to
align the loop instruction by 16 so that it can fit inside the loopback buffer
better.


### Execution port

The Core 2 and Nehalem CPU can execute up to 4 μops per clock cycle which is an
increase of one over the older Pentium-M processors. There is also multiple
execution port which an μops can be assigned to be processed at. These
execution ports allow the CPU to process up to 4 μops per clock cycle on
average depending on how well distributed the execution is on the Ports.

* Throughput of 4 μops (5 if there is macro-op fusion) per clock cycle.
* RAT can handle up to 4 register renames per clock cycle.
* ROB can handle up to a max of three read register that has not been written
  to recently per clock cycle, otherwise it stalls.
* Multiple execution ports for processing μops.
    * Port 0, 1, 5 are used for ALU related μops.
    * Port 2 is used for memory read μops.
    * Port 3 is used for memory write address calculation.
    * Port 4 is used for Memory write μops.


### Data Bypass delay

On the Nehalem CPU the execution units are divided up into several domains:

* Integer --- All operations in general purpose registers.
* SMID Integer --- All integer operation in SSE/MMX registers.
* Floating Point --- Floating point operations in SSE and x87 registers.
* Load --- Memory reads.
* Store --- Memory writes.

Now there is a latency of one to two clock cycle if the output of an operation
in one domain is right away used as input in another domain. The table below
details the bypass latency, the leftmost column is the *From* domain and the
rest of the table is in the *To* domain.

|---
| | Integer | SMID Integer | Floating Point | Store |
|:-|:-:|:-:|:-:|:-:|
| **Integer** | 0 | 1 | 2 | 0 |
| **SMID Integer** | 1 | 0 | 2 | 1 |
| **Floating Point** | 2 | 2 | 0 | 1 |
| **Load** | 0 | 1 | 2 | 0 |
|---


### Cache and memory latency

The numbers presented in this section is not absolute and should be taken with
a grain of sand but they should roughly represent the cache and memory latency.

|--
| | Memory Latency |
|-:|:-:|
| L1 Cache | 4 Cycles |
| L2 Cache | 11 Cycles |
| L3 Cache | 38 Cycles |
| Ram | 78 to 104 Cycles |
|---

Now the latency was calculated assuming *ddr3-1067* sticks of RAM. The x5650's
can use up to *ddr3-1333* sticks of RAM, however I didn't have the specs on
hand so I used the specs of the *ddr3-1067* sticks of RAM, the actual
performance should be a bit faster than calculated here. Also I did not take in
accord the [NUMA](http://en.wikipedia.org/wiki/Non-Uniform_Memory_Access)
aspect of the dual x5650 because it would had made calculations more
complicated. On my motherboard each x5650 gets two bank of three ddr3 ram slots
and the CPU can access each other's memory banks through QPI but it'll be slower
than direct access to the CPU's own bank of memory.

Anyway a single full bank of three *ddr3-1067* with the following spec
*7-7-7-20* is roughly 15,057 MB/s read, 14,272 MB/s write, 15,522 MB/s copy,
and has an benchmarked latency of 39.2 nanosecond. The last value in the
*7-7-7-20* is the [CAS latency](http://en.wikipedia.org/wiki/CAS_latency) which
computes out to an estimated 30 nanosecond latency.

Now the x5650s are running at 2.6 GHz which leads to an 0.385 nanosecond per
cycle timing which means that it takes roughly 78 (30ns) to 104 (39ns) cycles
for the processor to start getting data from the RAM.



## Analysis

Now with the bulk of the detailed CPU specification on the Nehalem architecture
which is interesting on its own but I went into detail so that we will be able
to understand and analyze the instruction that gcc emits when it compiles the
code with the SSE intrinsics. The knowledge and information outlined in the
previous specification section are going to come in use here and now in the
analysis.

Basically what is going to happen here is there is going to be four iteration
of the code under analysis and in each iteration we will look at the C code
with the SSE intrinsics, then the assembler output that gcc emits, finally we
will go into detailed analysis of the assembly output.


### SSE Float

This section is going to cover the best SSE Float implement from part 1. This
version was able to hit 6.19 million compares a second which was a very nice
improvement over the initial C version, but its still not fast enough!

Anyway I am going to omit the extra/supporting code such as the code that processes
the sums of the calculations. Below is the C code with the SSE intrinsics in it.

    // Init partial sums
    __m128 vsum = _mm_setzero_ps();

    for(k = 0; k < ARRAY_LENGTH; k += 4) {
	    // Load 4 floats from sima, simb
	    __m128 va = _mm_load_ps(&sima[k]);
	    __m128 vb = _mm_load_ps(&simb[k]);

	    // calc diff = sima - simb
	    __m128 vdiff = _mm_sub_ps(va, vb);

	    // calc neg diff = 0.0 - diff
	    __m128 vnegdiff = _mm_sub_ps(_mm_setzero_ps(), vdiff);

	    // calc abs diff = max(diff, - diff)
	    __m128 vabsdiff = _mm_max_ps(vdiff, vnegdiff);

	    // accumulate two partial sums
	    vsum = _mm_add_ps(vsum, vabsdiff);
    }
{:.c}

Now here below is the assembly output from gcc of the code above. As you can
see its not an exact copy of the intrinsics because gcc was able to optimize
and reorder some of the instructions to work better.

    .L76:
	    movaps  (%rsi,%rax), %xmm0      # __m128 vb = _mm_load_ps(&sima[k]);
	    movaps  %xmm3, %xmm2            # zero out xmm2 register
	    subps   (%rcx,%rax), %xmm0      # __m128 vdiff = _mm_sub_ps(va, vb); - va loaded from ram
	    addq    $16, %rax
	    cmpq    $12288, %rax
	    subps   %xmm0, %xmm2            # __m128 vnegdiff = _mm_sub_ps(_mm_setzero_ps(), vdiff);
	    maxps   %xmm2, %xmm0            # __m128 vabsdiff = _mm_max_ps(vdiff, vnegdiff);
	    addps   %xmm0, %xmm1            # vsum = _mm_add_ps(vsum, vabsdiff);
	    jne     .L76                    # Loop again till done
{:.gas}


#### Analysis

To start here's the list of the possible bottlenecks per run-through of the loop:

* 2.25 cycles if only taking in accord μops retirement.
* Execution port 1 & 5 at 4.66 or 2.66 cycles.
* 4 cycles for instruction fetching.
* Add in 2 cycles for data bypass delays.
* Read only registers: rsi, rcx, xmm3 (3 registers max).
* 4 float compares per loop.

So from the list above it looks like the bottleneck is the execution ports at 4.66
cycles and then also the 2 cycles from the data bypass delays for 6.66 cycle per
loop.  This breaks down into 768 loop run-through which is 5,114.88 cycles with
no memory loading delay.

Below is a table of the cycle breakdowns:

|---
| | CPU | L1/Ram | Total | μs/Compare | Compare/s |
|:-|:-:|:-:|:-:|:-:|
| **Best case** | 5,115 | 6,144 | 11,259 | 4.335 | 230,680 |
| **Worst Case** | 5,115 | 159,744 | 164,899 | 64.49 | 15,751 |
|---

Now take the best case and multiple it by 24 (core/thread on the dual x5650's),
this ends up at ~5.53 million compares a second.


### Improved and unrolled SSE Float

This is an improved version of the SSE Float implement uses a different
approach of using *_mm_andnot_ps* for computing the absolute value instead of
the previous approach of taking the max of the difference & negative
difference.  Also this version does a partial unrolling of the loop to help
keep the execution ports a bit more saturated.

The reason why this version is faster is better distribution of execution port
usage and the partial unrolling to do more per loop.

Anyway I am going to omit the extra/supporting code such as the code that processes
the sums of the calculations. Below is the C code with the SSE intrinsics in it.

    // Sign mask for abs - -0.0f = 1 << 31
    const __m128 sign_mask = _mm_set1_ps(-0.0f);

    // Init sum
    __m128 sum1 = _mm_setzero_ps();
    __m128 sum2 = _mm_setzero_ps();

    for(k = 0; k < ARRAY_LENGTH/4; k += 2) {
	    sum1 += _mm_andnot_ps(sign_mask, _mm_sub_ps(sima[k], simb[k]));
	    sum2 += _mm_andnot_ps(sign_mask, _mm_sub_ps(sima[k+1], simb[k+1]));
    }
{:.c}

Now here below is the assembly output from gcc of the code above. As you can
see its not an exact copy of the intrinsics because gcc was able to optimize
and reorder some of the instructions to work better.

    .L64:
	    movaps  (%rsi,%rax), %xmm0      # va1 = load from ram
	    movaps  %xmm3, %xmm8            # Copy sign_mask into xmm8
	    subps   (%rcx,%rax), %xmm0      # diff1 = _mm_sub_ps(va1, vb1); - vb1 loaded from ram
	    andnps  %xmm0, %xmm8            # abs1 = _mm_andnot_ps(sign_mask, diff1);
	    movaps  16(%rsi,%rax), %xmm0    # va2 = load from ram
	    subps   16(%rcx,%rax), %xmm0    # diff2 = _mm_sub_ps(va2, vb2); - vb2 loaded from ram
	    addq    $32, %rax
	    cmpq    $12288, %rax
	    addps   %xmm8, %xmm2            # sum1 += abs1
	    movaps  %xmm3, %xmm8            # copy sign_mask into xmm8
	    andnps  %xmm0, %xmm8            # abs2 = _mm_andnot_ps(sign_mask, diff2);
	    addps   %xmm8, %xmm1            # sum2 += abs2
	    jne     .L64                    # Loop again till done
{:.gas}


#### Analysis

To start here's the list of the possible bottlenecks per run-through of the loop:

* 3.24 cycles if only taking in accord μops retirement.
* Execution port 1, 5 & 2 at 4.66, 5.66, 4 cycles.
* 5 cycles for instruction fetching.
* Add in 3 cycles for data bypass delays.
* Read only registers: rsi, rcx, xmm3 (3 registers max).
* 8 float compares per loop.

So from the list above it looks like the bottleneck again is the execution
ports at 5.66 cycles and also the 3 cycles from the data bypass delays for a
total of 8.66 cycles per loop.  This breaks down into 384 loop (thanks to
partial unrolling) run-through which is 3,325.44 cycles with no memory
loading delays.

Below is a table of the cycle breakdowns:

|---
| | CPU | L1/Ram | Total | μs/Compare | Compare/s |
|:-|:-:|:-:|:-:|:-:|
| **Best case** | 3,326 | 6,144 | 9,470 | 3.646 | 274,270 |
| **Worst Case** | 3,326 | 159,744 | 163,070 | 62.78 | 15,929 |
|---

Now take the best case and multiple it by 24 (core/thread on the dual x5650's),
this ends up at ~6.58 million compares a second. This is a nice improvement
over the previous float implementation but this still is not fast
enough... Onward to unsigned 8bit integers!


### SSE Unsigned 8bit integers

Basically, we have been barking up the wrong tree for a while. Looking at all
of the images that I want to compare, they're all almost universally 8bit
and/or 8bit with alpha, so there was not much sense in doing the image crunching
in floats or doubles. As this section will show, there is an impressive gain in
performance by migrating from floats to unsigned 8bit integers.

This version is quicker because of a couple reasons:

* Smaller memory footprint so more data can be fitted in the cache.
* Can cramp more *unit8* into each *xmm* registers which also speeds things up.

Anyway I am going to omit the extra/supporting code such as the code that processes
the sums of the calculations. Below is the C code with the SSE intrinsics in it.

    // Init partial sums
    __m128i vsum = _mm_setzero_si128();

    for(k = 0; k < ARRAY_LENGTH; k += 16) {
	    // Load 16 uint8_t from sima, simb
	    __m128i va = _mm_load_si128((const __m128i*)&sima[k]);
	    __m128i vb = _mm_load_si128((const __m128i*)&simb[k]);

	    // Calc Sum Absolute Difference over the 16x uint8_t
	    // 0, 0, 0, uint16 | 0, 0, 0, uint16
	    __m128i vabsdiff = _mm_sad_epu8(va, vb);

	    // Accumulate the two int32 (uint16 "extended")
	    vsum = _mm_add_epi32(vsum, vabsdiff);
    }
{:.c}

Now here below is the assembly output from gcc of the code above. As you can
see its not an exact copy of the intrinsics because gcc was able to optimize
and reorder some of the instructions to work better.

    .L40:
	    movdqa  (%rax), %xmm1           # __m128i va = _mm_load_si128((const __m128i*)&sima[k]);
	    addq    $16, %rax
	    psadbw  (%rcx), %xmm1           # __m128i vabsdiff = _mm_sad_epu8(va, vb); - vb loaded from 
	    addq    $16, %rcx
	    cmpq    %rsi, %rax
	    paddd   %xmm1, %xmm0            # vsum = _mm_add_epi32(vsum, vabsdiff);
	    jne     .L40                    # Keep looping till done
{:.gas}


#### Analysis

To start here's the list of the possible bottlenecks per run-through of the loop:

* 1.75 cycles if only taking in accord μops retirement.
* Execution port 1, 5 & 2 at 2, 2.5, 2 cycles.
* 2 cycles for instruction fetching.
* Read only registers: rsi (3 registers max).
* 16 *uint8* compares per loop.

Bottleneck in this case again is the execution ports at 2.5 cycles per loop,
there also is no data bypass delays on this one which means the total cycle
per loop is 2.5 cycles.  Also the fact that we are doing 16 integer compares
per loop breaks it down to 192 loop run-through which is 480 cycles with no
memory loading delays.

Below is a table of the cycle breakdowns:

|---
| | CPU | L1/Ram | Total | μs/Compare | Compare/s |
|:-|:-:|:-:|:-:|:-:|
| **Best case** | 480 | 1,536 | 2,016 | 0.7762 | 1,288,300 |
| **Worst Case** | 480 | 39,936 | 40,416 | 15.56 | 64,267 |
|---

Now take the best case and multiple it by 24 (core/thread on the dual x5650's),
this ends up at ~30.91 million compares a second. This is the fastest
implementation that I've been able to do.


### Unrolled SSE Unsigned 8bit Integers

Since we got such a nice performance increase with the non-unrolled version
above and the fact that the busiest execution port was at 2.5 cycles, I wanted
to see if unrolling would had helped performance any.  Surprisingly in this
case, it actually degraded performance mildly.

Anyway I am going to omit the extra/supporting code such as the code that processes
the sums of the calculations. Below is the C code with the SSE intrinsics in it.

    // Init sum
    __m128i sum1 = _mm_setzero_si128();
    __m128i sum2 = _mm_setzero_si128();

    for(k = 0; k < ARRAY_LENGTH/16; k += 2) {
	    sum1 = _mm_add_epi32(sum1, _mm_sad_epu8(sima[k], simb[k]));
	    sum2 = _mm_add_epi32(sum2, _mm_sad_epu8(sima[k+1], simb[k+1]));
    }
{:.c}

Now here below is the assembly output from gcc of the code above. As you can
see its not an exact copy of the intrinsics because gcc was able to optimize
and reorder some of the instructions to work better.

    .L28:
	    movdqa  (%rsi,%rax), %xmm2      # va1 = load from ram
	    psadbw  (%rcx,%rax), %xmm2      # sad1 = _mm_sad_epu8(va1, vb1); - vb1 load from ram
	    paddd   %xmm2, %xmm1            # sum1 = _mm_add_epi32(sum1, sad1);
	    movdqa  16(%rsi,%rax), %xmm2    # va2 = load from ram
	    psadbw  16(%rcx,%rax), %xmm2    # sad2 = _mm_sad_epu8(va2, vb2); - vb2 load from ram
	    addq    $32, %rax
	    paddd   %xmm2, %xmm0            # sum2 = _mm_add_epi32(sum2, sad2)
	    cmpq    $3072, %rax
	    jne     .L28                    # Loop till done
{:.gas}


#### Analysis

To start here's the list of the possible bottlenecks per run-through of the loop:

* 2.25 cycles if only taking in accord μops retirement.
* Execution port 1, 5 & 2 at 2.66, 2.66, 4 cycles.
* 3 cycles for instruction fetching.
* Add in 2 cycles for data bypass delays.
* Read only registers: rsi, rcx (3 registers max).
* 32 *uint8* compares per loop.

Bottleneck in this case again is the execution ports at 4 cycles per loop,
and there's also the 2 cycles for the data bypass delays which means the total
cycles per loop is 6 cycles.  Also with this, we are doing 32 integer compares
per loop which breaks it down to 96 run-through which is 576 cycles total with
no memory loading delays.

Below is a table of the cycle breakdowns:

|---
| | CPU | L1/Ram | Total | μs/Compare | Compare/s |
|:-|:-:|:-:|:-:|:-:|
| **Best case** | 576 | 1,536 | 2,112 | 0.8131 | 1,229,900 |
| **Worst Case** | 576 | 39,936 | 40,512 | 15.6 | 64,100 |
|---

Now take the best case and multiple it by 24 (core/thread on the dual x5650's),
this ends up at ~29.51 million compares a second. This is slightly slower, because
in this case its the data bypass delays that really hurts us here.  This goes to show
that unrolling isn't always a good thing.


## Conclusion

The final result of ~30.91 million compares a second is a massive increase over
our previous article's max of ~6.19 million compares a second.  Here, below is
a chart that shows the performance of all of the above 4 options along with the
performance from the previous article. In this chart _uint8_ is Unsigned 8bit
Integers. Also the first two entry are for Doubles.

|---
| | Compare/s |
|:-|:-|
| OpenMP | 1.23 Million |
|---
| Scheduling | 2.92 Million |
|---
| Floats | 5.53 - 6.19 Million |
|---
| Unrolled Floats | 6.58 Million |
|---
| uint8 | 30.91 Million |
|---
| Unrolled uint8 | 29.51 Million |
|---

The key takeaway to take from this, is you can often bring in a large amount of
improvement by dropping into a lower level language and bringing in
parallelization. However it is critically important to ensure that you also are
spending the effort and work on the proper data-type/structure. Such as in this
case since the majority of the images are already in 8bit Integer format or can
be nearly losslessly converted into 8bit Integer its better to work in this
form.


## Resources

python-duplicate-finder
: <https://github.com/pharaun/python-duplicate-finder>

Software optimization resources
: <http://www.agner.org/optimize/>
