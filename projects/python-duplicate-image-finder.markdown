---
title: Python Duplicate Image Finder
menu: projects
sources: https://github.com/pharaun/python-duplicate-finder
licenses: lgplv2
math: true
---

_Python Duplicate Image Finder_ is a project in which I test out various
approaches for discovering duplicate images and dealing with them.

So far I have implemented these following approaches:

- File content hash
- Basic image similarity - $\sum |a - b| > threshold$

I am hoping to get around to implementing these following approaches:

- SIFT/SURF
- Perceptual Hashing
- FFT Analysis

I was able to speed up the basic image similarity by moving the whole block of
code that did the n-way compare into C, SSE, and use of the
[OpenMP](http://www.openmp.org/) API to spread the workload cross multiple
cores.

I am also hoping to be able to start doing some
[CUDA](http://www.nvidia.com/object/cuda_home.html) or
[OpenCL](http://www.khronos.org/opencl/) codes to accelerate various parts of
the project.
