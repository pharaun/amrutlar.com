---
title: The Creation of this Site
menu: blog
categories: nanoc
---
This post documents the generation of this site from a set of templates and
markdown files.  This site uses the remarkable static site generator named
[nanoc](http://nanoc.stoneship.org/) to transform and apply various templates
to each markdown file with this site's content in them.

This site is still a work in progress, so things may get changed around.

##nanoc

Basically I use nanoc to manage, and generate this site from various discrete
components scattered all over. It is able to assembly this site into a format
that is easy to rsync up to my web-server. The core of nanoc is the Rules,
which tells it how to build all of the content in the site from various
discrete files. It has a couple of rules; *compile*, *route*, *layout* which is
better summarized by the nanoc site itself:

> Compilation rules determine the actions that should be executed during
> compilation (filtering, layouting), routing rules determine the path where
> the compiled files should be written to, and layouting rules determine the
> filter that should be used for a given layout.

Then nanoc also makes it really easy to extend it with your own filters such as
the quick and dirty one I wrote to convert my site's background image from a
png source into jpeg so that I can directly edit the png file and not worry
about re-compression artifacts. Then I also have another set of custom filters
that I got from [Dominik Honnef](http://dominik.honnef.co/) for colorizing my
code samples in my blog posts, along with dealing with licenses links for my
projects.

##Layout and CSS

The basic layout of this site is a mixture of handcrafted HTML plus a bit of
[erb](http://www.ruby-doc.org/stdlib/libdoc/erb/rdoc/) templating for inserting
the needed information here and there on the layout phase of the site
generation.

I may eventually migrate my HTML on over to [Haml](http://haml-lang.com/) to
make managing it even easier, but since the total amount of content in HTML is
pretty low it probably won't happen for a while.

Then finally to take care of the CSS which can be a big pain to manage, I used
[Sass](http://sass-lang.com/) which actually made working with CSS fun again
because it provides several very nice features such as Variables, Nesting,
Mixins, and so forth.  These features allows me to dry up my CSS stylesheet
into short, sweet, and simple SCSS stylesheet. It also enables me to break up
my stylesheets into several files with each containing a logical chunk of
related styles. I also borrowed a couple of the Mixins from the
[Compass](http://compass-style.org/) project which is a open-source CSS
Authoring Framework.

##Content

Finally for the content of this site, I use either plain HTML for some of the
pages such as my About page that are unlikely to change. However for the
remainder of the site I use an extended Markdown via
[kramdown](http://kramdown.rubyforge.org/). This enables me to focus on the
content of each page without being distracted by all of the HTML markup.

Since this site is still being worked on and there is still quite a bit of a
mess in regards of my resume generator code... Its sadly not up on GitHub just
quite yet. But it'll eventually make it up when I finally settle on something
that I am happy with.
