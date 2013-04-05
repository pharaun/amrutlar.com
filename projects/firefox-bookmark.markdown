---
title: Firefox Bookmark
menu: projects
sources: https://github.com/pharaun/firefox-bookmark
licenses: bsd3
---

_Firefox Bookmark_ is a small [Haskell](http://www.haskell.org) project for
sorting Firefox bookmarks that are in the Firefox backup JSON format.

The genesis of this project was that I was tired of manually resorting my
bookmarks according to the following rules:

1. Sort by the display (proportional font) length of the bookmark title or url in descending order.
2. Bookmark folders are sorted first then the actual bookmarks.
3. Respects bookmark separator bars and keep both groups separate.

I didn't want to write it as a plugin to Firefox and I was wanting to learn
some more about JSON parsing in Haskell, more specifically the
[aeson](http://hackage.haskell.org/package/aeson-0.6.1.0) package. Anyway this
is a fully functional command line program, so feel free to make use of it.
