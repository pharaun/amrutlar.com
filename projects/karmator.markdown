---
title: Karmator
menu: projects
sources: https://github.com/pharaun/Karmator
licenses: bsd3
---

_Karmator_ is a hackathon project to replace two IRC bot's karma modules with
an unified karma module that has strong parsing support for various form/manner
of karmaing.

This was a joint project between myself and [Habnabit](https://github.com/habnabit).
The project was implemented in python and twisted for the IRC
connectivity part and defined a simple JSON "IPC" via std-in/out. The actual
parsing part is done by a Haskell parser which is able to do fairly complicated
parsing to extract possible karma candidates.

Karmator is able to do some basic nickname cleanup, then breaking down a
sentence into possible karma candidates based on a list of characters that
would qualify for "karma" such as +, - for ex. Then it does some heuristics to
eliminate unlikely candidates such as "./gnu --args" for ex. Its also able to
deal with braced karma such as "foobar (bar)++".
