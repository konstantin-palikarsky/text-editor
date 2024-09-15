Startup
======
To build run `stack build --allow-newer` in the project root, then start
the binary text-editor in the folder indicated by the stack output

Usage as `text-editor /path/to/file` to edit file


Operation
========
**A cursor for an X is a data structure that represents both the entire X and also where you are looking with in X.**

``` haskell
data ListCursor a = ListCursor [a] [a] -- Look between two elements
```

``` haskell
data NonEmptyListCursor a = NonEmptyListCursor [a] a [a] -- Look at an element
```

# Brick architecture

```
         Start
           |
           v
Event -> State -> Draw
  ^                |
  |                |
   \-    Brick  <-/
```