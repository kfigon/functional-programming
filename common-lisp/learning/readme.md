# common lisp

[book](https://gigamonkeys.com/book/)

use makefile to run things
```
make repl
make run FILE=foo.lisp
```

in repl, we can use function to refresh definitions 
```
(load "filename")
(load (compile-file "filename"))
```