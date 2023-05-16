This repository contains definitions of most Common Lisp standard
macros that can be defined in a portable way, and that can generate
portable code.  Some of these macros may not be good enough as the
final version for a typical implementation, but they will work.  For
example, the DEFUN macros has no compile-time side effects, because
such side effects are not specified by the standard.