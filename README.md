# ido-ql-quickload

```ido-ql-quickload``` is a ```M-x``` enhancement for SLiME and Quicklisp. Built on top of Ido, it provides a convenient interface to your recently and most frequently loaded systems. And to all the other systems, too.

## Get started

* Download ```ido-ql-quickload``` and set-up your load path. [(Find out more.)](http://www.emacswiki.org/emacs/InstallingPackages)

* To auto-start ```ido-ql-quickload``` every time you open Emacs add these lines to your ```.emacs``` file:

```lisp
(require 'ido-ql-quickload)
(ido-ql-quickload-initialize)
```

* Run SLiME
* Type ```M-x ql:quickload```

The systems are displayed in an Ido completion buffer, ordered by relevance. The 5 most recently loaded systems come first, the rest are sorted by location (local sytems come before Quicklisp sytems), frequency of use and in alphabetical order.
