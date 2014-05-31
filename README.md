Prototype Optimization Coach for SpiderMonkey
=============================================

### Requires
* [A patched version of SpiderMonkey](https://github.com/stamourv/gecko-dev).
* [Racket](http://racket-lang.org) from recent [git](http://github.com/plt/racket/) or [nightly build](http://www.cs.utah.edu/plt/snapshots/).

### Instructions
* [Build the JS shell](https://wiki.mozilla.org/JavaScript:New_to_SpiderMonkey#Build_the_js_shell)
* Run your program (look in the `examples` subdirectory). This will generate optimization logs on stderr. Redirect those to a file.
* `racket coach.rkt <logfile>`

To run benchmarks, see `README.md` in the `benchmarks` subdirectory.
