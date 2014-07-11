Prototype Optimization Coach for SpiderMonkey
=============================================

### Requires
* [A patched version of SpiderMonkey](https://github.com/stamourv/gecko-dev/tree/profiler-opt-info) (Warning: I force push to that branch.).
* [Racket](http://racket-lang.org) from recent [git](http://github.com/plt/racket/) or [nightly build](http://www.cs.utah.edu/plt/snapshots/).

### Instructions
* [Build Firefox](https://developer.mozilla.org/en-US/docs/Simple_Firefox_build).
  The JIT coach gets its data from the Gecko profiler, so building only the JS
  shell is not enough.
* Gather profile data using `./js-profile <path-to-firefox-obj-dir> <program>`,
  redirecting the output to a file.
  (Look in the `examples` subdirectory for programs to try the coach on, or use
  one of the provided profile files directly.)
  Running the profiler from the browser may work, but hasn't been tested.
* `racket coach.rkt <profile-file>`

To run benchmarks, see `README.md` in the `benchmarks` subdirectory.
