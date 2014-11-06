Benchmarks to measure the effectiveness of the JIT coach's recommendations
==========================================================================

Uses some of the [Octane benchmarks](https://code.google.com/p/octane-benchmark/).

Also reuses part of the Octane infrastructure for benchmarking.


### To run:
* Install the Racket benchmarking library: `raco pkg install benchmark`
* Make sure you have a `js` in your path, that it's an optimized build, and that it doesn't have optimization logging.
* Run the benchmarks: `racket bench.rkt`
* Open `plot.pdf`


### Versions summary

#### Richards:
  1. baseline
  2. fixed location for `TaskControlBlock.state`
  3. remove packet queue flip-flopping using a sentinel object
  4. remove packet queue flip-flopping using a closure
  5. #2 + #4

#### DeltaBlue:
  1.  baseline
  2.  singleton `Direction` → globals
  3.  singleton `Strength` → globals
  4.  monomorphic `BinaryConstraint.output`
  5.  #2 + #3 + #4
  6.  #5 + monomorphic `UnaryConstraint.isSatisfied`
  7.  duplicate `addToGraph`
  8.  #7 + duplicate `addConstraint`
  9.  #8 + inline super constructors
  10. #5 + #9

#### RayTrace:
  1. baseline
  2. `IntersectionInfo` w/o class system
  3. `IntersectionInfo.isHit` on object
  4. reordered `Material` fields
  5. #2 + #4

#### Splay:
  1. baseline
  2. `root_` on object, not prototype
  3. `left` and `right` on object, not prototype

#### NavierStokes:
  1. baseline
  2. lin_solve2 indexes as integers (all on the relevant lines)
  3. more lin_solve2 indexes as integers (report shows up after 2. only)

#### PdfJS:
  1. baseline
  2. `buf1` and `buf2` initialized in constructor

#### Crypto:
  1. baseline
  2. BigInteger.s and BigInteger.t set in constructor
  3. #2 + various `getelem` with guaranteed integers

#### Box2D:
  1. baseline
  2. consistent field order for `r` objects
