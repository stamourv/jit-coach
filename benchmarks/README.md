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
  1. baseline
  2. singleton `Direction` → globals
  3. singleton `Strength` → globals
  4. monomorphic `BinaryConstraint.output`
  5. #2 + #3 + #4
  6. #5 + monomorphic `UnaryConstraint.isSatisfied`
  7. duplicate `addToGraph`

#### RayTrace:
  1. baseline
  2. `IntersectionInfo` w/o class system
  3. `IntersectionInfo.isHit` on object
  4. reordered `Material` fields
  5. #2 + #4


### Notes
* because multiple versions of the same benchmark exist, they must use the
  module pattern to avoid conflicting. That seems to add some overhead (i.e.
  it reduces scores a bit), but that should be consistent across versions.
