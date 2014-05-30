Benchmarks to measure the effectiveness of the JIT coach's recommendations
==========================================================================

Uses some of the [Octane benchmarks](https://code.google.com/p/octane-benchmark/).

Also reuses part of the Octane infrastructure for benchmarking.


### To run:
* Install the Racket benchmarking library: `raco pkg install benchmark`
* Run the benchmarks: `racket bench.rkt`
* Open `plot.pdf`


### Versions summary

#### Richards:
  1. baseline
  2. fixed location for `TaskControlBlock.state`
  3. remove packet queue flip-flopping

#### DeltaBlue:
  1. baseline
  2. singleton `Direction` → globals
  3. singleton `Strength` → globals
  4. monomorphic `BinaryConstraint.output`

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
