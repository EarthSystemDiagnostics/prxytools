# prxytools 0.5.0

* New auxiliary function `is.equidistant()` to test whether a vector (e.g. depth
  vector or time series) has equidistant steps, i.e. resolution. The
  implementation includes a numerical tolerance that accounts for the machine
  respresentation of floating-point numbers, which circumvents the problems
  popular methods of checking equidistance have using, e.g., `sd()` or
  `unique()` on the difference vector.

# prxytools 0.4.0

* New function `Lag()` to shift numeric, character or factor vectors by a
  specified number of elements.

# prxytools 0.3.0

* New function `LocatePeaks()` added which allows to find the positions of local
  maxima or minima in a sequence.

# prxytools 0.2.0

* New function `AverageByIndex()` added which allows to calculate bin averages
  defined by vector index positions.

# prxytools 0.1.2

* Allow variable offsets of "reservoir ages" in radiocarbon age calibration with
  `CalibrateAge()` (pulled from 'ecustools'
  [`dev-CalibrateAge`](https://github.com/EarthSystemDiagnostics/ecustools/tree/dev-CalibrateAge)
  branch as of 2021-01-18; see
  [5b8379e](https://github.com/EarthSystemDiagnostics/ecustools/commit/5b8379e64632cd48fcc41a51cec916869aa49a70)).

# prxytools 0.1.1

* Improved function documentations.
* `Bchron::BchronCalibrate` does not need to be imported into namespace.
* Tidied source code and restructured source code files.

# prxytools 0.1.0

* Initial package version with number and scope of the functions incl. their
  documentation identical to their original versions in deprecated package
  'ecustools' which they were a part of ('ecustools'
  [`main`](https://github.com/EarthSystemDiagnostics/ecustools/tree/master)
  branch as of 2020-11-20).
