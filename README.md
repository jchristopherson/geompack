# geompack
A modernization of the GEOMPACK library for computing Delaunay triangulations.


## Status
![Build Status](https://github.com/jchristopherson/geompack/actions/workflows/cmake.yml/badge.svg)
[![Actions Status](https://github.com/jchristopherson/geompack/workflows/fpm/badge.svg)](https://github.com/jchristopherson/geompack/actions)


## Documentation
Documentation can be found [here](https://jchristopherson.github.io/geompack/)

## Building GEOMPACK
[CMake](https://cmake.org/)This library can be built using CMake.  For instructions see [Running CMake](https://cmake.org/runningcmake/).

[FPM](https://github.com/fortran-lang/fpm) can also be used to build this library using the provided fpm.toml.
```txt
fpm build
```
The GEOMPACK library can be used within your FPM project by adding the following to your fpm.toml file.
```toml
[dependencies]
geompack = { git = "https://github.com/jchristopherson/geompack" }
```