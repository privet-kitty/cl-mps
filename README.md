# cl-mps

[![Build Status](https://github.com/privet-kitty/cl-mps/workflows/CI/badge.svg)](https://github.com/privet-kitty/cl-mps/actions)

**This library is still in alpha stage.**

This module provides readers (but no writers, at least for now) for MPS format.


## Requirements

`cl-mps` is tested on the (usually latest version of the) following implementations:

- SBCL
- Clozure CL
- Allegro CL


## Notes on MPS format and two readers

This module provides two readers, `read-fixed-mps` and `read-free-mps`, which
deal with fixed MPS format and a kind of "free MPS format", respectively. Please
see [MPS file format](http://lpsolve.sourceforge.net/5.0/mps-format.htm) for the
details. 

It is intended that `read-fixed-mps` can read all of the [NETLIB LP Test Problem
Set](https://www.cuter.rl.ac.uk/Problems/netlib.shtml).

Below are several notes on the specifications common to two readers:

- This module always behaves case-sensitive.
- Fixed MPS format allows BOUNDS section contains multiple sets of bounds, i.e.,
  a single file may contain multiple problem instances. As this specification is
  not widely used, in this module, both readers simply ignore the names of the
  bounds, and assume that only a single set of bounds exist. RHS section is also
  handled in the same way.
- This module doesn't deal with the bound type SC, which indicates
  semi-continuity. Both reader warns and ignores it if BOUND section contains
  it.
- In ROWS section, the first row that is indicated as 'N' is regarded as the
  objective. Other such rows are ignored.
- Variables whose bound type is BV, LI, or UI are regarded as integral.
- Both reader interpret parts of CPLEX extensions.
  - In COLUMNS section, variables between the two markers, INTORG and INTEND, are regarded as integral. (See https://www.ibm.com/docs/en/icos/20.1.0?topic=extensions-integer-variables-in-mps-files.)
  - Both reader interprets OBJSENSE section if it exists. (See https://www.ibm.com/docs/en/icos/20.1.0?topic=extensions-objective-sense-name-offset-in-mps-files.)
- The type of the parsed real values follows `*read-default-float-format*`. Some
  implementations allow this variable to be `rational` and this module also
  supports it.

Please see the docstrings for the difference between `read-fixed-mps` and
`read-free-mps`.




## Copyright

Copyright (c) 2022 Hugo Sansaqua.
