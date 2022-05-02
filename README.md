# cl-mps

**This library is unfinished and still under development.**

This module provides reader and writer for MPS format.

## Usage

## Requirements

`cl-mps` is tested on (the usually latest version of) the following implementations:

- SBCL
- Clozure CL
- Allegro CL


## Notes on MPS format

This module deals with a kind of "free MPS format". Please see [MPS file format](http://lpsolve.sourceforge.net/5.0/mps-format.htm) for the details. Below are the several notes on the specifications.


- This module always behaves case-sensitive.
- The length of the item (e.g. variable) name is arbitrary.
- You cannot use whitespaces in item names. More precisely, characters that match the regex `\s` are regarded as separators.
- In COLUMN and RHS section, you can put three or more variables on a single line. (In fixed MPS format, only one or two variables are allowed.)
- In fixed MPS format, BOUNDS section may contain multiple sets of bounds. That is, a single file may contain multiple problem instances. As this specification is not widely used, `read-mps` simply ignores the names of the bound, and assumes that only a single set of bounds exist. RHS section is also handled in the same way.
- When BOUNDS section contains a bound type SC, which indicates semi-continuity, `read-mps` warns and ignores it.
- In ROWS section, the first row that is indicated as 'N' is regarded as the objective. Other such rows are ignored.
- RANGES section is ignored.
- Variables whose bound type is BV, LI, or UI are regarded as integral.
- In COLS section, variables between the two markers, INTORG and INTEND, are regarded as integral. (This is CPLEX extension. See https://www.ibm.com/docs/en/icos/20.1.0?topic=extensions-integer-variables-in-mps-files.)
- `read-mps` interprets OBJSENSE section if it exists.
- You cannot use the following reserved words for item names: `NAME`, `ENDATA`, `ROWS`, `COLUMNS`, `RHS`, `BOUNDS`, `RANGES`, and `OBJSENSE`.


## Copyright

Copyright (c) 2022 Hugo Sansaqua.
