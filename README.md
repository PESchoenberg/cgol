# fmath - simple fortran modules.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2611307.svg)](https://doi.org/10.5281/zenodo.2611307)


## Overview:

A collection under development of Fortran modules and functions written over
time that might be useful for calculations in maths, astronomy, aeronautics,
etc. Some of these functions are still unfinished or need further testing; all
of them originated as "scrapbook material" from various Matlab and Frotran
projects that I kept jsut because they could be useful in the future.


## Dependencies and requirements:

* gfortran compiler.

* OpenMP library.

* MPICH, OpenMPI or compatible libraries.


## Installation:

* Assuming you have all dependencies installed on your system, unpack it into
a folder of your choice and cd into it.


## Uninstall:

* You just need to remove the fmath folder.


## Usage:

* See the comments on each module.

* Compilation and linking:

  - See the comments of each module or write

    - ./cfmath.sh <ENT> at the command line.

  - You can use tfmath.f95 to test these modules. See the comments at this
  file for compilation and linking.


## Credits and Sources:

* Many of the functions included are of my own design, but I also adapted code
from other sources. These are indicated on each case, in the comments that
correspond to each function.

* Please let me know if I forgot to add any credits or sources.


## License:

* LGPL-3.0-or-later.


