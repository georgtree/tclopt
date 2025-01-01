![Static Badge](https://img.shields.io/badge/version-0.14-blue)

![Static Badge](https://img.shields.io/badge/license-GPL3-blue)

![Static Badge](https://img.shields.io/badge/Tcl_version-9.0-blue)
![Static Badge](https://img.shields.io/badge/Tcl_version-8.6.15-blue)

![Static Badge](https://img.shields.io/badge/Kubuntu_24.04-pass-green)

![Static Badge](https://img.shields.io/badge/Windows_11-pass-green)

![Static Badge](https://img.shields.io/badge/Tcl_coverage-98.6%25-green)

# Content

This package provides tcl wrapper for interpolation and approximation procedures.
The sources of procedures are:
- [Linear interpolation routines](https://people.math.sc.edu/Burkardt/c_src/interp/interp.html)
- [Spline interpolation and approximation routines](https://people.math.sc.edu/Burkardt/c_src/spline/spline.html)

# Installation and dependencies

For building you need:
- [SWIG of version 4.3](https://www.swig.org/download.html)
- [Tcl9](https://www.tcl.tk/software/tcltk/9.0.html) or [Tcl8.6.15](https://www.tcl.tk/software/tcltk/8.6.html)
- [gcc compiler](https://gcc.gnu.org/)
- [make tool](https://www.gnu.org/software/make/)

For run you also need:
- [argparse](https://wiki.tcl-lang.org/page/argparse)
- [Tcllib](https://www.tcl.tk/software/tcllib/)

To build, run 
```bash
./configure
make
sudo make install
```
If you have different versions of Tcl on the same machine, you can set the path to this version with `-with-tcl=path`
flag to configure script.

For Windows build it is strongly recommended to use [MSYS64 UCRT64 environment](https://www.msys2.org/), the above
steps are identical if you run it from UCRT64 shell. 

There are prebuilt packages that contains .so/.dll files, tcl code and tests for Windows and Linux.

# Supported platforms

I've tested it on:
- Kubuntu 24.04 with Tcl 9 and Tcl 8.6.15
- Windows 11 in MSYS64 UCRT64 environment with Tcl9

# Documentation

You can find some documentation [here](https://georgtree.github.io/tclinterp)
