![Static Badge](https://img.shields.io/badge/version-0.1-blue)

![Static Badge](https://img.shields.io/badge/license-MIT-blue)

![Static Badge](https://img.shields.io/badge/Tcl_version-9.0-blue)

![Static Badge](https://img.shields.io/badge/Kubuntu_24.04-pass-green)

![Static Badge](https://img.shields.io/badge/Windows_11-pass-green)

![Static Badge](https://img.shields.io/badge/coverage-93.4%25-green)

# Content

This package provides tcl wrapper for optimization procedures.
The sources of procedures are:
- [Levenberg-Marquardt Least Squares Fitting](http://cow.physics.wisc.edu/%7Ecraigm/idl/cmpfit.html)

# Installation and dependencies

For building you need:
- [SWIG of version 4.3](https://www.swig.org/download.html)
- [Tcl9](https://www.tcl.tk/software/tcltk/9.0.html)
- [gcc compiler](https://gcc.gnu.org/)
- [make tool](https://www.gnu.org/software/make/)

For run you also need:
- [argparse](https://wiki.tcl-lang.org/page/argparse)
- [Tcllib](https://www.tcl.tk/software/tcllib/)

To build run 
```bash
./configure
make
sudo make install
```
If you have different versions of Tcl on the same machine, you can set the path to this version with `-with-tcl=path`
flag to configure script.

For Windows build it is strongly recommended to use [MSYS64 UCRT64 environment](https://www.msys2.org/), the above
steps are identical if you run it from UCRT64 shell.

# Supported platforms

I've tested it on:
- Kubuntu 24.04 with Tcl 9
- Windows 11 in MSYS64 UCRT64 environment with Tcl9

# Documentation

You can find some documentation [here](https://georgtree.github.io/tclopt)
