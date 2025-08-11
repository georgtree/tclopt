![Static Badge](https://img.shields.io/badge/version-0.21-blue)

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
- [Tcl9](https://www.tcl.tk/software/tcltk/9.0.html) or [Tcl8.6.15](https://www.tcl.tk/software/tcltk/8.6.html)
- [gcc compiler](https://gcc.gnu.org/)
- [make tool](https://www.gnu.org/software/make/)

For run you also need:
- [argparse](https://github.com/georgtree/argparse)
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

# Interactive help

All public methods have interactive help. To get information about method (including `new` and `create`) and its\
arguments call it with `-help` switch:

``` tcl
package require tclopt
namespace import ::tclopt::*
Mpfit new -help
```

``` text
Creates optimization object that does least squares fitting using modified
Levenberg-Marquardt algorithm. For more detailed description please see
documentation. Can accepts unambiguous prefixes instead of switches names.
Accepts switches only before parameters.
    Switches:
        -funct value - Required. Name of the procedure that should be
            minimized.
        -m value - Required. Number of data points.
        -pdata value - List or dictionary that provides private data to funct
            that is needed to evaluate residuals. Usually it contains x and y values
            lists, but you can provide any data necessary for function residuals
            evaluation. Will be passed upon each function evaluation without
            modification. Default value is .
        -ftol value - Control termination of mpfit. Termination occurs when both
            the actual and predicted relative reductions in the sum of squares are
            at most ftol. Default value is 1e-10.
        -xtol value - Control termination of mpfit. Termination occurs when the
            relative error between two consecutive iterates is at most xtol. Default
            value is 1e-10.
        -gtol value - Control termination of mpfit. Termination occurs when the
            cosine of the angle between fvec and any column of the jacobian is at
            most gtol in absolute value. Default value is 1e-10.
        -stepfactor value - Used in determining the initial step bound. This
            bound is set to the product of factor and the euclidean norm of diag*x
            if nonzero, or else to factor itself. Default value is 100.
        -covtol value - Range tolerance for covariance calculation. Default
            value is 1e-14.
        -maxiter value - Maximum number of iterations. Default value is 200.
        -maxfev value - Control termination of mpfit. Termination occurs when
            the number of calls to funct is at least maxfev by the end of an
            iteration. If it equals to 0, number of evaluations is not restricted.
            Default value is 0.
        -epsfcn value - Finite derivative step size. Default value is
            2.2204460e-16.
        -nofinitecheck - Boolean. Enable check for infinite quantities.
        -help - Help switch, when provided, forces ignoring all other switches
            and parameters, prints the help message to stdout, and returns up to 2
            levels above the current level.
```

Best to do it in interactive console, see [tkcon](https://github.com/bohagan1/TkCon)
