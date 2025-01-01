package require tclopt
namespace import ::tclopt::*


proc linfunc {xall pdata} {
    set x [dget $pdata x]
    set y [dget $pdata y]
    set ey [dget $pdata ey]
    foreach xVal $x yVal $y eyVal $ey {
        set f [= {[@ $xall 0]+[@ $xall 1]*$xVal}]
        lappend fval [= {($yVal-$f)/$eyVal}]
    }
    return [dcreate fvec $fval]
}

set x [list -1.7237128 1.8712276 -9.6608055E-01 -2.8394297E-01 1.3416969 1.3757038 -1.3703436 4.2581975E-02\
               -1.4970151E-01 8.2065094E-01]
set y [list 1.9000429E-01 6.5807428E+00 1.4582725E+00 2.7270851E+00 5.5969253E+00 5.6249280E+00 0.787615 3.2599759E+00\
               2.9771762E+00 4.5936475E+00]
set ey [list 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07]
set pdata [dcreate x $x y $y ey $ey]

puts [::tclopt::mpfit -funct linfunc -m 10 -npar 2 -xall [list 1.0 1.0] -pars "" -pdata $pdata]
