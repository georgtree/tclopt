package require tclopt
package require gnuplotutil
namespace import ::tcl::mathfunc::*
namespace import ::tclopt::*

# generate random integer number in the range [min,max]
proc randFloat {min max} {
    return [expr {rand()*($max-$min+1)+$min}]
}
srand 1
proc sinfunc {xall pdata args} {
    set x [dget $pdata x]
    set y [dget $pdata y]
    foreach xVal $x yVal $y {
        set f [= {sin([@ $xall 0]*$xVal)+sin([@ $xall 1]*$xVal)+sin([@ $xall 2]*$xVal)}]
        lappend fvec [= {$yVal-$f}]
        lappend fval $f
    }
    return [dcreate fvec $fvec fval $fval]
}


for {set i 0} {$i<100} {incr i} {
    set xi [= {$i*0.01}]
    lappend x $xi
    lappend y [= {[randFloat 0.95 1.0]*(sin(1.5*$xi)+sin(11*$xi)+sin(6*$xi))}]
}

set pdata [dcreate x $x y $y]
set par0 [::tclopt::parCreate -lowlim 0 -uplim 20]
set par1 [::tclopt::parCreate -lowlim 0 -uplim 20]
set par2 [::tclopt::parCreate -lowlim 0 -uplim 20]
set pars [list $par0 $par1 $par2]

set result [::tclopt::mpfit -funct sinfunc -m 100 -xall [list 3.0 8.0 1.0] -pars $pars -pdata $pdata]
set yinitial [dget [sinfunc [list 3.0 8.0 1.0] $pdata] fval]
set yfinal [dget [sinfunc [dget $result x] $pdata] fval]
puts [dget $result x]
gnuplotutil::plotXYN $x -xlabel "x" -ylabel "y" -grid -names [list  data initial final] -columns $y $yinitial $yfinal
