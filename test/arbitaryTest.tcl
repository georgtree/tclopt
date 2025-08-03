package require tcltest
namespace import ::tcltest::*
package require math::constants
::math::constants::constants pi
package require tclopt
namespace import ::tclopt::*

set epsilon 1e-6
proc matchList {expected actual} {
    variable epsilon
    set match true
    set len [llength $expected]
    for {set i 0} {$i<$len} {incr i} {
        set exp [lindex $expected $i]
        set act [lindex $actual $i]
        if {(abs($act-$exp) > $epsilon) || (abs($act-$exp) > $epsilon)} {
            set match false
            break
        }
    }
    return $match
}
customMatch mtchLst matchList


proc rastrigin1d {pars pdata args} {
    global pi
    set x0 [@ $pars 0]
    set x1 [@ $pars 1]
    return [= {($x0*$x0-10*cos(2.0*$pi*$x0))+($x1*$x1-10*cos(2.0*$pi*$x1))+10.0*2}]
}
test GSAClassTest-1 {test procedure of optimization} -setup {
    set pdata {}
    set par0 [Parameter new x0 0.0 -lowlim -5.12 -uplim 5.12]
    set par1 [Parameter new x1 0.0 -lowlim -5.12 -uplim 5.12]
    set optimizer [GSA new -funct rastrigin1d -pdata $pdata -seed 12345 -maxiter 5000 -nbase 1000 -specified]
    $optimizer addPars $par0 $par1 
} -body {
    $optimizer run
} -result "objfunc 0.0 x {127.83049444305921 0.04559893842416926 -255.2790324882704 -0.21803502712599265\
159.19065156778535 0.21783327230638996 -31.74646730054404 -0.05499126256330574 0.9943771757016615} generation 308\
nfev 18540 strategy best/1/exp std 0.0" -cleanup {
    foreach par $pars {
        $par destroy
    }
    unset pdata optimizer pars
}
