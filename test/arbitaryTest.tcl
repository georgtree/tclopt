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
proc sample {x pdata args} {
    set fx 0.0
    set g [lrepeat [= {[llength $x]}] 0.0]
    for {set i 0} {$i<[llength $x]} {incr i 2} {
        set t1 [= {1.0-li($x,$i)}]
        set t2 [= {10.0*(li($x,$i+1)-li($x,$i)*li($x,$i))}]
        lset g [= {$i+1}] [= {20.0*$t2}]
        lset g $i [= {-2.0*(li($x,$i)*li($g,$i+1)+$t1)}]
        set fx [= {$fx+($t1*$t1+$t2*$t2)}]
    }
    return [dcreate f $fx dvec $g]
 
}

test LBFGSClassTest-11 {} -setup {
    for {set i 0} {$i<100} {incr i 2} {
        lappend pars [Parameter new x$i -1.2 -lowlim -100 -uplim 100]
        lappend pars [Parameter new x[= {$i+1}] 1.0 -lowlim -100 -uplim 100]
    }
    set pdata {}
} -body {
    set optimizer  [LBFGS new -funct sample -pdata $pdata]
    $optimizer addPars {*}$pars
    $optimizer run
} -result "" -cleanup {
    unset pdata
}

