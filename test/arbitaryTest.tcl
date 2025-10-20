package require tcltest
namespace import ::tcltest::*
package require math::constants
::math::constants::constants pi
package require tclopt
namespace import ::tclopt::*
set dir [file normalize [file dirname [info script]]]
source [file join $dir benchmarkFuncs.tcl]
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
testConstraint isInterface false
testConstraint isOptimization true
testConstraint isDE true
testConstraint isMpfit true
testConstraint isGSA true
testConstraint isLBFGS false
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

test MpfitClassTest-4 {test procedure of optimization of gaussian function without constraints}\
        -constraints {isOptimization isMpfit} -setup {
    proc gaussfunc {xall pdata args} {
        set x [dget $pdata x]
        set y [dget $pdata y]
        set ey [dget $pdata ey]
        set sig2 [= {[@ $xall 3]*[@ $xall 3]}]
        foreach xVal $x yVal $y eyVal $ey {
            set xc [= {$xVal-[@ $xall 2]}]
            lappend fvec [= {($yVal-[@ $xall 1]*exp(-0.5*$xc*$xc/$sig2)-[@ $xall 0])/$eyVal}]
        }
        return [dcreate fvec $fvec]
    }
    set x [list -1.7237128 1.8712276 -9.6608055E-01 -2.8394297e-01 1.3416969 1.3757038E+00 -1.3703436 4.2581975e-02\
                   -1.4970151E-01 8.2065094E-01]
    set y [list -4.4494256e-02 8.7324673e-01 7.4443483e-01 4.7631559 1.7187297e-01 1.1639182e-01 1.5646480 5.2322268\
                   4.2543168 6.2792623e-01]
    set ey [list 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
    set pdata [dcreate x $x y $y ey $ey]
    set par0 [ParameterMpfit new a 0.0]
    set par1 [ParameterMpfit new b 1.0]
    set par2 [ParameterMpfit new c 1.0]
    set par3 [ParameterMpfit new d 1.0]
    set optimizer [Mpfit new -funct gaussfunc -m 10 -pdata $pdata -refresh 5 -debug]
    $optimizer addPars $par0 $par1 $par2 $par3
} -body {
    $optimizer run
} -result "bestnorm 10.35003196129402 orignorm 231.5337319086307 status {Convergence in chi-square value} niter 28 nfev 139 npar 4 nfree 4 npegged 0 nfunc 10 resid {-1.0513418422715526 0.7855408029824684 -0.1591719970348312 0.7716056139751757 -0.6348701753127879 -0.7411596196260082 2.127827285979557 0.7150067330901377 -1.3376413435157142 -0.47579545836728143} xerror {0.2322349318512935 0.3954344774076113 0.07471490962927792 0.08999568304321699} x {0.4804433588338699 4.550752473146258 -0.06256246342568401 0.3974717357243806} debug {Best-so-far norm value=1.10196e+02
Parameter a=-1.11384e+00
Parameter b=3.36794e+00
Parameter c=-2.71620e-01
Parameter d=2.00009e+00
Iteration=5 NFEs=22
Best-so-far norm value=3.02360e+02
Parameter a=-7.89301e-01
Parameter b=3.94695e+00
Parameter c=1.20898e-01
Parameter d=1.22340e-01
Iteration=5 NFEs=27
Best-so-far norm value=1.11733e+01
Parameter a=5.76153e-01
Parameter b=4.32933e+00
Parameter c=-1.17529e-01
Parameter d=3.96908e-01
Iteration=10 NFEs=48
Best-so-far norm value=1.03505e+01
Parameter a=4.78602e-01
Parameter b=4.55158e+00
Parameter c=-6.18340e-02
Parameter d=3.98816e-01
Iteration=15 NFEs=73
Best-so-far norm value=1.03500e+01
Parameter a=4.80530e-01
Parameter b=4.55070e+00
Parameter c=-6.26002e-02
Parameter d=3.97410e-01
Iteration=20 NFEs=98
Best-so-far norm value=1.03500e+01
Parameter a=4.80438e-01
Parameter b=4.55076e+00
Parameter c=-6.25603e-02
Parameter d=3.97475e-01
Iteration=25 NFEs=123} covar {0.05393306357197494 -0.04311191421319561 -0.0015901067733744695 -0.011515875954133913 -0.04311191421319561 0.15636842592263064 0.008672077997269465 0.0006258000745328732 -0.0015901067733744695 0.008672077997269465 0.0055823177209111665 0.0011685192745230502 -0.011515875954133913 0.0006258000745328732 0.0011685192745230502 0.008099222966415173}" -cleanup {
    $par0 destroy
    $par1 destroy
    $par2 destroy
    $par3 destroy
    $optimizer destroy
    unset x y ey pdata par0 par1 par2 par3 optimizer
    rename gaussfunc ""
}


test GSAClassTest-9 {test procedure of optimization with debug info} -constraints {isOptimization isGSA} -setup {
    set pdata {}
    for {set i 0} {$i<2} {incr i} {
        lappend pars [Parameter new x$i 0.0 -lowlim -5.12 -uplim 5.12]
    }
    set optimizer [GSA new -funct fRastrigin -pdata $pdata -seed 1 -threshold 1e-6 -debug -refresh 5]
    $optimizer addPars {*}$pars
} -body {
    $optimizer run
} -result "objfunc 1.976976697903865e-7 x {3.070811904049009e-5 7.315134586072247e-6} nfev 20591 temp0 13.990784556992347 tempend 0.11175377483544734 info {Optimization stopped due to reaching threshold of objective function '1e-6'} niter 30 debug {{Best-so-far cost funct. value=1.06539e+01
Parameter x0=1.12377e+00
Parameter x1=1.92743e-01
Iteration=5 NFEs=51 Temperature: 1.68463e+00
NT=10 F=1.06539e+01 Acc. ratio=0.00000e+00} {Best-so-far cost funct. value=3.10514e-01
Parameter x0=1.40780e-02
Parameter x1=3.70574e-02
Iteration=10 NFEs=741 Temperature: 6.08924e-01
NT=408 F=3.10514e-01 Acc. ratio=7.35294e-03} {Best-so-far cost funct. value=1.51305e-02
Parameter x0=6.00857e-03
Parameter x1=-6.33817e-03
Iteration=15 NFEs=5606 Temperature: 3.28713e-01
NT=999 F=1.51305e-02 Acc. ratio=0.00000e+00} {Best-so-far cost funct. value=1.33275e-03
Parameter x0=1.17157e-03
Parameter x1=-2.31199e-03
Iteration=20 NFEs=10601 Temperature: 2.10740e-01
NT=999 F=1.33275e-03 Acc. ratio=2.00200e-03} {Best-so-far cost funct. value=4.79323e-05
Parameter x0=4.73360e-04
Parameter x1=1.32418e-04
Iteration=25 NFEs=15596 Temperature: 1.48786e-01
NT=999 F=4.79323e-05 Acc. ratio=1.60160e-02} {Best-so-far cost funct. value=1.97698e-07
Parameter x0=3.07081e-05
Parameter x1=7.31513e-06
Iteration=30 NFEs=20591 Temperature: 1.11754e-01
NT=999 F=1.97698e-07 Acc. ratio=6.70671e-02}}" -cleanup {
    foreach par $pars {
        $par destroy
    }
    unset pdata optimizer pars
}

test DEClassTest-20 {test procedure of optimization with debug info} -constraints {isOptimization isDE} -setup {
    set pdata {}
    for {set i 0} {$i<2} {incr i} {
        lappend pars [Parameter new x$i 0.0 -lowlim -100 -uplim 100]
    }
    set optimizer [DE new -funct fSphere -pdata $pdata -strategy rand/1/exp -genmax 3000 -refresh 20\
                           -np 60 -f 0.9 -cr 1 -seed 1 -debug]
    $optimizer addPars {*}$pars
} -body {
    $optimizer run
} -result "objfunc 2.4499833209600593e-8 x {-0.00011381148119937734 0.00010745594425998206} generation 73 nfev 4440 strategy rand/1/exp std 9.97713331150614e-7 info {Optimization stopped due to crossing threshold 'abstol+reltol*abs(mean)=1.0126350697028596e-6' of objective function population member standard deviation} debug {{Best-so-far cost funct. value=4.04677e-01
Parameter x0=5.07782e-01
Parameter x1=-3.83190e-01
Generation=20 NFEs=1260 Strategy: rand/1/exp
NP=60 F=0.9  CR=1    std=1.42775e+01} {Best-so-far cost funct. value=5.49325e-04
Parameter x0=2.34316e-02
Parameter x1=-5.35185e-04
Generation=40 NFEs=2460 Strategy: rand/1/exp
NP=60 F=0.9  CR=1    std=3.14171e-02} {Best-so-far cost funct. value=2.58261e-06
Parameter x0=6.21152e-04
Parameter x1=1.48215e-03
Generation=60 NFEs=3660 Strategy: rand/1/exp
NP=60 F=0.9  CR=1    std=1.31350e-04}}" -cleanup {
    foreach par $pars {
        $par destroy
    }
    unset pdata optimizer pars
}
