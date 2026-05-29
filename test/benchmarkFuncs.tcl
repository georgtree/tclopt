 
### Sphere function
proc fSphere {x pdata} {
    #          n     
    #        ____    
    #        ╲       
    #   →     ╲     2
    # f(x) =  ╱    x 
    #        ╱      i
    #        ‾‾‾‾    
    #        i = 1   
    # Domain: x ∈ [-100, 100]
    #          i             
    # Global min: f(0,...,0)=0
    set sum 0.0
    foreach xi $x {
        set sum [expr {$sum+$xi*$xi}]
    }
    return $sum
}

### Rosenbrock (Banana) function
proc fRosenbrock {x pdata} {
    #        n - 1                                 
    #        ____                                  
    #        ╲                        2            
    #   →     ╲          ⎛          2⎞            2
    # f(x) =  ╱    100 ⋅ ⎜x      - x ⎟  + ⎛1 - x ⎞ 
    #        ╱           ⎝ i + 1    i⎠    ⎝     i⎠ 
    #        ‾‾‾‾                                  
    #        i = 1                                  
    # Domain: x ∈ [-30, 30]
    #          i             
    # Global min: f(1,...,1)=0
    set sum 0.0
    for {set i 0} {$i < [expr {[llength $x]-1}]} {incr i} {
        set xi  [lindex $x $i]
        set xip [lindex $x [expr {$i+1}]]
        set term1 [expr {100.0*($xip-$xi*$xi)*($xip-$xi*$xi)}]
        set term2 [expr {(1.0-$xi)*(1.0-$xi)}]
        set sum [expr {$sum+$term1+$term2}]
    }
    return $sum
}

### Rastrigin function
proc fRastrigin {x pdata} {
    #                   n                             
    #                 ____                            
    #                 ╲                               
    #   →              ╲     2                        
    # f(x) = 10 ⋅ n +  ╱    x  - 10 ⋅ cos ⎛2 ⋅ π ⋅ x ⎞
    #                 ╱      i            ⎝         i⎠
    #                 ‾‾‾‾                            
    #                 i = 1                           
    # Domain: x ∈ [-5.12, 5.12]
    #          i             
    # Global min: f(0,...,0)=0
    set n [llength $x]
    set sum 0.0
    foreach xi $x {
        set sum [expr {$sum+$xi*$xi-10.0*cos(2.0*acos(-1.0)*$xi)}]
    }
    return [expr {10.0*$n + $sum}]
}

### Ackley function
proc fAckley {x pdata} {
    #                  ⎛                ____________⎞                                            
    #                  ⎜               ╱      n     ⎟                                            
    #                  ⎜              ╱     ____    ⎟                                            
    #                  ⎜             ╱      ╲       ⎟       ⎛      n                   ⎞         
    #   →              ⎜            ╱   1    ╲     2⎟       ⎜1    ___                  ⎟         
    # f(x) = -20 ⋅ exp ⎜-0.2 ⋅     ╱    ─ ⋅  ╱    x ⎟ - exp ⎜─ ⋅  ╲    cos ⎛2 ⋅ π ⋅ x ⎞⎟ + 20 + e
    #                  ⎜          ╱     n   ╱      i⎟       ⎜n    ╱        ⎝         i⎠⎟         
    #                  ⎜         ╱          ‾‾‾‾    ⎟       ⎜     ‾‾‾                  ⎟         
    #                  ⎝       ╲╱           i = 1   ⎠       ⎝    i = 1                 ⎠         
    # Domain: x ∈ [-32.768, 32.768]
    #          i             
    # Global min: f(0,...,0)=0
    set n [llength $x]
    set sum1 0.0
    set sum2 0.0
    foreach xi $x {
        set sum1 [expr {$sum1+$xi*$xi}]
        set sum2 [expr {$sum2+cos(2.0*acos(-1.0)*$xi)}]
    }
    set term1 [expr {-20.0*exp(-0.2*sqrt($sum1/$n))}]
    set term2 [expr {-exp($sum2/double($n))}]
    return [expr {$term1+$term2+20.0+exp(1.0)}]
}

### Griewank function
proc fGriewank {x pdata} {
    #                     n                   
    #                   ____          n       
    #                   ╲          ━┳━━━┳━ x  
    #   →          1     ╲     2    ┃   ┃   i 
    # f(x) = 1 + ──── ⋅  ╱    x  -  ┃   ┃  ───
    #            4000   ╱      i    ┃   ┃    _
    #                   ‾‾‾‾        ┃   ┃  ╲╱i
    #                   i = 1       i = 1     
    # Domain: x ∈ [-600, 600]
    #          i             
    # Global min: f(0,...,0)=0
    set sum 0.0
    set prod 1.0
    for {set i 0} {$i < [llength $x]} {incr i} {
        set xi [lindex $x $i]
        set sum [expr {$sum + $xi*$xi}]
        set prod [expr {$prod*cos($xi/sqrt($i+1.0))}]
    }
    return [expr {1.0 + $sum/4000.0 - $prod}]
}

### Schwefel function
proc fSchwefel {x pdata} {
    #                         n                     
    #                       ____                    
    #                       ╲                       
    #   →                    ╲             ⎛   ____⎞
    # f(x) = 418.9829 ⋅ n -  ╱    x  ⋅ sin ⎜  ╱|x |⎟
    #                       ╱      i       ⎝╲╱ | i|⎠
    #                       ‾‾‾‾                    
    #                       i = 1                   
    # Domain: x ∈ [-500, 500]
    #          i             
    # Global min: f(420.9687,...,420.9687)=0
    set n [llength $x]
    set sum 0.0
    foreach xi $x {
        set sum [expr {$sum+$xi*sin(sqrt(abs($xi)))}]
    }
    return [expr {418.9829*$n - $sum}]
}

### Linear function
proc linfunc {xall pdata args} {
    set x [dict get $pdata x]
    set y [dict get $pdata y]
    set ey [dict get $pdata ey]
    foreach xVal $x yVal $y eyVal $ey {
        set f [expr {[lindex $xall 0]+[lindex $xall 1]*$xVal}]
        lappend fval [expr {($yVal-$f)/$eyVal}]
    }
    return [dict create fvec $fval]
}

### Quadratic function
proc quadfunc {xall pdata args} {
    set x [dict get $pdata x]
    set y [dict get $pdata y]
    set ey [dict get $pdata ey]
    foreach xVal $x yVal $y eyVal $ey {
        lappend fvec [expr {($yVal-[lindex $xall 0]-[lindex $xall 1]*$xVal-[lindex $xall 2]*$xVal*$xVal)/$eyVal}]
    }
    return [dict create fvec $fvec]
}

proc quadfuncDebug {xall pdata args} {
    set x [dict get $pdata x]
    set y [dict get $pdata y]
    set ey [dict get $pdata ey]
    foreach xVal $x yVal $y eyVal $ey {
        lappend fvec [expr {($yVal-[lindex $xall 0]-[lindex $xall 1]*$xVal-[lindex $xall 2]*$xVal*$xVal)/$eyVal}]
    }
    if {[lindex $args 0]!=""} {
        set derivs [lindex $args 0]
        foreach deriv $derivs {
            if {$deriv==0} {
                foreach xVal $x yVal $y eyVal $ey {
                    lappend dvec [expr {-1/$eyVal}]
                }   
            }
            if {$deriv==1} {
                foreach xVal $x yVal $y eyVal $ey {
                    lappend dvec [expr {(-$xVal)/$eyVal}]
                }
            }
            if {$deriv==2} {
                foreach xVal $x yVal $y eyVal $ey {
                    lappend dvec [expr {(-$xVal*$xVal)/$eyVal}]
                }
            }
        }
        return [dict create fvec $fvec dvec $dvec]
    } else {
        return [dict create fvec $fvec]
    }
}

### Gauss function
proc gaussfunc {xall pdata args} {
    set x [dict get $pdata x]
    set y [dict get $pdata y]
    set ey [dict get $pdata ey]
    set sig2 [expr {[lindex $xall 3]*[lindex $xall 3]}]
    foreach xVal $x yVal $y eyVal $ey {
        set xc [expr {$xVal-[lindex $xall 2]}]
        lappend fvec [expr {($yVal-[lindex $xall 1]*exp(-0.5*$xc*$xc/$sig2)-[lindex $xall 0])/$eyVal}]
    }
    return [dict create fvec $fvec]
}

### Polynomial function
proc polyfunc {pars pdata args} {
    set m 60
    set dx $m
    set dx [expr {double(2.0/$dx)}]
    set d [llength $pars]
    set x -1
    set result 0.0
    for {set i 0} {$i<=$m} {incr i} {
        set px [lindex $pars 0]
        for {set j 1} {$j<$d} {incr j} {
            set px [expr {$x*$px+[lindex $pars $j]}]
        }
        if {($px < -1) || ($px > 1)} {
            set result [expr {$result+(1.0-$px)*(1.0-$px)}]
        }
        set x [expr {$x+$dx}]
    }
    set px [lindex $pars 0]
    for {set j 1} {$j<$d} {incr j} {
        set px [expr {1.2*$px+[lindex $pars $j]}]
    }
    set px [expr {$px-72.661}]
    if {$px<0} {
        set result [expr {$result+$px*$px}]
    }
    set px [lindex $pars 0]
    for {set j 1} {$j<$d} {incr j} {
        set px [expr {-1.2*$px+[lindex $pars $j]}]
    }
    set px [expr {$px-72.661}]
    if {$px<0} {
        set result [expr {$result+$px*$px}]
    }
    return $result
}

### Rosenbrock (Banana) function with gradient
proc rosenbrockWithGradient {x pdata args} {
    set fx 0.0
    set g [lrepeat [expr {[llength $x]}] 0.0]
    for {set i 0} {$i<[llength $x]} {incr i 2} {
        set t1 [expr {1.0-li($x,$i)}]
        set t2 [expr {10.0*(li($x,$i+1)-li($x,$i)*li($x,$i))}]
        lset g [expr {$i+1}] [expr {20.0*$t2}]
        lset g $i [expr {-2.0*(li($x,$i)*li($g,$i+1)+$t1)}]
        set fx [expr {$fx+($t1*$t1+$t2*$t2)}]
    }
    return [dict create f $fx dvec $g]
}
