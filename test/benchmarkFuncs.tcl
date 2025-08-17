 
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
        set sum [= {$sum+$xi*$xi}]
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
    for {set i 0} {$i < [= {[llength $x]-1}]} {incr i} {
        set xi  [@ $x $i]
        set xip [@ $x [= {$i+1}]]
        set term1 [= {100.0*($xip-$xi*$xi)*($xip-$xi*$xi)}]
        set term2 [= {(1.0-$xi)*(1.0-$xi)}]
        set sum [= {$sum+$term1+$term2}]
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
        set sum [= {$sum+$xi*$xi-10.0*cos(2.0*acos(-1.0)*$xi)}]
    }
    return [= {10.0*$n + $sum}]
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
        set sum1 [= {$sum1+$xi*$xi}]
        set sum2 [= {$sum2+cos(2.0*acos(-1.0)*$xi)}]
    }
    set term1 [= {-20.0*exp(-0.2*sqrt($sum1/$n))}]
    set term2 [= {-exp($sum2/double($n))}]
    return [= {$term1+$term2+20.0+exp(1.0)}]
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
        set xi [@ $x $i]
        set sum [= {$sum + $xi*$xi}]
        set prod [= {$prod*cos($xi/sqrt($i+1.0))}]
    }
    return [= {1.0 + $sum/4000.0 - $prod}]
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
        set sum [= {$sum+$xi*sin(sqrt(abs($xi)))}]
    }
    return [= {418.9829*$n - $sum}]
}
