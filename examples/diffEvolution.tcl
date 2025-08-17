package require tclopt
package require ticklecharts
namespace import ::tcl::mathfunc::*
namespace import ::tclopt::*

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

proc floatRange {start end step} {
    set temp {}
    while 1 {
        if {$start < $end} {
            lappend temp $start
            set start [expr {$start + $step}]
        } else {
            break
        }
    }
    return $temp
}

proc surface3dData {} {
    set data {}
    foreach t0 [floatRange -5.12 5.12 0.05] {
        set y $t0
        foreach t1 [floatRange -5.12 5.12 0.05] {
            set x $t1
            set z [fRastrigin [list $x $y] {}]
            lappend data [list $x $y $z]
        }
    }
    return $data
}

set pdata {}
for {set i 0} {$i<2} {incr i} {
    lappend pars [Parameter new x$i 1.0 -lowlim -5.12 -uplim 5.12]
}
set optimizer [DE new -funct fRastrigin -pdata $pdata -strategy rand/1/exp -genmax 3000 -refresh 100\
                       -np 60 -f 0.9 -cr 1 -seed 1 -history -histfreq 1 -savepop]
$optimizer addPars {*}$pars
set results [$optimizer run]
set trajectory [dict get $results besttraj]
set bestf [dict get $results history]
foreach genTr $trajectory genF $bestf {
    lappend optData [list {*}[dict get $genTr x] [dict get $genF bestf]]
}


set chart3D [ticklecharts::chart3D new]
$chart3D SetOptions -tooltip {} \
        -grid3D {viewControl {}} \
        -visualMap  [list type "continuous" show "False" dimension 2 min 0 max 80 \
                             inRange [list color [list "#313695 #4575b4 #74add1 #abd9e9 #e0f3f8 #ffffbf #fee090\
                                                             #fdae61 #f46d43 #d73027 #a50026"]]]
$chart3D Xaxis3D -type "value" -name "x" -axisTick {show True} -show "True" -interval 1
$chart3D Yaxis3D -type "value" -name "y" -axisTick {show True} -show "True" -interval 1
$chart3D Zaxis3D -type "value" -name "z" -axisTick {show True} -show "True" -interval 5
set data [surface3dData]
$chart3D Add "surfaceSeries" -wireframe {show "False"} \
                             -data $data
$chart3D Add "line3DSeries" -data $optData -lineStyle {width 1 symbol circle}
set fbasename [file rootname [file tail [info script]]]
$chart3D Render -outfile [file normalize [file join html_charts $fbasename.html]] -height 600px
