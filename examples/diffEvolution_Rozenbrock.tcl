package require tclopt
package require ticklecharts
namespace import ::tcl::mathfunc::*
namespace import ::tclopt::*

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

# generate data for Rosenbrock surface
proc surface3dData {xrange yrange} {
    set data {}
    foreach t0 [lseq {*}$xrange] {
        set y $t0
        foreach t1 [lseq {*}$yrange] {
            set x $t1
            set z [fRosenbrock [list $x $y] {}]
            lappend data [list $x $y $z]
        }
    }
    return $data
}

# create two parameters
lappend pars [Parameter new x 1.0 -lowlim -30 -uplim 30]
lappend pars [Parameter new y 1.0 -lowlim -30 -uplim 30]

# setup Differential Evolution optimization
set optimizer [DE new -funct fRosenbrock -pdata {} -strategy rand/1/exp -genmax 3000 -np 60 -f 0.9 -cr 1 -seed 1\
                       -history -histfreq 1]
# add parameters
$optimizer addPars {*}$pars

# run optimization and get the best trajectory points
set results [$optimizer run]
set trajectory [dict get $results besttraj]
set bestf [dict get $results history]
foreach genTr $trajectory genF $bestf {
    lappend optData [list {*}[dict get $genTr x] [dict get $genF bestf]]
    lappend functionTrajectory [list [dict get $genTr gen] [dict get $genF bestf]]
}

# print results and final parameters
puts "Global minimum value of objective function: [format "%3e" [dget $results objfunc]]"
puts [format "x=%3f, y=%3f" {*}[dict get $results x]]
puts "Number of generation: [dget $results generation]"
puts "Number of function evaluation: [dget $results nfev]"
puts "Convergence info: [dget $results info]"

# plot Rosenbrock surface with best trajectory on the surface
set chart3D [ticklecharts::chart3D new]
$chart3D SetOptions -tooltip {} -grid3D {viewControl {} axisPointer {show false}}\
        -visualMap [list type "continuous" show "False" dimension 2 min 0 max 30000 seriesIndex {0}\
                             inRange [list color [list "#313695 #4575b4 #74add1 #abd9e9 #e0f3f8 #ffffbf #fee090\
                                                             #fdae61 #f46d43 #d73027 #a50026"]]]
$chart3D Xaxis3D -type "value" -name "x" -axisTick {show "True"} -show "True" -min -7 -max 7
$chart3D Yaxis3D -type "value" -name "y" -axisTick {show "True"} -show "True" -min -15 -max 30
$chart3D Zaxis3D -type "value" -name "z" -axisTick {show "True"} -show "True" 
set data [surface3dData {-15 30 0.1} {-7 7 0.1}]
$chart3D Add "surfaceSeries" -name "Rosenbrock surface" -wireframe {show "False"} -data $data -itemStyle {opacity 0.7}\
        -shading "lambert"
$chart3D Add "scatter3DSeries" -data $optData -itemStyle {color "#f79802" borderColor "#000000"} -symbolSize 4
$chart3D Add "line3DSeries" -data $optData -lineStyle {width 2 opacity 1} -silent "True"
set seFmt [ticklecharts::jsfunc new {
    function (p) { return p.dataIndex === 0 ? "start" : "end"; }
}]
$chart3D Add "scatter3DSeries" -coordinateSystem cartesian3D -data [list [lindex $optData 0] [lindex $optData end]]\
        -symbolSize 8 -itemStyle {color "#ff0000" borderColor "#000000"} \
        -label [dict create show true position top formatter $seFmt textStyle\
                        [dict create color black fontSize 12 fontWeight bold]]
set fbasename [file rootname [file tail [info script]]]
$chart3D Render -outfile [file normalize [file join html_charts $fbasename.html]] -width 800px -height 500px

# plot 2D trajectory
set chart [ticklecharts::chart new]
$chart Xaxis -name "Generation" -minorTick {show "True"} -type "value" -splitLine {show "True"}
$chart Yaxis -name "Rozenbrock function value" -minorTick {show "True"} -min 1e-9 -max 100 -type "log" -splitLine\
        {show "True"}
$chart SetOptions -title {} -tooltip {trigger "axis"} -animation "False" -toolbox\
        {feature {dataZoom {yAxisIndex "none"}}}
$chart Add "lineSeries" -name "Best trajectory" -data $functionTrajectory -showAllSymbol "nothing"
set fbasename [file rootname [file tail [info script]]]

$chart Render -outfile [file normalize [file join html_charts ${fbasename}_plot.html]] -width 800px -height 500px
