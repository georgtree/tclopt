# docs-begin packages-import
package require tclopt
package require ticklecharts
namespace import ::tcl::mathfunc::*
namespace import ::tclopt::*
# docs-end packages-import

# docs-begin set-seed
srand 10
# docs-end set-seed
# generate random number in the range [min,max]
# docs-begin random-gen-proc
proc randFloat {min max} {
    return [expr {rand()*($max-$min)+$min}]
}
# docs-end random-gen-proc

# docs-begin data-generation
for {set i 0} {$i<100} {incr i} {
    set xi [expr {$i*0.01}]
    lappend x $xi
    lappend y [expr {[randFloat 0.9 1.1]*(sin(1.5*$xi)+sin(11*$xi)+sin(6*$xi))}]
}
set pdata [dict create x $x y $y]
# docs-end data-generation

# docs-begin sinfunc-proc
proc sinfunc {xall pdata args} {
    set x [dict get $pdata x]
    set y [dict get $pdata y]
    foreach xVal $x yVal $y {
        set f [expr {sin([lindex $xall 0]*$xVal)+sin([lindex $xall 1]*$xVal)+sin([lindex $xall 2]*$xVal)}]
        lappend fvec [expr {$yVal-$f}]
        lappend fval $f
    }
    return [dict create fvec $fvec fval $fval]
}
# docs-end sinfunc-proc
# docs-begin create-parameters
set xInitial [list 3.0 8.0 1.0]
set par0 [ParameterMpfit new a [lindex $xInitial 0] -lowlim 0 -uplim 20]
set par1 [ParameterMpfit new b [lindex $xInitial 1] -lowlim 0 -uplim 20]
set par2 [ParameterMpfit new c [lindex $xInitial 2] -lowlim 0 -uplim 20]
# docs-end create-parameters

# docs-begin create-optimizer
set optimizer [Mpfit new -funct sinfunc -m 100 -pdata $pdata]
$optimizer addPars $par0 $par1 $par2
# docs-end create-optimizer
# docs-begin run-optimizer
set result [$optimizer run]
set yinitial [dict get [sinfunc $xInitial $pdata] fval]
set yfinal [dict get [sinfunc [dict get $result x] $pdata] fval]
# docs-end run-optimizer

# docs-begin print-results
puts "Chi^2 final: [format %3f [dict get $result bestnorm]]"
puts "Chi^2 initial: [format %3f [dict get $result orignorm]]"
puts "number of interations: [dict get $result niter]"
puts "number of function evaluation: [dict get $result nfev]"
set i -1
foreach xerr [dict get $result xerror] xVal [dict get $result x] xini $xInitial {
    puts [format "p[incr i]: %.3f ± %.3f" $xVal $xerr]
}
# docs-end print-results

# docs-begin plot-ticklecharts
set chart [ticklecharts::chart new]
$chart Xaxis -name x -minorTick {show True} -min 0 -max 1 -type value -splitLine {show True}
$chart Yaxis -name y -minorTick {show True} -min 0 -max 2.5 -type value -splitLine {show True}
$chart SetOptions -title {} -legend {} -tooltip {trigger axis} -animation False\
        -toolbox {feature {dataZoom {yAxisIndex none}}}
$chart Add lineSeries -data [lmap xVal $x yVal $y {list $xVal $yVal}] -showAllSymbol nothing -name Data
$chart Add lineSeries -data [lmap xVal $x yVal $yinitial {list $xVal $yVal}] -showAllSymbol nothing -name Initial
$chart Add lineSeries -data [lmap xVal $x yVal $yfinal {list $xVal $yVal}] -showAllSymbol nothing -name Fitted

set fbasename [file rootname [file tail [info script]]]
$chart Render -outfile [file normalize [file join html_charts $fbasename.html]] -width 800px -height 500px\
        -divid $fbasename -jschartvar chart_$fbasename -jsvar option_$fbasename
# docs-end plot-ticklecharts
