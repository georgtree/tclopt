package require tclopt
package require ticklecharts
namespace import ::tcl::mathfunc::*
namespace import ::tclopt::*

# generate random number in the range [min,max]
proc randFloat {min max} {
    return [expr {rand()*($max-$min)+$min}]
}
srand 10
for {set i 0} {$i<100} {incr i} {
    set xi [= {$i*0.01}]
    lappend x $xi
    lappend y [= {[randFloat 0.9 1.1]*(sin(1.5*$xi)+sin(11*$xi)+sin(6*$xi))}]
}
set pdata [dcreate x $x y $y]

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
set xInitial [list 3.0 8.0 1.0]
set par0 [ParameterMpfit new a [@ $xInitial 0] -lowlim 0 -uplim 20]
set par1 [ParameterMpfit new b [@ $xInitial 1] -lowlim 0 -uplim 20]
set par2 [ParameterMpfit new c [@ $xInitial 2] -lowlim 0 -uplim 20]

set optimizer [Mpfit new -funct sinfunc -m 100 -pdata $pdata]
$optimizer addPars $par0 $par1 $par2
set result [$optimizer run]
set yinitial [dget [sinfunc $xInitial $pdata] fval]
set yfinal [dget [sinfunc [dget $result x] $pdata] fval]

puts "Chi^2 final: [format "%3f" [dget $result bestnorm]]"
puts "Chi^2 initial: [format "%3f" [dget $result orignorm]]"
puts "number of interations: [dget $result niter]"
puts "number of function evaluation: [dget $result nfev]"
set i -1
foreach xerr [dget $result xerror] xVal [dget $result x] xini $xInitial {
    puts [format "p[incr i]: %.3f ± %.3f" $xVal $xerr]
}

set chart [ticklecharts::chart new]
$chart Xaxis -name "x" -minorTick {show "True"} -min 0 -max 1 -type "value" -splitLine {show "True"}
$chart Yaxis -name "y" -minorTick {show "True"} -min 0 -max 2.5 -type "value" -splitLine {show "True"}
$chart SetOptions -title {} -legend {} -tooltip {trigger "axis"} -animation "False"\
        -toolbox {feature {dataZoom {yAxisIndex "none"}}}
$chart Add "lineSeries" -data [lmap xVal $x yVal $y {list $xVal $yVal}] -showAllSymbol "nothing" -name "Data"
$chart Add "lineSeries" -data [lmap xVal $x yVal $yinitial {list $xVal $yVal}] -showAllSymbol "nothing" -name "Initial"
$chart Add "lineSeries" -data [lmap xVal $x yVal $yfinal {list $xVal $yVal}] -showAllSymbol "nothing" -name "Fitted"

set fbasename [file rootname [file tail [info script]]]
$chart Render -outfile [file normalize [file join html_charts $fbasename.html]] -width 800px -height 500px
