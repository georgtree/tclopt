package require tcltest
namespace import ::tcltest::*
set dir [file normalize [file dirname [info script]]]

package require tclopt
configure {*}$argv -testdir $dir
runAllTests
