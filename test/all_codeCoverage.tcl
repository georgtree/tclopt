package require tcltest
namespace import ::tcltest::*
set dir [file normalize [file dirname [info script]]]

load [file join $dir .. libtcl9tclopt0.2.so]
source [file join $dir .. tclopt.tcl]
source [file join $dir test.test]
