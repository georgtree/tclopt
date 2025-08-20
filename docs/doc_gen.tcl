
set path_to_hl_tcl "/home/georgtree/tcl/hl_tcl"
#package require ruff
source /home/georgtree/ruff/src/ruff.tcl
package require fileutil
source [file join $path_to_hl_tcl hl_tcl_html.tcl]

set docDir [file dirname [file normalize [info script]]]
set sourceDir "${docDir}/../"
source [file join $docDir startPage.ruff]
source [file join $docDir examples.ruff]
source [file join $sourceDir tclopt.tcl]

set packageVersion [package versions tclopt]
set title "Tcl wrapper for C optimization procedures"
set commonHtml [list -title $title -sortnamespaces false -preamble $startPage -pagesplit namespace -recurse false\
                        -includesource true -pagesplit namespace -autopunctuate true -compact false\
                        -excludeprocs {^[A-Z].*} -includeprivate false -product tclopt\
                        -diagrammer "ditaa --border-width 1" -version $packageVersion -copyright "George Yashin"\
                        -excludeprocs {^(?!(mpfit|parCreate)$).*$} {*}$::argv]
set commonNroff [list -title $title -sortnamespaces false -preamble $startPage -pagesplit namespace -recurse false\
                         -pagesplit namespace -autopunctuate true -compact true -includeprivate false \
                         -excludeprocs {^[A-Z].*} -product tclopt -diagrammer "ditaa --border-width 1"\
                         -version $packageVersion -copyright "George Yashin"\
                         -excludeprocs {^(?!(mpfit|parCreate)$).*$} {*}$::argv]
set namespaces [list Examples ::tclopt]

if {[llength $argv] == 0 || "html" in $argv} {
    ruff::document $namespaces -format html -outdir $docDir -outfile index.html {*}$commonHtml
    ruff::document $namespaces -format nroff -outdir $docDir -outfile tclopt.n {*}$commonNroff
}

# add new command keywords to hl_tcl
lappend ::hl_tcl::my::data(CMD_TCL) {*}{Parameter ParameterMpfit Mpfit DE GSA}
set ::hl_tcl::my::data(CMD_TCL) [lsort $::hl_tcl::my::data(CMD_TCL)]

foreach file [glob ${docDir}/*.html] {
    ::hl_tcl_html::highlight $file no \
        {<pre class='ruff'>} </pre> \
        <div id='*' class='ruff_dyn_src'><pre> </pre> \
        <code> </code>  
}

# change default width
proc processContentsCss {fileContents} {
    return [string map [list max-width:60rem max-width:100rem "overflow-wrap:break-word" "overflow-wrap:normal"]\
                    $fileContents]
}
# change default theme 
proc processContentsJs {fileContents} {
    return [string map {init()\{currentTheme=localStorage.ruff_theme init()\{currentTheme=currentTheme="v1"}\
                    $fileContents]
}

fileutil::updateInPlace [file join $docDir assets ruff-min.css] processContentsCss
fileutil::updateInPlace [file join $docDir assets ruff-min.js] processContentsJs

proc processContents {fileContents} {
    global path chartsMap
    dict for {mark file} $chartsMap {
        set fileData [fileutil::cat [file join $path $file]]
        set fileContents [string map [list $mark $fileData] $fileContents]
    }
    return $fileContents
}
set tableWrapping {
    .ruff-bd table.ruff_deflist th:first-child,
    .ruff-bd table.ruff_deflist td:first-child {
        white-space: nowrap;      /* never wrap */
        overflow-wrap: normal;
        word-break: normal;
    }
}
::fileutil::appendToFile [file join $docDir assets ruff-min.css] $tableWrapping

set chartsMap [dcreate !ticklechart_mark_sinfit! sinfit.html]
set path [file join $docDir .. examples html_charts]
fileutil::updateInPlace [file join $docDir index-Examples.html] processContents
