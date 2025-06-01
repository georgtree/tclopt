
set path_to_hl_tcl "/home/georgtree/tcl/hl_tcl"
package require ruff
package require fileutil
package require hl_tcl
set docDir [file dirname [file normalize [info script]]]
set sourceDir "${docDir}/../"
source [file join $docDir startPage.ruff]
source [file join $docDir examples.ruff]
source [file join $sourceDir tclopt.tcl]

set packageVersion [package versions tclopt]
set title "Tcl wrapper for C optimization procedures"
set commonHtml [list -title $title -sortnamespaces false -preamble $startPage -pagesplit namespace -recurse false\
                        -includesource true -pagesplit namespace -autopunctuate true -compact true\
                        -includeprivate false -product tclopt -diagrammer "ditaa --border-width 1"\
                        -version $packageVersion -copyright "George Yashin" -excludeprocs {^(?!(mpfit|parCreate)$).*$}\
                        {*}$::argv]
set commonNroff [list -title $title -sortnamespaces false -preamble $startPage -pagesplit namespace -recurse false\
                         -pagesplit namespace -autopunctuate true -compact true -includeprivate false\
                         -product tclopt -diagrammer "ditaa --border-width 1" -version $packageVersion\
                         -copyright "George Yashin" -excludeprocs {^(?!(mpfit|parCreate)$).*$} {*}$::argv]
set namespaces [list Examples ::tclopt]

if {[llength $argv] == 0 || "html" in $argv} {
    ruff::document $namespaces -format html -outdir $docDir -outfile index.html {*}$commonHtml
    ruff::document $namespaces -format nroff -outdir $docDir -outfile tclopt.n {*}$commonNroff
}

foreach file [glob ${docDir}/*.html] {
    exec tclsh "${path_to_hl_tcl}/tcl_html.tcl" [file join ${docDir} $file]
}

# change default width
proc processContentsCss {fileContents} {
    return [string map {max-width:60rem max-width:100rem} $fileContents]
}
# change default theme 
proc processContentsJs {fileContents} {
    return [string map {init()\{currentTheme=localStorage.ruff_theme init()\{currentTheme=currentTheme="dark"}\
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

set chartsMap [dcreate !ticklechart_mark_sinfit! sinfit.html]
set path [file join $docDir .. examples html_charts]
fileutil::updateInPlace [file join $docDir index-Examples.html] processContents
