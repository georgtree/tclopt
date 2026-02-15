
set path_to_hl_tcl "/home/georgtree/tcl/hl_tcl"
#package require ruff
source /home/georgtree/tcl/ruff/src/ruff.tcl
package require fileutil
source [file join $path_to_hl_tcl hl_tcl_html.tcl]

set docDir [file dirname [file normalize [info script]]]
set sourceDir "${docDir}/../"
source [file join $docDir startPage.ruff]
source [file join $docDir examples.ruff]
source [file join $sourceDir tclopt.tcl]

set packageVersion [package versions tclopt]
set title "Tcl wrapper for C optimization procedures"
set commonSphinx [list -title $title -sortnamespaces false -preamble $startPage -pagesplit namespace -recurse false\
                        -includesource false -pagesplit namespace -autopunctuate true -compact false\
                        -excludeprocs {^[A-Z].*} -includeprivate false -product tclopt -diagrammer\
                        "ditaa --border-width 1" -version $packageVersion -copyright "George Yashin" {*}$::argv]
set commonNroff [list -title $title -sortnamespaces false -preamble $startPage -pagesplit namespace -recurse false\
                         -pagesplit namespace -autopunctuate true -compact true -includeprivate false \
                         -excludeprocs {^[A-Z].*} -product tclopt -diagrammer "ditaa --border-width 1"\
                         -version $packageVersion -copyright "George Yashin" {*}$::argv]
set namespaces [list Examples ::tclopt]

if {[llength $argv] == 0 || "html" in $argv} {
    ruff::document $namespaces -format sphinx -outdir [file join $docDir sphinx] {*}$commonSphinx
    ruff::document $namespaces -format nroff -outdir $docDir -outfile tclopt.n {*}$commonNroff
}

::fileutil::appendToFile [file join $docDir sphinx conf.py] {html_theme = "classic"
extensions = [
    "sphinx.ext.githubpages",
]}
catch {exec sphinx-build -b html [file join $docDir sphinx] [file join $docDir]}


proc processContents {fileContents} {
    global path chartsMap
    dict for {mark file} $chartsMap {
        set fileData [fileutil::cat [file join $path $file]]
        set fileContents [string map [list $mark $fileData] $fileContents]
    }
    return $fileContents
}

set chartsMap [dcreate !ticklechart_mark_sinfit! sinfit.html !ticklechart_mark_diffEvolution_Rozenbrock!\
                       diffEvolution_Rozenbrock.html !ticklechart_mark_diffEvolution_Rozenbrock_plot!\
                       diffEvolution_Rozenbrock_plot.html !ticklechart_mark_genSimAnneal_Rozenbrock_plot!\
                       genSimAnneal_Rozenbrock_plot.html !ticklechart_mark_genSimAnneal_Rozenbrock!\
                       genSimAnneal_Rozenbrock.html !ticklechart_mark_LBFGS_Rozenbrock_plot!\
                       LBFGS_Rozenbrock_plot.html !ticklechart_mark_LBFGS_Rozenbrock! LBFGS_Rozenbrock.html]
set path [file join $docDir .. examples html_charts]
fileutil::updateInPlace [file join $docDir Examples-Examples.html] processContents
