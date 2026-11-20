# Run from the repository root in an x64 Visual Studio developer PowerShell.
# Tcl and Tcllib source trees must be at _ci/tcl and _ci/tcllib.
$ErrorActionPreference = 'Stop'
$root = (Get-Location).Path
$prefix = Join-Path $root '_ci/runtime'
function Invoke-Nmake {
    & nmake /nologo /f makefile.vc @args
    if ($LASTEXITCODE -ne 0) { throw "nmake failed with exit code $LASTEXITCODE" }
}
if ($env:RUNTIME_CACHE_HIT -eq 'true') {
    Write-Host "Using cached Tcl runtime"
} else {
    Push-Location _ci/tcl/win
    try {
        Invoke-Nmake "INSTALLDIR=$prefix" release
        Invoke-Nmake "INSTALLDIR=$prefix" install
    } finally { Pop-Location }
}

$out = Join-Path $root '_ci/opt-build'
$env:Path = "$prefix\bin;" + $env:Path
$extexpr = Join-Path $root '_ci/extexpr-package'
if ($env:EXTEXPR_CACHE_HIT -ne 'true') {
    $src = Join-Path $root '_ci/extexpr/win'
    New-Item -ItemType Directory -Force $src | Out-Null
    Copy-Item win/rules*.vc, win/targets.vc, win/nmakehlp.c $src
    Copy-Item .github/scripts/extexpr.vc "$src/makefile.vc"
    Push-Location $src
    try {
        Invoke-Nmake "INSTALLDIR=$prefix" "OUT_DIR=$extexpr" "TMP_DIR=$root\_ci\extexpr-obj"
    } finally { Pop-Location }
}
$pkg = (Get-Content "$root/_ci/argparse/package-dir.txt" -Raw).Trim()
$paths = @($out, "$root/_ci/argparse/lib/$pkg", $extexpr, "$root/_ci/tcllib/modules")
$env:TCLLIBPATH = ($paths | ForEach-Object { '{' + $_.Replace('\','/') + '}' }) -join ' '
$buildArgs = @("INSTALLDIR=$prefix", "TCLDIR=$prefix", "OUT_DIR=$out", "TMP_DIR=$out\obj")
Push-Location win
try {
    Invoke-Nmake @buildArgs
    Invoke-Nmake @buildArgs test
    if ($env:MAKE_DIST -ne 'true') { return }
    Invoke-Nmake @buildArgs "DIST_ROOT=$root\_ci\dist" dist-zip
} finally { Pop-Location }

$archive = Get-Item "$root/_ci/dist/*.zip"
New-Item -ItemType Directory -Force "$root/_ci/package" | Out-Null
Copy-Item $archive.FullName "$root/_ci/package/$($archive.BaseName)-windows-msvc.zip"
