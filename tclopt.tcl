package require argparse
package provide tclopt 0.1

interp alias {} dget {} dict get
interp alias {} @ {} lindex
interp alias {} = {} expr
interp alias {} dexist {} dict exists
interp alias {} dcreate {} dict create
interp alias {} dappend {} dict append

namespace eval ::tclopt {
    namespace import ::tcl::mathop::*
    namespace export qrfac lmpar mpfit parCreate

    # Double precision numeric constants
    variable MP_MACHEP0 2.2204460e-16
    variable MP_DWARF 2.2250739e-308
    variable MP_GIANT 1.7976931e+308

    # Error codes
    variable MP_ERR_INPUT 0; # General input parameter error 
    variable MP_ERR_NAN -16; # User function produced non-finite values 
    variable MP_ERR_FUNC -17; # No user function was supplied 
    variable MP_ERR_NPOINTS -18; # No user data points were supplied 
    variable MP_ERR_NFREE -19; # No free parameters 
    variable MP_ERR_MEMORY -20; # Memory allocation error 
    variable MP_ERR_INITBOUNDS -21; # Initial values inconsistent w constraints*/
    variable MP_ERR_BOUNDS -22; # Initial constraints inconsistent 
    variable MP_ERR_PARAM -23; # General input parameter error 
    variable MP_ERR_DOF -24; # Not enough degrees of freedom 
    # Potential success status codes
    variable MP_OK_CHI 1; # Convergence in chi-square value
    variable MP_OK_PAR 2; # Convergence in parameter value
    variable MP_OK_BOTH 3; # Both MP_OK_PAR and MP_OK_CHI hold
    variable MP_OK_DIR 4; # Convergence in orthogonality
    variable MP_MAXITER 5; # Maximum number of iterations reached
    variable MP_FTOL 6; # ftol is too small, no further improvement
    variable MP_XTOL 7; # xtol is too small, no further improvement
    variable MP_GTOL 8; # gtol is too small, no further improvement

}

proc ::tclopt::list2array {list} {
    # Create and initialize doubleArray object from the list
    #  list - list of values
    # Returns: array object
    set length [llength $list]
    set a [::tclopt::new_doubleArray $length]
    for {set i 0} {$i<$length} {incr i} {
        set iElem [@ $list $i]
        try {
            ::tclopt::doubleArray_setitem $a $i $iElem
        } on error {errmsg erropts} {
            if {[dget $erropts -errorcode]=="SWIG TypeError"} {
                error "List must contains only double elements, but get '$iElem'"
            } else {
                error "Array creation failed with message '$errmsg' and opts '$erropts'"
            }
        }    
    }
    return $a
}

proc ::tclopt::list2intArray {list} {
    # Create and initialize intArray object from the list
    #  list - list of values
    # Returns: array object
    set length [llength $list]
    set a [::tclopt::new_intArray $length]
    for {set i 0} {$i<$length} {incr i} {
        set iElem [@ $list $i]
        try {
            ::tclopt::intArray_setitem $a $i $iElem
        } on error {errmsg erropts} {
            if {[dget $erropts -errorcode]=="SWIG TypeError"} {
                error "List must contains only int elements, but get '$iElem'"
            } else {
                error "Array creation failed with message '$errmsg' and opts '$erropts'"
            }
        }    
    }
    return $a
}

proc ::tclopt::lists2arrays {varNames lists} {
    # Create and initialize doubleArray objects from lists, and set these objects to variables
    #  varNames - list of variables names
    #  lists - list of lists
    # Returns: variables with doubleArray objects are set in caller's scope
    if {[llength $varNames]!=[llength $lists]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of lists list '[llength $lists]'"
    }
    foreach varName $varNames list $lists {
        uplevel 1 [list set $varName [::tclopt::list2array $list]]
    }
    return
}

proc ::tclopt::lists2intArrays {varNames lists} {
    # Create and initialize intArray objects from lists, and set these objects to variables
    #  varNames - list of variables names
    #  lists - list of lists
    # Returns: variables with intArray objects are set in caller's scope
    if {[llength $varNames]!=[llength $lists]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of lists list '[llength $lists]'"
    }
    foreach varName $varNames list $lists {
        uplevel 1 [list set $varName [::tclopt::list2intArray $list]]
    }
    return
}

proc ::tclopt::array2list {array length} {
    # Create list from doubleArray object
    #  array - doubleArray object
    #  length - number of elements in doubleArray
    # Returns: list
    for {set i 0} {$i<$length} {incr i} {
        lappend list [::tclopt::doubleArray_getitem $array $i]
    }
    return $list
}

proc ::tclopt::arrayInt2list {array length} {
    # Create list from intArray object
    #  array - intArray object
    #  length - number of elements in intArray
    # Returns: list
    for {set i 0} {$i<$length} {incr i} {
        lappend list [::tclopt::intArray_getitem $array $i]
    }
    return $list
}

proc ::tclopt::arrays2lists {varNames arrays lengths} {
    # Create lists from doubleArray objects, and set these lists to variables
    #  varNames - list of variables names
    #  arrays - list of doubleArray
    #  lengths - list of doubleArray lengths
    # Returns: variables with lists are set in caller's scope
    if {[llength $varNames]!=[llength $arrays]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of array list '[llength $arrays]'"
    } elseif {[llength $varNames]!=[llength $lengths]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of lengths list\
                '[llength $lengths]'"
    }
    foreach varName $varNames array $arrays length $lengths {
        uplevel 1 [list set $varName [::tclopt::array2list $array $length]]
    }
    return
}

proc ::tclopt::arraysInt2lists {varNames arrays lengths} {
    # Create lists from intArray objects, and set these lists to variables
    #  varNames - list of variables names
    #  arrays - list of intArray
    #  lengths - list of intArray lengths
    # Returns: variables with lists are set in caller's scope
    if {[llength $varNames]!=[llength $arrays]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of array list '[llength $arrays]'"
    } elseif {[llength $varNames]!=[llength $lengths]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of lengths list\
                '[llength $lengths]'"
    }
    foreach varName $varNames array $arrays length $lengths {
        uplevel 1 [list set $varName [::tclopt::arrayInt2list $array $length]]
    }
    return
}

proc ::tclopt::newArrays {varNames lengths} {
    # Creates doubleArray objects, and set these objects to variables
    #  varNames - list of variables names
    #  lengths - list of doubleArray's lengths
    # Returns: variables with doubleArray objects are set in caller's scope
    if {[llength $varNames]!=[llength $lengths]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of lengths list\
                '[llength $lengths]'"
    }
    foreach varName $varNames length $lengths {
        uplevel 1 [list set $varName [::tclopt::new_doubleArray $length]]
    }
    return
}

proc ::tclopt::newIntArrays {varNames lengths} {
    # Creates intArray objects, and set these objects to variables
    #  varNames - list of variables names
    #  lengths - list of intArray's lengths
    # Returns: variables with intArray objects are set in caller's scope
    if {[llength $varNames]!=[llength $lengths]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of lengths list\
                '[llength $lengths]'"
    }
    foreach varName $varNames length $lengths {
        uplevel 1 [list set $varName [::tclopt::new_intArray $length]]
    }
    return
}

proc ::tclopt::newDoubleps {varNames} {
    # Creates doubleps objects, and set these objects to variables
    #  varNames - list of variables names
    # Returns: variables with doubleps objects are set in caller's scope
    foreach varName $varNames {
        uplevel 1 [list set $varName [::tclopt::new_doublep]]
    }
    return
}


proc ::tclopt::deleteArrays {args} {
    # Deletes doubleArray objects
    #  args - list of arrays objects
    foreach arg $args {
        ::tclopt::delete_doubleArray $arg
    }
    return
}

proc ::tclopt::deleteIntArrays {args} {
    # Deletes intArray objects
    #  args - list of arrays objects
    foreach arg $args {
        ::tclopt::delete_intArray $arg
    }
    return
}

proc ::tclopt::deleteDoubleps {args} {
    # Deletes doublep objects
    #  args - list of doublep objects
    foreach arg $args {
        ::tclopt::delete_doublep $arg
    }
    return
}

proc ::tclopt::duplListCheck {list} {
    # Checks if list contains duplicates.
    #  list - list to check
    # Returns: false if there are no duplicates and true if there are.
    set flag false
    set new {}
    foreach item $list {
        if {[lsearch $new $item] < 0} {
            lappend new $item
        } else {
            set flag true
            break
        }
    }
    return $flag
}


### Levenberg-Marquardt square-least fitting optimization

proc ::tclopt::qfrac {m n a lda pivot lipvt} {
    # Does QR factorization
    #  m - number of rows of matrix a
    #  n - number of columns of matrix a
    #  a - matrix of size m by n in form of 1d list [[column0] [column1] [column2] ... [columnn]]
    #  lda - leading dimension of a
    #  pivot - true for column pivoting enforcing
    #  lipvt - a positive integer input variable. if pivot is false,
    #	 then lipvt may be as small as 1. if pivot is true, then
    #	 lipvt must be at least n.
    # Returns: dictionary 
    # Synopsis: -x list -y list -xi list
    set aLen [llength $a]
    if {$aLen!=[= {$m*$n}]} {
        error "Length of a '$aLen' must be equal to m*n '[= {$m*$n}]'"
    }
    ::tclopt::lists2arrays [list aArray] [list $a]
    ::tclopt::newArrays [list rdiagArray acnormArray waArray] [list $n $n $n]
    ::tclopt::newIntArrays [list ipvtArray] [list $lipvt]
    mp_qrfac $m $n $aArray $lda $pivot $ipvtArray $lipvt $rdiagArray $acnormArray $waArray
    ::tclopt::arrays2lists [list aList rdiagList acnormList waList]\
            [list $aArray $rdiagArray $acnormArray $waArray] [list $aLen $n $n $n]
    ::tclopt::arraysInt2lists [list ipvtList] [list $ipvtArray] [list $lipvt]
    ::tclopt::deleteArrays $aArray $rdiagArray $acnormArray $waArray
    ::tclopt::deleteIntArrays $ipvtArray
    return [dcreate a $aList rdiag $rdiagList acnorm $acnormList wa $waList ipvt $ipvtList]
}

proc ::tclopt::enorm {x} {
    # Calculate euclidean norm of vector
    #  x - list with values of vector x
    # Returns: norm 
    # Synopsis: -x list -y list -xi list
    set xLen [llength $x]
    ::tclopt::lists2arrays [list xArray] [list $x]
    set norm [mp_enorm $xLen $xArray]
    ::tclopt::deleteArrays $xArray
    return $norm
}

proc ::tclopt::lmpar {n r ldr ipvt ifree diag qtb delta par} {
    #  n - is a positive integer input variable set to the order of r
    #  r - is an ldr by n array
    #  ldr - the leading dimension of the array r
    #  ipvt - input array of length n which defines the permutation matrix p
    #  diag - array of length n which must contain the diagonal elements of the matrix d
    #  qtb - is an input array of length n which must contain the first n elements of the vector (q transpose)*b
    #  delta - is a positive input variable which specifies an upper bound on the euclidean norm of d*x
    #  par - is a nonnegative variable. on input par contains an initial estimate of the levenberg-marquardt parameter.
    # Returns: dictionary 
    # Synopsis: -x list -y list -xi list
    set rLen [llength $r]
    if {$rLen!=[= {$n*$ldr}]} {
        error "Length of r '$rLen' must be equal to n*ldr '[= {$n*$ldr}]'"
    }
    ::tclopt::lists2arrays [list rArray diagArray qtbArray] [list $r $diag $qtb]
    ::tclopt::lists2intArrays [list ipvtArray ifreeArray] [list $ipvt $ifree]
    ::tclopt::newDoubleps [list parPnt]
    ::tclopt::doublep_assign $parPnt $par
    ::tclopt::newArrays [list xArray sdiagArray wa1Array wa2Array] [list $n $n $n $n]
    mp_lmpar $n $rArray $ldr $ipvtArray $ifreeArray $diagArray $qtbArray $delta $parPnt $xArray $sdiagArray $wa1Array\
            $wa2Array
    
    ::tclopt::arrays2lists [list rList xList sdiagList wa1List wa2List]\
            [list $rArray $xArray $sdiagArray $wa1Array $wa2Array] [list $rLen $n $n $n $n]
    set parVal [::tclopt::doublep_value $parPnt]
    ::tclopt::deleteArrays $rArray $xArray $sdiagArray $wa1Array $wa2Array
    ::tclopt::deleteIntArrays $ipvtArray $ifreeArray
    ::tclopt::deleteDoubleps $parPnt
    return [dcreate par $parVal r $rList x $xList sdiag $sdiagList wa1 $wa1List wa2 $wa2List]
}

proc ::tclopt::covar {n r ldr ipvt tol} {
    #  n - is a positive integer input variable set to the order of r
    #  r - is an ldr by n array
    #  ldr - the leading dimension of the array r
    #  ipvt - input array of length n which defines the permutation matrix p
    #  tol - nonnegative input variable used to define the numerical rank of a in the manner described above
    # Returns: dictionary 
    # Synopsis: -x list -y list -xi list
    set rLen [llength $r]
    if {$rLen!=[= {$n*$ldr}]} {
        error "Length of r '$rLen' must be equal to n*ldr '[= {$n*$ldr}]'"
    }
    ::tclopt::lists2arrays [list rArray] [list $r]
    ::tclopt::lists2intArrays [list ipvtArray] [list $ipvt]
    ::tclopt::newArrays [list waArray] [list $n]
    mp_covar $n $rArray $ldr $ipvtArray $tol $waArray
    ::tclopt::arrays2lists [list rList waList] [list $rArray $waArray] [list $rLen $n]
    ::tclopt::deleteArrays $rArray $waArray
    ::tclopt::deleteIntArrays $ipvtArray
    return [dcreate r $rList wa $waList]
}

proc ::tclopt::dmax1 {a b} {
    if {$a>=$b} {
        return $a
    } else {
        return $b
    }
}

proc ::tclopt::dmin1 {a b} {
    if {$a<=$b} {
        return $a
    } else {
        return $b
    }
}

proc ::tclopt::min0 {a b} {
    if {$a<=$b} {
        return $a
    } else {
        return $b
    }
}

proc ::tclopt::parCreate {args} {
    argparse {
        {-fixed= -default 0}
        -lowlim=
        -uplim=
        -parname=
        -step=
        -relstep=
        {-side= -enum {auto right left both an} -default auto}
        {-debugder -require {debugreltol debugabstol}}
        {-debugreltol= -require debugder}
        {-debugabstol= -require debugder}
    }
    set sideMap [dict create auto 0 right 1 left -1 both 2 an 3]
    set params [dict create fixed $fixed]
    if {[info exists lowlim]} {
        dappend params limited 1 
        dappend params limits $lowlim
    } else {
        dappend params limited 0 
        dappend params limits [list ""]
    }
    if {[info exists uplim]} {
        dict lappend params limited 1
        dict lappend params limits $uplim
    } else {
        dict lappend params limited 0
        dict lappend params limits [list ""]
    }
    if {[info exists parname]} {
        dappend params parname $parname
    } else {
        dappend params parname 0
    }
    if {[info exists step]} {
        dappend params step $step
    } else {
        dappend params step ""
    }
    if {[info exists relstep]} {
        dappend params relstep $relstep
    } else {
        dappend params relstep ""
    }
    dappend params side [dict get $sideMap $side]
    if {[info exists debugder]} {
        dappend params deriv_debug 1
        dappend params deriv_reltol $debugreltol
        dappend params deriv_abstol $debugabstol
    } else {
        dappend params deriv_debug 0
        dappend params deriv_reltol ""
        dappend params deriv_abstol ""
    }
    return $params
}

proc ::tclopt::mpfit {args} {
    argparse {
        {-funct= -required}
        {-m= -required}
        {-npar= -required}
        {-xall= -required}
        {-pars= -required}
        {-pdata= -required}
        {-ftol= -default 1e-10}
        {-xtol= -default 1e-10}
        {-gtol= -default 1e-10}
        {-stepfactor= -default 100}
        {-covtol= -default 1e-14}
        {-maxiter= -default 200}
        {-maxfev= -default 0}
        {-nprint= -default 1}
        {-epsfcn= -default 2.2204460e-16}
        {-douserscale= -default 0}
        {-nofinitecheck= -default 0}
    }
    set one 1.0
    set p1 0.1
    set p5 0.5
    set p25 0.25
    set p75 0.75
    set p0001 1e-4
    set zero 0.0

    if {$funct==""} {
        return -code error [list "Name of function must not be empty string" $::tclopt::MP_ERR_FUNC]
    }
    if {$m<=0 || $xall==""} {
        return -code error [list "m must be >0 and xall can't be empty string" $::tclopt::MP_ERR_NPOINTS]
    }
    if {$npar<=0} {
        return -code error [list "n must be >0" $::tclopt::MP_ERR_NFREE]
    }

    set iflag 0
    set qanylim 0
    set npegged 0
    set nfev 0
    set info 0
    
    set fnorm -1.0
    set fnorm1 -1.0
    set xnorm -1.0
    set delta 0.0
    set par 0.0

    if {$pars!=""} {
        for {set i 0} {$i<$npar} {incr i} {
            set par [@ $pars $i]
            if {[dget $par fixed]} {
                lappend pfixed 1
            } else {
                lappend pfixed 0
            }
            lappend step [dget $par step]
            lappend dstep [dget $par relstep]
            lappend mpside [dget $par side]
            lappend ddebug [dget $par deriv_debug]
            lappend ddrtol [dget $par deriv_reltol]
            lappend ddatol [dget $par deriv_abstol]
        }
    } else {
        for {set i 0} {$i<$npar} {incr i} {
            lappend pfixed 0
        }
        set step ""
        set dstep ""
        set mpside ""
        set ddebug ""
        set ddrtol ""
        set ddatol ""
    }

    # Finish up the free parameters
    set nfree 0
    for {set i 0} {$i<$npar} {incr i} {
        lappend ifree 0
    }
    set j 0
    for {set i 0} {$i<$npar} {incr i} {
        if {[@ $pfixed $i]==0} {
            incr nfree
            lset ifree $j $i
            incr j
        }
    }
    if {$nfree==0} {
        return -code error [list "All parameters are fixed, optimization is not possible" $::tclopt::MP_ERR_NFREE]
    }

    
    if {$pars!=""} {
        for {set i 0} {$i<[llength $pars]} {incr i} {
            set par [@ $pars $i]
            if {([@ [dget $par limited] 0] && ([@ $xall $i] < [@ [dget $par limits] 0])) ||\
                        ([@ [dget $par limited] 1] && ([@ $xall $i] > [@ [dget $par limits] 1]))} {
                return -code error [list "Initial parameters values are outside the boundaries" $::tclopt::MP_ERR_INITBOUNDS]
            }
            if {([dget $par fixed]==0) && [@ [dget $par limited] 0] && [@ [dget $par limited] 1] &&\
                        ([@ [dget $par limits] 0] >= [@ [dget $par limits] 1])} {
                return -code error [list "Lower limit of parameter cannot be higher than upper limit"\
                                            $::tclopt::MP_ERR_BOUNDS]
            }
        }
        for {set i 0} {$i<$nfree} {incr i} {
            lappend qllim [@ [dget [@ $pars [@ $ifree $i]] limited] 0]
            lappend qulim [@ [dget [@ $pars [@ $ifree $i]] limited] 1]
            lappend llim [@ [dget [@ $pars [@ $ifree $i]] limits] 0]
            lappend ulim [@ [dget [@ $pars [@ $ifree $i]] limits] 1]
            if {[@ $qllim $i] || [@ $qulim $i]} {
                set qanylim 1
            }
        }
    } else {
        set qllim ""
        set qulim ""
        set llim ""
        set ulim ""
    }
    if {$npar<=0 || $ftol<=0 || $xtol<=0 || $gtol<=0 || $maxiter < 0 || $stepfactor <=0 } {
        return -code error [list "Error in configuration parameters" $::tclopt::MP_ERR_PARAM]
    }

    if {$m < $nfree} {
        return -code error [list "Degree of freedom check failed because of '$m>=$nfree'" $::tclopt::MP_ERR_DOF]
    }

    # allocate temporary storage
    for {set i 0} {$i<$npar} {incr i} {
        lappend diag 0
    }
    for {set i 0} {$i<$m} {incr i} {
        lappend wa4 0
    }
    set ldfjac $m
    
    # Evaluate user function with initial parameter values
    set fvec [dget [$funct $xall $pdata] fvec]
    incr nfev
    set fnorm [::tclopt::enorm $fvec]
    #puts $fnorm
    set orignorm [= {$fnorm*$fnorm}]

    # Make a new copy
    set xnew $xall
    # Transfer free parameters to 'x'
    for {set i 0} {$i<$nfree} {incr i} {
        lappend x [@ $xall [@ $ifree $i]]
    }
    # Initialize Levelberg-Marquardt parameter and iteration counter
    set par 0.0
    set iter 1
    for {set i 0} {$i<$nfree} {incr i} {
        lappend qtf 0
    }

    # Beginning of the outer loop
    while {true} {
        # puts "beginning of the outer loop"
        for {set i 0} {$i<$nfree} {incr i} {
            lset xnew [@ $ifree $i] [@ $x $i]
        }
        # Calculate the jacobian matrix
        set fdjac2Data [::tclopt::fdjac2 $funct $m $ifree $nfree $npar $xnew $fvec $ldfjac $epsfcn $pdata $nfev $step\
                                $dstep $mpside $qulim $ulim $ddebug $ddrtol $ddatol]
        set fjac [dget $fdjac2Data fjac]
        
        set nfev [dget $fdjac2Data nfev]

        # Determine if any of the parameters are pegged at the limits
        if {$qanylim} {
            for {set j 0} {$j<$nfree} {incr j} {
                set lpegged [= {[@ $qllim $j] && ([@ $x $j]==[@ $llim $j])}]
                set upegged [= {[@ $qulim $j] && ([@ $x $j]==[@ $ulim $j])}]
                set sum 0
                # If the parameter is pegged at a limit, compute the gradient direction
                if {$lpegged || $upegged} {
                    set ij [= {$j*$ldfjac}]
                    for {set i 0} {$i<$m} {incr i} {
                        set sum [= {$sum+[@ $fvec $i]*[@ $fjac $ij]}]
                        incr ij
                    }
                }
                # If pegged at lower limit and gradient is toward negative then reset gradient to zero
                if {$lpegged && ($sum>0)} {
                    set ij [= {$j*$ldfjac}]
                    for {set i 0} {$i<$m} {incr i} {
                        lset $fjac $ij 0
                        incr ij
                    }
                }
                # If pegged at upper limit and gradient is toward positive then reset gradient to zero
                if {$upegged && ($sum<0)} {
                    set ij [= {$j*$ldfjac}]
                    for {set i 0} {$i<$m} {incr i} {
                        lset $fjac $ij 0
                        incr ij
                    }
                }
            }
        }
        # Compute the QR factorization of the jacobian
        set qfracData [::tclopt::qfrac $m $nfree $fjac $ldfjac 1 $nfree]
        set ipvt [dget $qfracData ipvt]
        set fjac [dget $qfracData a]
        set wa1 [dget $qfracData rdiag]
        set wa2 [dget $qfracData acnorm]
        set wa3 [dget $qfracData wa]
        
        # on the first iteration and if mode is 1, scale according to the norms of the columns of the initial jacobian.
        if {$iter==1} {
            if {$douserscale==0} {
                for {set j 0} {$j<$nfree} {incr j} {
                    lset diag [@ $ifree $j] [@ $wa2 $j]
                    if {[@ $wa2 $j]==$zero} {
                        lset diag [@ $ifree $j] $one
                    }
                }
            }
            # on the first iteration, calculate the norm of the scaled x and initialize the step bound delta.
            for {set j 0} {$j<$nfree} {incr j} {
                lset wa3 $j [= {[@ $diag [@ $ifree $j]]*[@ $x $j]}]
            }
            set xnorm [::tclopt::enorm $wa3]
            set delta [= {$stepfactor*$xnorm}]
            if {$delta==$zero} {
                set delta $stepfactor
            }
            #puts "xnorm=$xnorm delta=$delta"
        }

        # form (q transpose)*fvec and store the first n components in qtf
        for {set i 0} {$i<$m} {incr i} {
            lset wa4 $i [@ $fvec $i]
        }
        #puts $wa4
        set jj 0
        for {set j 0} {$j<$nfree} {incr j} {
            set temp3 [@ $fjac $jj]
            if {$temp3!=$zero} {
                set sum $zero
                set ij $jj
                for {set i $j} {$i<$m} {incr i} {
                    set sum [= {$sum+[@ $fjac $ij]*[@ $wa4 $i]}]
                    incr ij; # fjac[i+m*j]
                }
                set temp [= {-$sum/$temp3}]
                set ij $jj
                for {set i $j} {$i<$m} {incr i} {
                    lset wa4 $i [= {[@ $wa4 $i]+[@ $fjac $ij]*$temp}]
                    incr ij; # fjac[i+m*j]
                }
            }
            lset fjac $jj [@ $wa1 $j]
            incr jj [= {$m+1}]; # fjac[j+m*j]
            #puts "wa4\[j\]=[@ $wa4 $j]"
            lset qtf $j [@ $wa4 $j]
        }
        #puts "fjac=$fjac\n qtf=$qtf"
        # (From this point on, only the square matrix, consisting of the triangle of R, is needed.)
        
        if {$nofinitecheck} {
            # Check for overflow.  This should be a cheap test here since FJAC
            # has been reduced to a (small) square matrix, and the test is O(N^2).
            set off 0
            set nonfinite 0
            for {set j 0} {$j<$nfree} {incr j} {
                for {set i 0} {$i<$nfree} {incr i} {
                    if {[@ $fjac [= {$off+$i}]]>$::tclopt::MP_GIANT} {
                        set nonfinite 1
                    }
                }
                incr off $ldfjac
            }
            if {$nonfinite} {
                return -code error [list "Overflow occured during finite check" $::tclopt::MP_ERR_NAN]
            }
        }

        # compute the norm of the scaled gradient.
        set gnorm $zero
        if {$fnorm!=$zero} {
            set jj 0
            for {set j 0} {$j<$nfree} {incr j} {
                set l [@ $ipvt $j]
                if {[@ $wa2 $l]!=$zero} {
                    set sum $zero
                    set ij $jj
                    for {set i 0} {$i<=$j} {incr i} {
                        set sum [= {$sum+[@ $fjac $ij]*[@ $qtf $i]/$fnorm}]
                        incr ij; # fjac[i+m*j] 
                    }
                    set gnorm [::tclopt::dmax1 $gnorm [= {abs($sum/[@ $wa2 $l])}]]
                }
                incr jj $m
            }
        }
        #puts $gnorm
        # test for convergence of the gradient norm.
        if {$gnorm<=$gtol} {
            set info $::tclopt::MP_OK_DIR
        }
        if {$info!=0} {
            break
        }
        if {$maxiter==0} {
            set info $::tclopt::MP_MAXITER
            break
        }

        # rescale if necessary.
        if {$douserscale==0} {
            for {set j 0} {$j<$nfree} {incr j} {
                lset diag [@ $ifree $j] [::tclopt::dmax1 [@ $diag [@ $ifree $j]] [@ $wa2 $j]]
            }
        }
        #puts $diag
        # beginning of the inner loop.
        while {true} {
            # puts "beginning of the inner loop"
            # determine the levenberg-marquardt parameter.
            # puts "qtf=$qtf"
            set lmparData [::tclopt::lmpar $nfree $fjac $ldfjac $ipvt $ifree $diag $qtf $delta $par]
            set fjac [dget $lmparData r]
            set par [dget $lmparData par]
            set wa1 [dget $lmparData x]
            set wa2 [dget $lmparData sdiag]
            set wa3 [dget $lmparData wa1]
            set wa4 [dget $lmparData wa2]
            #puts "fjac=$fjac\n par=$par\n wa1=$wa1\n wa2=$wa2\n wa3=$wa3\n wa4=$wa4"
            # store the direction p and x + p. calculate the norm of p.
            for {set j 0} {$j<$nfree} {incr j} {
                lset wa1 $j [= {-[@ $wa1 $j]}]
            }
            
            set alpha 1.0
            #puts $qanylim
            if {$qanylim==0} {
                # No parameter limits, so just move to new position WA2
                for {set j 0} {$j<$nfree} {incr j} {
                    lset wa2 $j [= {[@ $x $j]+[@ $wa1 $j]}]
                }
                #puts $wa2
            } else {
                # Respect the limits.  If a step were to go out of bounds, then 
                # we should take a step in the same direction but shorter distance.
                # The step should take us right to the limit in that case.
                for {set j 0} {$j<$nfree} {incr j} {
                    set lpegged [= {[@ $qllim $j] && ([@ $x $j]<=[@ $llim $j])}]
                    set upegged [= {[@ $qulim $j] && ([@ $x $j]>=[@ $ulim $j])}]
                    set dwa1 [= {abs([@ $wa1 $j])>$::tclopt::MP_MACHEP0}]

                    if {$lpegged && ([@ $wa1 $j]<0)} {
                        lset wa1 $j 0
                    }
                    if {$upegged && ([@ $wa1 $j]>0)} {
                        lset wa1 $j 0
                    }
                    if {$dwa1 && [@ $qllim $j] && ([= {[@ $x $j]+[@ $wa1 $j]}]<[@ $llim $j])} {
                        set alpha [::tclopt::dmin1 $alpha [= {([@ $llim $j]-[@ $x $j])/[@ $wa1 $j]}]]
                    }
                    if {$dwa1 && [@ $qulim $j] && ([= {[@ $x $j]+[@ $wa1 $j]}]>[@ $ulim $j])} {
                        set alpha [::tclopt::dmin1 $alpha [= {([@ $ulim $j]-[@ $x $j])/[@ $wa1 $j]}]]
                    }
                }

                # Scale the resulting vector, advance to the next position
                for {set j 0} {$j<$nfree} {incr j} {
                    lset wa1 $j [= {[@ $wa1 $j]*$alpha}]
                    lset wa2 $j [= {[@ $x $j]+[@ $wa1 $j]}]
                    # Adjust the output values.  If the step put us exactly on a boundary, make sure it is exact.
                    set sgnu [= {([@ $ulim $j]>=0) ? 1 : -1}]
                    set sgnl [= {([@ $llim $j]>=0) ? 1 : -1}]
                    set ulim1 [= {[@ $ulim $j]*(1-$sgnu*$::tclopt::MP_MACHEP0)-\
                                          (([@ $ulim $j]==0) ? $::tclopt::MP_MACHEP0 : 0)}]
                    set llim1 [= {[@ $llim $j]*(1+$sgnl*$::tclopt::MP_MACHEP0)+\
                                          (([@ $llim $j]==0) ? $::tclopt::MP_MACHEP0 : 0)}]
                    if {[@ $qulim $j] && ([@ $wa2 $j]>=$ulim1)} {
                        lset wa2 $j [@ $ulim $j]
                    }
                    if {[@ $qllim $j] && ([@ $wa2 $j]<=$llim1)} {
                        lset wa2 $j [@ $llim $j]
                    }
                }
            }
            for {set j 0} {$j<$nfree} {incr j} {
                lset wa3 $j [= {[@ $diag [@ $ifree $j]]*[@ $wa1 $j]}]
            }
            #puts $wa3
            set pnorm [::tclopt::enorm $wa3]
            #puts $pnorm
            
            # on the first iteration, adjust the initial step bound.
            if {$iter==1} {
                set delta [::tclopt::dmin1 $delta $pnorm]
            }
            #puts $delta
            # evaluate the function at x + p and calculate its norm.
            for {set i 0} {$i<$nfree} {incr i} {
                lset xnew [@ $ifree $i] [@ $wa2 $i]
            }
            set functData [$funct $xnew $pdata]
            set wa4 [dget $functData fvec]
            incr nfev
            #puts $xnew
            set fnorm1 [::tclopt::enorm $wa4]
            #puts $fnorm1
            # compute the scaled actual reduction.
            set actred [= {-$one}]
            if {[= {$p1*$fnorm1}]<$fnorm} {
                set temp [= {$fnorm1/$fnorm}]
                set actred [= {$one-$temp*$temp}]
            }
            #puts $actred
            # compute the scaled predicted reduction and the scaled directional derivative.
            set jj 0
            for {set j 0} {$j<$nfree} {incr j} {
                lset wa3 $j $zero
                set l [@ $ipvt $j]
                set temp [@ $wa1 $l]
                set ij $jj
                for {set i 0} {$i<=$j} {incr i} {
                    lset wa3 $i [= {[@ $wa3 $i]+[@ $fjac $ij]*$temp}]
                    incr ij
                }
                incr jj $m
            }

            # Remember, alpha is the fraction of the full LM step actually taken
            set temp1 [= {[::tclopt::enorm $wa3]*$alpha/$fnorm}]
            set temp2 [= {sqrt($par*$alpha)*$pnorm/$fnorm}]
            set prered [= {$temp1*$temp1+($temp2*$temp2)/$p5}]
            set dirder [= {-($temp1*$temp1+$temp2*$temp2)}]
            #puts "temp1=$temp1, temp2=$temp2, prered=$prered, dirder=$dirder"
            # compute the ratio of the actual to the predicted reduction
            set ratio $zero
            if {$prered!=$zero} {
                set ratio [= {$actred/$prered}]
            }
            #puts $ratio
            # update the step bound.
            if {$ratio<=$p25} {
                if {$actred>=$zero} {
                    set temp $p5
                } else {
                    set temp [= {$p5*$dirder/($dirder+$p5*$actred)}]
                }
                if {([= {$p1*$fnorm1}]>=$fnorm) || ($temp<$p1)} {
                    set temp $p1
                }
                set delta [= {$temp*[::tclopt::dmin1 $delta [= {$pnorm/$p1}]]}]
                set par [= {$par/$temp}]
            } else {
                if {($par==$zero) || ($ratio>=$p75)} {
                    set delta [= {$pnorm/$p5}]
                    set par [= {$p5*$par}]
                }
            }

            # test for successful iteration.
            #puts "ratio=$ratio"
            if {$ratio>=$p0001} {
                # successful iteration. update x, fvec, and their norms.
                for {set j 0} {$j<$nfree} {incr j} {
                    lset x $j [@ $wa2 $j]
                    #puts $x
                    lset wa2 $j [= {[@ $diag [@ $ifree $j]]*[@ $x $j]}]
                }
                for {set i 0} {$i<$m} {incr i} {
                    lset fvec $i [@ $wa4 $i]
                }
                set xnorm [::tclopt::enorm $wa2]
                set fnorm $fnorm1
                incr iter
            }

            # tests for convergence.
            #puts "abs($actred)=[= {abs($actred)}], ftol=$ftol, prered=$prered, ftol=$ftol, p5*ratio=[= {$p5*$ratio}]"
            if {(abs($actred)<=$ftol) && ($prered<=$ftol) && ($p5*$ratio<=$one)} {
                set info $::tclopt::MP_OK_CHI
            }
            if {$delta<=($xtol*$xnorm)} {
                set info $::tclopt::MP_OK_PAR
            }
            if {(abs($actred)<=$ftol) && ($prered<=$ftol) && ($p5*$ratio<=$one) && ($info==2)} {
                set info $::tclopt::MP_OK_BOTH
            }
            if {$info!=0} {
                break
            }

            # tests for termination and stringent tolerances.
            if {$maxfev>0 && $nfev>=$maxfev} {
                # Too many function evaluations
                set info $::tclopt::MP_MAXITER
            }
            if {$iter>=$maxiter} {
                # Too many iterations
                set info $::tclopt::MP_MAXITER
            }
            if {(abs($actred)<=$::tclopt::MP_MACHEP0) && ($prered<=$::tclopt::MP_MACHEP0) && ($p5*$ratio<=$one)} {
                set info $::tclopt::MP_FTOL
            }
            if {$delta<=$::tclopt::MP_MACHEP0*$xnorm} {
                set info $::tclopt::MP_XTOL
            }
            if {$gnorm<=$::tclopt::MP_MACHEP0} {
                set info $::tclopt::MP_GTOL
            }
            if {$info!=0} {
                break
            }

            # end of the inner loop. repeat if iteration unsuccessful.
            if {$ratio<$p0001} {
                continue
            } else {
                break
            }
            
        }
        if {$info!=0} {
            break
        }      
    }
    
    # termination, either normal or user imposed.
    for {set i 0} {$i<$nfree} {incr i} {
        lset xall [@ $ifree $i] [@ $x $i]
    }

    if {$nprint>0 && $info>0} {
        set functData [$funct $xall $pdata]
        set fvec [dget $functData fvec]
        incr nfev
    }

    # Compute number of pegged parameters
    set npegged 0
    if {$pars!=""} {
        for {set i 0} {$i<$npar} {incr i} {
            set parsLim0 [@ [dget [@ $pars $i] limited] 0]
            set parsLim1 [@ [dget [@ $pars $i] limited] 1]
            if {($parsLim0 && ($parsLim0==[@ $xall $i])) || ($parsLim1 && ($parsLim1==[@ $xall $i]))} {
                incr npegged
            }
        }
    }

    # Compute and return the covariance matrix and/or parameter errors
    set covarData [::tclopt::covar $nfree $fjac $ldfjac $ipvt $covtol]
    set fjac [dget $covarData r]
    set wa2 [dget $covarData wa]
    for {set j 0} {$j<$npar*$npar} {incr j} {
        lappend covar 0.0
    }
    # Transfer the covariance array
    for {set j 0} {$j<$nfree} {incr j} {
        for {set i 0} {$i<$nfree} {incr i} {
            lset covar [= {int([@ $ifree $j]*$npar+[@ $ifree $i])}] [@ $fjac [= {int($j*$ldfjac+$i)}]]
        }
    }

    for {set j 0} {$j<$npar} {incr j} {
        lappend xerror 0.0
    }
    for {set j 0} {$j<$nfree} {incr j} {
        set cc [@ $fjac [= {int($j*$ldfjac+$j)}]]
        if {$cc>0} {
            lset xerror [@ $ifree $j] [= {sqrt($cc)}]
        }
    }
    set bestnorm [= {[::tclopt::dmax1 $fnorm $fnorm1]**2}]
    for {set j 0} {$j<$m} {incr j} {
        lappend resid [@ $fvec $j]
    }
    return [dcreate bestnorm $bestnorm orignorm $orignorm status $info niter $iter nfev $nfev npar $npar nfree $nfree\
                   npegged $npegged nfunc $m resid $resid xerror $xerror x $xall]
}

proc ::tclopt::fdjac2 {funct m ifree n npar x fvec ldfjac epsfcn pdata nfev step dstep dside qulimited ulimit ddebug\
                               ddrtol ddatol} {
    # return: nfev dvec
    global ::tclopt::MP_MACHEP0
    set zero 0.0
    set has_analytical_deriv 0
    set has_numerical_deriv 0
    set has_debug_deriv 0

    set temp [::tclopt::dmax1 $epsfcn $::tclopt::MP_MACHEP0]
    set eps [= {sqrt($temp)}]
    set ij 0
    set ldfjac 0

    for {set i 0} {$i<$npar} {incr i} {
        lappend dvec 0
    }
    # Initialize the Jacobian derivative matrix
    for {set i 0} {$i<[= {$n*$m}]} {incr i} {
        lappend fjac 0
    }
    # Check for which parameters need analytical derivatives and which need numerical ones
    for {set j 0} {$j<$n} {incr j} {
        if {$dside!="" && [@ $dside [@ $ifree $j]]==3 && [@ $ddebug [@ $ifree $j]]==0} {
            # Purely analytical derivatives
            #lset dvec [@ $ifree $j]
            set has_analytical_deriv 1
        } elseif {$dside!="" && [@ $ddebug [@ $ifree $j]]==1} {
            # Numerical and analytical derivatives as a debug cross-check
            set has_analytical_deriv 1
            set has_numerical_deriv 1
            set has_debug_deriv 1
        } else {
            set has_numerical_deriv 1
        }
    }

    # If there are any parameters requiring analytical derivatives, then compute them first.
    if {$has_analytical_deriv} {
        set fdata [$funct $x $pdata]
        set dvec [dget $fdata dvec]
        set wa [dget $fdata fvec]
        if {$nfev!=""} {
            incr nfev
        }
    }
    if {$has_debug_deriv} {

    }

    # Any parameters requiring numerical derivatives
    if {$has_numerical_deriv} {
        # Loop thru free parms
        for {set j 0} {$j<$n} {incr j} {
            if {$dside!=""} {
                set dsidei [@ $dside [@ $ifree $j]]
            } else {
                set dsidei 0
            }
            set debug [@ $ddebug [@ $ifree $j]]
            set dr [@ $ddrtol [@ $ifree $j]]
            set da [@ $ddatol [@ $ifree $j]]
        
            # Check for debugging
            if {$debug==1} {
                puts "FJAC PARM [@ $ifree $j]"
            }
            # Skip parameters already done by user-computed partials
            if {$dside!="" && $dsidei == 3} {
                incr ij $m; # still need to advance fjac pointer 
                continue
            }

            set temp [@ $x [@ $ifree $j]]
            set h [= {$eps*abs($temp)}]
            if {$step!="" && [@ $step [@ $ifree $j]]>0} {
                set h [@ $step [@ $ifree $j]]
            }
            if {$dstep!="" && [@ $dstep [@ $ifree $j]]>0} {
                set h [= {abs([@ $dstep [@ $ifree $j]]*$temp)}]
            }
            if {$h==$zero} {
                set h $eps
            }

            # If negative step requested, or we are against the upper limit
            if {$dside!="" && $dsidei==-1} {
                set h [= {-$h}]
            }
            if {$dside!="" && $dsidei==0 && $qulimited!="" && [@ $ulimit $j]!={{}}} {
                if {[@ $qulimited $j] && ($temp>[= {[@ $ulimit $j]-$h}])} {
                    set h [= {-$h}]
                }
            }


            lset x [@ $ifree $j] [= {$temp+$h}]
            set fdata [$funct $x $pdata]
            set wa [dget $fdata fvec]
            if {$nfev!=""} {
                incr nfev
            }
            lset x [@ $ifree $j] $temp

            if {$dsidei<=1} {
                # COMPUTE THE ONE-SIDED DERIVATIVE
                if {$debug=="" || $debug==0} {
                    # Non-debug path for speed
                    for {set i 0} {$i<$m} {incr i} {
                        lset fjac $ij [= {([@ $wa $i]-[@ $fvec $i])/$h}]
                        incr ij
                    }
                } else {
                    # Debug path for correctness
                    # TODO
                }
            } else {
                # dside > 2 
                # COMPUTE THE TWO-SIDED DERIVATIVE
                for {set i 0} {$i<$m} {incr i} {
                    lappend wa2 [@ $wa $i]
                }
                # Evaluate at x - h
                lset x [@ $ifree $j] [= {$temp-$h}]
                set fdata [$funct $x $pdata]
                set wa [dget $fdata fvec]
                if {$nfev!=""} {
                    incr nfev
                }
                lset x [@ $ifree $j] $temp

                # Now compute derivative as (f(x+h) - f(x-h))/(2h)
                if {$debug=="" || $debug==0} {
                    # Non-debug path for speed
                    for {set i 0} {$i<$m} {incr i} {
                        lset fjac $ij [= {([@ $wa2 $ij]-[@ $wa $i])/(2*$h)}]
                        incr ij
                    }
                } else {
                    # Debug path for correctness
                    # TODO
                }
                
            }

            
        }
    }
    return [dcreate fjac $fjac nfev $nfev]
}
