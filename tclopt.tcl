package require argparse
package provide tclopt 0.2

interp alias {} dget {} dict get
interp alias {} @ {} lindex
interp alias {} = {} expr
interp alias {} dkeys {} dict keys
interp alias {} dvalues {} dict values
interp alias {} dexist {} dict exists
interp alias {} dcreate {} dict create
interp alias {} dappend {} dict append

namespace eval ::tclopt {
    namespace import ::tcl::mathop::*
    namespace export Parameter ParameterMpfit Mpfit

    # Double precision numeric constants
    variable MP_MACHEP0 2.2204460e-16
    variable MP_DWARF 2.2250739e-308
    variable MP_GIANT 1.7976931e+308
}

proc ::tclopt::list2array {list {type double}} {
    # Create and initialize doubleArray object from the list
    #  list - list of values
    #  type - type of array, double or int
    # Returns: array object
    set length [llength $list]
    if {$type ni {double int}} {
        error "Type '$type' must be int or double"
    }
    set a [::tclopt::new_${type}Array $length]
    for {set i 0} {$i<$length} {incr i} {
        set iElem [@ $list $i]
        try {
            ::tclopt::${type}Array_setitem $a $i $iElem
        } on error {errmsg erropts} {
            if {[dget $erropts -errorcode]=="SWIG TypeError"} {
                error "List must contains only $type elements, but get '$iElem'"
            } else {
                error "Array creation failed with message '$errmsg' and opts '$erropts'"
            }
        }    
    }
    return $a
}

proc ::tclopt::lists2arrays {varNames lists {type double}} {
    # Create and initialize doubleArray objects from lists, and set these objects to variables
    #  varNames - list of variables names
    #  lists - list of lists
    #  type - type of array, double or int
    # Returns: variables with doubleArray objects are set in caller's scope
    if {$type ni {double int}} {
        error "Type '$type' must be int or double"
    }
    if {[llength $varNames]!=[llength $lists]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of lists list '[llength $lists]'"
    }
    foreach varName $varNames list $lists {
        uplevel 1 [list set $varName [::tclopt::list2array $list $type]]
    }
    return
}

proc ::tclopt::array2list {array length {type double}} {
    # Create list from doubleArray object
    #  array - doubleArray object
    #  length - number of elements in doubleArray
    #  type - type of array, double or int
    # Returns: list
    if {$type ni {double int}} {
        error "Type '$type' must be int or double"
    }
    for {set i 0} {$i<$length} {incr i} {
        lappend list [::tclopt::${type}Array_getitem $array $i]
    }
    return $list
}

proc ::tclopt::arrays2lists {varNames arrays lengths {type double}} {
    # Create lists from doubleArray objects, and set these lists to variables
    #  varNames - list of variables names
    #  arrays - list of doubleArray
    #  lengths - list of doubleArray lengths
    #  type - type of arrays, double or int
    # Returns: variables with lists are set in caller's scope
    if {$type ni {double int}} {
        error "Type '$type' must be int or double"
    }
    if {[llength $varNames]!=[llength $arrays]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of array list '[llength $arrays]'"
    } elseif {[llength $varNames]!=[llength $lengths]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of lengths list\
                '[llength $lengths]'"
    }
    foreach varName $varNames array $arrays length $lengths {
        uplevel 1 [list set $varName [::tclopt::array2list $array $length $type]]
    }
    return
}

proc ::tclopt::newArrays {varNames lengths {type double}} {
    # Creates doubleArray objects, and set these objects to variables
    #  varNames - list of variables names
    #  lengths - list of doubleArray's lengths
    #  type - type of arrays, double or int
    # Returns: variables with doubleArray or intArray objects are set in caller's scope
    if {$type ni {double int}} {
        error "Type '$type' must be int or double"
    }
    if {[llength $varNames]!=[llength $lengths]} {
        error "Length of varName list '[llength $varNames]' must be equal to length of lengths list\
                '[llength $lengths]'"
    }
    foreach varName $varNames length $lengths {
        uplevel 1 [list set $varName [::tclopt::new_${type}Array $length]]
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


proc ::tclopt::deleteArrays {arrays {type double} } {
    # Deletes doubleArray objects
    #  arrays - list of arrays objects
    #  type - type of arrays, double or int
    if {$type ni {double int}} {
        error "Type '$type' must be int or double"
    }
    foreach array $arrays {
        ::tclopt::delete_${type}Array $array
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

###  DuplChecker class definition 
   
oo::configurable create ::tclopt::DuplChecker {
    self mixin -append oo::abstract
    method duplListCheck {list} {
        # Checks if list contains duplicates.
        #  list - list to check
        # Returns: 0 if there are no duplicates and 1 if there are.
        set itemDup ""
        set new {}
        foreach item $list {
            if {[lsearch $new $item] < 0} {
                lappend new $item
            } else {
                set itemDup $item
                break
            }
        }
        return $itemDup
    }
}

### Levenberg-Marquardt square-least fitting optimization

oo::configurable create ::tclopt::Parameter {
    property name -set {
        if {$value==""} {
            return -code error "Parameter must have a name, empty string was provided"
        } elseif {[regexp {[^A-Za-z0-9_]+} $value]} {
            return -code error "Parameter name '$value' is not a valid name"
        }
        set name $value
    } -get {
        return $name
    }
    property fixed -set {
        if {[string is boolean -strict $value]} {
            set fixed $value
            return
        } else {
            return -code error "fixed property value '$value' must be a boolean type"
        }
    } -get {
        return $fixed
    }
    property initval -set {
        if {[string is double -strict $value]} {
            set initval $value
            return
        } else {
            return -code error "Initial value '$value' of parameter must be a double type"
        }
    } -get {
        return $initval
    }
    property lowlim -set {
        if {[string is double -strict $value]} {
            set lowlim $value
            return
        } else {
            return -code error "Lower limit value '$value' of parameter must be a double type"
        }
    } -get {
        return $lowlim
    }
    property uplim -set {
        if {[string is double -strict $value]} {
            set uplim $value
            return
        } else {
            return -code error "Upper limit value '$value' of parameter must be a double type"
        }
    } -get {
        return $uplim
    }
    variable name fixed initval lowlim uplim
    constructor {name initval args} {
        argparse {
            {-fixed -boolean}
            -lowlim=
            -uplim=
        }
        my configure -fixed $fixed
        my configure -initval $initval
        my configure -name $name
        if {[info exists lowlim]} {
            if {$lowlim>$initval} {
                return -code error "Initial value must be higher than the lower limit"
            }
            my configure -lowlim $lowlim
        }
        if {[info exists uplim]} {
            if {[info exists lowlim]} {
                if {$lowlim>=$uplim} {
                    return -code error "Lower limit must be lower than the upper limit"
                }
            }
            if {$uplim<$initval} {
                return -code error "Initial value must be lower than the upper limit"
            }
            my configure -uplim $uplim
        }
    }

}

oo::configurable create ::tclopt::ParameterMpfit {
    superclass ::tclopt::Parameter
    property step -set {
        if {[string is double -strict $value]} {
            if {$value<=0} {
                return -code error "Step value '$value' of parameter must be more than zero"
            } else {
                set step $value
                return
            }
        } else {
            return -code error "Step value '$value' of parameter must be a double type"
        }
    } -get {
        return $step
    }
    property relstep -set {
        if {[string is double -strict $value]} {
            if {$value<=0} {
                return -code error "Relative step value '$value' of parameter must be more than zero"
            } else {
                set relstep $value
                return
            }
        } else {
            return -code error "Relative step value '$value' of parameter must be a double type"
        }
    } -get {
        return $relstep
    }
    property side -set {
        if {$value in {auto right left both an}} {
            set side $value
            return
        } else {
            return -code error "Side of derivative selector value '$value' of parameter must be of one of the type\
                    'auto', 'right', 'left', 'both' or 'an'"
        }
    } -get {
        return $side
    }
    property debugder -set {
        if {[string is boolean -strict $value]} {
            set debugder $value
            return
        } else {
            return -code error "debugder property value '$value' must be a boolean type"
        }
    } -get {
        return $debugder
    }
    property derivreltol -set {
        if {[string is double -strict $value]} {
            if {$value<0} {
                return -code error "derivreltol value '$value' of parameter must be more or equal to zero"
            } else {
                set derivreltol $value
                return
            }
        } else {
            return -code error "derivreltol value '$value' of parameter must be a double type"
        }
    } -get {
        return $derivreltol
    }
    property derivabstol -set {
        if {[string is double -strict $value]} {
            if {$value<0} {
                return -code error "derivabstol value '$value' of parameter must be more or equal to zero"
            } else {
                set derivabstol $value
                return
            }
        } else {
            return -code error "derivabstol value '$value' of parameter must be a double type"
        }
    } -get {
        return $derivabstol
    }
    variable name fixed lowlim uplim step relstep side debugder derivreltol derivabstol initval
    constructor {name initval args} {
        # Creates parameter object for [::tclopt::Mpfit] class.
        #  name - name of the parameter
        #  initval - initial value of parameter
        #  -fixed - specify that parameter is fixed during optimization, optional
        #  -lowlim - specify lower limit for parameter, must be lower than upper limit if upper limit is provided,
        #    optional
        #  -uplim - specify upper limit for parameter, must be higher than lower limit if lower limit is provided, 
        #    optional
        #  -step - the step size to be used in calculating the numerical derivatives.  If set to zero, then the step 
        #    size is computed automatically, optional
        #  -relstep - the *relative* step size to be used in calculating the numerical derivatives. This number is the
        #    fractional size of the step, compared to the parameter value. This value supercedes the -step setting.
        #    If the parameter is zero, then a default step size is chosen.
        #  -side - the sidedness of the finite difference when computing numerical derivatives. This field can take four
        #    values: auto : one-sided derivative computed automatically, right : one-sided derivative (f(x+h)-f(x))/h,
        #    left : one-sided derivative (f(x)-f(x-h))/h, both : two-sided derivative (f(x+h)-f(x-h))/(2*h), an :
        #    user-computed explicit derivatives, where h is the -step parameter described above. The "automatic" 
        #    one-sided derivative method will chose a direction for the finite difference which does not violate any 
        #    constraints. The other methods do not perform this check. The two-sided method is in principle more precise, 
        #    but requires twice as many function evaluations. Default is auto.
        #  -debugder - switch to enable console debug logging of user-computed derivatives, as described above. Note that
        #    when debugging is enabled, then -side should be set to auto, right, left or both, depending on which 
        #    numerical derivative you wish to compare to. Requires -debugreltol and -debugabstol values.
        #  -debugreltol - relative error that controls printing of derivatives comparison if relative error exceeds this
        #    value. Requires -debugder and -debugabstol.
        #  -debugabstol - absolute error that controls printing of derivatives comparison if absolute error exceeds this
        #    value. Requires -debugder and -debugreltol.
        # Synopsis: value value ?-fixed? ?-lowlim value? ?-uplim value? ?-step value? ?-relstep value? ?-side value?
        #   ?-debugder -debugreltol value -debugabstol value?
        #
        # Example of building 4 parameters with different constraints:
        # ```
        # set par0 [::tclopt::ParameterMpfit new a 1.0 -fixed -side both]
        # set par1 [::tclopt::ParameterMpfit new b 2.0]
        # set par2 [::tclopt::ParameterMpfit new c 0.0 -fixed]
        # set par3 [::tclopt::ParameterMpfit new d 0.1 -lowlim -0.3 -uplim 0.2]
        # ```
        set arguments [argparse -inline {
            {-fixed -boolean}
            -lowlim=
            -uplim=
            -step=
            -relstep=
            {-side= -enum {auto right left both an} -default auto}
            {-debugder -boolean -require {debugreltol debugabstol}}
            -debugreltol=
            -debugabstol=
        }]
        my configure -side [dget $arguments side]
        if {[dexist $arguments step]} {
            my configure -step [dget $arguments step]
        }
        if {[dexist $arguments relstep]} {
            my configure -relstep [dget $arguments relstep]
        }
        my configure -debugder [dget $arguments debugder]
        if {[dget $arguments debugder]} {
            my configure -derivreltol [dget $arguments debugreltol]
            my configure -derivabstol [dget $arguments debugabstol]
        }
        set params ""
        if {[dget $arguments fixed]} {
            lappend params -fixed
        }
        dict for {paramName value} $arguments {
            if {$paramName ni {side step relstep debugder debugreltol debugabstol fixed}} {
                lappend params -$paramName $value
            }
        }
        next $name $initval {*}$params
    }
}

oo::configurable create ::tclopt::Mpfit {
    mixin ::tclopt::DuplChecker
    property funct -set {
        if {$value==""} {
            return -code error "Function must have a name, empty string was provided"
        } elseif {$value ni [info commands $value]} {
            return -code error "Function with name '$value' does not exist"
        } else {
            set funct $value
        }
    } -get {
        return $funct
    }
    property m -set {
        if {[string is integer -strict $value]} {
            if {$value<=0} {
                return -code error "Number of values m must be more than 0"
            } else {
                set m $value
                return
            }
        } else {
            return -code error "Number of values m '$value' must be an integer type"
        }
    } -get {
        return $m
    }
    property ftol -set {
        if {[string is double -strict $value]} {
            if {$value<=0} {
                return -code error "ftol value '$value' must be more than zero"
            } else {
                set ftol $value
                return
            }
        } else {
            return -code error "ftol value '$value' must be a double type"
        }
    } -get {
        return $ftol
    }
    property xtol -set {
        if {[string is double -strict $value]} {
            if {$value<=0} {
                return -code error "xtol value '$value' must be more than zero"
            } else {
                set xtol $value
                return
            }
        } else {
            return -code error "xtol value '$value' must be a double type"
        }
    } -get {
        return $xtol
    }
    property gtol -set {
        if {[string is double -strict $value]} {
            if {$value<=0} {
                return -code error "gtol value '$value' must be more than zero"
            } else {
                set gtol $value
                return
            }
        } else {
            return -code error "gtol value '$value' must be a double type"
        }
    } -get {
        return $gtol
    }
    property stepfactor -set {
        if {[string is double -strict $value]} {
            if {$value<=0} {
                return -code error "stepfactor value '$value' must be more than zero"
            } else {
                set stepfactor $value
                return
            }
        } else {
            return -code error "stepfactor value '$value' must be a double type"
        }
    } -get {
        return $stepfactor
    }
    property covtol -set {
        if {[string is double -strict $value]} {
            if {$value<=0} {
                return -code error "covtol value '$value' must be more than zero"
            } else {
                set covtol $value
                return
            }
        } else {
            return -code error "covtol value '$value' must be a double type"
        }
    } -get {
        return $covtol
    }
    property maxiter -set {
        if {[string is integer -strict $value]} {
            if {$value<0} {
                return -code error "Maximum iterations number '$value' must be more than or equal to 0"
            } else {
                set maxiter $value
                return
            }
        } else {
            return -code error "Maximum iterations number '$value' must be an integer type"
        }
    } -get {
        return $maxiter
    }
    property maxfev -set {
        if {[string is integer -strict $value]} {
            if {$value<0} {
                return -code error "Maximum number of function evaluations '$value' must be more than or equal to 0"
            } else {
                set maxfev $value
                return
            }
        } else {
            return -code error "Maximum number of function evaluations '$value' must be an integer type"
        }
    } -get {
        return $maxfev
    }
    property epsfcn -set {
        if {[string is double -strict $value]} {
            if {$value<=0} {
                return -code error "epsfcn value '$value' must be more than zero"
            } else {
                set epsfcn $value
                return
            }
        } else {
            return -code error "epsfcn value '$value' must be a double type"
        }
    } -get {
        return $epsfcn
    }
    property nofinitecheck -set {
        if {[string is boolean -strict $value]} {
            set nofinitecheck $value
            return
        } else {
            return -code error "nofinitecheck property value '$value' must be a boolean type"
        }
    } -get {
        return $nofinitecheck
    }
    property pdata
    property results
    variable funct m ftol xtol gtol stepfactor covtol maxiter maxfev epsfcn nofinitecheck pdata results
    variable Pars
    method getAllParsNames {} {
        # Gets names of all parameters.
        # Returns: list of elements names
        if {![info exists Pars]} {
            return -code error "There are no parameters attached to optimizer"
        } else {
            return [dkeys $Pars]
        }
    }
    method getAllPars {} {
        # Gets names of all parameters.
        # Returns: list of elements names
        if {![info exists Pars]} {
            return -code error "There are no parameters attached to optimizer"
        } else {
            return $Pars
        }
    }
    method addPars {args} {
        foreach arg $args {
            set argClass [info object class $arg]
            if {$argClass!={::tclopt::ParameterMpfit}} {
                return -code error "Only ::tclopt::ParameterMpfit could be added to optimizer, '$argClass' was provided"
            }
            lappend parsNamesList [$arg configure -name]
        }
        if {[info exists Pars]} {
            lappend parsNamesList {*}[my getAllParsNames]
        }
        set dup [my duplListCheck $parsNamesList]
        if {$dup!=""} {
            return -code error "Optimizer already contains parameter with name '$dup'"
        }
        foreach arg $args {
            set parName [$arg configure -name]
            dict append Pars $parName $arg
        }
        return
    }

    constructor {args} {
        # Creates optimization object that does least squares fitting using modified Levenberg-Marquardt algorithm.
        #  -funct - name of the procedure that should be minimized 
        #  -m - number of data points
        #  -pdata - list or dictionary that provides private data to funct that is needed to evaluate residuals. Usually
        #    it contains x and y values lists, but you can provide any data necessary for function residuals evaluation.
        #    Will be passed upon each function evaluation without modification.
        #  -ftol - control termination of mpfit. Termination occurs when both the actual and predicted relative
        #    reductions in the sum of squares are at most ftol. Therefore, ftol measures the relative error desired
        #    in the sum of squares. Value must be of the type float more than zero, default is 1e-10.
        #  -xtol - control termination of mpfit. Termination occurs when the relative error between two consecutive 
        #    iterates is at most xtol. Therefore, xtol measures the relative error desired in the approximate solution.
        #    Value must be of the type float more than zero, default is 1e-10.
        #  -gtol - control termination of mpfit. Termination occurs when the cosine of the angle between fvec and any
        #    column of the jacobian is at most gtol in absolute value. Therefore, gtol measures the orthogonality desired
        #    between the function vector and the columns of the jacobian. Value must be of the type float more than zero,
        #    default is 1e-10.
        #  -maxfev - control termination of mpfit. Termination occurs when the number of calls to funct is at least
        #    maxfev by the end of an iteration. Value must be the positive integer, default is 0. If it equals to 0,
        #    number of evaluations is not restricted.
        #  -stepfactor - used in determining the initial step bound. This bound is set to the product of factor and the
        #    euclidean norm of diag*x if nonzero, or else to factor itself. in most cases factor should lie in the 
        #    interval (.1,100.). 100. is a generally recommended value. Value must be of the type float more than zero, 
        #    default is 100.
        #  -covtol - range tolerance for covariance calculation. Value must be of the type float more than zero, default 
        #    is 1e-14.
        #  -maxiter - maximum number of iterations. If maxiter equal to 0, then basic error checking is done, and 
        #    parameter errors/covariances are estimated based on input arameter values, but no fitting iterations are
        #    done. Value must be the positive integer, default is 200.
        #  -epsfcn - finite derivative step size. Value must be of the type float more than zero, default is
        #    2.2204460e-16.
        #  -nofinitecheck - enable check for infinite quantities, default is off.
        # Returns: object of class
        #
        # Class uses the Levenberg-Marquardt technique to solve the least-squares problem. In its typical use, it will
        # be used to fit a user-supplied function (the "model") to user-supplied data points (the "data") by adjusting a 
        # set of parameters. mpfit is based upon MINPACK-1 (LMDIF.F) by More' and collaborators.
        # The user-supplied function should compute an array of weighted deviations between model and data. In a typical
        # scientific problem the residuals should be weighted so that each deviate has a gaussian sigma of 1.0. If x
        # represents values of the independent variable, y represents a measurement for each value of x, and err 
        # represents the error in the measurements, then the deviates could be calculated as follows:
        # ```
        # for {set i 0} {$i<$m} {incr i} {
        #     lset deviates $i [expr {([lindex $y $i] - [f [lindex $x $i]])/[lindex $err $i]}]
        # }
        # ```
        # where m is the number of data points, and where f is the function representing the model evaluated at x. If ERR
        # are the 1-sigma uncertainties in Y, then the sum of deviates squared will be the total chi-squared value, which
        # mpfit will seek to minimize.
        # Simple constraints are placed on parameter values by adding objects of class [::tclopt::ParameterMpfit] to 
        # mpfit with method [:tclopt::Mpfit::addPars], where other parameter-specific options can be set.
        # For details of how to specify constraints, please look at the
        # description of [::tclopt::parCreate] procedure. Please note, that order in which we attach parameters objects
        # is the order in which values will be supplied to minimized function, and the order in which resulted will
        # be written to X property of the class.
        # Example of user defined function (using linear equation t=a+b*x):
        # ```
        # proc f {xall pdata args} {
        #     set x [dget $pdata x]
        #     set y [dget $pdata y]
        #     set ey [dget $pdata ey]
        #     foreach xVal $x yVal $y eyVal $ey {
        #         set f [= {[@ $xall 0]+[@ $xall 1]*$xVal}]
        #         lappend fval [= {($yVal-$f)/$eyVal}]
        #     }
        #     return [dcreate fvec $fval]
        # }
        # ```
        # where xall is list of initial parameters values, pdata - dictionary that contains x, y and ey lists with 
        # length m. It returns dictionary with residuals values.
        # Alternative form of function f could also provide analytical derivatives:
        # ```
        # proc quadfunc {xall pdata args} {
        #     set x [dget $pdata x]
        #     set y [dget $pdata y]
        #     set ey [dget $pdata ey]
        #     foreach xVal $x yVal $y eyVal $ey {
        #         lappend fvec [= {($yVal-[@ $xall 0]-[@ $xall 1]*$xVal-[@ $xall 2]*$xVal*$xVal)/$eyVal}]
        #     }
        #     if {[@ $args 0]!=""} {
        #         set derivs [@ $args 0]
        #         foreach deriv $derivs {
        #             if {$deriv==0} {
        #                 foreach xVal $x yVal $y eyVal $ey {
        #                     lappend dvec [= {-1/$eyVal}]
        #                 }   
        #             }
        #             if {$deriv==1} {
        #                 foreach xVal $x yVal $y eyVal $ey {
        #                     lappend dvec [= {(-$xVal)/$eyVal}]
        #                 }
        #             }
        #             if {$deriv==2} {
        #                 foreach xVal $x yVal $y eyVal $ey {
        #                     lappend dvec [= {(-$xVal*$xVal)/$eyVal}]
        #                 }
        #             }
        #         }
        #         return [dcreate fvec $fvec dvec $dvec]
        #     } else {
        #         return [dcreate fvec $fvec]
        #     }
        # }
        # ```
        # The first element of the `args` list is a list specifying the ordinal numbers of the parameters for which we 
        # need to calculate the analytical derivative. In this case, the returned `dvec` list contains the derivative at
        # each x point for each specified parameter, following the same order as in the input list. For example, if the 
        # input list is {0, 2} and the number m of x points is 3, the `dvec` list will look like this:
        # ```
        # ⎛⎛df ⎞   ⎛df ⎞   ⎛df ⎞   ⎛df ⎞   ⎛df ⎞   ⎛df ⎞  ⎞
        # ⎜⎜───⎟   ⎜───⎟   ⎜───⎟   ⎜───⎟   ⎜───⎟   ⎜───⎟  ⎟
        # ⎜⎝dp0⎠   ⎝dp0⎠   ⎝dp0⎠   ⎝dp2⎠   ⎝dp2⎠   ⎝dp2⎠  ⎟
        # ⎝     x0      x1      x2      x0      x1      x2⎠
        # ```
        #
        # Description of keys and data in returned dictionary:
        #   -bestnorm - final chi^2
        #   -orignorm - starting value of chi^2
        #   -status - fitting status code
        #   -niter - number of iterations
        #   -nfev - number of function evaluations
        #   -npar - total number of parameters
        #   -nfree - number of free parameters
        #   -npegged - number of pegged parameters
        #   -nfunc - number of residuals (= num. of data points)
        #   -resid - list of final residuals
        #   -xerror - final parameter uncertainties (1-sigma), in the order of elements in `Pars` property dictionary.
        #   -x - final parameters values list in the order of elements in `Pars` property dictionary.
        #   -debug - string with derivatives debugging output
        #   -covar - final parameters covariance matrix.
        # You can also access all results by [my configure propertyName] mechanism.
        #
        # Synopsis: -funct value -m value -pdata value ?-ftol value? ?-xtol value? ?-gtol value? ?-stepfactor value?
        #   ?-covtol value? ?-maxiter value? ?-maxfev value? ?-epsfcn value? ?-nofinitecheck? 
        argparse {
            {-funct= -required}
            {-m= -required}
            {-pdata= -default ""}
            {-ftol= -default 1e-10}
            {-xtol= -default 1e-10}
            {-gtol= -default 1e-10}
            {-stepfactor= -default 100}
            {-covtol= -default 1e-14}
            {-maxiter= -default 200}
            {-maxfev= -default 0}
            {-epsfcn= -default 2.2204460e-16}
            {-nofinitecheck -boolean}
        }
        my configure -funct $funct
        my configure -m $m
        my configure -pdata $pdata
        my configure -ftol $ftol
        my configure -gtol $gtol
        my configure -xtol $xtol
        my configure -stepfactor $stepfactor
        my configure -covtol $covtol
        my configure -maxiter $maxiter
        my configure -maxfev $maxfev
        my configure -epsfcn $epsfcn
        my configure -nofinitecheck $nofinitecheck
    }
    method run {} {
        # Runs optimization.
        # Returns: dictionary containing resulted data
        set sideMap [dict create auto 0 right 1 left -1 both 2 an 3]
        set one 1.0
        set p1 0.1
        set p5 0.5
        set p25 0.25
        set p75 0.75
        set p0001 1e-4
        set zero 0.0
        if {![info exists Pars]} {
            return -code error "At least one parameter must be attached to optimizer before call to run"
        }
        set pars [dvalues [my getAllPars]]
        set npar [llength $pars]
        set pdata [my configure -pdata]
        set funct [my configure -funct]
        set mLoc [my configure -m]
        foreach par $pars {
            lappend xall [$par configure -initval]
        }
        set ftol [my configure -ftol]
        set xtol [my configure -xtol]
        set gtol [my configure -gtol]
        set stepfactor [my configure -stepfactor]
        set covtol [my configure -covtol]
        set maxiter [my configure -maxiter]
        set maxfev [my configure -maxfev]
        set epsfcn [my configure -epsfcn]
        set nofinitecheck [my configure -nofinitecheck]
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

        foreach par $pars {
            if {[$par configure -fixed]} {
                lappend pfixed 1
            } else {
                lappend pfixed 0
            }
            if {[catch {$par configure -step}]} {
                lappend step 0
            } else {
                lappend step [$par configure -step]
            }
            if {[catch {$par configure -relstep}]} {
                lappend dstep 0
            } else {
                lappend dstep [$par configure -relstep]
            }
            lappend mpside [dict get $sideMap [$par configure -side]]
            if {![$par configure -debugder]} {
                lappend ddebug 0
                lappend ddrtol 0
                lappend ddatol 0
            } else {
                lappend ddebug 1
                lappend ddrtol [$par configure -derivreltol]
                lappend ddatol [$par configure -derivabstol]
            }
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
            return -code error "All parameters are fixed, optimization is not possible"
        }
        
        # allocate lists with zeros
        for {set i 0} {$i<$npar} {incr i} {
            lappend qllim 0
            lappend qulim 0
            lappend llim 0
            lappend ulim 0
        }
        for {set i 0} {$i<$nfree} {incr i} {
            set par [@ $pars [@ $ifree $i]]
            if {[catch {$par configure -lowlim}]} {
                lset qllim $i 0
                lset llim $i 0
            } else {
                lset qllim $i 1
                lset llim $i [$par configure -lowlim]
            }
            if {[catch {$par configure -uplim}]} {
                lset qulim $i 0
                lset ulim $i 0
            } else {
                lset qulim $i 1
                lset ulim $i [$par configure -uplim]
            }
            if {[@ $qllim $i] || [@ $qulim $i]} {
                set qanylim 1
            }
        }

        if {$mLoc<$nfree} {
            return -code error "Degree of freedom check failed because of 'm=$mLoc>=n=$nfree'"
        }

        # allocate temporary storage
        for {set i 0} {$i<$npar} {incr i} {
            lappend diag 0
        }
        for {set i 0} {$i<$mLoc} {incr i} {
            lappend wa4 0
        }
        set ldfjac $mLoc
        
        # Evaluate user function with initial parameter values
        set fvec [dget [$funct $xall $pdata] fvec]
        if {[llength $fvec]!=$mLoc} {
            return -code error "Length of list '[llength $fvec]' returned from the function is less than m '$mLoc' value"
        }
        incr nfev
        set fnorm [my Enorm $fvec]
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
            set fdjac2Data [my Fdjac2 $funct $mLoc $ifree $nfree $xnew $fvec $ldfjac $epsfcn $pdata $nfev $step\
                                    $dstep $mpside $qulim $ulim $ddebug $ddrtol $ddatol]
            set fjac [dget $fdjac2Data fjac]
            set nfev [dget $fdjac2Data nfev]
            if {[dexist $fdjac2Data debug]} {
                lappend debugOutput {*}[dget $fdjac2Data debug]
            }
            # Determine if any of the parameters are pegged at the limits
            if {$qanylim} {
                for {set j 0} {$j<$nfree} {incr j} {
                    set lpegged [= {[@ $qllim $j] && ([@ $x $j]==[@ $llim $j])}]
                    set upegged [= {[@ $qulim $j] && ([@ $x $j]==[@ $ulim $j])}]
                    set sum 0
                    # If the parameter is pegged at a limit, compute the gradient direction
                    if {$lpegged || $upegged} {
                        set ij [= {$j*$ldfjac}]
                        for {set i 0} {$i<$mLoc} {incr i} {
                            set sum [= {$sum+[@ $fvec $i]*[@ $fjac $ij]}]
                            incr ij
                        }
                    }
                    # If pegged at lower limit and gradient is toward negative then reset gradient to zero
                    if {$lpegged && ($sum>0)} {
                        set ij [= {$j*$ldfjac}]
                        for {set i 0} {$i<$mLoc} {incr i} {
                            lset fjac $ij 0
                            incr ij
                        }
                    }
                    # If pegged at upper limit and gradient is toward positive then reset gradient to zero
                    if {$upegged && ($sum<0)} {
                        set ij [= {$j*$ldfjac}]
                        for {set i 0} {$i<$mLoc} {incr i} {
                            lset fjac $ij 0
                            incr ij
                        }
                    }
                }
            }
            # Compute the QR factorization of the jacobian
            set qfracData [my Qfrac $mLoc $nfree $fjac $ldfjac 1 $nfree]
            set ipvt [dget $qfracData ipvt]
            set fjac [dget $qfracData a]
            set wa1 [dget $qfracData rdiag]
            set wa2 [dget $qfracData acnorm]
            set wa3 [dget $qfracData wa]
            
            # on the first iteration and if mode is 1, scale according to the norms of the columns of the initial jacobian.
            if {$iter==1} {
                for {set j 0} {$j<$nfree} {incr j} {
                    lset diag [@ $ifree $j] [@ $wa2 $j]
                    if {[@ $wa2 $j]==$zero} {
                        lset diag [@ $ifree $j] $one
                    }
                }
                # on the first iteration, calculate the norm of the scaled x and initialize the step bound delta.
                for {set j 0} {$j<$nfree} {incr j} {
                    lset wa3 $j [= {[@ $diag [@ $ifree $j]]*[@ $x $j]}]
                }
                set xnorm [my Enorm $wa3]
                set delta [= {$stepfactor*$xnorm}]
                if {$delta==$zero} {
                    set delta $stepfactor
                }
            }

            # form (q transpose)*fvec and store the first n components in qtf
            for {set i 0} {$i<$mLoc} {incr i} {
                lset wa4 $i [@ $fvec $i]
            }
            set jj 0
            for {set j 0} {$j<$nfree} {incr j} {
                set temp3 [@ $fjac $jj]
                if {$temp3!=$zero} {
                    set sum $zero
                    set ij $jj
                    for {set i $j} {$i<$mLoc} {incr i} {
                        set sum [= {$sum+[@ $fjac $ij]*[@ $wa4 $i]}]
                        incr ij; # fjac[i+m*j]
                    }
                    set temp [= {-$sum/$temp3}]
                    set ij $jj
                    for {set i $j} {$i<$mLoc} {incr i} {
                        lset wa4 $i [= {[@ $wa4 $i]+[@ $fjac $ij]*$temp}]
                        incr ij; # fjac[i+m*j]
                    }
                }
                lset fjac $jj [@ $wa1 $j]
                incr jj [= {$mLoc+1}]; # fjac[j+m*j]"
                lset qtf $j [@ $wa4 $j]
            }
            
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
                    return -code error "Overflow occured during finite check"
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
                        set gnorm [my Dmax1 $gnorm [= {abs($sum/[@ $wa2 $l])}]]
                    }
                    incr jj $mLoc
                }
            }

            # test for convergence of the gradient norm.
            if {$gnorm<=$gtol} {
                set info "Convergence in orthogonality"
            }
            if {$info!=0} {
                break
            }
            if {$maxiter==0} {
                set info "Maximum number of iterations reached"
                break
            }

            # rescale
            for {set j 0} {$j<$nfree} {incr j} {
                lset diag [@ $ifree $j] [my Dmax1 [@ $diag [@ $ifree $j]] [@ $wa2 $j]]
            }

            # beginning of the inner loop.
            while {true} {
                # determine the levenberg-marquardt parameter.
                set lmparData [my Lmpar $nfree $fjac $ldfjac $ipvt $ifree $diag $qtf $delta $par]
                set fjac [dget $lmparData r]
                set par [dget $lmparData par]
                
                set wa1 [dget $lmparData x]
                set wa2 [dget $lmparData sdiag]
                set wa3 [dget $lmparData wa1]
                # store the direction p and x + p. calculate the norm of p.
                for {set j 0} {$j<$nfree} {incr j} {
                    lset wa1 $j [= {-[@ $wa1 $j]}]
                }
                set alpha 1.0
                if {$qanylim==0} {
                    # No parameter limits, so just move to new position WA2
                    for {set j 0} {$j<$nfree} {incr j} {
                        lset wa2 $j [= {[@ $x $j]+[@ $wa1 $j]}]
                    }
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
                            set alpha [my Dmin1 $alpha [= {([@ $llim $j]-[@ $x $j])/[@ $wa1 $j]}]]
                        }
                        if {$dwa1 && [@ $qulim $j] && ([= {[@ $x $j]+[@ $wa1 $j]}]>[@ $ulim $j])} {
                            set alpha [my Dmin1 $alpha [= {([@ $ulim $j]-[@ $x $j])/[@ $wa1 $j]}]]
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
                set pnorm [my Enorm $wa3]
                
                # on the first iteration, adjust the initial step bound.
                if {$iter==1} {
                    set delta [my Dmin1 $delta $pnorm]
                }

                # evaluate the function at x + p and calculate its norm.
                for {set i 0} {$i<$nfree} {incr i} {
                    lset xnew [@ $ifree $i] [@ $wa2 $i]
                }
                set functData [$funct $xnew $pdata]
                set wa4 [dget $functData fvec]
                incr nfev
                set fnorm1 [my Enorm $wa4]
                
                # compute the scaled actual reduction.
                set actred [= {-$one}]
                if {[= {$p1*$fnorm1}]<$fnorm} {
                    set temp [= {$fnorm1/$fnorm}]
                    set actred [= {$one-$temp*$temp}]
                }

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
                    incr jj $mLoc
                }

                # Remember, alpha is the fraction of the full LM step actually taken
                set temp1 [= {[my Enorm $wa3]*$alpha/$fnorm}]
                set temp2 [= {sqrt($par*$alpha)*$pnorm/$fnorm}]
                set prered [= {$temp1*$temp1+($temp2*$temp2)/$p5}]
                set dirder [= {-($temp1*$temp1+$temp2*$temp2)}]

                # compute the ratio of the actual to the predicted reduction
                set ratio $zero
                if {$prered!=$zero} {
                    set ratio [= {$actred/$prered}]
                }

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
                    set delta [= {$temp*[my Dmin1 $delta [= {$pnorm/$p1}]]}]
                    set par [= {$par/$temp}]
                } else {
                    if {($par==$zero) || ($ratio>=$p75)} {
                        set delta [= {$pnorm/$p5}]
                        set par [= {$p5*$par}]
                    }
                }

                # test for successful iteration.
                if {$ratio>=$p0001} {
                    # successful iteration. update x, fvec, and their norms.
                    for {set j 0} {$j<$nfree} {incr j} {
                        lset x $j [@ $wa2 $j]
                        #puts $x
                        lset wa2 $j [= {[@ $diag [@ $ifree $j]]*[@ $x $j]}]
                    }
                    for {set i 0} {$i<$mLoc} {incr i} {
                        lset fvec $i [@ $wa4 $i]
                    }
                    set xnorm [my Enorm $wa2]
                    set fnorm $fnorm1
                    incr iter
                }

                # tests for convergence.
                if {(abs($actred)<=$ftol) && ($prered<=$ftol) && ($p5*$ratio<=$one)} {
                    set info "Convergence in chi-square value"
                }
                if {$delta<=($xtol*$xnorm)} {
                    set info "Convergence in parameter value"
                }
                if {(abs($actred)<=$ftol) && ($prered<=$ftol) && ($p5*$ratio<=$one) && ($info==2)} {
                    set info "Both convergence in parameter value and convergence in chi-square value hold"
                }
                if {$info!=0} {
                    break
                }

                # tests for termination and stringent tolerances.
                if {$maxfev>0 && $nfev>=$maxfev} {
                    # Too many function evaluations
                    set info "Maximum number of function evaluations reached"
                }
                if {$iter>=$maxiter} {
                    # Too many iterations
                    set info "Maximum number of iterations reached"
                }
                if {(abs($actred)<=$::tclopt::MP_MACHEP0) && ($prered<=$::tclopt::MP_MACHEP0) && ($p5*$ratio<=$one)} {
                    set info "ftol is too small, no further improvement"
                }
                if {$delta<=$::tclopt::MP_MACHEP0*$xnorm} {
                    set info "xtol is too small, no further improvement"
                }
                if {$gnorm<=$::tclopt::MP_MACHEP0} {
                    set info "gtol is too small, no further improvement"
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

        if {$info>0} {
            set functData [$funct $xall $pdata]
            set fvec [dget $functData fvec]
            incr nfev
        }

        # Compute number of pegged parameters
        set npegged 0
        for {set i 0} {$i<$npar} {incr i} {
            set par [@ $pars $i]
            if {[catch {$par configure -lowlim}]} {
                set parsLim0 0
            } else {
                set parsLim0 [$par configure -lowlim]
            }
            if {[catch {$par configure -uplim}]} {
                set parsLim1 0
            } else {
                set parsLim1 [$par configure -uplim]
            }
            if {($parsLim0 && ($parsLim0==[@ $xall $i])) || ($parsLim1 && ($parsLim1==[@ $xall $i]))} {
                incr npegged
            }
        }

        # Compute and return the covariance matrix and/or parameter errors
        set covarData [my Covar $nfree $fjac $ldfjac $ipvt $covtol]
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
        set bestnorm [= {[my Dmax1 $fnorm $fnorm1]**2}]
        for {set j 0} {$j<$mLoc} {incr j} {
            lappend resid [@ $fvec $j]
        }
        if {[info exists debugOutput]} {
            set debugOutput [join $debugOutput "\n"]
        } else {
            set debugOutput ""
        }
        set resDict [dcreate bestnorm $bestnorm orignorm $orignorm status $info niter $iter nfev $nfev npar $npar\
                   nfree $nfree npegged $npegged nfunc $m resid $resid xerror $xerror x $xall debug $debugOutput\
                   covar $covar]
        my configure -results $resDict
        return $resDict
    }
    method Fdjac2 {funct mLoc ifree n x fvec ldfjac epsfcn pdata nfev step dstep dside qulimited ulimit ddebug\
                                   ddrtol ddatol} {
        # Calculate Jacobian matrix.
        # Returns: list containing Jacobian matrix
        global ::tclopt::MP_MACHEP0
        set zero 0.0
        set has_analytical_deriv 0
        set has_numerical_deriv 0
        set has_debug_deriv 0
        set temp [my Dmax1 $epsfcn $::tclopt::MP_MACHEP0]
        set eps [= {sqrt($temp)}]
        set ij 0
        set ldfjac 0

        # Initialize the Jacobian derivative matrix
        for {set i 0} {$i<[= {$n*$mLoc}]} {incr i} {
            lappend fjac 0
        }

        # Check for which parameters need analytical derivatives and which need numerical ones
        for {set j 0} {$j<$n} {incr j} {
            if {[@ $dside [@ $ifree $j]]==3 && [@ $ddebug [@ $ifree $j]]==0} {
                # Purely analytical derivatives
                lappend derivs [@ $ifree $j]; # get index of parameter for which we need to calculate analytical derivative
                set has_analytical_deriv 1
            } elseif {[@ $ddebug [@ $ifree $j]]==1} {
                # Numerical and analytical derivatives as a debug cross-check
                lappend derivs [@ $ifree $j]; # get index of parameter for which we need to calculate analytical derivative
                set has_analytical_deriv 1
                set has_numerical_deriv 1
                set has_debug_deriv 1
            } else {
                set has_numerical_deriv 1
            }
        }
        # If there are any parameters requiring analytical derivatives, then compute them first.
        if {$has_analytical_deriv} {
            set fdata [$funct $x $pdata $derivs]
            set dvecData [dget $fdata dvec]
            set i 0
            foreach deriv $derivs {
                # gets start index for position of parameter in free parameters list 'ifree' for which replacing of
                # fjac elements starts with calculated analytical derivatives.
                # For example, we have ifree list {0 2}, it means that parameters 0 and 2 are free parameters.
                # Then, we need calculate index for parameter 2 for which we calculate analytic derivative, it is 1,
                # so we replace fjac elements from 1*m to 1*m+m.
                set insertIndex [lsearch -exact [lrange $ifree 0 $n] $deriv]
                for {set j 0} {$j<$mLoc} {incr j} {
                    # replace m elements in fjac for each parameter for which which we calculate the analytic derivatives
                    lset fjac [= {$insertIndex*$mLoc+$j}] [@ $dvecData [= {$i*$mLoc+$j}]]
                }
                incr i
            }
            set wa [dget $fdata fvec]
            incr nfev
        }
        if {$has_debug_deriv} {
            puts "FJAC DEBUG BEGIN"
            set header [format "%10s %10s %10s %10s %10s %10s" INPUT FUNC DERIV_U DERIV_N DIFF_ABS DIFF_REL]
            puts $header
            lappend debugOutput $header
        }

        # Any parameters requiring numerical derivatives
        if {$has_numerical_deriv} {
            # Loop thru free parms
            for {set j 0} {$j<$n} {incr j} {
                set dsidei [@ $dside [@ $ifree $j]]
                set debug [@ $ddebug [@ $ifree $j]]
                set dr [@ $ddrtol [@ $ifree $j]]
                set da [@ $ddatol [@ $ifree $j]]
                # Check for debugging
                if {$debug==1} {
                    set paramNumb "FJAC PARM [@ $ifree $j]"
                    puts $paramNumb
                    lappend debugOutput $paramNumb
                }
                # Skip parameters already done by user-computed partials
                if {$dsidei == 3} {
                    incr ij $mLoc; # still need to advance fjac pointer 
                    continue
                }

                set temp [@ $x [@ $ifree $j]]
                set h [= {$eps*abs($temp)}]
                if {[@ $step [@ $ifree $j]]>0} {
                    set h [@ $step [@ $ifree $j]]
                }
                if {[@ $dstep [@ $ifree $j]]>0} {
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
                incr nfev
                lset x [@ $ifree $j] $temp
                if {$dsidei<=1} {
                    # COMPUTE THE ONE-SIDED DERIVATIVE
                    if {$debug=="" || $debug==0} {
                        # Non-debug path for speed
                        for {set i 0} {$i<$mLoc} {incr i} {
                            lset fjac $ij [= {([@ $wa $i]-[@ $fvec $i])/$h}]
                            incr ij
                        }
                    } else {
                        # Debug path for correctness
                        for {set i 0} {$i<$mLoc} {incr i} {
                            set fjold [@ $fjac $ij]
                            lset fjac $ij [= {([@ $wa $i]-[@ $fvec $i])/$h}]
                            if {($da==0 && $dr==0 && ($fjold!=0 || [@ $fjac $ij]!=0)) || (($da!=0 || $dr!=0) &&\
                                    (abs($fjold-[@ $fjac $ij])>($da+abs($fjold)*$dr)))} {
                                set debugLine [format "%10d %10.4g %10.4g %10.4g %10.4g %10.4g" $i [@ $fvec $i] $fjold\
                                                       [@ $fjac $ij] [= {$fjold-[@ $fjac $ij]}]\
                                                       [= {($fjold==0) ? 0 : (($fjold-[@ $fjac $ij])/$fjold)}]]
                                puts $debugLine
                                lappend debugOutput $debugLine
                            }
                            incr ij
                        }
                    }
                } else {
                    # dside > 2 
                    # COMPUTE THE TWO-SIDED DERIVATIVE
                    for {set i 0} {$i<$mLoc} {incr i} {
                        lappend wa2 [@ $wa $i]
                    }
                    # Evaluate at x - h
                    lset x [@ $ifree $j] [= {$temp-$h}]
                    set fdata [$funct $x $pdata]
                    set wa [dget $fdata fvec]
                    incr nfev
                    lset x [@ $ifree $j] $temp
                    # Now compute derivative as (f(x+h) - f(x-h))/(2h)
                    if {$debug=="" || $debug==0} {
                        # Non-debug path for speed
                        for {set i 0} {$i<$mLoc} {incr i} {
                            lset fjac $ij [= {([@ $wa2 $ij]-[@ $wa $i])/(2*$h)}]
                            incr ij
                        }
                    } else {
                        # Debug path for correctness
                        for {set i 0} {$i<$mLoc} {incr i} {
                            set fjold [@ $fjac $ij]
                            lset fjac $ij [= {([@ $wa2 $i]-[@ $wa $i])/(2*$h)}]
                            if {($da==0 && $dr==0 && ($fjold!=0 || [@ $fjac $ij]!=0)) || (($da!=0 || $dr!=0) &&\
                                    (abs($fjold-[@ $fjac $ij])>($da+abs($fjold)*$dr)))} {
                                set debugLine [format "%10d %10.4g %10.4g %10.4g %10.4g %10.4g" $i [@ $fvec $i] $fjold\
                                                       [@ $fjac $ij] [= {$fjold-[@ $fjac $ij]}]\
                                                       [= {($fjold==0) ? 0 : (($fjold-[@ $fjac $ij])/$fjold)}]]
                                puts $debugLine
                                lappend debugOutput $debugLine
                            }
                            incr ij
                        }
                    }
                    
                }
            }
        }
        if {$has_debug_deriv} {
            set footer "FJAC DEBUG END"
            puts $footer
            lappend debugOutput $footer
        }
        if {[info exists debugOutput]} {
            return [dcreate fjac $fjac nfev $nfev debug $debugOutput]
        } else {
            return [dcreate fjac $fjac nfev $nfev]
        }
        return
    }
    method Qfrac {mLoc n a lda pivot lipvt} {
        # Does QR factorization
        #  mLoc - number of rows of matrix a
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
        ::tclopt::lists2arrays [list aArray] [list $a]
        ::tclopt::newArrays [list rdiagArray acnormArray waArray] [list $n $n $n]
        ::tclopt::newArrays [list ipvtArray] [list $lipvt] int
        ::tclopt::mp_qrfac $mLoc $n $aArray $lda $pivot $ipvtArray $lipvt $rdiagArray $acnormArray $waArray
        ::tclopt::arrays2lists [list aList rdiagList acnormList waList]\
                [list $aArray $rdiagArray $acnormArray $waArray] [list $aLen $n $n $n]
        ::tclopt::arrays2lists [list ipvtList] [list $ipvtArray] [list $lipvt] int
        ::tclopt::deleteArrays [list $aArray $rdiagArray $acnormArray $waArray]
        ::tclopt::deleteArrays [list $ipvtArray] int
        return [dcreate a $aList rdiag $rdiagList acnorm $acnormList wa $waList ipvt $ipvtList]
    }
    method Enorm {x} {
        # Calculate euclidean norm of vector
        #  x - list with values of vector x
        # Returns: norm 
        # Synopsis: -x list -y list -xi list
        set xLen [llength $x]
        ::tclopt::lists2arrays [list xArray] [list $x]
        set norm [::tclopt::mp_enorm $xLen $xArray]
        ::tclopt::deleteArrays [list $xArray]
        return $norm
    }

    method Lmpar {n r ldr ipvt ifree diag qtb delta par} {
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
        ::tclopt::lists2arrays [list rArray diagArray qtbArray] [list $r $diag $qtb]
        ::tclopt::lists2arrays [list ipvtArray ifreeArray] [list $ipvt $ifree] int
        ::tclopt::newDoubleps [list parPnt]
        ::tclopt::doublep_assign $parPnt $par
        ::tclopt::newArrays [list xArray sdiagArray wa1Array wa2Array] [list $n $n $n $n]
        ::tclopt::mp_lmpar $n $rArray $ldr $ipvtArray $ifreeArray $diagArray $qtbArray $delta $parPnt $xArray\
                $sdiagArray $wa1Array $wa2Array
        ::tclopt::arrays2lists [list rList xList sdiagList wa1List wa2List]\
                [list $rArray $xArray $sdiagArray $wa1Array $wa2Array] [list $rLen $n $n $n $n]
        set parVal [::tclopt::doublep_value $parPnt]
        ::tclopt::deleteArrays [list $rArray $xArray $sdiagArray $wa1Array $wa2Array]
        ::tclopt::deleteArrays [list $ipvtArray $ifreeArray] int
        ::tclopt::deleteDoubleps $parPnt
        return [dcreate par $parVal r $rList x $xList sdiag $sdiagList wa1 $wa1List wa2 $wa2List]
    }

    method Covar {n r ldr ipvt tol} {
        #  n - is a positive integer input variable set to the order of r
        #  r - is an ldr by n array
        #  ldr - the leading dimension of the array r
        #  ipvt - input array of length n which defines the permutation matrix p
        #  tol - nonnegative input variable used to define the numerical rank of a in the manner described above
        # Returns: dictionary 
        # Synopsis: -x list -y list -xi list
        set rLen [llength $r]
        ::tclopt::lists2arrays [list rArray] [list $r]
        ::tclopt::lists2arrays [list ipvtArray] [list $ipvt] int
        ::tclopt::newArrays [list waArray] [list $n]
        ::tclopt::mp_covar $n $rArray $ldr $ipvtArray $tol $waArray
        ::tclopt::arrays2lists [list rList waList] [list $rArray $waArray] [list $rLen $n]
        ::tclopt::deleteArrays [list $rArray $waArray]
        ::tclopt::deleteArrays [list $ipvtArray] int
        return [dcreate r $rList wa $waList]
    }

    method Dmax1 {a b} {
        if {$a>=$b} {
            return $a
        } else {
            return $b
        }
    }

    method Dmin1 {a b} {
        if {$a<=$b} {
            return $a
        } else {
            return $b
        }
    }

}




