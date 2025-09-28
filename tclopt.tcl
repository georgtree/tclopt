package require argparse
package require control
package require gnuplotutil
package require extexpr
namespace import ::control::*
package provide tclopt 0.21

interp alias {} dget {} dict get
interp alias {} @ {} lindex
interp alias {} = {} expr
interp alias {} dkeys {} dict keys
interp alias {} dvalues {} dict values
interp alias {} dexist {} dict exists
interp alias {} dcreate {} dict create
interp alias {} dappend {} dict append
interp alias {} dset {} dict set

namespace eval tcl::mathfunc {
    proc llength {list} {
        # Wraps `llength` command into expr function
        ::llength $list
    }
    proc lindex {list index args} {
        # Wraps `lindex` command into expr function
        ::lindex $list $index {*}$args
    }
    proc li {list index args} {
        # Wraps `lindex` command into expr function
        ::lindex $list $index {*}$args
    }
    proc lrange {list first last} {
        # Wraps `lrange` command into expr function
        ::lrange $list $first $last
    }
}

namespace eval ::tclopt {
    namespace import ::tcl::mathop::* 
    namespace export Parameter ParameterMpfit Mpfit DE GSA LBFGS

    # Double precision numeric constants
    variable MP_MACHEP0 2.2204460e-16
    variable MP_DWARF 2.2250739e-308
    variable MP_GIANT 1.7976931e+308
    const numberEqConfigureCheck {
        if {[string is @type@ -strict $value]} {
            if {@condition@} {
                return -code error "@name@ value '$value' must be @condString@"
            } else {
                set @name@ $value
                return
            }
        } else {
            return -code error "@name@ value '$value' must be @article@ @type@ type"
        }
    }
    const numberConfigureCheck {
        if {[string is @type@ -strict $value]} {
            set @name@ $value
            return
        } else {
            return -code error "@name@ value '$value'  must be @article@ @type@ type"
        }
    }

}

proc ::tclopt::NewArrays {varNames lengths {type double}} {
    # Creates doubleArray objects, and set these objects to variables
    #  varNames - list of variables names
    #  lengths - list of doubleArray's lengths
    #  type - type of array, double or int
    # Returns: variables with doubleArray objects are set in caller's scope
    if {$type ni {double int}} {
        return -code error "Type '$type' must be int or double"
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

proc ::tclopt::List2array {list {type double}} {
    # Create and initialize doubleArray object from the list
    #  list - list of values
    #  type - type of array, double or int
    # Returns: array object
    set length [llength $list]
    if {$type ni {double int}} {
        return -code error "Type '$type' must be int or double"
    }
    set a [::tclopt::new_${type}Array $length]
    for {set i 0} {$i<$length} {incr i} {
        set iElem [@ $list $i]
        try {
            ::tclopt::${type}Array_setitem $a $i $iElem
        } on error {errmsg erropts} {
            if {[dget $erropts -errorcode] eq {SWIG TypeError}} {
                return -code error "List must contains only $type elements, but get '$iElem'"
            } else {
                return -code error "Array creation failed with message '$errmsg' and opts '$erropts'"
            }
        }    
    }
    return $a
}
proc ::tclopt::Lists2arrays {varNames lists {type double}} {
    # Create and initialize doubleArray objects from lists, and set these objects to variables
    #  varNames - list of variables names
    #  lists - list of lists
    #  type - type of array, double or int
    # Returns: variables with doubleArray objects are set in caller's scope
    if {$type ni {double int}} {
        return -code error "Type '$type' must be int or double"
    }
    if {[llength $varNames]!=[llength $lists]} {
        return -code error "Length of varName list '[llength $varNames]' must be equal to length of lists list\
                 '[llength $lists]'"
    }
    foreach varName $varNames list $lists {
        uplevel 1 [list set $varName [::tclopt::List2array $list $type]]
    }
    return
}
proc ::tclopt::Array2list {array length {type double}} {
    # Create list from doubleArray object
    #  array - doubleArray object
    #  length - number of elements in doubleArray
    #  type - type of array, double or int
    # Returns: list
    if {$type ni {double int}} {
        return -code error "Type '$type' must be int or double"
    }
    for {set i 0} {$i<$length} {incr i} {
        lappend list [::tclopt::${type}Array_getitem $array $i]
    }
    return $list
}
proc ::tclopt::Arrays2lists {varNames arrays lengths {type double}} {
    # Create lists from doubleArray objects, and set these lists to variables
    #  varNames - list of variables names
    #  arrays - list of doubleArray
    #  lengths - list of doubleArray lengths
    #  type - type of arrays, double or int
    # Returns: variables with lists are set in caller's scope
    if {$type ni {double int}} {
        return -code error "Type '$type' must be int or double"
    }
    if {[llength $varNames]!=[llength $arrays]} {
        return -code error "Length of varName list '[llength $varNames]' must be equal to length of array list\
                 '[llength $arrays]'"
    } elseif {[llength $varNames]!=[llength $lengths]} {
        return -code error "Length of varName list '[llength $varNames]' must be equal to length of lengths list\
                '[llength $lengths]'"
    }
    foreach varName $varNames array $arrays length $lengths {
        uplevel 1 [list set $varName [::tclopt::Array2list $array $length $type]]
    }
    return
}
proc ::tclopt::List2array {list {type double}} {
    # Create and initialize doubleArray object from the list
    #  list - list of values
    #  type - type of array, double or int
    # Returns: array object
    set length [llength $list]
    if {$type ni {double int}} {
        return -code error "Type '$type' must be int or double"
    }
    set a [::tclopt::new_${type}Array $length]
    for {set i 0} {$i<$length} {incr i} {
        set iElem [@ $list $i]
        try {
            ::tclopt::${type}Array_setitem $a $i $iElem
        } on error {errmsg erropts} {
            if {[dget $erropts -errorcode] eq {SWIG TypeError}} {
                return -code error "List must contains only $type elements, but get '$iElem'"
            } else {
                return -code error "Array creation failed with message '$errmsg' and opts '$erropts'"
            }
        }    
    }
    return $a
}

proc ::tclopt::AssignPointersValues {pointers values {type double}} {
    # Set values for pointers of specified types
    #  pointers - list of pointers objects
    #  values - list of values
    #  type - type of arrays, double or int
    # Returns: nothing
    if {$type ni {double int long}} {
        return -code error "Type '$type' must be int, long or double"
    }
    if {[llength $pointers]!=[llength $values]} {
        return -code error "Length of pointers list '[llength $pointers]' must be equal to length of values list\
                '[llength $values]'"
    }
    foreach pointer $pointers value $values {
        ::tclopt::${type}p_assign $pointer $value
    }
    return
}
proc ::tclopt::GetPointersValues {pointers varNames {type double}} {
    # Get pointers values and store it in variables
    #  pointers - list of pointers objects
    #  varNames - list of variables names
    #  type - type of array, double or int
    # Returns: list
    if {$type ni {double int long}} {
        return -code error "Type '$type' must be int, long or double"
    }
    if {[llength $pointers]!=[llength $varNames]} {
        return -code error "Length of pointers list '[llength $pointers]' must be equal to length of variables names\
                list '[llength $varNames]'"
    }
    foreach pointer $pointers varName $varNames {
        uplevel 1 [list set $varName [::tclopt::${type}p_value $pointer]]
    }
    return 
}

proc ::tclopt::NewPointers {varNames {type double}} {
    # Creates doubleps objects, and set these objects to variables
    #  varNames - list of variables names
    #  type - type of pointer, double or int
    # Returns: variables with pointers objects are set in caller's scope
    if {$type ni {double int long}} {
        return -code error "Type '$type' must be int, long or double"
    }
    foreach varName $varNames {
        uplevel 1 [list set $varName [::tclopt::new_${type}p]]
    }
    return
}
proc ::tclopt::DeleteArrays {arrays {type double}} {
    # Deletes doubleArray objects
    #  arrays - list of arrays objects
    #  type - type of arrays, double or int
    if {$type ni {double int}} {
        return -code error "Type '$type' must be int or double"
    }
    foreach array $arrays {
        ::tclopt::delete_${type}Array $array
    }
    return
}
proc ::tclopt::DeletePointers {pointers {type double}} {
    # Deletes pointers objects
    #  pointers - list of pointers objects
    #  type - type of arrays, double or int
    if {$type ni {double int long}} {
        return -code error "Type '$type' must be int, long or double"
    }
    foreach pointer $pointers {
        ::tclopt::delete_${type}p $pointer
    }
    return
}

oo::configurable create ::tclopt::DuplChecker {
    self mixin -append oo::abstract
    method duplListCheck {list} {
        # Checks if list contains duplicates.
        #  list - list to check
        # Returns: 0 if there are no duplicates and 1 if there are.
        set itemDup {}
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

oo::configurable create ::tclopt::Parameter {
    property name -set {
        if {$value eq {}} {
            return -code error "Parameter must have a name, empty string was provided"
        } elseif {[regexp {[^A-Za-z0-9_]+} $value]} {
            return -code error "Parameter name '$value' is not a valid name"
        }
        set name $value
    }
    property initval -set [string map {@type@ double @name@ initval @article@ a} $::tclopt::numberConfigureCheck]
    property lowlim -set [string map {@type@ double @name@ lowlim @article@ a} $::tclopt::numberConfigureCheck]
    property uplim -set [string map {@type@ double @name@ uplim @article@ a} $::tclopt::numberConfigureCheck]
    variable name initval lowlim uplim
    constructor {args} {
        # Creates parameter object.
        #  name - name of the parameter
        #  initval - initial value of parameter
        #  -lowlim value - specify lower limit for parameter, must be lower than upper limit if upper limit is provided,
        #    optional
        #  -uplim value - specify upper limit for parameter, must be higher than lower limit if lower limit is provided,
        #    optional
        # Synopsis: value value ?-fixed? ?-lowlim value? ?-uplim value? ?-step value? ?-relstep value? ?-side value?
        #   ?-debugder -debugreltol value -debugabstol value?
        #
        # Example of building 4 parameters with different constraints:
        # ```
        # set par0 [::tclopt::Parameter new a 1.0 -lowlim 0.0]
        # set par1 [::tclopt::Parameter new b 2.0]
        # set par2 [::tclopt::Parameter new c 0.0]
        # set par3 [::tclopt::Parameter new d 0.1 -lowlim -0.3 -uplim 0.2]
        # ```
        argparse -pfirst {
            {-lowlim= -help {Specify lower limit for parameter, must be lower than upper limit if upper limit is\
                                     provided}}
            {-uplim= -help {Specify upper limit for parameter, must be higher than lower limit if lower limit is\
                                    provided}}
            {name -help {Name of the parameter}}
            {initval -help {Initial value of parameter}}
        }
        my configure -initval $initval -name $name
        if {[info exists lowlim]} {
            if {$lowlim>$initval} {
                return -code error {Initial value must be higher than the lower limit}
            }
            my configure -lowlim $lowlim
        }
        if {[info exists uplim]} {
            if {[info exists lowlim]} {
                if {$lowlim>=$uplim} {
                    return -code error {Lower limit must be lower than the upper limit}
                }
            }
            if {$uplim<$initval} {
                return -code error {Initial value must be lower than the upper limit}
            }
            my configure -uplim $uplim
        }
    }
}
oo::configurable create ::tclopt::ParameterMpfit {
    superclass ::tclopt::Parameter
    property fixed -set [string map {@type@ boolean @name@ fixed @article@ a} $::tclopt::numberConfigureCheck]
    property step -set [string map {@type@ double @name@ step @condition@ {$value<=0} @condString@\
                                            {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property relstep -set [string map {@type@ double @name@ relstep @condition@ {$value<=0} @condString@\
                                               {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property side -set {
        if {$value in {auto right left both an}} {
            set side $value
            return
        } else {
            return -code error "Side of derivative selector value '$value' of parameter must be of one of the type\
                    'auto', 'right', 'left', 'both' or 'an'"
        }
    }
    property debugder -set [string map {@type@ boolean @name@ debugder @article@ a} $::tclopt::numberConfigureCheck]
    property derivreltol -set [string map {@type@ double @name@ derivreltol @condition@ {$value<0} @condString@\
                                                   {more or equal to zero} @article@ a}\
                                       $::tclopt::numberEqConfigureCheck]
    property derivabstol -set [string map {@type@ double @name@ derivabstol @condition@ {$value<0} @condString@\
                                                   {more or equal to zero} @article@ a}\
                                       $::tclopt::numberEqConfigureCheck]
    variable name fixed lowlim uplim step relstep side debugder derivreltol derivabstol initval
    constructor {args} {
        # Creates parameter object for [::tclopt::Mpfit] class.
        #  name - name of the parameter
        #  initval - initial value of parameter
        #  -fixed - specify that parameter is fixed during optimization, optional
        #  -lowlim value - specify lower limit for parameter, must be lower than upper limit if upper limit is provided,
        #    optional
        #  -uplim value - specify upper limit for parameter, must be higher than lower limit if lower limit is provided,
        #    optional
        #  -step value - the step size to be used in calculating the numerical derivatives.  If set to zero, then the
        #    step size is computed automatically, optional
        #  -relstep value - the *relative* step size to be used in calculating the numerical derivatives. This number is
        #    the fractional size of the step, compared to the parameter value. This value supercedes the `-step` setting.
        #    If the parameter is zero, then a default step size is chosen.
        #  -side value - the sidedness of the finite difference when computing numerical derivatives. This field can
        #    take four values: auto : one-sided derivative computed automatically, right : one-sided derivative
        #    (f(x+h)-f(x))/h, left : one-sided derivative (f(x)-f(x-h))/h, both : two-sided derivative
        #    (f(x+h)-f(x-h))/(2*h), an : user-computed explicit derivatives, where h is the `-step` parameter described
        #    above. The "automatic" one-sided derivative method will chose a direction for the finite difference which
        #    does not violate any constraints. The other methods do not perform this check. The two-sided method is in
        #    principle more precise, but requires twice as many function evaluations. Default is auto.
        #  -debugder - switch to enable console debug logging of user-computed derivatives, as described above. Note
        #    that when debugging is enabled, then -side should be set to auto, right, left or both, depending on which
        #    numerical derivative you wish to compare to. Requires -debugreltol and -debugabstol values.
        #  -debugreltol value - relative error that controls printing of derivatives comparison if relative error
        #    exceeds this value. Requires -debugder and -debugabstol.
        #  -debugabstol value - absolute error that controls printing of derivatives comparison if absolute error
        #    exceeds this value. Requires -debugder and -debugreltol.
        # Synopsis: value value ?-fixed? ?-lowlim value? ?-uplim value? ?-step value? ?-relstep value? ?-side value?
        #   ?-debugder -debugreltol value -debugabstol value?
        #
        # Example of building 4 parameters with different constraints:
        # ```
        # set par0 [ParameterMpfit new a 1.0 -fixed -side both]
        # set par1 [ParameterMpfit new b 2.0]
        # set par2 [ParameterMpfit new c 0.0 -fixed]
        # set par3 [ParameterMpfit new d 0.1 -lowlim -0.3 -uplim 0.2]
        # ```
        set arguments [argparse -inline -pfirst -help {Creates parameter object for '::tclopt::Mpfit' class} {
            {-fixed -boolean -help {Specify that parameter is fixed during optimization}}
            {-lowlim= -help {Specify lower limit for parameter, must be lower than upper limit if upper limit is\
                                     provided}}
            {-uplim= -help {Specify upper limit for parameter, must be higher than lower limit if lower limit is\
                                    provided}}
            {-step= -help {The step size to be used in calculating the numerical derivatives.  If set to zero, then the\
                                   size is computed automatically step}}
            {-relstep= -help {The *relative* step size to be used in calculating the numerical derivatives. This number\
                                      is the fractional size of the step, compared to the parameter value. This value\
                                      supercedes the -step setting. If the parameter is zero, then a default step size\
                                      is chosen}}
            {-side= -enum {auto right left both an} -default auto\
                     -help {The sidedness of the finite difference when computing numerical derivatives. This field can\
                                    take four values: auto : one-sided derivative computed automatically, right :\
                                    one-sided derivative (f(x+h)-f(x))/h, left : one-sided derivative (f(x)-f(x-h))/h,\
                                    both : two-sided derivative (f(x+h)-f(x-h))/(2*h), an : user-computed explicit\
                                    derivatives, where h is the -step parameter described above. The "automatic"\
                                    one-sided derivative method will chose a direction for the finite difference which\
                                    does not violate any constraints. The other methods do not perform this check. The\
                                    two-sided method is in principle more precise, but requires twice as many function\
                                    evaluations. Default is auto}}
            {-debugder -boolean -require {debugreltol debugabstol}\
                     -help {Switch to enable console debug logging of user-computed derivatives, as described above.\
                                    Note that when debugging is enabled, then -side should be set to auto, right, left\
                                    or both, depending on which numerical derivative you wish to compare to}}
            {-debugreltol= -help {Relative error that controls printing of derivatives comparison if relative error\
                                          exceeds this value}}
            {-debugabstol= -help {Absolute error that controls printing of derivatives comparison if absolute error\
                                          exceeds this value}}
            {name -help {Name of the parameter}}
            {initval -help {Initial value of parameter}}
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
        set params {}
        if {[dget $arguments fixed]} {
            set fixed 1
        } else {
            set fixed 0
        }
        dict for {paramName value} $arguments {
            if {$paramName ni {name initval side step relstep debugder debugreltol debugabstol fixed}} {
                lappend params -$paramName $value
            }
        }
        next [dget $arguments name] [dget $arguments initval] {*}$params
    }
}
oo::configurable create ::tclopt::Optimization {
    mixin ::tclopt::DuplChecker
    self mixin -append oo::abstract
    property funct -set {
        if {$value eq {}} {
            return -code error {Function must have a name, empty string was provided}
        } elseif {$value ni [info commands $value]} {
            return -code error "Function with name '$value' does not exist"
        } else {
            set funct $value
        }
    }
    property pdata
    property results
    variable funct pdata results Pars
    method getAllParsNames {args} {
        # Gets names of all parameters.
        # Returns: list of elements names
        argparse -help {Gets names of all parameters. Returns: list of elements names} {}
        if {![info exists Pars]} {
            return -code error {There are no parameters attached to optimizer}
        } else {
            return [dkeys $Pars]
        }
    }
    method getAllPars {args} {
        # Gets references of all parameters objects.
        # Returns: list of elements names
        argparse -help {Gets references of all parameters objects. Returns: list of references} {}
        if {![info exists Pars]} {
            return -code error {There are no parameters attached to optimizer}
        } else {
            return $Pars
        }
    }
    method addPars {args} {
        argparse -help {Attaches parameters to optimizer object} {
            {params -catchall -help {References to objects of class '::tclopt::ParameterMpfit'}}
        }
        foreach arg $params {
            set argClass [info object class $arg]
            if {$argClass ni {::tclopt::ParameterMpfit ::tclopt::Parameter}} {
                return -code error "Only ::tclopt::ParameterMpfit or ::tclopt::Parameter could be added to optimizer,\
                        '$argClass' was provided"
            }
            lappend parsNamesList [$arg configure -name]
        }
        if {[info exists Pars]} {
            lappend parsNamesList {*}[my getAllParsNames]
        }
        set dup [my duplListCheck $parsNamesList]
        if {$dup ne {}} {
            return -code error "Optimizer already contains parameter with name '$dup'"
        }
        foreach arg $params {
            set parName [$arg configure -name]
            dict append Pars $parName $arg
        }
        return
    }
}
oo::configurable create ::tclopt::Mpfit {
    superclass ::tclopt::Optimization
    property m -set [string map {@type@ integer @name@ m @condition@ {$value<=0} @condString@\
                                         {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property ftol -set [string map {@type@ double @name@ ftol @condition@ {$value<=0} @condString@\
                                            {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property xtol -set [string map {@type@ double @name@ xtol @condition@ {$value<=0} @condString@\
                                            {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property gtol -set [string map {@type@ double @name@ gtol @condition@ {$value<=0} @condString@\
                                            {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property stepfactor -set [string map {@type@ double @name@ stepfactor @condition@ {$value<=0} @condString@\
                                                  {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property covtol -set [string map {@type@ double @name@ covtol @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property maxiter -set [string map {@type@ integer @name@ maxiter @condition@ {$value<0} @condString@\
                                               {more than or equal to zero} @article@ an}\
                                   $::tclopt::numberEqConfigureCheck]
    property maxfev -set [string map {@type@ integer @name@ maxfev @condition@ {$value<0} @condString@\
                                              {more than or equal to zero} @article@ an}\
                                  $::tclopt::numberEqConfigureCheck]
    property epsfcn -set [string map {@type@ double @name@ epsfcn @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property nofinitecheck -set [string map {@type@ boolean @name@ nofinitecheck @article@ a}\
                                         $::tclopt::numberConfigureCheck]
    property pdata
    property results -kind readable
    variable funct m ftol xtol gtol stepfactor covtol maxiter maxfev epsfcn nofinitecheck pdata results
    variable Pars
    constructor {args} {
        # Creates optimization object that does least squares fitting using modified Levenberg-Marquardt algorithm.
        #  -funct value - name of the procedure that should be minimized 
        #  -m value - number of data points
        #  -pdata value - list or dictionary that provides private data to funct that is needed to evaluate
        #    residuals. Usually it contains x and y values lists, but you can provide any data necessary for function
        #    residuals evaluation.  Will be passed upon each function evaluation without modification.
        #  -ftol value - control termination of mpfit. Termination occurs when both the actual and predicted relative
        #    reductions in the sum of squares are at most ftol. Therefore, ftol measures the relative error desired
        #    in the sum of squares. Value must be of the type float more than zero, default is 1e-10.
        #  -xtol value - control termination of mpfit. Termination occurs when the relative error between two
        #    consecutive iterates is at most xtol. Therefore, xtol measures the relative error desired in the
        #    approximate solution.  Value must be of the type float more than zero, default is 1e-10.
        #  -gtol value - control termination of mpfit. Termination occurs when the cosine of the angle between fvec and
        #    any column of the jacobian is at most gtol in absolute value. Therefore, gtol measures the orthogonality
        #    desired between the function vector and the columns of the jacobian. Value must be of the type float more
        #    than zero, default is 1e-10.
        #  -maxfev value - control termination of mpfit. Termination occurs when the number of calls to funct is at
        #    least maxfev by the end of an iteration. Value must be the positive integer, default is 0. If it equals to
        #    0, number of evaluations is not restricted.
        #  -stepfactor value - used in determining the initial step bound. This bound is set to the product of factor
        #    and the euclidean norm of diag*x if nonzero, or else to factor itself. In most cases factor should lie in
        #    the interval (.1,100.). 100. is a generally recommended value. Value must be of the type float more than
        #    zero, default is 100.
        #  -covtol value - range tolerance for covariance calculation. Value must be of the type float more than zero,
        #    default is 1e-14.
        #  -maxiter value - maximum number of iterations. If maxiter equal to 0, then basic error checking is done, and 
        #    parameter errors/covariances are estimated based on input arameter values, but no fitting iterations are
        #    done. Value must be the positive integer, default is 200.
        #  -epsfcn value - finite derivative step size. Value must be of the type float more than zero, default is
        #    2.2204460e-16.
        #  -nofinitecheck - enables check for infinite quantities, default is off.
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
        # mpfit with method [::tclopt::Optimization::addPars], where other parameter-specific options can be set.
        # For details of how to specify constraints, please look at the
        # description of [::tclopt::ParameterMpfit] class. Please note, that order in which we attach parameters objects
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
        #   bestnorm - final chi^2
        #   orignorm - starting value of chi^2
        #   status - fitting status code
        #   niter - number of iterations
        #   nfev - number of function evaluations
        #   npar - total number of parameters
        #   nfree - number of free parameters
        #   npegged - number of pegged parameters
        #   nfunc - number of residuals (= num. of data points)
        #   resid - list of final residuals
        #   xerror - final parameter uncertainties (1-sigma), in the order of elements in `Pars` property dictionary.
        #   x - final parameters values list in the order of elements in `Pars` property dictionary.
        #   debug - string with derivatives debugging output
        #   covar - final parameters covariance matrix.
        # You can also access result dictionary with `[my configure -results]`.
        #
        # Synopsis: -funct value -m value -pdata value ?-ftol value? ?-xtol value? ?-gtol value? ?-stepfactor value?
        #   ?-covtol value? ?-maxiter value? ?-maxfev value? ?-epsfcn value? ?-nofinitecheck? 
        set arguments [argparse -inline\
                               -help {Creates optimization object that does least squares fitting using modified \
                                              Levenberg-Marquardt algorithm. For more detailed description please see\
                                              documentation} {
            {-funct= -required -help {Name of the procedure that should be minimized}}
            {-m= -required -help {Number of data points}}
            {-pdata= -default {} -help {List or dictionary that provides private data to funct that is needed to\
                                                evaluate residuals. Usually it contains x and y values lists, but you\
                                                can provide any data necessary for function residuals evaluation. Will\
                                                be passed upon each function evaluation without modification}}
            {-ftol= -default 1e-10 -help {Control termination of mpfit. Termination occurs when both the actual and\
                                                  predicted relative reductions in the sum of squares are at most ftol}}
            {-xtol= -default 1e-10 -help {Control termination of mpfit. Termination occurs when the relative error\
                                                  between two consecutive iterates is at most xtol}}
            {-gtol= -default 1e-10 -help {Control termination of mpfit. Termination occurs when the cosine of the angle\
                                                  between fvec and any column of the jacobian is at most gtol in\
                                                  absolute value}}
            {-stepfactor= -default 100 -help {Used in determining the initial step bound. This bound is set to the\
                                                      product of factor and the euclidean norm of diag*x if nonzero, or\
                                                      else to factor itself}}
            {-covtol= -default 1e-14 -help {Range tolerance for covariance calculation}}
            {-maxiter= -default 200 -help {Maximum number of iterations}}
            {-maxfev= -default 0 -help {Control termination of mpfit. Termination occurs when the number of calls to\
                                                funct is at least maxfev by the end of an iteration. If it equals to 0,\
                                                number of evaluations is not restricted}}
            {-epsfcn= -default 2.2204460e-16 -help {Finite derivative step size}}
            {-nofinitecheck -boolean -help {Enable check for infinite quantities}}
        }]
        dict for {elName elValue} $arguments {
            my configure -$elName $elValue
        }
    }
    method run {} {
        # Runs optimization.
        # Returns: dictionary containing resulted data
        set sideMap [dict create auto 0 right 1 left -1 both 2 an 3]
        if {![info exists Pars]} {
            return -code error {At least one parameter must be attached to optimizer before call to run}
        }
        set pars [dvalues [my getAllPars]]
        set npar [llength $pars]
        foreach par $pars {
            lappend xall [$par configure -initval]
        }
        set qanylim false
        set npegged 0
        set nfev 0
        set info 0
        set fnorm -1.0
        set fnorm1 -1.0
        set xnorm -1.0
        set delta 0.0
        foreach par $pars {
            if {[$par configure -fixed]} {
                lappend pfixed true
            } else {
                lappend pfixed false
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
            if {![@ $pfixed $i]} {
                incr nfree
                lset ifree $j $i
                incr j
            }
        }
        if {$nfree==0} {
            return -code error {All parameters are fixed, optimization is not possible}
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
            if {li($qllim,$i) || li($qulim,$i)} {
                set qanylim true
            }
        }
        if {$m<$nfree} {
            return -code error "Degree of freedom check failed because of 'm=$m>=n=$nfree'"
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
        if {[llength $fvec]!=$m} {
            return -code error "Length of list '[llength $fvec]' returned from the function is less than m '$m' value"
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
            set fdjac2Data [my Fdjac2 $funct $ifree $nfree $xnew $fvec $ldfjac $epsfcn $pdata $nfev $step $dstep\
                                    $mpside $qulim $ulim $ddebug $ddrtol $ddatol]
            set fjac [dget $fdjac2Data fjac]
            set nfev [dget $fdjac2Data nfev]
            if {[dexist $fdjac2Data debug]} {
                lappend debugOutput {*}[dget $fdjac2Data debug]
            }
            # Determine if any of the parameters are pegged at the limits
            if {$qanylim} {
                for {set j 0} {$j<$nfree} {incr j} {
                    set lpegged [= {li($qllim,$j) && (li($x,$j)==li($llim,$j))}]
                    set upegged [= {li($qulim,$j) && (li($x,$j)==li($ulim,$j))}]
                    set sum 0
                    # If the parameter is pegged at a limit, compute the gradient direction
                    if {$lpegged || $upegged} {
                        set ij [= {$j*$ldfjac}]
                        for {set i 0} {$i<$m} {incr i} {
                            set sum [= {$sum+li($fvec,$i)*li($fjac,$ij)}]
                            incr ij
                        }
                    }
                    # If pegged at lower limit and gradient is toward negative then reset gradient to zero
                    if {$lpegged && ($sum>0)} {
                        set ij [= {$j*$ldfjac}]
                        for {set i 0} {$i<$m} {incr i} {
                            lset fjac $ij 0
                            incr ij
                        }
                    }
                    # If pegged at upper limit and gradient is toward positive then reset gradient to zero
                    if {$upegged && ($sum<0)} {
                        set ij [= {$j*$ldfjac}]
                        for {set i 0} {$i<$m} {incr i} {
                            lset fjac $ij 0
                            incr ij
                        }
                    }
                }
            }
            # Compute the QR factorization of the jacobian
            set qfracData [my Qfrac $nfree $fjac $ldfjac 1 $nfree]
            set ipvt [dget $qfracData ipvt]
            set fjac [dget $qfracData a]
            set wa1 [dget $qfracData rdiag]
            set wa2 [dget $qfracData acnorm]
            set wa3 [dget $qfracData wa]
            # on the first iteration and if mode is 1, scale according to the norms of the columns of the initial 
            # jacobian.
            if {$iter==1} {
                for {set j 0} {$j<$nfree} {incr j} {
                    lset diag [@ $ifree $j] [@ $wa2 $j]
                    if {li($wa2,$j)==0.0} {
                        lset diag [@ $ifree $j] 1.0
                    }
                }
                # on the first iteration, calculate the norm of the scaled x and initialize the step bound delta.
                for {set j 0} {$j<$nfree} {incr j} {
                    lset wa3 $j [= {li($diag,li($ifree,$j))*li($x,$j)}]
                }
                set xnorm [my Enorm $wa3]
                set delta [= {$stepfactor*$xnorm}]
                if {$delta==0.0} {
                    set delta $stepfactor
                }
            }
            # form (q transpose)*fvec and store the first n components in qtf
            for {set i 0} {$i<$m} {incr i} {
                lset wa4 $i [@ $fvec $i]
            }
            set jj 0
            for {set j 0} {$j<$nfree} {incr j} {
                set temp3 [@ $fjac $jj]
                if {$temp3!=0.0} {
                    set sum 0.0
                    set ij $jj
                    for {set i $j} {$i<$m} {incr i} {
                        set sum [= {$sum+li($fjac,$ij)*li($wa4,$i)}]
                        incr ij; # fjac[i+m*j]
                    }
                    set temp [= {-$sum/$temp3}]
                    set ij $jj
                    for {set i $j} {$i<$m} {incr i} {
                        lset wa4 $i [= {li($wa4,$i)+li($fjac,$ij)*$temp}]
                        incr ij; # fjac[i+m*j]
                    }
                }
                lset fjac $jj [@ $wa1 $j]
                incr jj [= {$m+1}]; # fjac[j+m*j]"
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
                        if {li($fjac,$off+$i)>$::tclopt::MP_GIANT} {
                            set nonfinite 1
                        }
                    }
                    incr off $ldfjac
                }
                if {$nonfinite} {
                    return -code error {Overflow occured during finite check}
                }
            }
            # compute the norm of the scaled gradient.
            set gnorm 0.0
            if {$fnorm!=0.0} {
                set jj 0
                for {set j 0} {$j<$nfree} {incr j} {
                    set l [@ $ipvt $j]
                    if {li($wa2,$l)!=0.0} {
                        set sum 0.0
                        set ij $jj
                        for {set i 0} {$i<=$j} {incr i} {
                            set sum [= {$sum+li($fjac,$ij)*li($qtf,$i)/$fnorm}]
                            incr ij; # fjac[i+m*j] 
                        }
                        set gnorm [my Dmax1 $gnorm [= {abs($sum/li($wa2,$l))}]]
                    }
                    incr jj $m
                }
            }
            # test for convergence of the gradient norm.
            if {$gnorm<=$gtol} {
                set info {Convergence in orthogonality}
            }
            if {$info!=0} {
                break
            }
            if {$maxiter==0} {
                set info {Maximum number of iterations reached}
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
                    lset wa1 $j [= {-li($wa1,$j)}]
                }
                set alpha 1.0
                if {!$qanylim} {
                    # No parameter limits, so just move to new position WA2
                    for {set j 0} {$j<$nfree} {incr j} {
                        lset wa2 $j [= {li($x,$j)+li($wa1,$j)}]
                    }
                } else {
                    # Respect the limits.  If a step were to go out of bounds, then 
                    # we should take a step in the same direction but shorter distance.
                    # The step should take us right to the limit in that case.
                    for {set j 0} {$j<$nfree} {incr j} {
                        set lpegged [= {li($qllim,$j) && (li($x,$j)<=li($llim,$j))}]
                        set upegged [= {li($qulim,$j) && (li($x,$j)>=li($ulim,$j))}]
                        set dwa1 [= {abs(li($wa1,$j))>$::tclopt::MP_MACHEP0}]
                        if {$lpegged && (li($wa1,$j)<0)} {
                            lset wa1 $j 0
                        }
                        if {$upegged && (li($wa1,$j)>0)} {
                            lset wa1 $j 0
                        }
                        if {$dwa1 && li($qllim,$j) && ((li($x,$j)+li($wa1,$j))<li($llim,$j))} {
                            set alpha [my Dmin1 $alpha [= {(li($llim,$j)-li($x,$j))/li($wa1,$j)}]]
                        }
                        if {$dwa1 && li($qulim,$j) && ((li($x,$j)+li($wa1,$j))>li($ulim,$j))} {
                            set alpha [my Dmin1 $alpha [= {(li($ulim,$j)-li($x,$j))/li($wa1,$j)}]]
                        }
                    }
                    # Scale the resulting vector, advance to the next position
                    for {set j 0} {$j<$nfree} {incr j} {
                        lset wa1 $j [= {li($wa1,$j)*$alpha}]
                        lset wa2 $j [= {li($x,$j)+li($wa1,$j)}]
                        # Adjust the output values.  If the step put us exactly on a boundary, make sure it is exact.
                        set sgnu [= {(li($ulim,$j)>=0) ? 1 : -1}]
                        set sgnl [= {(li($llim,$j)>=0) ? 1 : -1}]
                        set ulim1 [= {li($ulim,$j)*(1-$sgnu*$::tclopt::MP_MACHEP0)-\
                                              ((li($ulim,$j)==0) ? $::tclopt::MP_MACHEP0 : 0)}]
                        set llim1 [= {li($llim,$j)*(1+$sgnl*$::tclopt::MP_MACHEP0)+\
                                              ((li($llim,$j)==0) ? $::tclopt::MP_MACHEP0 : 0)}]
                        if {li($qulim,$j) && (li($wa2,$j)>=$ulim1)} {
                            lset wa2 $j [@ $ulim $j]
                        }
                        if {li($qllim,$j) && (li($wa2,$j)<=$llim1)} {
                            lset wa2 $j [@ $llim $j]
                        }
                    }
                }
                for {set j 0} {$j<$nfree} {incr j} {
                    lset wa3 $j [= {li($diag,li($ifree,$j))*li($wa1,$j)}]
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
                set actred -1.0
                if {[= {0.1*$fnorm1}]<$fnorm} {
                    set temp [= {$fnorm1/$fnorm}]
                    set actred [= {1.0-$temp*$temp}]
                }
                # compute the scaled predicted reduction and the scaled directional derivative.
                set jj 0
                for {set j 0} {$j<$nfree} {incr j} {
                    lset wa3 $j 0.0
                    set l [@ $ipvt $j]
                    set temp [@ $wa1 $l]
                    set ij $jj
                    for {set i 0} {$i<=$j} {incr i} {
                        lset wa3 $i [= {li($wa3,$i)+li($fjac,$ij)*$temp}]
                        incr ij
                    }
                    incr jj $m
                }
                # Remember, alpha is the fraction of the full LM step actually taken
                set temp1 [= {[my Enorm $wa3]*$alpha/$fnorm}]
                set temp2 [= {sqrt($par*$alpha)*$pnorm/$fnorm}]
                set prered [= {$temp1*$temp1+($temp2*$temp2)/0.5}]
                set dirder [= {-($temp1*$temp1+$temp2*$temp2)}]
                # compute the ratio of the actual to the predicted reduction
                set ratio 0.0
                if {$prered!=0.0} {
                    set ratio [= {$actred/$prered}]
                }
                # update the step bound.
                if {$ratio<=0.25} {
                    if {$actred>=0.0} {
                        set temp 0.5
                    } else {
                        set temp [= {0.5*$dirder/($dirder+0.5*$actred)}]
                    }
                    if {((0.1*$fnorm1)>=$fnorm) || ($temp<0.1)} {
                        set temp 0.1
                    }
                    set delta [= {$temp*[my Dmin1 $delta [= {$pnorm/0.1}]]}]
                    set par [= {$par/$temp}]
                } else {
                    if {($par==0.0) || ($ratio>=0.75)} {
                        set delta [= {$pnorm/0.5}]
                        set par [= {0.5*$par}]
                    }
                }
                # test for successful iteration.
                if {$ratio>=1e-4} {
                    # successful iteration. update x, fvec, and their norms.
                    for {set j 0} {$j<$nfree} {incr j} {
                        lset x $j [@ $wa2 $j]
                        #puts $x
                        lset wa2 $j [= {li($diag,li($ifree,$j))*li($x,$j)}]
                    }
                    for {set i 0} {$i<$m} {incr i} {
                        lset fvec $i [@ $wa4 $i]
                    }
                    set xnorm [my Enorm $wa2]
                    set fnorm $fnorm1
                    incr iter
                }
                # tests for convergence.
                if {(abs($actred)<=$ftol) && ($prered<=$ftol) && (0.5*$ratio<=1.0)} {
                    set info {Convergence in chi-square value}
                }
                if {$delta<=($xtol*$xnorm)} {
                    set info {Convergence in parameter value}
                }
                if {(abs($actred)<=$ftol) && ($prered<=$ftol) && (0.5*$ratio<=1.0) && ($info==2)} {
                    set info {Both convergence in parameter value and convergence in chi-square value hold}
                }
                if {$info!=0} {
                    break
                }
                # tests for termination and stringent tolerances.
                if {($maxfev>0) && ($nfev>=$maxfev)} {
                    # Too many function evaluations
                    set info {Maximum number of function evaluations reached}
                }
                if {$iter>=$maxiter} {
                    # Too many iterations
                    set info {Maximum number of iterations reached}
                }
                if {(abs($actred)<=$::tclopt::MP_MACHEP0) && ($prered<=$::tclopt::MP_MACHEP0) && (0.5*$ratio<=1.0)} {
                    set info {ftol is too small, no further improvement}
                }
                if {$delta<=$::tclopt::MP_MACHEP0*$xnorm} {
                    set info {xtol is too small, no further improvement}
                }
                if {$gnorm<=$::tclopt::MP_MACHEP0} {
                    set info {gtol is too small, no further improvement}
                }
                if {$info!=0} {
                    break
                }
                # end of the inner loop. repeat if iteration unsuccessful.
                if {$ratio<1e-4} {
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
            if {($parsLim0 && ($parsLim0==li($xall,$i))) || ($parsLim1 && ($parsLim1==li($xall,$i)))} {
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
                lset covar [= {int(li($ifree,$j)*$npar+li($ifree,$i))}] [@ $fjac [= {int($j*$ldfjac+$i)}]]
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
        for {set j 0} {$j<$m} {incr j} {
            lappend resid [@ $fvec $j]
        }
        if {[info exists debugOutput]} {
            set debugOutput [join $debugOutput \n]
        } else {
            set debugOutput {}
        }
        set resDict [dcreate bestnorm $bestnorm orignorm $orignorm status $info niter $iter nfev $nfev npar $npar\
                   nfree $nfree npegged $npegged nfunc $m resid $resid xerror $xerror x $xall debug $debugOutput\
                   covar $covar]
        set results $resDict
        return $resDict
    }
    method Fdjac2 {funct ifree n x fvec ldfjac epsfcn pdata nfev step dstep dside qulimited ulimit ddebug ddrtol\
                           ddatol} {
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
        for {set i 0} {$i<$n*$m} {incr i} {
            lappend fjac 0
        }
        # Check for which parameters need analytical derivatives and which need numerical ones
        for {set j 0} {$j<$n} {incr j} {
            if {li($dside,li($ifree,$j))==3 && li($ddebug,li($ifree,$j))==0} {
                # Purely analytical derivatives
                lappend derivs [@ $ifree $j]; # get index of parameter for which we need to calculate analytical derivative
                set has_analytical_deriv 1
            } elseif {li($ddebug,li($ifree,$j))==1} {
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
                for {set j 0} {$j<$m} {incr j} {
                    # replace m elements in fjac for each parameter for which which we calculate the analytic derivatives
                    lset fjac [= {$insertIndex*$m+$j}] [@ $dvecData [= {$i*$m+$j}]]
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
                    incr ij $m; # still need to advance fjac pointer 
                    continue
                }
                set temp [@ $x [@ $ifree $j]]
                set h [= {$eps*abs($temp)}]
                if {li($step,li($ifree,$j))>0} {
                    set h [@ $step [@ $ifree $j]]
                }
                if {li($dstep,li($ifree,$j))>0} {
                    set h [= {abs(li($dstep,li($ifree,$j))*$temp)}]
                }
                if {$h==0.0} {
                    set h $eps
                }
                # If negative step requested, or we are against the upper limit
                if {($dside ne {}) && $dsidei==-1} {
                    set h [= {-$h}]
                }
                if {($dside ne {}) && $dsidei==0 && ($qulimited ne {}) && (li($ulimit,$j) ne {{}})} {
                    if {li($qulimited,$j) && ($temp>(li($ulimit,$j)-$h))} {
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
                    if {($debug eq {}) || ($debug==0)} {
                        # Non-debug path for speed
                        for {set i 0} {$i<$m} {incr i} {
                            lset fjac $ij [= {(li($wa,$i)-li($fvec,$i))/$h}]
                            incr ij
                        }
                    } else {
                        # Debug path for correctness
                        for {set i 0} {$i<$m} {incr i} {
                            set fjold [@ $fjac $ij]
                            lset fjac $ij [= {(li($wa,$i)-li($fvec,$i))/$h}]
                            if {($da==0 && $dr==0 && ($fjold!=0 || li($fjac,$ij)!=0)) || (($da!=0 || $dr!=0) &&\
                                    (abs($fjold-li($fjac,$ij))>($da+abs($fjold)*$dr)))} {
                                set debugLine [format "%10d %10.4g %10.4g %10.4g %10.4g %10.4g" $i [@ $fvec $i] $fjold\
                                                       [@ $fjac $ij] [= {$fjold-li($fjac,$ij)}]\
                                                       [= {($fjold==0) ? 0 : (($fjold-li($fjac,$ij))/$fjold)}]]
                                puts $debugLine
                                lappend debugOutput $debugLine
                            }
                            incr ij
                        }
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
                    incr nfev
                    lset x [@ $ifree $j] $temp
                    # Now compute derivative as (f(x+h) - f(x-h))/(2h)
                    if {$debug eq {} || $debug==0} {
                        # Non-debug path for speed
                        for {set i 0} {$i<$m} {incr i} {
                            lset fjac $ij [= {(li($wa2,$ij)-li($wa,$i))/(2*$h)}]
                            incr ij
                        }
                    } else {
                        # Debug path for correctness
                        for {set i 0} {$i<$m} {incr i} {
                            set fjold [@ $fjac $ij]
                            lset fjac $ij [= {(li($wa2,$i)-li($wa,$i))/(2*$h)}]
                            if {($da==0 && $dr==0 && ($fjold!=0 || li($fjac,$ij)!=0)) || (($da!=0 || $dr!=0) &&\
                                    (abs($fjold-li($fjac,$ij))>($da+abs($fjold)*$dr)))} {
                                set debugLine [format "%10d %10.4g %10.4g %10.4g %10.4g %10.4g" $i [@ $fvec $i] $fjold\
                                                       [@ $fjac $ij] [= {$fjold-li($fjac,$ij)}]\
                                                       [= {($fjold==0) ? 0 : (($fjold-li($fjac,$ij))/$fjold)}]]
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
            set footer {FJAC DEBUG END}
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
    method Qfrac {n a lda pivot lipvt} {
        # Does QR factorization
        #  n - number of columns of matrix a
        #  a - matrix of size m by n in form of 1d list [[column0] [column1] [column2] ... [columnn]]
        #  lda - leading dimension of a
        #  pivot - true for column pivoting enforcing
        #  lipvt - a positive integer input variable. if pivot is false,
        #	 then lipvt may be as small as 1. if pivot is true, then
        #	 lipvt must be at least n.
        # Returns: dictionary 
        ::tclopt::Lists2arrays aArray [list $a]
        ::tclopt::NewArrays {rdiagArray acnormArray waArray} [list $n $n $n]
        ::tclopt::NewArrays ipvtArray $lipvt int
        ::tclopt::mp_qrfac $m $n $aArray $lda $pivot $ipvtArray $lipvt $rdiagArray $acnormArray $waArray
        ::tclopt::Arrays2lists {aList rdiagList acnormList waList}\
                [list $aArray $rdiagArray $acnormArray $waArray] [list [llength $a] $n $n $n]
        ::tclopt::Arrays2lists ipvtList $ipvtArray $lipvt int
        ::tclopt::DeleteArrays [list $aArray $rdiagArray $acnormArray $waArray]
        ::tclopt::DeleteArrays [list $ipvtArray] int
        return [dcreate a $aList rdiag $rdiagList acnorm $acnormList wa $waList ipvt $ipvtList]
    }
    method Enorm {x} {
        # Calculate euclidean norm of vector
        #  x - list with values of vector x
        # Returns: norm 
        # Synopsis: -x list -y list -xi list
        ::tclopt::Lists2arrays xArray [list $x]
        set norm [::tclopt::mp_enorm [llength $x] $xArray]
        ::tclopt::DeleteArrays $xArray
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
        #  par - is a nonnegative variable. on input par contains an initial estimate of the levenberg-marquardt 
        #    parameter.
        # Returns: dictionary 
        # Synopsis: -x list -y list -xi list
        ::tclopt::Lists2arrays {rArray diagArray qtbArray} [list $r $diag $qtb]
        ::tclopt::Lists2arrays {ipvtArray ifreeArray} [list $ipvt $ifree] int
        ::tclopt::NewPointers parPnt
        ::tclopt::doublep_assign $parPnt $par
        ::tclopt::NewArrays {xArray sdiagArray wa1Array wa2Array} [list $n $n $n $n]
        ::tclopt::mp_lmpar $n $rArray $ldr $ipvtArray $ifreeArray $diagArray $qtbArray $delta $parPnt $xArray\
                $sdiagArray $wa1Array $wa2Array
        ::tclopt::Arrays2lists {rList xList sdiagList wa1List wa2List}\
                [list $rArray $xArray $sdiagArray $wa1Array $wa2Array] [list [llength $r] $n $n $n $n]
        set parVal [::tclopt::doublep_value $parPnt]
        ::tclopt::DeleteArrays [list $rArray $xArray $sdiagArray $wa1Array $wa2Array]
        ::tclopt::DeleteArrays [list $ipvtArray $ifreeArray] int
        ::tclopt::DeletePointers $parPnt
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
        ::tclopt::Lists2arrays rArray [list $r]
        ::tclopt::Lists2arrays ipvtArray [list $ipvt] int
        ::tclopt::NewArrays waArray $n
        ::tclopt::mp_covar $n $rArray $ldr $ipvtArray $tol $waArray
        ::tclopt::Arrays2lists {rList waList} [list $rArray $waArray] [list [llength $r] $n]
        ::tclopt::DeleteArrays [list $rArray $waArray]
        ::tclopt::DeleteArrays $ipvtArray int
        return [dcreate r $rList wa $waList]
    }
    method Dmax1 {a b} {
        return [= {$a>=$b ? $a : $b}]
    }
    method Dmin1 {a b} {
        return [= {$a<=$b ? $a : $b}]
    }
}

oo::configurable create ::tclopt::DE {
    superclass ::tclopt::Optimization
    property strategy -set {
        classvariable availableStrategies
        if {$value in $availableStrategies} {
            set strategy $value
            return
        } else {
            return -code error "Strategy '$value' is not in the list of availible strategies '$availableStrategies'"
        }
    }
    property initype -set {
        classvariable availibleInitTypes
        if {$value in $availibleInitTypes} {
            set initype $value
            return
        } else {
            return -code error "initype '$value' is not in the list of availible types '$availibleInitTypes'"
        }
    }
    property genmax -set [string map {@type@ integer @name@ genmax @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property refresh -set [string map {@type@ integer @name@ refresh @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property d -set [string map {@type@ integer @name@ d @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property np -set [string map {@type@ integer @name@ np @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property f -set [string map {@type@ double @name@ f @article@ a} $::tclopt::numberConfigureCheck]
    property cr -set [string map {@type@ double @name@ cr @condition@ {($value<0) || ($value>1.0)} @condString@\
                                              {within [0,1] range} @article@ a} $::tclopt::numberEqConfigureCheck]
    property seed -set [string map {@type@ integer @name@ seed @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property abstol -set [string map {@type@ double @name@ abstol @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property reltol -set [string map {@type@ double @name@ reltol @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property debug -set [string map {@type@ boolean @name@ debug @article@ a} $::tclopt::numberConfigureCheck]
    property threshold -set [string map {@type@ double @name@ threshold @condition@ {$value<=0.0} @condString@\
                                                 {more than 0.0} @article@ a} $::tclopt::numberEqConfigureCheck]
    property history -set [string map {@type@ boolean @name@ history @article@ a} $::tclopt::numberConfigureCheck]
    property histfreq -set [string map {@type@ integer @name@ histfreq @condition@ {$value<=0} @condString@\
                                                {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property savepop -set [string map {@type@ boolean @name@ savepop @article@ a} $::tclopt::numberConfigureCheck]
    property pdata
    property initpop
    property results -kind readable
    variable funct strategy genmax refresh d np f cr seed abstol reltol debug initype initpop pdata results threshold\
            history histfreq savepop
    variable Pars
    initialize {
        variable availableStrategies
        const availableStrategies {best/1/exp rand/1/exp rand-to-best/1/exp best/2/exp rand/2/exp best/1/bin rand/1/bin\
                                           rand-to-best/1/bin best/2/bin rand/2/bin}
        variable availibleInitTypes
        const availibleInitTypes {random specified}
    }
    constructor {args} {
        # Creates optimization object that tuns optimization using modified Differential Evolution algorithm.
        #  -funct value - name of the procedure that should be minimized 
        #  -strategy value - choice of strategy. Possible strategies: best/1/exp rand/1/exp rand-to-best/1/exp
        #    best/2/exp rand/2/exp best/1/bin rand/1/bin rand-to-best/1/bin best/2/bin rand/2/bin.
        #  -pdata value - list or dictionary that provides private data to funct that is needed to evaluate object
        #    (cost) function. Usually it contains x and y values lists, but you can provide any data necessary for
        #    function evaluation.  Will be passed upon each function evaluation without modification.
        #  -genmax value - maximum number of generations. Controls termination of optimization. Default 3000.
        #  -refresh value - output refresh cycle. Represent the frequency of printing debug information to stdout.
        #  -np value - population size. Represents number of random parameter vector per generation. As a first guess
        #    for the value it is recommended to set it from 5 to 10 times the number of parameters. Default is 20.
        #  -f value - weight factor (mutation rate). Controls the amplification of the differential variation between
        #    individuals. It is a scaling factor applied to the difference between two randomly selected population
        #    vectors before adding the result to a third vector to create a mutant vector (exact mechanism is dependent
        #    on selected strategy). The mutation rate influences the algorithm's ability to explore the search space; a
        #    higher value of `f` increases the diversity of the mutant vectors, leading to broader exploration, while a
        #    lower value encourages convergence by making smaller adjustments. The typical range for `f` is between 0.4
        #    and 1.0, though values outside this range can be used depending on the problem characteristics. Default is
        #    0.9.
        #  -cr value - crossing over factor (crossover rate). Controls the probability of mixing components from the
        #    target vector and the mutant vector to form a trial vector. It determines how much of the trial vector
        #    inherits its components from the mutant vector versus the target vector. A high crossover rate means that
        #    more components will come from the mutant vector, promoting exploration of new solutions. Conversely, a low
        #    crossover rate results in more components being taken from the target vector, which can help maintain
        #    existing solutions and refine them. The typical range for CR is between 0.0 and 1.0. Default is 0.9.
        #  -seed value - random seed.
        #  -abstol value - absolute tolerance. Controls termination of optimization. Default 1e-6.
        #  -reltol value - relative tolerance. Controls termination of optimization. Default 1e-2.
        #  -debug - print debug messages during optimization.
        #  -threshold value - objective function threshold that stops optimization
        #  -random - select population initialization with random values over the individual parameters ranges.
        #  -specified - select population initialization with specified population values, requires `-initpop`.
        #  -initpop value - list of lists (matrix) with size np x d, requires `-specified`.
        #  -history - enables collecting scalar history and best trajectory
        #  -histfreq value - save history every N generations. Default is 1.
        #  -savepop - enables including population snapshots in history (every `-histfreq` generations), requires
        #    `-history`.
        # Returns: object of class
        #
        # Class implements the Differential Evolution (DE) algorithm to solve global optimization problems over
        # continuous parameter spaces. Typically, it is used to minimize a user-supplied objective function by evolving
        # a population of candidate solutions through mutation, crossover, and selection. 
        #
        # Differential Evolution is a stochastic, population-based optimizer that works well for non-linear,
        # non-differentiable, and multi-modal objective functions. It does not require gradient information and is
        # effective in high-dimensional or rugged search spaces. The user-supplied objective function should take a
        # vector of parameters as input and return a scalar value to be minimized. For example, the objective function
        # might compute the volume of material used in a structure given its geometric parameters, the error rate of a
        # machine learning model, or the energy of a physical system. DE begins by initializing a population of random
        # candidate solutions within given parameter bounds and iteratively refines them by combining members of the
        # population and selecting better solutions over generations.
        #
        # Simple constraints are placed on parameter values by adding objects of class [::tclopt::Parameter] to DE with
        # method [::tclopt::Optimization::addPars]. For details of how to specify constraints, please look at the
        # description of [::tclopt::Parameter] class. Please note, that order in which we attach parameters objects is
        # the order in which values will be supplied to minimized function, and the order in which resulted will be
        # written to `x` property of the class.
        #
        # #### General advices
        # - f is usually between 0.5 and 1 (in rare cases > 1)
        # - cr is between 0 and 1 with 0., 0.3, 0.7 and 1. being worth to be tried first
        # - To start off np = 10*d is a reasonable choice. Increase np if misconvergence happens.
        # - If you increase np, f usually has to be decreased
        # - When the DE/best... schemes fail DE/rand... usually works and vice versa
        #
        # #### Strategies overview
        # Naming convention for strategies: x/y/z, where:
        #  - x - a string which denotes the vector to be perturbed (mutated)
        #  - y - number of difference vectors taken for perturbation (mutation) of x
        #  - z - crossover method (exp = exponential, bin = binomial)
        #
        # ##### Mutation
        # Combination of x and y gives following mutation function:
        #
        # best/1:
        #```
        #  →    →         →     →    
        #  u  = x  + f ⋅ ⎛x   - x  ⎞
        #   i    b       ⎝ r2    r3⎠
        #```
        #
        # rand/1:
        #```
        #  →    →         →     →    
        #  u  = x  + f ⋅ ⎛x   - x  ⎞
        #   i    r1      ⎝ r2    r3⎠
        #```
        #
        # rand-to-best/1 (custom variant):
        #```
        #  →    →         →     →           →     → 
        #  u  = x  + f ⋅ ⎛x   - x  ⎞ + f ⋅ ⎛x   - x  ⎞
        #   i    i       ⎝ b     i ⎠       ⎝ r1    r2⎠
        #```
        #
        # best/2:
        #```
        #  →    →         →     →     →     →
        #  u  = x  + f ⋅ ⎛x   + x   - x   - x  ⎞
        #   i    b       ⎝ r1    r2    r3    r4⎠
        #```
        #
        # rand/2:
        #```
        #  →    →         →     →     →     →
        #  u  = x  + f ⋅ ⎛x   + x   - x   - x  ⎞
        #   i    r5      ⎝ r1    r2    r3    r4⎠
        #```
        #
        # x_i - trial vector, x_b - best vector, x_rn - randomly selected individuals from population.
        #
        # A crossover operation between the new generated mutant vector v_i and the target vector x_i is used to further
        # increase the diversity of the new candidate solution.
        #
        # ##### Exponential crossover
        # In exponential crossover, a contiguous block of dimensions is modified, starting from a random index, and
        # continues as long as random values are less than CR. The mutation happens inline during crossover, and
        # wrapping around is supported.
        #
        #```
        #Example (D = 10, n = 3, L = 4):
        #
        #Parent x_i:         [x0 x1 x2 x3 x4 x5 x6 x7 x8 x9]
        #Exponential mask:             →  →  →  →
        #                              n  n+1 n+2 n+3
        #
        #Trial u_i:          [x0 x1 x2 v3 v4 v5 v6 x7 x8 x9]
        #                             ↑ mutated from DE strategy
        #```
        #
        # - Starts from a random index n ∈ \[0, D)
        # - Replaces a contiguous block of components (dimension-wise)
        # - Continues as long as rand() < CR, up to D components
        # - Mutation and crossover are applied together in the code, not as separate stages.
        #
        # ##### Binomial crossover
        # In binomial crossover, each dimension has an independent probability CR of being replaced by the mutant
        # vector. At least one dimension is guaranteed to be copied from the mutant (typically by forcing one fixed
        # index to be included).
        #
        #```
        #Example (D = 10):
        #
        #Parent x_i:         [x0 x1 x2 x3 x4 x5 x6 x7 x8 x9]
        #Random mask:         ✗  ✓  ✗  ✗  ✓  ✗  ✓  ✗  ✗  ✓
        #Mutant values:      [v0 v1 v2 v3 v4 v5 v6 v7 v8 v9]
        #
        #Trial u_i:          [x0 v1 x2 x3 v4 x5 v6 x7 x8 v9]
        #                          ↑       ↑       ↑     ↑
        #                    replaced where rand() < CR
        #```
        #
        # - Applies independent crossover decision for each dimension
        # - Starts at a random dimension n, iterates D steps circularly
        # - Each dimension is replaced with probability CR
        # - Ensures at least one dimension is modified (usually last)
        # - Mutation and crossover are applied together in the code, not as separate stages.
        #
        # ##### Summary of strategies
        #
        #ruffopt includedformats html
        # <div style="ruff_bd"> <table class="ruff_deflist"> <tbody>
        # <tr><th>ID</th><th>Base</th><th>Difference</th><th>XOV</th><th>Description</th></tr>
        # <tr><td>1</td><td>best</td><td>r2-r3</td><td>exp</td><td>Exploitative, may misconv</td></tr>
        # <tr><td>2</td><td>r1</td><td>r2-r3</td><td>exp</td><td>Balanced, exploratory<br>Try e.g. F=0.7 and CR=0.5
        # first</td></tr> <tr><td>3</td><td>x_i</td><td>(best-x_i)+(r1-r2)</td><td>exp</td><td>Hybrid: pull +
        # variation<br>Try e.g. F=0.85 and CR=1 first</td></tr>
        # <tr><td>4</td><td>best</td><td>(r1+r2)-(r3+r4)</td><td>exp</td><td>Exploratory, best-guided</td></tr>
        # <tr><td>5</td><td>r5</td><td>(r1+r2)-(r3+r4)</td><td>exp</td><td>Fully random, robust</td></tr>
        # <tr><td>6</td><td>best</td><td>r2-r3</td><td>bin</td><td>Same as 1, binomial crossover</td></tr>
        # <tr><td>7</td><td>r1</td><td>r2-r3</td><td>bin</td><td>Same as 2, binomial crossover</td></tr>
        # <tr><td>8</td><td>x_i</td><td>(best-x_i)+(r1-r2)</td><td>bin</td><td>Same as 3, binomial crossover</td></tr>
        # <tr><td>9</td><td>best</td><td>(r1+r2)-(r3+r4)</td><td>bin</td><td>Same as 4, binomial crossover</td></tr>
        # <tr><td>10</td><td>r5</td><td>(r1+r2)-(r3+r4)</td><td>bin</td><td>Same as 5, binomial crossover</td></tr>
        # </tbody> </table> </div>
        #
        #ruffopt includedformats nroff
        #```
        # ┌────┬──────────┬─────────────────────────────┬─────┬──────────────────────────────────┐
        # │ ID │  Base    │ Difference                  │ XOV │ Description                      │
        # ├────┼──────────┼─────────────────────────────┼─────┼──────────────────────────────────┤
        # │ 1  │ best     │ r2 - r3                     │ exp │ Exploitative, may misconverge    │
        # │ 2  │ r1       │ r2 - r3                     │ exp │ Balanced, exploratory            │
        # │    │          │                             │     │ Try e.g. F=0.7 and CR=0.5 first  │
        # │ 3  │ x_i      │ (best - x_i) + (r1 - r2)    │ exp │ Hybrid: pull + variation         │
        # │    │          │                             │     │ Try e.g  F=0.85 and CR=1 first   │
        # │ 4  │ best     │ (r1 + r2) - (r3 + r4)       │ exp │ Exploratory, best-guided         │
        # │ 5  │ r5       │ (r1 + r2) - (r3 + r4)       │ exp │ Fully random, robust             │
        # │ 6  │ best     │ r2 - r3                     │ bin │ Same as 1, binomial crossover    │
        # │ 7  │ r1       │ r2 - r3                     │ bin │ Same as 2, binomial crossover    │
        # │ 8  │ x_i      │ (best - x_i) + (r1 - r2)    │ bin │ Same as 3, binomial crossover    │
        # │ 9  │ best     │ (r1 + r2) - (r3 + r4)       │ bin │ Same as 4, binomial crossover    │
        # │ 10 │ r5       │ (r1 + r2) - (r3 + r4)       │ bin │ Same as 5, binomial crossover    │
        # └────┴──────────┴─────────────────────────────┴─────┴──────────────────────────────────┘
        #```
        #
        #ruffopt includedformats markdown
        # | ID | Base  | Difference                         | XOV | Description                         |
        # |----|-------|------------------------------------|-----|-------------------------------------|
        # | 1  | `best`| `r2 - r3`                          | exp | Exploitative, may misconverge       |
        # | 2  | `r1`  | `r2 - r3`                          | exp | Balanced, exploratory<br>Try e.g. F=0.7 and CR=0.5 first |
        # | 3  | `x_i` | `(best - x_i) + (r1 - r2)`         | exp | Hybrid: pull + variation<br>Try e.g. F=0.85 and CR=1 first |
        # | 4  | `best`| `(r1 + r2) - (r3 + r4)`            | exp | Exploratory, best-guided            |
        # | 5  | `r5`  | `(r1 + r2) - (r3 + r4)`            | exp | Fully random, robust                |
        # | 6  | `best`| `r2 - r3`                          | bin | Same as 1, binomial crossover       |
        # | 7  | `r1`  | `r2 - r3`                          | bin | Same as 2, binomial crossover       |
        # | 8  | `x_i` | `(best - x_i) + (r1 - r2)`         | bin | Same as 3, binomial crossover       |
        # | 9  | `best`| `(r1 + r2) - (r3 + r4)`            | bin | Same as 4, binomial crossover       |
        # | 10 | `r5`  | `(r1 + r2) - (r3 + r4)`            | bin | Same as 5, binomial crossover       |
        #
        #
        # See more information in [techreport](http://mirror.krakadikt.com/2004-11-13-genetic-algorithms/www.icsi.berkeley.edu/%257Estorn/deshort1.ps)
        #
        # Description of keys and data in returned dictionary (not including history mode):
        #   objfunc - final value of object (cost) function `funct`
        #   x - final vector of parameters
        #   generation - number of generations
        #   strategy - strategy used for optimization
        #   std - standard deviation of final population
        # You can also access result dictionary with `[my configure -results]`.
        #
        # #### History mode
        #
        # When the `-history` flag is provided, `result` also includes the following keys:
        #
        # Key `history` \- a dictionary with keys (one per `-histfreq` generation):
        #   gen - generation index
        #   bestf - best-so-far objective value after this generation
        #   mean - mean objective value across the current population
        #   std - standard deviation of objective values in the current population
        #   nfev - cumulative number of function evaluations at the end of this generation
        #
        # Key `besttraj` \- a dictionary with keys (one per `-histfreq` generation):
        #   gen - generation index
        #   x - parameter vector achieving the best-so-far objective value after this generation
        #
        # If the `-savepop` switch is provided as well, `result` additionally contains key `pophistory` with dictionary
        # with keys (one per `-histfreq` generation):
        #   gen - generation index
        #   pop - list of population vectors for this generation (length = `-np`; each vector length = d)
        #   cost - list of objective values aligned with `pop` (length = `-np`)

        # Synopsis: -funct value -strategy value -pdata value ?-genmax value? ?-refresh value? ?-np value?
        #   ?-f value? ?-cr value? ?-seed value? ?-abstol value? ?-reltol value? ?-debug?
        #   ?-random|specified -initpop value? ?-history? ?-histfreq value? ?-savepop?
        set arguments [argparse -inline\
                               -help {Creates optimization object that does Differential Evolution optimization.\
                                              For more detailed description please see documentation} {
            {-funct= -required -help {Name of the procedure that should be minimized}}
            {-strategy= -required -help {Choice of strategy}}
            {-pdata= -default {} -help {List or dictionary that provides private data to funct that is needed to\
                                                evaluate residuals. Usually it contains x and y values lists, but you\
                                                can provide any data necessary for function residuals evaluation. Will\
                                                be passed upon each function evaluation without modification}}
            {-genmax= -default 3000 -help {Maximum number of generations}}
            {-refresh= -default 100 -help {Output refresh cycle}}
            {-np= -default 20 -help {Population size}}
            {-f= -default 0.9 -help {Weight factor (mutation rate)}}
            {-cr= -default 0.9 -help {Crossing over factor (crossover rate)}}
            {-seed= -default 1 -help {Random seed}}
            {-abstol= -default 1e-6 -help {Absolute tolerance}}
            {-reltol= -default 0.01 -help {Relative tolerance}}
            {-debug -boolean -help {Print debug information}}
            {-threshold= -help {Objective function threshold that stops optimization}}
            {-random -key initype -default random -help {Random population initialization}}
            {-specified -key initype -value specified -help {Specified points population initialization}}
            {-initpop= -require specified -reciprocal -help {Specified initial population}}
            {-history -boolean -help {Collect scalar history and best trajectory}}
            {-histfreq= -default 1 -help {Save history every N generations}}
            {-savepop -boolean -require history -help {Include population snapshots in history (every -histfreq\
                                                            generations)}}
        }]
        dict for {elName elValue} $arguments {
            if {$elName ni {threshold}} {
                my configure -$elName $elValue
            }
        }
        if {[dict exists $arguments threshold]} {
            set threshold [dict get $arguments threshold]
        }
    }
    method ReflectScalarMod {x L U} {
        # Reflect a scalar x into [L,U] using modulo arithmetic (no loop).
        if {$U <= $L} {
            return [= {$x < $L ? $L : ($x > $U ? $U : $x)}]
        }
        set L [= {double($L)}]
        set U [= {double($U)}]
        set W [= {$U-$L}] ;# width > 0
        set twoW [= {2.0*$W}]
        set y [= {fmod($x - $L, $twoW)}]
        if {$y<0.0} { 
            set y [= {$y + $twoW}] 
        }
        # 0..W → forward, W..2W → reflected back
        if {$y<=$W} {
            set xr [= {$L+$y}]
        } else {
            set xr [= {$U-($y-$W)}]
        }
        # FP safety
        if {$xr<$L} { 
            set xr $L 
        }
        if {$xr>$U} { 
            set xr $U 
        }
        return $xr
    }
    method run {} {
        # Runs optimization.
        # Returns: dictionary containing resulted data

        ### Initialize random number generator
        ::tclopt::NewPointers idum long
        ::tclopt::longp_assign $idum [= {-$seed}]
        set nfeval 0 ;# reset number of function evaluations
        ### Initialization
        set pars [dvalues [my getAllPars]]
        set d [llength $pars]
        for {set j 0} {$j<$d} {incr j} {
            set par [@ $pars $j]
            lappend lowlims [$par configure -lowlim]
            lappend uplims [$par configure -uplim]
        }
        if {$initype eq {specified}} {
            if {[llength $initpop]!=$np} {
                return -code error "Number of specified initial parameters populations should be equal to populations\
                        size '$np' but '[llength $initpop]' was provided"
            } else {
                foreach paramSet $initpop {
                    if {[llength $paramSet]!=$d} {
                        return -code error "Number of specified initial  parameters per population should be equal to\
                                number of parameters '$d' but '[llength $paramSet]' was provided"
                    }
                }
            }
        }
        for {set i 0} {$i<$np} {incr i} {
        ### spread initial population members
            for {set j 0} {$j<$d} {incr j} {
                set par [@ $pars $j]
                if {$initype eq {specified}} {
                    set value [@ $initpop $i $j]
                    if {($value<[$par configure -lowlim]) || ($value>[$par configure -uplim])} {
                        return -code error "Value '$value' from specified initial population is outside provided lower\
                                '[$par configure -lowlim]' or upper limit '$value>[$par configure -uplim]' for\
                                parameter '[$par configure -name]'"
                    }
                    lappend cj [@ $initpop $i $j]
                } elseif {$initype eq {random}} {
                    lappend cj [= {[$par configure -lowlim]+[::tclopt::rnd_uni $idum]*\
                                           ([$par configure -uplim]-[$par configure -lowlim])}]
                }
            }
            lappend c $cj
            lappend cost [$funct $cj $pdata]
            incr nfeval
            unset cj
        }
        set cmin [@ $cost 0]
        set imin 0
        for {set i 1} {$i<$np} {incr i} {
            if {[@ $cost $i]<$cmin} {
                set cmin [@ $cost $i]
                set imin $i
            }
        }
        set best [@ $c $imin] ;# save best member ever
        set bestit [@ $c $imin] ;# save best member of generation
        set pold $c
        for {set i 0} {$i<$np} {incr i} {
            for {set j 0} {$j<$d} {incr j} {
                lappend pnewj 0.0
            }
            lappend pnew $pnewj
            unset pnewj
        }
        ### Iteration loop
        set gen 0 ;# generation counter reset
        set histScalar {} ;# list of dicts: {gen ... bestf ... mean ... std ... nfev ...}
        set histBestx {} ;# list of dicts: {gen ... x {...}}
        set histPop {} ;# list of dicts (optional): {gen ... pop {...} cost {...}}
        while true {
            incr gen
            set imin 0
            ####  Start of loop through ensemble
            for {set i 0} {$i<$np} {incr i} {
                do {
                    set r1 [= {int([::tclopt::rnd_uni $idum]*$np)}]
                } while {$r1==$i}
                do {
                    set r2 [= {int([::tclopt::rnd_uni $idum]*$np)}]
                } while {($r2==$i) || ($r2==$r1)}
                do {
                    set r3 [= {int([::tclopt::rnd_uni $idum]*$np)}]
                } while {($r3==$i) || ($r3==$r1) || ($r3==$r2)}
                do {
                    set r4 [= {int([::tclopt::rnd_uni $idum]*$np)}]
                } while {($r4==$i) || ($r4==$r1) || ($r4==$r2) || ($r4==$r3)}
                do {
                    set r5 [= {int([::tclopt::rnd_uni $idum]*$np)}]
                } while {($r5==$i) || ($r5==$r1) || ($r5==$r2) || ($r5==$r3) || ($r5==$r4)}
                ####  Choice of strategy
                #####   best/1/exp
                if {$strategy eq {best/1/exp}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    do {
                        set tmpValue [= {[@ $bestit $n]+$f*([@ $pold $r2 $n]-[@ $pold $r3 $n])}]
                        set lowLim [@ $lowlims $n]
                        set upLim [@ $uplims $n]
                        lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        set n [= {($n+1)%$d}]
                        incr l
                    } while {([::tclopt::rnd_uni $idum]<$cr) && ($l<$d)}
                #####   rand/1/exp
                } elseif {$strategy eq {rand/1/exp}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    do {
                        set tmpValue [= {[@ $pold $r1 $n]+$f*([@ $pold $r2 $n]-[@ $pold $r3 $n])}]
                        set lowLim [@ $lowlims $n]
                        set upLim [@ $uplims $n]
                        lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        set n [= {($n+1)%$d}]
                        incr l
                    } while {([::tclopt::rnd_uni $idum]<$cr) && ($l<$d)}
                #####   rand-to-best/1/exp
                } elseif {$strategy eq {rand-to-best/1/exp}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    do {
                        set tmpValue [= {[@ $tmp $n]+$f*([@ $bestit $n]-[@ $tmp $n])+$f*([@ $pold $r1 $n]-\
                                                                                                 [@ $pold $r2 $n])}]
                        set lowLim [@ $lowlims $n]
                        set upLim [@ $uplims $n]
                        lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        set n [= {($n+1)%$d}]
                        incr l
                    } while {([::tclopt::rnd_uni $idum]<$cr) && ($l<$d)}
                #####   best/2/exp
                } elseif {$strategy eq {best/2/exp}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    do {
                        set tmpValue [= {[@ $bestit $n]+([@ $pold $r1 $n]+[@ $pold $r2 $n]-[@ $pold $r3 $n]-\
                                                                 [@ $pold $r4 $n])*$f}]
                        set lowLim [@ $lowlims $n]
                        set upLim [@ $uplims $n]
                        lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        set n [= {($n+1)%$d}]
                        incr l
                    } while {([::tclopt::rnd_uni $idum]<$cr) && ($l<$d)}
                #####   rand/2/exp
                } elseif {$strategy eq {rand/2/exp}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    do {
                        set tmpValue [= {[@ $pold $r5 $n]+([@ $pold $r1 $n]+[@ $pold $r2 $n]-[@ $pold $r3 $n]-\
                                                                   [@ $pold $r4 $n])*$f}]
                        set lowLim [@ $lowlims $n]
                        set upLim [@ $uplims $n]
                        lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        set n [= {($n+1)%$d}]
                        incr l
                    } while {([::tclopt::rnd_uni $idum]<$cr) && ($l<$d)}
                #####   best/1/bin
                } elseif {$strategy eq {best/1/bin}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    # perform D binomial trials
                    for {set l 0} {$l<$d} {incr l} {
                        if {([::tclopt::rnd_uni $idum]<$cr) || ($l==($d-1))} {
                            set tmpValue [= {[@ $bestit $n]+$f*([@ $pold $r2 $n]-[@ $pold $r3 $n])}]
                            set lowLim [@ $lowlims $n]
                            set upLim [@ $uplims $n]
                            lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        }
                        set n [= {($n+1)%$d}]
                    }
                #####   rand/1/bin
                } elseif {$strategy eq {rand/1/bin}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    # perform D binomial trials
                    for {set l 0} {$l<$d} {incr l} {
                        if {([::tclopt::rnd_uni $idum]<$cr) || ($l==($d-1))} {
                            set tmpValue [= {[@ $pold $r1 $n]+$f*([@ $pold $r2 $n]-[@ $pold $r3 $n])}]
                            set lowLim [@ $lowlims $n]
                            set upLim [@ $uplims $n]
                            lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        }
                        set n [= {($n+1)%$d}]
                    }
                #####   rand-to-best/1/bin
                } elseif {$strategy eq {rand-to-best/1/bin}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    # perform D binomial trials
                    for {set l 0} {$l<$d} {incr l} {
                        if {([::tclopt::rnd_uni $idum]<$cr) || ($l==($d-1))} {
                            set tmpValue [= {[@ $tmp $n]+$f*([@ $bestit $n]-[@ $tmp $n])+$f*([@ $pold $r1 $n]-\
                                                                                                     [@ $pold $r2 $n])}]
                            set lowLim [@ $lowlims $n]
                            set upLim [@ $uplims $n]
                            lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        }
                        set n [= {($n+1)%$d}]
                    }
                #####   best/2/bin
                } elseif {$strategy eq {best/2/bin}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    # perform D binomial trials
                    for {set l 0} {$l<$d} {incr l} {
                        if {([::tclopt::rnd_uni $idum]<$cr) || ($l==($d-1))} {
                            set tmpValue [= {[@ $bestit $n]+$f*([@ $pold $r1 $n]+[@ $pold $r2 $n]-[@ $pold $r3 $n]-\
                                                                        [@ $pold $r4 $n])*$f}]
                            set lowLim [@ $lowlims $n]
                            set upLim [@ $uplims $n]
                            lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        }
                        set n [= {($n+1)%$d}]
                    }
                #####   rand/2/bin
                } elseif {$strategy eq {rand/2/bin}} {
                    set tmp [@ $pold $i]
                    set n [= {int([::tclopt::rnd_uni $idum]*$d)}]
                    set l 0
                    # perform D binomial trials
                    for {set l 0} {$l<$d} {incr l} {
                        if {([::tclopt::rnd_uni $idum]<$cr) || ($l==($d-1))} {
                            set tmpValue [= {[@ $pold $r5 $n]+$f*([@ $pold $r1 $n]+[@ $pold $r2 $n]-[@ $pold $r3 $n]-\
                                                                          [@ $pold $r4 $n])*$f}]
                            set lowLim [@ $lowlims $n]
                            set upLim [@ $uplims $n]
                            lset tmp $n [my ReflectScalarMod $tmpValue $lowLim $upLim]
                        }
                        set n [= {($n+1)%$d}]
                    }
                }
                ####  Test how good this choice really was
                set trial_cost [$funct $tmp $pdata]
                incr nfeval
                # improved objective function value ?
                if {$trial_cost<=[@ $cost $i]} {
                    lset cost $i $trial_cost
                    lset pnew $i $tmp
                    if {$trial_cost<$cmin} {
                        set cmin $trial_cost
                        set imin $i
                        set best $tmp
                    }
                } else {
                    lset pnew $i [@ $pold $i] ;# replace target with old value
                }
            }
            set bestit $best ;# Save best population member of current iteration
            set pold $pnew
            ####  Compute the energy variance
            set cmean 0.0 ;# compute the mean value first
            for {set j 0} {$j<$np} {incr j} {
                set cmean [= {$cmean+[@ $cost $j]}]
            }
            set cmean [= {$cmean/$np}]
            set cvar 0.0 ;# compute the variance
            for {set j 0} {$j<$np} {incr j} {
                set cvar [= {$cvar+([@ $cost $j]-$cmean)*([@ $cost $j]-$cmean)}]
            }
            set stddev [= {sqrt($cvar/($np-1))}]
            ####  Save history information
            if {($gen%$refresh==1) && $debug} {
                puts [format "Best-so-far cost funct. value=%-15.10g" $cmin]
                for {set j 0} {$j<$d} {incr j} {
                    set par [@ $pars $j]
                    puts [format "Parameter %s=%-15.10g" [$par configure -name] [@ $best $j]]
                }
                puts [format "Generation=%d  NFEs=%ld   Strategy: %s" $gen $nfeval $strategy]
                puts [format "NP=%d F=%-4.2g CR=%-4.2g std=%-10.5g" $np $f $cr $stddev]
            }
            if {$history && ($gen%$histfreq)==0} {
                lappend histScalar [dcreate gen $gen bestf $cmin mean $cmean std $stddev nfev $nfeval]
                lappend histBestx [dcreate gen $gen x $best]
                if {$savepop} {
                    lappend histPop [dcreate gen $gen pop $pold cost $cost]
                }
            }
            ####  Check stop criterias
            if {[info exists threshold]} {
                if {$cmin<=$threshold} {
                    set info "Optimization stopped due to reaching threshold of objective function '$threshold'"
                    break
                }
            }
            if {($stddev<=[= {$abstol+$reltol*abs($cmean)}])} {
                set info "Optimization stopped due to crossing threshold\
                        'abstol+reltol*abs(mean)=[= {$abstol+$reltol*abs($cmean)}]' of objective function\
                        population member standard deviation"
                break
            }
            if {$gen>=$genmax} {
                set info "Optimization stopped due to reaching maximum number of generations '$genmax'"
                break
            }
        }
        ::tclopt::DeletePointers $idum long
        ### Save results
        if {$history} {
            lappend histScalar [dcreate gen $gen bestf $cmin mean $cmean std $stddev nfev $nfeval]
            lappend histBestx [dcreate gen $gen x $best]
            if {$savepop} {
                lappend histPop [dcreate gen $gen pop $pold cost $cost]
                set results [dcreate objfunc $cmin x $best generation $gen nfev $nfeval strategy $strategy std $stddev\
                                     info $info history $histScalar besttraj $histBestx pophistory $histPop]
            } else {
                set results [dcreate objfunc $cmin x $best generation $gen nfev $nfeval strategy $strategy std $stddev\
                                     info $info history $histScalar besttraj $histBestx]
            }
        } else {
            set results [dcreate objfunc $cmin x $best generation $gen nfev $nfeval strategy $strategy std $stddev info\
                                 $info]
        }
        return $results
    }
}

oo::configurable create ::tclopt::GSA {
    superclass ::tclopt::Optimization
    property maxiter -set [string map {@type@ integer @name@ maxiter @condition@ {$value<0} @condString@\
                                               {more than or equal to zero} @article@ an}\
                                   $::tclopt::numberEqConfigureCheck]
    property mininniter -set [string map {@type@ integer @name@ mininniter @condition@ {$value<1} @condString@\
                                                  {more than or equal to 1} @article@ an}\
                                      $::tclopt::numberEqConfigureCheck]
    property maxinniter -set [string map {@type@ integer @name@ maxinniter @condition@ {$value<20} @condString@\
                                                  {more than or equal to 20} @article@ an}\
                                      $::tclopt::numberEqConfigureCheck]
    property maxfev -set [string map {@type@ integer @name@ maxfev @condition@ {$value<1} @condString@\
                                              {more or equal to 1} @article@ an} $::tclopt::numberEqConfigureCheck]
    property seed -set [string map {@type@ integer @name@ seed @condition@ {$value<=0} @condString@\
                                            {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property ntrial -set [string map {@type@ integer @name@ ntrial @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property nbase -set [string map {@type@ integer @name@ nbase @condition@ {$value<=0} @condString@\
                                              {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property debug -set [string map {@type@ boolean @name@ debug @article@ a} $::tclopt::numberConfigureCheck]
    property qv -set [string map {@type@ double @name@ qv @condition@ {$value<=1.0 || $value>=3.0} @condString@\
                                          {more than 1.0 and lower than 3.0} @article@ a}\
                              $::tclopt::numberEqConfigureCheck]
    property qa -set [string map {@type@ double @name@ qa @condition@ {$value==1.0} @condString@\
                                          {not equal to 1.0} @article@ a} $::tclopt::numberEqConfigureCheck]
    property tmin -set [string map {@type@ double @name@ tmin @condition@ {$value<=0.0} @condString@\
                                            {more than 0.0} @article@ a} $::tclopt::numberEqConfigureCheck]
    property temp0 -set [string map {@type@ double @name@ temp0 @condition@ {$value<=0.0} @condString@\
                                                {more than 0.0} @article@ a} $::tclopt::numberEqConfigureCheck]
    property history  -set [string map {@type@ boolean @name@ history @article@ a} $::tclopt::numberConfigureCheck]
    property histfreq -set [string map {@type@ integer @name@ histfreq @condition@ {$value<=0} @condString@\
                                                {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property savemoves  -set [string map {@type@ boolean @name@ savemoves @article@ a} $::tclopt::numberConfigureCheck]
    property initype -set {
        classvariable availibleInitTypes
        if {$value in $availibleInitTypes} {
            set initype $value
            return
        } else {
            return -code error "initype '$value' is not in the list of availible types '$availibleInitTypes'"
        }
    }
    property threshold -set [string map {@type@ double @name@ threshold @condition@ {$value<=0.0} @condString@\
                                                 {more than 0.0} @article@ a} $::tclopt::numberEqConfigureCheck]
    property pdata
    property results -kind readable
    variable funct maxiter maxfev pdata results debug ntrial mininniter tmin qa qv nbase seed maxinniter initype\
            temp0 threshold history histfreq savemoves
    variable Pars
    initialize {
        variable availibleInitTypes
        const availibleInitTypes {random specified}
    }
    method ReflectScalarMod {x L U} {
        # Reflect a scalar x into [L,U] using modulo arithmetic (no loop).
        if {$U <= $L} {
            return [= {$x < $L ? $L : ($x > $U ? $U : $x)}]
        }
        set L [= {double($L)}]
        set U [= {double($U)}]
        set W [= {$U-$L}] ;# width > 0
        set twoW [= {2.0*$W}]
        set y [= {fmod($x - $L, $twoW)}]
        if {$y<0.0} { 
            set y [= {$y + $twoW}] 
        }
        # 0..W → forward, W..2W → reflected back
        if {$y<=$W} {
            set xr [= {$L+$y}]
        } else {
            set xr [= {$U-($y-$W)}]
        }
        # FP safety
        if {$xr<$L} { 
            set xr $L 
        }
        if {$xr>$U} { 
            set xr $U 
        }
        return $xr
    }
    constructor {args} {
        # Creates optimization object that tuns optimization using modified Gegeneralized Simulation Annealing
        # algorithm.
        #  -funct value - name of the procedure that should be minimized
        #  -pdata value - list or dictionary that provides private data to funct that is needed to evaluate object
        #    (cost) function. Usually it contains x and y values lists, but you can provide any data necessary for
        #    function evaluation.  Will be passed upon each function evaluation without modification.
        #  -maxiter value - maximum number of temperature steps. Controls termination of optimization. Default is 5000
        #  -mininniter value - minimum number of iterations per temperature, default is 10
        #  -maxinniter value - maximum number of iterations per temperature, default is 1000
        #  -maxfev value - maximum number of objective function evaluation. Controls termination of optimization if
        #    provided.
        #  -seed value - random seed, default is 0
        #  -ntrial value - initial number of samples to determine initial temperature `temp0` (if not provided), default
        #    is 20
        #  -nbase value - base number of iterations within single temperature, default is 30
        #  -qv value - visiting distribution parameter, must satisfy 1 < qv < 3. Default 2.62.
        #  -qa value - acceptance distribution parameter (qa ≠ 1, can be negative). Default -5.0.
        #  -tmin value - stop when temperature ≤ tmin. Default 1e-5.
        #  -temp0 value - initial temperature value; if not given, estimated from ntrial samples.
        #  -debug - enables debug information printing
        #  -threshold value - stop when best objective ≤ threshold (optional).
        #  -random - random parameter vector initialization
        #  -specified - specified points parameter vector initialization
        #  -history - enables collecting scalar history and best trajectory
        #  -histfreq value - save history every N generations. Default is 1.
        #  -savemoves - enables including accepted moves snapshots in history (every `-histfreq` generations), requires
        #    `-history`.
        # Returns: object of class
        #
        # Class implements the Generalized Simulated Annealing (GSA) algorithm to solve global optimization problems
        # over continuous parameter spaces.
        # 
        # Generalized Simulated Annealing (GSA) is an enhanced version of the classical simulated annealing algorithm,
        # rooted in Tsallis statistics. It replaces traditional temperature schedules and perturbation distributions
        # with generalized forms: specifically, it uses a distorted Cauchy–Lorentz visiting distribution controlled by a
        # parameter `qv​`, allowing for more flexible exploration of the solution space. The algorithm introduces
        # artificial “temperatures” that gradually cool, injecting stochasticity to help the search process escape local
        # minima and eventually converge within the basin of a global minimum.
        #
        # Main source of information is [this article](https://journal.r-project.org/archive/2013/RJ-2013-002/RJ-2013-002.pdf).
        # 
        # Simple constraints are placed on parameter values by adding objects of class [::tclopt::Parameter] to GSA with
        # method [::tclopt::Optimization::addPars]. For details of how to specify constraints, please look at the
        # description of [::tclopt::Parameter] class. Please note, that order in which we attach parameters objects is
        # the order in which values will be supplied to minimized function, and the order in which resulted will be
        # written to `x` property of the class.
        #
        # #### General steps of algorithm
        # ##### 1. Inputs & setup
        # - Provide: objective proc name, parameter objects, and algorithm controls parameters.
        # - Initialize RNG state
        # ##### 2. Choose initial parameter vector x_0
        # - If `-specified`, take each parameter’s `-initval`.
        # - If `-random`, sample uniformly within bounds: x_i = Unif[low_i​, up_i], i - i'th parameter
        # ##### 3. Estimate initial temperature temp0 (if not provided)
        # - Draw `-ntrial` random vectors uniformly within the box; evaluate objective at each.
        # - If `-random`, sample uniformly within bounds: x_i = Unif[low_i​, up_i], i - i'th parameter
        # - Let d be the number of parameters. Compute sample mean and std. dev. of objective values; set:
        #```
        # temp  = stddev({f(x)})
        #     0              
        #```
        #
        # ##### 4. Initialize loop state
        # - Current point/value:
        #```
        #  →       →   →      →  →  
        #  x     = x , f    = f ⎛x ⎞
        #   curr    0   curr    ⎝ 0⎠
        #```
        # - Best-so-far within the current temperature: copy current to “best”.
        # ##### 5. Outer loop over temperatures (cooling)
        # - For outer iteration k=0,1,2,…, temperature is (Tsallis cooling):
        #```
        #              ⎛ (qv - 1)    ⎞
        #      temp0 ⋅ ⎝2         - 1⎠
        # T  = ───────────────────────
        #  k            (qv - 1)      
        #        (1 + t)         - 1  
        #```
        # ##### 6. Choose inner-iterations at this temperature
        # - Inner iteration budget at T_k:
        #```
        #          ⎛                ⎛                  ⎛         ⎛  -d  ⎞⎞⎞⎞
        #          ⎜                ⎜                  ⎜         ⎜──────⎟⎟⎟⎟
        #          ⎜                ⎜                  ⎜         ⎝3 - qv⎠⎟⎟⎟
        # n  = min ⎜maxinniter, max ⎜mininniter, floor ⎜nbase ⋅ T        ⎟⎟⎟
        #  t       ⎝                ⎝                  ⎝         k       ⎠⎠⎠
        #```
        # where d \- number of parameters.
        # ##### 7. Inner loop: propose, clamp, evaluate, accept. For t=1,..., n_t:
        # - Visit/perturb each coordinate (distorted Cauchy–Lorentz with qv). Draw u~Unif(0,1). If u>=0.5, sign=1,
        #   else sign=-1. Then step is:
        #```
        #                                ____________________
        #                               ╱        (qv - 1)    
        #              ⎛   1  ⎞        ╱ ⎛   1  ⎞            
        #              ⎜──────⎟       ╱  ⎜──────⎟         - 1
        #              ⎝3 - qv⎠      ╱   ⎝|2u−1|⎠            
        # Δx = sign ⋅ T         ⋅   ╱    ────────────────────
        #              k          ╲╱            qv - 1       
        #```
        #   Apply per coordinate, then clamp with modulo reflection into [low, up].
        # - Evaluate candidate and calculate the difference:
        #```
        #    →               →
        # f ⎛x    ⎞; Δf = f ⎛x    ⎞ - f    
        #   ⎝ cand⎠         ⎝ cand⎠    curr
        #```
        # - Acceptance rule (generalized qa-Metropolis): if Δf<=0 - accept, else accept with probability:
        #
        #```
        # If qa=1:
        #         ⎛-Δf ⋅ k⎞
        # p = exp ⎜───────⎟
        #         ⎜  T    ⎟
        #         ⎝   k   ⎠
        # If qa < 1:
        #             (1 - qa) ⋅ Δf ⋅ k
        #     z = 1 - ─────────────────
        #                    T         
        #                     k        
        #
        #     If z<=0 then p=0, else:
        #
        #          ⎛   1  ⎞
        #          ⎜──────⎟
        #          ⎝1 - qa⎠
        #     p = z  
        #
        # If qa > 1:
        #                            ⎛  -1  ⎞
        #                            ⎜──────⎟
        #                            ⎝qa - 1⎠
        #     ⎛    (qa - 1) ⋅ Δf ⋅ k⎞        
        # p = ⎜1 + ─────────────────⎟        
        #     ⎜           T         ⎟        
        #     ⎝            k        ⎠        
        #```
        # Accept with probability p.
        # ##### 8. Best-of-temperature recentering
        # - Track (x_best, f_best) during inner loop.
        # - After finishing n_k iterations, set:
        #```
        # →       →
        # x     = x    
        #  curr    best
        # →       →    
        # f     = f    
        #  curr    best
        #```
        # - Count attempted/accepted moves for diagnostics.
        # ##### 9. Stopping conditions (checked each outer step)
        # - If `-threshold` is set and best value lower or equal to threshold then stop.
        # - If k>=maxiter then stop.
        # - If T_k<=tmin then stop.
        # - If `-maxfev` is set and total function evals higher or equal to `-maxfev` then stop.
        # ##### 10. Advance temperature or finish
        # - If none of the stops triggered, increment k and repeat.
        # - On exit, return: best objective, best `x`, total evals, `temp0`, last `temp_q` (final T_k​), and a 
        #   human-readable `info` message.
        #
        # Description of keys and data in returned dictionary (not including history mode):
        #   objfunc - final value of object (cost) function `funct`
        #   x - final vector of parameters
        #   nfev - number of function evalutions
        #   temp0 - initial temperature
        #   tempend - end temperature
        #   info - convergence information
        #   niter - number of temperature iterations
        #
        # #### History mode
        #
        # When the `-history` flag is provided, `result` also includes the following keys:
        #
        # Key `history` \- a dictionary with keys (one per `-histfreq` temperature and after the last iteration):
        #   iter - temperature iteration index
        #   temp - current temperature value
        #   bestf - best-so-far (global best) objective value after this iteration
        #   currf - current objective value
        #   nt - number of iterations within current iteration (temperature)
        #   accratio - acceptance ratio in current iteration (temperature)
        #   nfev - cumulative number of function evaluations at the end of this iteration
        #
        # Key `besttraj` \- a dictionary with keys (one per `-histfreq` temperature and after the last iteration):
        #   iter - temperature iteration index
        #   x - parameter vector achieving the best-so-far (global best) objective value after this iteration
        #
        # If the `-savemoves` switch is provided as well, `result` additionally contains key `histmoves` with dictionary
        # with keys (one per `-histfreq` temperature and after the last iteration):
        #   iter - temperature iteration index
        #   moves - list of dictionaries that contains accepted moves in this temperature
        #
        # Each move in the list of moves is a dictionary with keys:
        #   tstep - index of step inside of current temperature iteration
        #   x - accepted parameter vector
        #   fx - value of objective function for that accepted parameter vector
        #
        # Synopsis: -funct value -pdata value ?-maxiter value? ?-mininniter value? ?-maxfev value? ?-seed value?
        #   ?-ntrial value? ?-nbase value? ?-qv value? ?-qa value? ?-tmin value? ?-temp0 value? ?-debug? ?-threshold
        #   value? ?-random|specified -initpop value? ?-history? ?-histfreq value? ?-savemoves?
        set arguments [argparse -inline\
                               -help {Creates optimization object that does General annealing simulation optimization.\
                                              For more detailed description please see documentation} {
            {-funct= -required -help {Name of the procedure that should be minimized}}
            {-pdata= -default {} -help {List or dictionary that provides private data to funct that is needed to\
                                                evaluate residuals. Usually it contains x and y values lists, but you\
                                                can provide any data necessary for function residuals evaluation. Will\
                                                be passed upon each function evaluation without modification}}
            {-maxiter= -default 5000 -help {Maximum number of temperature steps}}
            {-mininniter= -default 10 -help {Minimum number of iterations per temperature}}
            {-maxinniter= -default 1000 -help {Maximum number of iterations per temperature}}
            {-maxfev= -help {Maximum number of objective function evaluation}}
            {-seed= -default 1 -help {Random seed}}
            {-ntrial= -default 20 -help {Initial number of samples to determine initial temperature}}
            {-nbase= -default 30 -help {Base number of iterations within single temperature}}
            {-qv= -default 2.62 -help {Visiting distribution parameter}}
            {-qa= -default -5.0 -help {Parameter defining shape of the acceptance probability distribution}}
            {-tmin= -default 1e-5 -help {Lowest temperature value}}
            {-temp0= -help {Initial temperature value}}
            {-debug -boolean -help {Print debug information}}
            {-threshold= -help {Objective function threshold that stops optimization}}
            {-random -key initype -default random -help {Random parameter vector initialization}}
            {-specified -key initype -value specified -help {Specified points parameter vector initialization}}
            {-history -boolean -help {Collect scalar history and best trajectory}}
            {-histfreq= -default 1 -help {Save history every N temperature steps}}
            {-savemoves -boolean -require history -help {Include accepted-move snapshots per temperature}}
        }]
        if {[dict exists $arguments r]} {
            set r [dict get $arguments r]
        }
        if {[dict exists $arguments temp0]} {
            set temp0 [dict get $arguments temp0]
        }
        if {[dict exists $arguments threshold]} {
            set threshold [dict get $arguments threshold]
        }
        if {[dict exists $arguments maxfev]} {
            set maxfev [dict get $arguments maxfev]
        }
        dict for {elName elValue} $arguments {
            if {$elName ni {temp0 threshold maxfev}} {
                my configure -$elName $elValue
            }
        }
    }
    method run {} {
        ::tclopt::NewPointers idum long
        ::tclopt::longp_assign $idum [= {-$seed}]
        set nfeval 0 ;# reset number of function evaluations
        set pars [dvalues [my getAllPars]]
        set d [llength $pars]
        ### Set inital parameter vector
        if {$initype eq {specified}} {
            foreach par $pars {
                lappend xinit [$par configure -initval]
            }
        } else {
            foreach par $pars {
                set l [$par configure -lowlim]
                set u [$par configure -uplim]
                lappend xinit [= {$l+[::tclopt::rnd_uni $idum]*($u-$l)}]
            }
        }
        ### Estimate initial temperature
        if {![info exists temp0]} {
            for {set i 0} {$i < $ntrial} {incr i} {
                set xvec {}
                foreach par $pars {
                    set l [$par configure -lowlim]
                    set u [$par configure -uplim]
                    lappend xvec [= {$l+[::tclopt::rnd_uni $idum]*($u-$l)}]
                }
                lappend values [$funct $xvec $pdata]
            }
            # calculate mean
            set sum 0.0
            foreach value $values {
                set sum [= {$sum+$value}]
            }
            set mean [= {$sum/double([llength $values])}]
            # Compute standard deviation
            set sumsq 0.0
            foreach value $values {
                set sumsq [= {$sumsq+($value-$mean)*($value-$mean)}]
            }
            set stddev [= {sqrt($sumsq/double($ntrial))}]
            set temp0 $stddev
        }
        ### Start of the outer loop (cooling)
        set niter 1
        set xVecCurr $xinit
        set functCurrVal [$funct $xinit $pdata]
        set xGlobalBest $xinit
        set fGlobalBest $functCurrVal
        set histScalar {} ;# list of dicts: {iter ... temp ... bestf ... currf ... nt ... accratio ... nfev ...}
        set histBestx {} ;# list of dicts: {iter ... x {...}}
        set histMoves {} ;# optional per-temperature accepted moves (if -savemoves)
        incr nfeval
        while true {
            set tempq [= {$temp0*(pow(2.0, $qv-1.0)-1.0)/(pow(1.0+$niter, $qv-1.0)-1.0)}]
            ####  Calculate number of inner iterations within temperature
            set logNt [= {min(log(double($maxinniter)),\
                                      max(log(double($mininniter)),\
                                                  log(double($nbase))-double($d)/(3.0-$qv)*log(max($tempq, $tmin))))}]
            set nt [= {int(exp($logNt))}]
            ####  Start inner loop within the temperature
            set accepted 0
            set attempted 0
            set xVecBest $xVecCurr
            set functBestVal $functCurrVal
            # If saving moves, collect accepted trajectory within this temperature
            if {$history && $savemoves} {
                set movesThisTemp {}   ;# each item: {tstep <int> x <list> fx <double>}
            }
            for {set nti 0} {$nti<$nt} {incr nti} {
                #####   Pertrub initial vector
                set xVecCandidate {}
                foreach par $pars i [lseq 0 to [= {[llength $pars]-1}]] {
                    set lowlim [$par configure -lowlim]
                    set uplim [$par configure -uplim]
                    set u [::tclopt::rnd_uni $idum]
                    set sign [= {$u<0.5 ? -1.0 : 1.0}]
                    set u [= {abs(2.0*$u-1.0)}]
                    if {$u<1e-16} {
                        set u 1e-16
                    }
                    set dx [= {$sign*pow($tempq, 1.0/(3.0-$qv))*sqrt((pow(1.0/$u, $qv-1.0)-1.0)/($qv-1.0))}]
                    set xnew [= {[@ $xVecCurr $i]+$dx}]
                    set xnew [my ReflectScalarMod $xnew $lowlim $uplim]
                    lappend xVecCandidate $xnew
                }
                incr attempted
                set functCandidateVal [$funct $xVecCandidate $pdata]
                if {$functCandidateVal<$fGlobalBest} {
                    set fGlobalBest $functCandidateVal
                    set xGlobalBest $xVecCandidate
                }
                incr nfeval
                #####   Check accept or not the new solution
                # Acceptance temperature (optional speed-up)
                set Ta [= {$tempq/max(1,$niter)}]
                set deltaFunct [= {$functCandidateVal-$functCurrVal}]
                if {$deltaFunct <= 0.0} {
                    # Always accept downhill
                    set functCurrVal $functCandidateVal
                    set xVecCurr $xVecCandidate
                    if {$history && $savemoves} {
                        lappend movesThisTemp [dcreate tstep $nti x $xVecCurr fx $functCurrVal]
                    }
                    incr accepted
                } else {
                    # Compute acceptance probability for any qa
                    if {[= {abs($qa-1.0) < 1e-12}]} {
                        # qa -> 1 : classical Metropolis
                        set prob [= {exp(-$deltaFunct/$Ta)}]
                    } elseif {$qa < 1.0} {
                        # qa < 1 : cutoff if bracket <= 0
                        set z [= {1.0 - (1.0 - $qa)*$deltaFunct/$Ta}]
                        if {[= {$z <= 0.0}]} {
                            set prob 0.0
                        } else {
                            set prob [= {pow($z, 1.0/(1.0 - $qa))}]
                        }
                    } else {
                        # qa > 1 : heavy-tailed acceptance
                        set prob [= {pow(1.0 + ($qa - 1.0)*$deltaFunct/$Ta, -1.0/($qa - 1.0))}]
                    }
                    if {$prob > 1.0} { 
                        set prob 1.0
                    }
                    set u [::tclopt::rnd_uni $idum]
                    if {$u < $prob} {
                        set functCurrVal $functCandidateVal
                        set xVecCurr $xVecCandidate
                        if {$history && $savemoves} {
                            lappend movesThisTemp [dcreate tstep $nti x $xVecCurr fx $functCurrVal]
                        }
                        incr accepted
                    }
                }
                if {$functCurrVal < $functBestVal} {
                    set functBestVal $functCurrVal
                    set xVecBest $xVecCurr
                }
            }
            set xVecCurr $xVecBest
            set functCurrVal $functBestVal
            if {$attempted > 0} {
                set ratio [= {double($accepted)/$attempted}]
            } else {
                set ratio 0.0
            }
            ####  Save history information
            if {$history && ($niter % $histfreq) == 0} {
                # Scalars per temperature (outer iteration)
                lappend histScalar [dcreate iter $niter temp $tempq bestf $fGlobalBest currf $functCurrVal nt $nt\
                                            accratio $ratio nfev $nfeval]
                # Best-so-far trajectory
                lappend histBestx [dcreate iter $niter x $xGlobalBest]
                # Optional: accepted-move snapshots within this temperature
                if {$savemoves} {
                    # Note: only accepted moves are stored; can be empty if nothing accepted
                    lappend histMoves [dcreate iter $niter moves $movesThisTemp]
                }
            }
            ####  Check stop criterias
            if {[info exists threshold]} {
                if {$fGlobalBest<=$threshold} {
                    set info "Optimization stopped due to reaching threshold of objective function '$threshold'"
                    break
                }
            }
            if {$niter>=$maxiter} {
                set info "Optimization stopped due to reaching maximum number of iterations '$maxiter'"
                break
            }
            if {$tempq<=$tmin} {
                set info "Optimization stopped due to reaching minimum temperature '$tmin'"
                break
            }
            if {[info exists maxfev]} {
                if {$nfeval>=$maxfev} {
                    set info "Optimization stopped due to reaching maximum number of objective functions evaluation\
                            '$maxfev'"
                    break
                }
            }
            incr niter
        }
        ::tclopt::DeletePointers $idum long
        ### Save result
        if {$history} {
            lappend histScalar [dcreate iter $niter temp $tempq bestf $fGlobalBest currf $functCurrVal nt $nt\
                                        accratio $ratio nfev $nfeval]
            lappend histBestx [dcreate iter $niter x $xGlobalBest]
            if {$savemoves} {
                lappend histMoves [dcreate iter $niter moves $movesThisTemp]
                set results [dcreate objfunc $fGlobalBest x $xGlobalBest nfev $nfeval temp0 $temp0 tempend $tempq\
                                     info $info niter $niter history $histScalar besttraj $histBestx\
                                     histmoves $histMoves]
            } else {
                set results [dcreate objfunc $fGlobalBest x $xGlobalBest nfev $nfeval temp0 $temp0 tempend $tempq\
                                     info $info niter $niter history $histScalar besttraj $histBestx]
            }
        } else {
            set results [dcreate objfunc $fGlobalBest x $xGlobalBest nfev $nfeval temp0 $temp0 tempend $tempq\
                                 info $info niter $niter]
        }
        return $results
    }
}


oo::configurable create ::tclopt::LBFGS {
    superclass ::tclopt::Optimization
    property linesearch -set {
        classvariable availableLineSearchAlgorithms
        if {$value in $availableLineSearchAlgorithms} {
            set linesearch $value
            return
        } else {
            return -code error "Line search algorithm '$value' is not in the list of availible algorithms\
                    '$availableLineSearchAlgorithms'"
        }
    }
    property condition -set {
        classvariable availibeConditions
        if {$value in $availibeConditions} {
            set condition $value
            return
        } else {
            return -code error "Condition '$value' is not in the list of availible conditions '$availibeConditions'"
        }
    }
    property m -set [string map {@type@ integer @name@ m @condition@ {$value<=0} @condString@\
                                         {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property epsilon -set [string map {@type@ double @name@ epsilon @condition@ {$value<=0} @condString@\
                                               {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property past -set [string map {@type@ integer @name@ past @condition@ {$value<0} @condString@\
                                         {more or equal to zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property delta -set [string map {@type@ double @name@ delta @condition@ {$value<=0} @condString@\
                                               {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property maxiter -set [string map {@type@ integer @name@ maxiter @condition@ {$value<0} @condString@\
                                               {more than or equal to zero} @article@ an}\
                                   $::tclopt::numberEqConfigureCheck]
    property maxlinesearch -set [string map {@type@ integer @name@ maxlinesearch @condition@ {$value<=0} @condString@\
                                                     {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property minstep -set [string map {@type@ double @name@ minstep @condition@ {$value<=0} @condString@\
                                               {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property maxstep -set [string map {@type@ double @name@ maxstep @condition@ {$value<=0} @condString@\
                                               {more than zero} @article@ an} $::tclopt::numberEqConfigureCheck]
    property ftol -set [string map {@type@ double @name@ ftol @condition@ {$value<=0 || $value>=0.5} @condString@\
                                            {more than zero and less than 0.5} @article@ a}\
                                $::tclopt::numberEqConfigureCheck]
    property wolfe ; #add custom checking - should be more than ftol and less than 1.0
    property gtol ; #add custom checking - should be more than ftol and less than 1.0
    property xtol -set [string map {@type@ double @name@ xtol @condition@ {$value<=0} @condString@\
                                            {more than zero} @article@ a} $::tclopt::numberEqConfigureCheck]
    property orthantwisec -set [string map {@type@ double @name@ orthantwisec @condition@ {$value<0} @condString@\
                                                    {more or equal to zero} @article@ an}\
                                        $::tclopt::numberEqConfigureCheck]
    property orthantwisestart ; #add custom checking - should be more or equal to 0 and less than N
    property orthantwiseend ; #add custom checking - should be more than 0 and less or equal to N
    property pdata
    property results -kind readable
    variable funct m epsilon past delta maxlinesearch minstep maxstep ftol wolfe gtol xtol orthantwisec orthantwiseend\
            orthantwisestart pdata results maxiter linesearch errorStatus condition
    variable Pars
    initialize {
        variable availableLineSearchAlgorithms
        variable availibeConditions
        const availableLineSearchAlgorithms {morethuente backtracking}
        const availibeConditions {armijo wolfe strongwolfe}
    }
    constructor {args} {
        set arguments [argparse -inline\
                               -help {Creates optimization object that does local optimization using L-BFGS algorithm.\
                                              For more detailed description please see documentation} {
            {-funct= -required -help {Name of the procedure that should be minimized}}
            {-m= -default 6 -help {The number of corrections to approximate the inverse hessian matrix}}
            {-pdata= -default {} -help {List or dictionary that provides private data to funct that is needed to\
                                                evaluate residuals. Usually it contains x and y values lists, but you\
                                                can provide any data necessary for function residuals evaluation. Will\
                                                be passed upon each function evaluation without modification}}
            {-epsilon= -default 1e-5 -help {Epsilon for convergence test}}
            {-past= -default 0 -help {Distance for delta-based convergence test}}
            {-delta= -default 1e-5 -help {Delta for convergence test}}
            {-maxiter= -help {The maximum number of iterations}}
            {-linesearch= -default morethuente -help {The line search algorithm}}
            {-maxlinesearch= -default 40 -help {The maximum number of trials for the line search}}
            {-minstep= -default 1e-20 -help {The minimum step of the line search routine}}
            {-maxstep= -default 1e20 -help {The maximum step of the line search}}
            {-ftol= -default 1e-4 -help {A parameter to control the accuracy of the line search routine}}
            {-wolfe= -default 0.9 -help {A coefficient for the Wolfe condition}}
            {-gtol= -default 0.9 -help {A parameter to control the accuracy of the line search routine}}
            {-xtol= -default 1e-16 -help {The machine precision for floating-point values}}
            {-orthantwisec= -help {Coefficient for the L1 norm of variables}}
            {-condition= -default wolfe -help {Condition to satisfy in backtracking algorithm}}
            {-orthantwisestart= -require orthantwisec\
                     -help {Start index for computing L1 norm of the variables}}
            {-orthantwiseend= -require orthantwisec\
                     -help {End index for computing L1 norm of the variables}}
        }]
        dict for {elName elValue} $arguments {
            my configure -$elName $elValue
        }
    }
    method run {} {
        # Runs optimization.
        # Returns: dictionary containing resulted data
        if {![info exists Pars]} {
            return -code error {At least one parameter must be attached to optimizer before call to run}
        }
        set pars [dvalues [my getAllPars]]
        set n [llength $pars]
        if {$maxstep<$minstep} {
            return -code error "Invalid maxstep value '$maxstep' provided, must be more than minstep value '$minstep'"
        }
        if {$condition in {strongwolfe}} {
            if {($wolfe<$ftol) || ($wolfe>=1.0)} {
                return -code error "Invalid Wolfe coefficient '$wolfe' provided, must be more than ftol value '$ftol'\
                        and less than 1.0"
            }
        }
        if {[info exists orthantwisec]} {
            if {[info exists orthantwisestart]} {
                if {($orthantwisestart<0) || ($orthantwisestart>$n)} {
                    return -code error "Invalid orthantwisestart value '$orthantwisestart' provided, must be more than 0\
                            and less or equal to the number of parameters '$n'"
                }
            } else {
                set orthantwisestart 0
            }
            if {[info exists orthantwiseend]} {
                if {($orthantwiseend<0) || ($orthantwiseend>$n)} {
                    return -code error "Invalid orthantwiseend value '$orthantwiseend' provided, must be more than 0 and\
                            less or equal to the number of parameters '$n'"
                }
            } else {
                set orthantwiseend $n
            }
            if {$orthantwiseend<=$orthantwisestart} {
                return -code error "Invalid orthantwiseend value '$orthantwiseend' provided, must be more than\
                    orthantwisestart value '$orthantwisestart'"
            }
            if {$linesearch ne {backtracking}} {
                return -code error "Only the backtracking method is available for Orthant-Wise Limited-memory\
                        Quasi-Newton (OWL-QN) method"
            }
        }
        foreach par $pars {
            lappend x [$par configure -initval]
        }
        # Allocate working space
        for {set i 0} {$i<$n} {incr i} {
            lappend w 0.0
        }
        # Initialize the limited memory
        for {set i 0} {$i<$m} {incr i} {
            for {set j 0} {$j<$n} {incr j} {
                lappend s 0.0
                lappend y 0.0
            }
            set it [dcreate alpha 0.0 ys 0.0 s $s y $y]
            lappend lm $it
            unset s y
        }
        # Allocate an array for storing previous values of the objective function
        if {$past>0} {
            for {set i 0} {$i<$past} {incr i} {
                lappend pf 0.0
            }
        }
        # Evaluate the function value and its gradient
        set funcData [$funct $x $pdata]
        set fx [dget $funcData f]
        set g [dget $funcData dvec]
        if {[llength $g]!=$n} {
            return -code error "Length of returned lists of gradient values is not equal to number of parameters '$n'"
        }
        if {[info exists orthantwisec]} {
            # Compute the L1 norm of the variable and add it to the object value
            set xnorm [my OwlqnX1norm $x $orthantwisestart $orthantwiseend]
            set fx [= {$fx+$xnorm*$orthantwisec}]
            set pg [my OwlqnPseudoGradient $x $g $n $orthantwisec $orthantwisestart $orthantwiseend]
            #puts $pg
        }
        # Store the initial value of the objective function
        if {[info exists pf]} {
            lset pf 0 $fx
        } else {
            set pf $fx
        }
        
        # Compute the direction, we assume the initial hessian matrix H_0 as the identity matrix.
        if {![info exists orthantwisec]} {
            set d [= {mul(-1.0,$g)}]
        } else {
            set d [= {mul(-1.0,$pg)}]
        }
        # Make sure that the initial variables are not a minimizer
        set xnorm [= {sqrt(dot($x,$x))}]
        if {![info exists orthantwisec]} {
            set gnorm [= {sqrt(dot($g,$g))}]
        } else {
            set gnorm [= {sqrt(dot($pg,$pg))}]
        }
        if {$xnorm<1.0} {
            set xnorm 1.0
        }
        if {($gnorm/$xnorm)<=$epsilon} {
            return -code error "The initial variables already minimize the objective function"
        }
        # Compute the initial step: step = 1.0 / sqrt(vecdot(d, d, n))
        set step [= {1.0/sqrt(dot($g,$g))}]
        set k 1
        set end 0
        # Main loop
        # puts "Iteration $k:"
        # puts "    fx = $fx, x\[0\] = [@ $x 0], x\[1\] = [@ $x 1]"
        # puts "    xnorm = $xnorm, gnorm = $gnorm, step = $step\n"
        while true {
            # Store the current position and gradient vectors
            set xp $x
            set gp $g
            # Search for an optimal step
            if {![info exists orthantwisec]} {
                set lineSearchData [my Linesearch $x $fx $g $d $step $xp $gp $w]
                set x [dget $lineSearchData x]
                set fx [dget $lineSearchData fx]
                set g [dget $lineSearchData g]
                set step [dget $lineSearchData step]
                #set w [dget $lineSearchData w]
                if {[dexist $lineSearchData info]} {
                    # Revert to the previous point
                    set x $xp
                    set g $gp
                    set info [dget $lineSearchData info]
                    break
                }
            } else {
                set lineSearchData [my Linesearch $x $fx $g $d $step $xp $pg $w]
                set x [dget $lineSearchData x]
                set fx [dget $lineSearchData fx]
                set g [dget $lineSearchData g]
                set step [dget $lineSearchData step]
                if {[dexist $lineSearchData info]} {
                    # Revert to the previous point
                    set x $xp
                    set g $gp
                    set info [dget $lineSearchData info]
                    break
                }
                set pg [my OwlqnPseudoGradient $x $g $n $orthantwisec $orthantwisestart $orthantwiseend]
            }
            # Compute x and g norms
            set xnorm [= {sqrt(dot($x,$x))}]
            if {![info exists orthantwisec]} {
                set gnorm [= {sqrt(dot($g,$g))}]
            } else {
                set gnorm [= {sqrt(dot($pg,$pg))}]
            }
            # Report the progress
            # puts "Iteration $k:"
            # puts "    fx = $fx, x\[0\] = [@ $x 0], x\[1\] = [@ $x 1]"
            # puts "    xnorm = $xnorm, gnorm = $gnorm, step = $step\n"
            # Convergence test. The criterion is given by the following formula: |g(x)| / \max(1, |x|) < \epsilon
            if {$xnorm<1.0} {
                set xnorm 1.0
            }
            if {($gnorm/$xnorm)<=$epsilon} {
                # Convergence
                set info {Success: reached convergence (gtol)}
                break
            }
            # Test for stopping criterion. The criterion is given by the following formula:
            # |(f(past_x) - f(x))| / f(x) < \delta
            if {($past<=$k) && ($past!=0)} {
                set rate [= {(li($pf,$k%$past)-$fx)/$fx}]
                # The stopping criterion
                if {abs($rate)<$delta} {
                    set info {Success: met stopping criteria (ftol)}
                    break
                }
            }
            # Store the current value of the objective function
            if {$past!=0} {
                lset pf [= {$k%$past}] $fx
            }
            if {[info exists maxiter] && ($maxiter<($k+1))} {
                # Maximum number of iterations
                set info {Maximum iterations reached}
                break
            }
            # Update vectors s and y:
            #     s_{k+1} = x_{k+1} - x_{k} = \step * d_{k}.
            #     y_{k+1} = g_{k+1} - g_{k}.
            set it [@ $lm $end]
            dict set it s [= {sub($x,$xp)}]
            dict set it y [= {sub($g,$gp)}]
            # Compute scalars ys and yy:
            #     ys = y^t \cdot s = 1 / \rho.
            #     yy = y^t \cdot y.
            # Notice that yy is used for scaling the hessian matrix H_0 (Cholesky factor)
            set ys [= {dot([dget $it y],[dget $it s])}]
            set yy [= {dot([dget $it y],[dget $it y])}]
            dict set it ys $ys
            lset lm $end $it
            # Recursive formula to compute dir = -(H \cdot g).
            #     This is described in page 779 of:
            #     Jorge Nocedal.
            #     Updating Quasi-Newton Matrices with Limited Storage.
            #     Mathematics of Computation, Vol. 35, No. 151,
            #     pp. 773--782, 1980.
            set bound [= {($m<=$k ? $m : $k)}]
            incr k
            set end [= {($end+1)%$m}]
            # Compute the steepest direction
            if {![info exists orthantwisec]} {
                # Compute the negative of gradients
                set d [= {mul($g, -1)}]
            } else {
                set d [= {mul($pg, -1)}]
            }
            set j $end
            for {set i 0} {$i<$bound} {incr i} {
                set j [= {($j+$m-1)%$m}]
                set it [lindex $lm $j]
                set alpha [= {dot([dget $it s],$d)}]
                set alpha [= {$alpha/[dget $it ys]}]
                set d [= {sum($d, mul([dget $it y], -$alpha))}]
                dset it alpha $alpha
                lset lm $j $it
            }
            set d [= {mul($d,$ys/$yy)}]
            for {set i 0} {$i<$bound} {incr i} {
                set it [lindex $lm $j]
                set beta [= {dot([dget $it y],$d)}]
                set beta [= {$beta/[dget $it ys]}]
                set d [= {sum($d, mul([dget $it s], [dget $it alpha]-$beta))}]
                set j [= {($j+1)%$m}]
            }
            # Constrain the search direction for orthant-wise updates
            if {[info exists orthantwisec]} {
                for {set i $orthantwisestart} {$i<$orthantwiseend} {incr i} {
                    if {li($d, $i)*li($pg, $i)>=0} {
                        lset d $i 0
                    }
                }
            }
            # Now the search direction d is ready. We try step = 1 first
            set step 1.0
        }
        # return result
        set result [dcreate objfunc $fx x $x info $info niter $k xnorm $xnorm gnorm $gnorm]
    }
    method OwlqnX1norm {x start end} {
        set norm 0.001
        for {set i $start} {$i<$end} {incr i} {
            set norm [= {$norm+abs(li($x,$i))}]
        }
        return $norm
    }
    method Linesearch {x f g s stp xp gp wp} {
        # Does linear search
        #  x - parameters vector
        #  f - objective function value
        #  g - gradient vector of the objective function
        #  d - direction vector
        #  stp - step
        #  xp - previous parameter vector
        #  gp - previous gradient vector
        #  wp - ??
        # Returns: dictionary count of steps in case of sucess, or error in other case
        set count 0
        set uinfo 0
        if {$stp<=0} {
            return -code error "Invalid step value '$stp'"
        }
        if {$linesearch eq {morethuente}} {
            # Compute the initial gradient in the search direction
            #puts $g
            #puts $s
            set dginit [= {dot($g,$s)}]
            #puts $dginit
            # Make sure that s points to a descent direction
            if {$dginit>0} {
                return -code error "Increase gradient"
            }
            # Initialize local variables
            set brackt 0
            set stage1 0
            set finit $f
            set dgtest [= {$ftol*$dginit}]
            set width [= {$maxstep-$minstep}]
            set prevWidth [= {2.0*$width}]
            # The variables stx, fx, dgx contain the values of the step,
            # function, and directional derivative at the best step.
            # The variables sty, fy, dgy contain the value of the step,
            # function, and derivative at the other endpoint of
            # the interval of uncertainty.
            # The variables stp, f, dg contain the values of the step,
            # function, and derivative at the current step.
            set stx 0.0
            set sty 0.0
            set fx $finit
            set fy $finit
            set dgx $dginit
            set dgy $dginit
            # main loop
            while true {
                # Set the minimum and maximum steps to correspond to the present interval of uncertainty.
                if {$brackt} {
                    set stmin [= {min($stx,$sty)}]
                    set stmax [= {max($stx,$sty)}]
                } else {
                    set stmin $stx
                    set stmax [= {$stp+4.0*($stp-$stx)}]
                }
                # Clip the step in the range of [stpmin, stpmax]
                if {$stp<$minstep} {
                    set stp $minstep
                }
                if {$maxstep<$stp} {
                    set stp $maxstep
                }
                # If an unusual termination is to occur then let stp be the lowest point obtained so far
                if {($brackt && ((($stp<=$stmin) || ($stmax<=$stp)) || ($maxlinesearch<=($count+1)) || ($uinfo!=0))) ||\
                            ($brackt && (($stmax-$stmin)<=($xtol*$stmax)))} {
                    set stp $stx
                }
                # Compute the current value of x: x <- x + (*stp) * s.
                set x $xp
                set x [= {sum($x, mul($s, $stp))}]
                # Evaluate the function and gradient values
                set funcData [$funct $x $pdata]
                set f [dget $funcData f]
                set g [dget $funcData dvec]
                set dg [= {dot($g,$s)}]
                set ftest1 [= {$finit+$stp*$dgtest}]
                incr count
                # Test for errors and convergence
                if {$brackt && (($stp<=$stmin || $stmax<=$stp) || ($uinfo!=0))} {
                    return [dcreate info {Rounding errors prevent further progress} x $x fx $f g $g step $stp]
                }
                if {($stp==$maxstep) && ($f<=$ftest1) && ($dg<=$dgtest)} {
                    return [dcreate info {The step is the maximum value} x $x fx $f g $g step $stp]
                }
                if {($stp==$minstep) && (($ftest1<$f) || ($dgtest<=$dg))} {
                    return [dcreate info {The step is the minimum value} x $x fx $f g $g step $stp]
                }
                if {$brackt && (($stmax-$stmin)<=($xtol*$stmax))} {
                    return [dcreate info {Relative width of the interval of uncertainty is at most xtol} x $x fx $f g $g\
                                    step $stp]
                }
                if {$maxlinesearch<=$count} {
                    return [dcreate info {Maximum number of iteration} x $x fx $f g $g step $stp]
                }
                if {($f<=$ftest1) && (abs($dg)<=($gtol*(-$dginit)))} {
                    # The sufficient decrease condition and the directional derivative condition hold
                    return [dcreate x $x fx $f g $g step $stp count $count]
                }
                # In the first stage we seek a step for which the modified function has a nonpositive value and
                # nonnegative derivative.
                if {$stage1 && ($f<=$ftest1) && ((min($ftol, $gtol)*$dginit)<=$dg)} {
                    # The sufficient decrease condition and the directional derivative condition hold
                    set stage1 0
                }
                # A modified function is used to predict the step only if we have not obtained a step for which the
                # modified function has a nonpositive function value and nonnegative derivative, and if a lower function
                # value has been obtained but the decrease is not sufficient.
                if {$stage1 && ($ftest1<$f) && ($f<=$fx)} {
                    # Define the modified function and derivative values
                    set fm [= {$f-$stp*$dgtest}]
                    set fxm [= {$fx-$stx*$dgtest}]
                    set fym [= {$fy-$sty*$dgtest}]
                    set dgm [= {$dg-$dgtest}]
                    set dgxm [= {$dgx-$dgtest}]
                    set dgym [= {$dgy-$dgtest}]
                    # Call update_trial_interval() to update the interval of uncertainty and to compute the new step
                    set trialIntervalData [my UpdateTrialInterval $stx $fxm $dgxm $sty $fym $dgym $stp $fm $dgm $stmin\
                                                   $stmax $brackt]
                    set uinfo [dget $trialIntervalData uinfo]
                    set info [dget $trialIntervalData info]
                    set stx [dget $trialIntervalData x]
                    set fxm [dget $trialIntervalData fx]
                    set dgxm [dget $trialIntervalData dx]
                    set sty [dget $trialIntervalData y]
                    set fym [dget $trialIntervalData fy]
                    set dgym [dget $trialIntervalData dy]
                    set stp [dget $trialIntervalData t]
                    set fm [dget $trialIntervalData ft]
                    set dgm [dget $trialIntervalData dt]
                    set brackt [dget $trialIntervalData brackt]
                    # Reset the function and gradient values for f
                    set fx [= {$fxm+$stx*$dgtest}]
                    set fy [= {$fym+$sty*$dgtest}]
                    set dgx [= {$dgxm+$dgtest}]
                    set dgy [= {$dgym+$dgtest}]
                } else {
                    # Call update_trial_interval() to update the interval of uncertainty and to compute the new step
                    set trialIntervalData [my UpdateTrialInterval $stx $fx $dgx $sty $fy $dgy $stp $f $dg $stmin\
                                                   $stmax $brackt]
                    set uinfo [dget $trialIntervalData uinfo]
                    set info [dget $trialIntervalData info]
                    set stx [dget $trialIntervalData x]
                    set fx [dget $trialIntervalData fx]
                    set dgx [dget $trialIntervalData dx]
                    set sty [dget $trialIntervalData y]
                    set fy [dget $trialIntervalData fy]
                    set dgy [dget $trialIntervalData dy]
                    set stp [dget $trialIntervalData t]
                    set f [dget $trialIntervalData ft]
                    set dg [dget $trialIntervalData dt]
                    set brackt [dget $trialIntervalData brackt]
                }
                # Force a sufficient decrease in the interval of uncertainty
                if {$brackt} {
                    if {(0.66*$prevWidth)<=abs($sty-$stx)} {
                        set stp [= {$stx+0.5*($sty-$stx)}]
                    }
                    set prevWidth $width
                    set width [= {abs($sty-$stx)}]
                }
            }
        } elseif {($linesearch eq {backtracking}) && [info exists orthantwisec]} {
            #puts here
            const width 0.5
            set norm 0.0
            set finit $f
            # Choose the orthant for the new point.
            for {set i 0} {$i<[llength $x]} {incr i} {
                lset wp $i [= {li($xp,$i)==0.0 ? -li($gp,$i) : li($xp,$i)}]
            }
            # main loop
            while true {
                # Update the current point.
                set x $xp
                set x [= {sum($x, mul($s, $stp))}]
                # The current point is projected onto the orthant.
                set x [my OwlqnProject $x $wp $orthantwisestart $orthantwiseend]
                # Evaluate the function and gradient values
                set funcData [$funct $x $pdata]
                set f [dget $funcData f]
                set g [dget $funcData dvec]
                 # Compute the L1 norm of the variables and add it to the object value.
                set norm [my OwlqnX1norm $x $orthantwisestart $orthantwiseend]
                set f [= {$f+$norm*$orthantwisec}]
                incr count
                set dgtest 0.0
                for {set i 0} {$i<[llength $x]} {incr i} {
                    set dgtest [= {$dgtest+(li($x,$i)-li($xp,$i))*li($gp,$i)}]
                }
                if {$f<=$finit+$ftol*$dgtest} {
                    # The sufficient decrease condition.
                    return [dcreate x $x fx $f g $g step $stp count $count]
                }
                # Test for errors 
                if {$stp<$minstep} {
                    return [dcreate info {The step is the minimum value} x $x fx $f g $g step $stp]
                }
                if {$maxstep<$stp} {
                    return [dcreate info {The step is the maximum value} x $x fx $f g $g step $stp]
                }
                if {$maxlinesearch<=$count} {
                    return [dcreate info {Maximum number of iteration} x $x fx $f g $g step $stp]
                }
                set stp [= {$stp*$width}]
            }
        } elseif {$linesearch eq {backtracking}} {
            const dec 0.5
            const inc 2.1
            set dginit [= {dot($g,$s)}]
            set finit $f
            # Make sure that s points to a descent direction
            if {$dginit>0} {
                return -code error "Increase gradient"
            }
            set dgtest [= {$ftol*$dginit}]
            # main loop
            while true {
                # Compute the current value of x: x <- x + (*stp) * s.
                set x $xp
                set x [= {sum($x, mul($s, $stp))}]
                # Evaluate the function and gradient values
                set funcData [$funct $x $pdata]
                set f [dget $funcData f]
                set g [dget $funcData dvec]
                incr count
                if {$f>$finit+$stp*$dgtest} {
                    set width $dec
                } else {
                    # The sufficient decrease condition (Armijo condition).
                    if {$condition eq {armijo}} {
                        # Exit with the Armijo condition.
                        return [dcreate x $x fx $f g $g step $stp count $count]
                    }
                    # Check the Wolfe condition.
                    set dg [= {dot($g,$s)}]
                    if {$dg<$wolfe*$dginit} {
                        set width $inc
                    } else {
                        if {$condition ne {strongwolfe}} {
                            # Exit with the regular Wolfe condition.
                            return [dcreate x $x fx $f g $g step $stp count $count]
                        } else {
                            if {$dg>-$wolfe*$dginit} {
                                set width $dec
                            } else {
                                # Exit with the strong Wolfe condition.
                                return [dcreate x $x fx $f g $g step $stp count $count]
                            }
                        }
                    }
                }
                # Test for errors 
                if {$stp<$minstep} {
                    return [dcreate info {The step is the minimum value} x $x fx $f g $g step $stp]
                }
                if {$maxstep<$stp} {
                    return [dcreate info {The step is the maximum value} x $x fx $f g $g step $stp]
                }
                if {$maxlinesearch<=$count} {
                    return [dcreate info {Maximum number of iteration} x $x fx $f g $g step $stp]
                }
                set stp [= {$stp*$width}]
            }
        } 
    }
    method UpdateTrialInterval {x fx dx y fy dy t ft dt tmin tmax brackt} {
        # Update a safeguarded trial value and interval for line search
        #  x - the value of one endpoint
        #  fx - the value of f(x)
        #  dx - the value of f'(x)
        #  y - the value of another endpoint
        #  fy - the value of f(y)
        #  dy - the value of f'(y)
        #  t - the value of the trial value, t
        #  ft - the value of f(t)
        #  dt - the value of f'(t)
        #  tmin - The minimum value for the trial value, t
        #  tmax - The maximum value for the trial value, t
        #  brackt - the predicate if the trial value is bracketed
        # Returns:  
        # Synopsis:
        ::tclopt::NewPointers {xPnt fxPnt dxPnt yPnt fyPnt dyPnt tPnt ftPnt dtPnt} double
        ::tclopt::NewPointers bracktPnt int
        ::tclopt::AssignPointersValues [list $xPnt $fxPnt $dxPnt $yPnt $fyPnt $dyPnt $tPnt $ftPnt $dtPnt]\
                [list $x $fx $dx $y $fy $dy $t $ft $dt] double
        ::tclopt::AssignPointersValues $bracktPnt $brackt int
        set status [::tclopt::update_trial_interval $xPnt $fxPnt $dxPnt $yPnt $fyPnt $dyPnt $tPnt $ftPnt $dtPnt\
                            $tmin $tmax $bracktPnt]
        if {$status==-1003} {
            set info {The line-search step went out of the interval of uncertainty} 
        } elseif {$status==-994} {
            set info {The current search direction increases the objective function value}
        } elseif {$status==-1001} {
            set info {A logic error occurred; alternatively, the interval of uncertainty became too small}
        } elseif {$status==0} {
            set info {Success}
        }
        ::tclopt::GetPointersValues [list $xPnt $fxPnt $dxPnt $yPnt $fyPnt $dyPnt $tPnt $ftPnt $dtPnt]\
                {xVal fxVal dxVal yVal fyVal dyVal tVal ftVal dtVal} double
        ::tclopt::GetPointersValues [list $bracktPnt] [list bracktVal] int
        ::tclopt::DeletePointers [list $xPnt $fxPnt $dxPnt $yPnt $fyPnt $dyPnt $tPnt $ftPnt $dtPnt] double
        ::tclopt::DeletePointers $bracktPnt int
        return [dcreate uinfo $status info $info x $xVal fx $fxVal dx $dxVal y $yVal fy $fyVal dy $dyVal t $tVal ft $ftVal\
                        dt $dtVal brackt $bracktVal]
    }
    method OwlqnPseudoGradient {x g n c start end} {
        set pg [lrepeat $n 0.0]
        ::tclopt::Lists2arrays {pgArray xArray gArray} [list $pg $x $g]
        ::tclopt::owlqn_pseudo_gradient $pgArray $xArray $gArray $n $c $start $end
        ::tclopt::Arrays2lists pgList $pgArray [llength $pg]
        ::tclopt::DeleteArrays [list $pgArray $xArray $gArray]
        return $pgList
    }
    method OwlqnProject {d sign start end} {
        for {set i $start} {$i<$end} {incr i} {
            if {li($d,$i)*li($sign,$i)<=0.0} {
                lset d $i 0.0
            }
        }
        return $d
    }
    method OwlqnX1norm {x start n} {
        set norm 0.0
        for {set i $start} {$i<$n} {incr i} {
            set norm [= {$norm+abs(li($x,$i))}]
        }
        return $norm
    }
}
