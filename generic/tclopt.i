%module tclopt
%{
    #include "mpfit.h"
%}
%typemap(in,numinputs=0) Tcl_Interp *interp {  
    $1 = interp;
}
%include carrays.i
%include cpointer.i
%array_functions(double, doubleArray);
%pointer_functions(double, doublep);
%array_functions(int, intArray);
%pointer_functions(int, intp);
extern void mp_qrfac(int m, int n, double *a, int lda, int pivot, int *ipvt, int lipvt, double *rdiag,
                            double *acnorm, double *wa);
extern double mp_enorm(int n, double *x);
extern void mp_lmpar(int n, double *r, int ldr, int *ipvt, int *ifree, double *diag, double *qtb, double delta,
                     double *par, double *x, double *sdiag, double *wa1, double *wa2);
extern int mp_covar(int n, double *r, int ldr, int *ipvt, double tol, double *wa);
