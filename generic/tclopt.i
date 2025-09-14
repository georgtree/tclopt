%module tclopt
%{
    #include "mpfit.h"
    #include "random_gen.h"
    #include "lbfgs.h"
    #include "lbfgs_api.h"
    #include "arithmetic_ansi.h"
%}

%include carrays.i
%include cpointer.i

%typemap(in,numinputs=0) Tcl_Interp *interp {  
    $1 = interp;
}

%array_functions(double, doubleArray);
%pointer_functions(double, doublep);
%array_functions(int, intArray);
%pointer_functions(int, intp);
%pointer_functions(long, longp);

/* Reuse double typemaps for lbfgsfloatval_t */
%apply double * { lbfgsfloatval_t * };
%apply double * { const lbfgsfloatval_t * };

// mpfit declarations
extern void mp_qrfac(int m, int n, double *a, int lda, int pivot, int *ipvt, int lipvt, double *rdiag,
                            double *acnorm, double *wa);
extern double mp_enorm(int n, double *x);
extern void mp_lmpar(int n, double *r, int ldr, int *ipvt, int *ifree, double *diag, double *qtb, double delta,
                     double *par, double *x, double *sdiag, double *wa1, double *wa2);
extern int mp_covar(int n, double *r, int ldr, int *ipvt, double tol, double *wa);

//random number generator
extern float rnd_uni(long *idum);

//lbfgs declarations
extern int update_trial_interval(lbfgsfloatval_t *x, lbfgsfloatval_t *fx, lbfgsfloatval_t *dx,
                                        lbfgsfloatval_t *y, lbfgsfloatval_t *fy, lbfgsfloatval_t *dy,
                                        lbfgsfloatval_t *t, lbfgsfloatval_t *ft, lbfgsfloatval_t *dt,
                                        const lbfgsfloatval_t tmin, const lbfgsfloatval_t tmax, int *brackt);
extern lbfgsfloatval_t owlqn_x1norm(const lbfgsfloatval_t *x, const int start, const int n);
extern void owlqn_pseudo_gradient(lbfgsfloatval_t *pg, const lbfgsfloatval_t *x, const lbfgsfloatval_t *g,
                                         const int n, const lbfgsfloatval_t c, const int start, const int end);
extern void owlqn_project(lbfgsfloatval_t *d, const lbfgsfloatval_t *sign, const int start, const int end);
typedef double lbfgsfloatval_t;
