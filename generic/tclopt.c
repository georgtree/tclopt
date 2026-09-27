/* List/scalar adapters for the original numerical kernels. Every C array is
 * private to one command invocation and freed on success or conversion error. */
#include <tcl.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include "mpfit.h"
#include "random_gen.h"
#include "lbfgs.h"
#include "lbfgs_api.h"

#if TCL_MAJOR_VERSION < 9
typedef int OptSize;
#define OptCreateCommand Tcl_CreateObjCommand
#else
typedef Tcl_Size OptSize;
#define OptCreateCommand Tcl_CreateObjCommand2
#endif

typedef struct {
    void *memory[12];
    int used;
} Call;

typedef struct {
    int length;
    double *values;
} Doubles;

typedef struct {
    int length;
    int *values;
} Ints;

typedef enum { QRFAC, ENORM, LMPAR, COVAR, RANDOM, INTERVAL, NORM1, GRADIENT, PROJECT } Operation;

typedef struct {
    const char *name, *usage;
    int argc;
    Operation operation;
} Command;

static int Error(Tcl_Interp *ip, const char *message) {
    Tcl_SetObjResult(ip, Tcl_NewStringObj(message, -1));
    Tcl_SetErrorCode(ip, "TCLOPT", "VALUE", NULL);
    return TCL_ERROR;
}

static int GetInt(Tcl_Interp *ip, Tcl_Obj *obj, int *value) {
    Tcl_WideInt wide;
    if (Tcl_GetWideIntFromObj(ip, obj, &wide) != TCL_OK) {
        return TCL_ERROR;
    }
    if (wide < INT_MIN || wide > INT_MAX) {
        return Error(ip, "integer is outside the C int range");
    }
    *value = (int)wide;
    return TCL_OK;
}

static void *Allocate(Call *call, Tcl_Interp *ip, int length, size_t width) {
    void *p;
    if (length < 0 || (size_t)length > SIZE_MAX / width) {
        Error(ip, "invalid or overflowing array length");
        return NULL;
    }
    p = calloc(length ? (size_t)length : 1, width);
    if (!p) {
        Error(ip, "unable to allocate numerical workspace");
        return NULL;
    }
    call->memory[call->used++] = p;
    return p;
}

static int ReadDoubles(Call *call, Tcl_Interp *ip, Tcl_Obj *obj, Doubles *out) {
    OptSize length;
    Tcl_Obj **items;
    if (Tcl_ListObjGetElements(ip, obj, &length, &items) != TCL_OK) {
        return TCL_ERROR;
    }        
    if (length > INT_MAX) {
        return Error(ip, "list is too long for the numerical kernel");
    }        
    out->length = (int)length;
    out->values = Allocate(call, ip, out->length, sizeof(double));
    if (!out->values) {
        return TCL_ERROR;
    }
    for (int i = 0; i < out->length; i++) {
        if (Tcl_GetDoubleFromObj(ip, items[i], &out->values[i]) != TCL_OK) {
            return TCL_ERROR;
        }
    }
    return TCL_OK;
}

static int ReadInts(Call *call, Tcl_Interp *ip, Tcl_Obj *obj, Ints *out) {
    OptSize length;
    Tcl_Obj **items;
    if (Tcl_ListObjGetElements(ip, obj, &length, &items) != TCL_OK) {
        return TCL_ERROR;
    }
    if (length > INT_MAX) {
        return Error(ip, "list is too long for the numerical kernel");
    }
    out->length = (int)length;
    out->values = Allocate(call, ip, out->length, sizeof(int));
    if (!out->values) {
        return TCL_ERROR;
    }
    for (int i = 0; i < out->length; i++) {
        if (GetInt(ip, items[i], &out->values[i]) != TCL_OK) {
            return TCL_ERROR;
        }
    }
    return TCL_OK;
}

static Tcl_Obj *DoubleList(const double *values, int length) {
    Tcl_Obj *list = Tcl_NewListObj(0, NULL);
    for (int i = 0; i < length; i++) {
        Tcl_ListObjAppendElement(NULL, list, Tcl_NewDoubleObj(values[i]));
    }        
    return list;
}

static Tcl_Obj *IntList(const int *values, int length) {
    Tcl_Obj *list = Tcl_NewListObj(0, NULL);
    for (int i = 0; i < length; i++) {
        Tcl_ListObjAppendElement(NULL, list, Tcl_NewIntObj(values[i]));
    }        
    return list;
}

static void Put(Tcl_Obj *dict, const char *key, Tcl_Obj *value) {
    Tcl_DictObjPut(NULL, dict, Tcl_NewStringObj(key, -1), value);
}

static int MatrixSize(Tcl_Interp *ip, int rows, int columns, int *size) {
    if (rows < 0 || columns < 0 || (columns && rows > INT_MAX / columns)) {
        return Error(ip, "invalid or overflowing matrix dimensions");
    }
    *size = rows * columns;
    return TCL_OK;
}

#define CHECK(EXPR)                                                                                                    \
    do {                                                                                                               \
        if ((EXPR) != TCL_OK) {                                                                                        \
            goto done;                                                                                                 \
        }                                                                                                              \
    } while (0)
#define REQUIRE(EXPR, MSG)                                                                                             \
    do {                                                                                                               \
        if (!(EXPR)) {                                                                                                 \
            Error(ip, MSG);                                                                                            \
            goto done;                                                                                                 \
        }                                                                                                              \
    } while (0)
#define INTEGER(VAR, ARG)                                                                                              \
    int VAR;                                                                                                           \
    CHECK(GetInt(ip, objv[ARG], &VAR))
#define REAL(VAR, ARG)                                                                                                 \
    double VAR;                                                                                                        \
    CHECK(Tcl_GetDoubleFromObj(ip, objv[ARG], &VAR))
#define DOUBLES(VAR, ARG)                                                                                              \
    Doubles VAR;                                                                                                       \
    CHECK(ReadDoubles(&call, ip, objv[ARG], &VAR))
#define INTS(VAR, ARG)                                                                                                 \
    Ints VAR;                                                                                                          \
    CHECK(ReadInts(&call, ip, objv[ARG], &VAR))
#define WORK(VAR, N, TYPE)                                                                                             \
    TYPE *VAR = Allocate(&call, ip, N, sizeof(TYPE));                                                                  \
    if (!VAR) {                                                                                                        \
        goto done;                                                                                                     \
    }
#define LENGTH(VAR, N) REQUIRE((N) >= 0 && VAR.length >= (N), "list is too short for the requested operation")
#define LIST(KEY, PTR, N) Put(result, KEY, DoubleList(PTR, N))

static int Dispatch(ClientData cd, Tcl_Interp *ip, OptSize objc, Tcl_Obj *const objv[]) {
    const Command *cmd = cd;
    Call call = {{0}, 0};
    int code = TCL_ERROR;
    Tcl_Obj *result = NULL;
    if (objc != cmd->argc + 1) {
        Tcl_WrongNumArgs(ip, 1, objv, cmd->usage);
        return TCL_ERROR;
    }
    switch (cmd->operation) {
    case ENORM: {
        DOUBLES(x, 1);
        result = Tcl_NewDoubleObj(mp_enorm(x.length, x.values));
        break;
    }
    case QRFAC: {
        INTEGER(m, 1);
        INTEGER(n, 2);
        DOUBLES(a, 3);
        INTEGER(lda, 4);
        INTEGER(pivot, 5);
        INTEGER(lipvt, 6);
        int size = 0;
        CHECK(MatrixSize(ip, m, n, &size));
        REQUIRE(lda >= m && lipvt >= 0 && (!pivot || lipvt >= n), "invalid QR dimensions");
        LENGTH(a, size);
        WORK(ipvt, lipvt, int);
        WORK(rdiag, n, double);
        WORK(acnorm, n, double);
        WORK(wa, n, double);
        mp_qrfac(m, n, a.values, lda, pivot, ipvt, lipvt, rdiag, acnorm, wa);
        result = Tcl_NewDictObj();
        LIST("a", a.values, a.length);
        LIST("rdiag", rdiag, n);
        LIST("acnorm", acnorm, n);
        LIST("wa", wa, n);
        Put(result, "ipvt", IntList(ipvt, lipvt));
        break;
    }
    case LMPAR: {
        INTEGER(n, 1);
        DOUBLES(r, 2);
        INTEGER(ldr, 3);
        INTS(ipvt, 4);
        INTS(ifree, 5);
        DOUBLES(diag, 6);
        DOUBLES(qtb, 7);
        REAL(delta, 8);
        REAL(par, 9);
        int size = 0;
        REQUIRE(n > 0 && ldr >= n, "invalid LM matrix dimensions");
        CHECK(MatrixSize(ip, ldr, n, &size));
        LENGTH(r, size);
        LENGTH(ipvt, n);
        LENGTH(ifree, n);
        LENGTH(qtb, n);
        for (int i = 0; i < n; i++) {
            REQUIRE(ipvt.values[i] >= 0 && ipvt.values[i] < n && ifree.values[i] >= 0 && ifree.values[i] < diag.length,
                    "LM permutation or free-parameter index out of range");
        }
        WORK(x, n, double);
        WORK(sdiag, n, double);
        WORK(wa1, n, double);
        WORK(wa2, n, double);
        mp_lmpar(n, r.values, ldr, ipvt.values, ifree.values, diag.values, qtb.values, delta, &par, x, sdiag, wa1, wa2);
        result = Tcl_NewDictObj();
        Put(result, "par", Tcl_NewDoubleObj(par));
        LIST("r", r.values, r.length);
        LIST("x", x, n);
        LIST("sdiag", sdiag, n);
        LIST("wa1", wa1, n);
        LIST("wa2", wa2, n);
        break;
    }
    case COVAR: {
        INTEGER(n, 1);
        DOUBLES(r, 2);
        INTEGER(ldr, 3);
        INTS(ipvt, 4);
        REAL(tol, 5);
        int size = 0;
        REQUIRE(n > 0 && ldr >= n, "invalid covariance matrix dimensions");
        CHECK(MatrixSize(ip, ldr, n, &size));
        LENGTH(r, size);
        LENGTH(ipvt, n);
        for (int i = 0; i < n; i++) {
            REQUIRE(ipvt.values[i] >= 0 && ipvt.values[i] < n, "covariance permutation index out of range");
        }            
        WORK(wa, n, double);
        mp_covar(n, r.values, ldr, ipvt.values, tol, wa);
        result = Tcl_NewDictObj();
        LIST("r", r.values, r.length);
        LIST("wa", wa, n);
        break;
    }
    case RANDOM: {
        /* Tcl variable, not a pointer handle. Preserve the original generator
         * and its sequence; only the seed representation changes. */
        Tcl_Obj *value = Tcl_ObjGetVar2(ip, objv[1], NULL, TCL_LEAVE_ERR_MSG);
        long seed;
        double number;
        if (!value) {
            goto done;
        }
        CHECK(Tcl_GetLongFromObj(ip, value, &seed));
        REQUIRE(seed != LONG_MIN, "random seed cannot be LONG_MIN");
        number = rnd_uni(&seed);
        Tcl_Obj *nextSeed = Tcl_NewLongObj(seed);
        Tcl_IncrRefCount(nextSeed);
        Tcl_Obj *assigned = Tcl_ObjSetVar2(ip, objv[1], NULL, nextSeed, TCL_LEAVE_ERR_MSG);
        Tcl_DecrRefCount(nextSeed);
        if (!assigned) {
            goto done;
        }
        result = Tcl_NewDoubleObj(number);
        break;
    }
    case INTERVAL: {
        REAL(x, 1);
        REAL(fx, 2);
        REAL(dx, 3);
        REAL(y, 4);
        REAL(fy, 5);
        REAL(dy, 6);
        REAL(t, 7);
        REAL(ft, 8);
        REAL(dt, 9);
        REAL(tmin, 10);
        REAL(tmax, 11);
        INTEGER(brackt, 12);
        int status = update_trial_interval(&x, &fx, &dx, &y, &fy, &dy, &t, &ft, &dt, tmin, tmax, &brackt);
        const char *info = "Success";
        if (status == -1003) {
            info = "The line-search step went out of the interval of uncertainty";
        } else if (status == -994) {
            info = "The current search direction increases the objective function value";
        } else if (status == -1001) {
            info = "A logic error occurred; alternatively, the interval of uncertainty became too small";
        }
        result = Tcl_NewDictObj();
        Put(result, "uinfo", Tcl_NewIntObj(status));
        Put(result, "info", Tcl_NewStringObj(info, -1));
        Put(result, "x", Tcl_NewDoubleObj(x));
        Put(result, "fx", Tcl_NewDoubleObj(fx));
        Put(result, "dx", Tcl_NewDoubleObj(dx));
        Put(result, "y", Tcl_NewDoubleObj(y));
        Put(result, "fy", Tcl_NewDoubleObj(fy));
        Put(result, "dy", Tcl_NewDoubleObj(dy));
        Put(result, "t", Tcl_NewDoubleObj(t));
        Put(result, "ft", Tcl_NewDoubleObj(ft));
        Put(result, "dt", Tcl_NewDoubleObj(dt));
        Put(result, "brackt", Tcl_NewIntObj(brackt));
        break;
    }
    case NORM1: {
        DOUBLES(x, 1);
        INTEGER(start, 2);
        INTEGER(end, 3);
        REQUIRE(start >= 0 && end >= start, "invalid norm interval");
        LENGTH(x, end);
        result = Tcl_NewDoubleObj(owlqn_x1norm(x.values, start, end));
        break;
    }
    case GRADIENT: {
        DOUBLES(x, 1);
        DOUBLES(g, 2);
        INTEGER(n, 3);
        REAL(c, 4);
        INTEGER(start, 5);
        INTEGER(end, 6);
        REQUIRE(n >= 0 && start >= 0 && end >= start && end <= n, "invalid gradient interval");
        LENGTH(x, n);
        LENGTH(g, n);
        WORK(pg, n, double);
        owlqn_pseudo_gradient(pg, x.values, g.values, n, c, start, end);
        result = DoubleList(pg, n);
        break;
    }
    case PROJECT: {
        DOUBLES(d, 1);
        DOUBLES(sign, 2);
        INTEGER(start, 3);
        INTEGER(end, 4);
        REQUIRE(start >= 0 && end >= start, "invalid projection interval");
        LENGTH(d, end);
        LENGTH(sign, end);
        owlqn_project(d.values, sign.values, start, end);
        result = DoubleList(d.values, d.length);
        break;
    }
    }
    Tcl_SetObjResult(ip, result);
    code = TCL_OK;
done:
    for (int i = 0; i < call.used; i++) {
        free(call.memory[i]);
    }        
    return code;
}

static const Command commands[] = {
    {"::tclopt::mp_qrfac", "m n a lda pivot lipvt", 6, QRFAC},
    {"::tclopt::mp_enorm", "x", 1, ENORM},
    {"::tclopt::mp_lmpar", "n r ldr ipvt ifree diag qtb delta par", 9, LMPAR},
    {"::tclopt::mp_covar", "n r ldr ipvt tol", 5, COVAR},
    {"::tclopt::rnd_uni", "seedVar", 1, RANDOM},
    {"::tclopt::update_trial_interval", "x fx dx y fy dy t ft dt tmin tmax brackt", 12, INTERVAL},
    {"::tclopt::owlqn_x1norm", "x start end", 3, NORM1},
    {"::tclopt::owlqn_pseudo_gradient", "x g n c start end", 6, GRADIENT},
    {"::tclopt::owlqn_project", "d sign start end", 4, PROJECT}};

DLLEXPORT int Tclopt_Init(Tcl_Interp *ip) {
    if (Tcl_InitStubs(ip, TCL_VERSION, 0) == NULL)
        return TCL_ERROR;
    if (!Tcl_FindNamespace(ip, "::tclopt", NULL, TCL_GLOBAL_ONLY) && !Tcl_CreateNamespace(ip, "::tclopt", NULL, NULL))
        return TCL_ERROR;
    for (size_t i = 0; i < sizeof(commands) / sizeof(commands[0]); i++)
        OptCreateCommand(ip, commands[i].name, Dispatch, (ClientData)&commands[i], NULL);
    return Tcl_PkgProvide(ip, "tclopt", PACKAGE_VERSION);
}

DLLEXPORT int Tclopt_SafeInit(Tcl_Interp *ip) { return Tclopt_Init(ip); }
