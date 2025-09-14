
int update_trial_interval(lbfgsfloatval_t *x, lbfgsfloatval_t *fx, lbfgsfloatval_t *dx,
                          lbfgsfloatval_t *y, lbfgsfloatval_t *fy, lbfgsfloatval_t *dy,
                          lbfgsfloatval_t *t, lbfgsfloatval_t *ft, lbfgsfloatval_t *dt,
                          const lbfgsfloatval_t tmin, const lbfgsfloatval_t tmax, int *brackt);

lbfgsfloatval_t owlqn_x1norm(const lbfgsfloatval_t *x, int start, int n);
void owlqn_pseudo_gradient(lbfgsfloatval_t *pg, const lbfgsfloatval_t *x, const lbfgsfloatval_t *g,
                           int n, lbfgsfloatval_t c, int start, int end);
void owlqn_project(lbfgsfloatval_t *d, const lbfgsfloatval_t *sign, int start, int end);
