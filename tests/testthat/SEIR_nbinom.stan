functions {
  vector X_model(real time, vector y, array[] real params) {
    vector[5] dydt;
    real S_to_E;
    real E1_to_I1;
    real I_to_R;
    real C_in;
    S_to_E = params[1]*y[1]*y[3]/10000;
    E1_to_I1 = 0.5*y[2];
    I_to_R = 0.5*y[3];
    C_in = params[2]*E1_to_I1;
    dydt[1] = -S_to_E;
    dydt[2] = S_to_E-E1_to_I1;
    dydt[3] = E1_to_I1-I_to_R;
    dydt[4] = I_to_R;
    dydt[5] = C_in;
    return dydt;
  }
}
data {
  int<lower = 1> n_obs;
  int<lower = 1> n_params;
  int<lower = 1> n_difeq;
  array[n_obs] int y;
  real t0;
  array[n_obs] real ts;
}
parameters {
  real<lower = 0> par_beta;
  real<lower = 0> I0;
  real<lower = 0, upper = 1> par_rho;
  real<lower = 0> phi_inv;
}
transformed parameters{
  array[n_obs + 1] vector[n_difeq] x; // Output from the ODE solver
  array[n_obs] real delta_x;
  real pred;
  vector[n_difeq] x0;
  array[n_params] real params;
  real phi_inv;
  //assignments
  phi_inv = 1 / phi;
  x0[1] = 10000 - 1 * I0;
  x0[2] = 0;
  x0[3] = I0;
  x0[4] = 0;
  x0[5] = 0; // This is C
  params[1] = beta;
  params[2] = rho;
  o = ode_rk45(X_model, x0, t0, ts, params);
  delta_x[1] =  x[1, 5]  - x0[5];
  for (i in 1:n_obs-1) {
    delta_x[i + 1] = x[i + 1, 5] - x[i, 5] + 1e-5;
  }
}
model {
  beta      ~ lognormal(0, 1);
  rho       ~ beta(2, 2);
  I0        ~ lognormal(0, 1);
  inv_phi   ~ exponential(5);
  y         ~ neg_binomial_2(delta_x, phi);
}
generated quantities {
  real log_lik;
  log_lik = neg_binomial_2_lpmf(y | x, phi_inv);
}
