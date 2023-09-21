functions {
  vector X_model(real time, vector y, array[] real params) {
    vector[5] dydt;
    real S_to_E;
    real E_to_I;
    real I_to_R;
    real C_in;
    S_to_E = params[1]*y[1]*y[3]/params[3];
    E_to_I = 0.5*y[2];
    I_to_R = 0.5*y[3];
    C_in = params[2]*E_to_I;
    dydt[1] = -S_to_E;
    dydt[2] = S_to_E-E_to_I;
    dydt[3] = E_to_I-I_to_R;
    dydt[4] = I_to_R;
    dydt[5] = C_in;
    return dydt;
  }
}
data {
  int<lower = 1> n_obs;
  array[n_obs] int y;
  array[n_obs] real ts;
  real N;
}
parameters {
  real<lower = 0> par_beta;
  real<lower = 0, upper = 1> par_rho;
  real<lower = 0> I0;
}
transformed parameters{
  array[n_obs] vector[5] x; // Output from the ODE solver
  array[3] real params;
  vector[5] x0; // init values
  array[n_obs] real delta_x_1;
  x0[1] = N - I0; // S
  x0[2] = 0; // E
  x0[3] = I0; // I
  x0[4] = 0; // R
  x0[5] = I0; // C
  params[1] = par_beta;
  params[2] = par_rho;
  params[3] = N;
  x = ode_rk45(X_model, x0, 0, ts, params);
  delta_x_1[1] =  x[1, 5] - x0[5] + 1e-5;
  for (i in 1:n_obs-1) {
    delta_x_1[i + 1] = x[i + 1, 5] - x[i, 5] + 1e-5;
  }
}
model {
  par_beta ~ lognormal(0, 1);
  par_rho ~ beta(2, 2);
  I0 ~ lognormal(0, 1);
  y ~ poisson(delta_x_1);
}
generated quantities {
  real log_lik;
  array[n_obs] int sim_y;
  log_lik = poisson_lpmf(y | delta_x_1);
  sim_y = poisson_rng(delta_x_1);
}
