functions {
  vector X_model(real time, vector y, array[] real params) {
    vector[11] dydt;
    real S_to_E;
    real dly_E_to_I_1_out;
    real dly_E_to_I_2_out;
    real dly_E_to_I_3_out;
    real dly_S_to_E_1_out;
    real dly_S_to_E_2_out;
    real dly_S_to_E_3_out;
    real I_to_R;
    real E_to_I;
    real C_in;
    S_to_E = params[1]*y[7]*y[9]/10000;
    dly_E_to_I_1_out = y[1]/((2)/3.0);
    dly_E_to_I_2_out = y[2]/((2)/3.0);
    dly_E_to_I_3_out = y[3]/((2)/3.0);
    dly_S_to_E_1_out = y[4]/((2)/3.0);
    dly_S_to_E_2_out = y[5]/((2)/3.0);
    dly_S_to_E_3_out = y[6]/((2)/3.0);
    I_to_R = dly_E_to_I_3_out;
    E_to_I = dly_S_to_E_3_out;
    C_in = params[2]*E_to_I;
    dydt[1] = E_to_I - dly_E_to_I_1_out;
    dydt[2] = dly_E_to_I_1_out - dly_E_to_I_2_out;
    dydt[3] = dly_E_to_I_2_out - dly_E_to_I_3_out;
    dydt[4] = S_to_E - dly_S_to_E_1_out;
    dydt[5] = dly_S_to_E_1_out - dly_S_to_E_2_out;
    dydt[6] = dly_S_to_E_2_out - dly_S_to_E_3_out;
    dydt[7] = -S_to_E;
    dydt[8] = S_to_E-E_to_I;
    dydt[9] = E_to_I-I_to_R;
    dydt[10] = I_to_R;
    dydt[11] = C_in;
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
  real<lower = 0, upper = 1> par_rho;
  real<lower = 0> I0;
}
transformed parameters{
  array[n_obs] vector[n_difeq] x; // Output from the ODE solver
  array[n_params] real params;
  vector[n_difeq] x0; // init values
  array[n_obs] real delta_x_1;
  x0[1] = 0.333333333333333; // dly_E_to_I_1
  x0[2] = 0.333333333333333; // dly_E_to_I_2
  x0[3] = 0.333333333333333; // dly_E_to_I_3
  x0[4] = 0; // dly_S_to_E_1
  x0[5] = 0; // dly_S_to_E_2
  x0[6] = 0; // dly_S_to_E_3
  x0[7] = (10000) - I0; // S
  x0[8] = 0; // E
  x0[9] = I0; // I
  x0[10] = 0; // R
  x0[11] = 0; // C
  params[1] = par_beta;
  params[2] = par_rho;
  params[3] = I0;
  x = ode_rk45(X_model, x0, t0, ts, params);
  delta_x_1[1] =  x[1, 11] - x0[11] + 1e-5;
  for (i in 1:n_obs-1) {
    delta_x_1[i + 1] = x[i + 1, 11] - x[i, 11] + 1e-5;
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