// The input data is a vector 'y' of length 'N'.
data {
  //1. Sample size information
  int<lower=0> n_t; // Number of realized return time points
  int<lower=0> N; // Number of stocks assets
  int<lower=0> J; // Number of experts
  int<lower=0> n_X; // Number of stock return realizations
  int<lower=0> n_M_obs; // Number of observed experts forecasts
  int<lower=0> n_M_mis; // Number of missing experts forecasts, n_X+N*J-n_M_obs
  //2. Market data
  vector<lower = -0.1>[n_t+4] r; // Risk free rates
  vector[n_X] x; // stock returns realizations
  int<lower=0, upper = N> i_X[n_X]; // stock asset ID:s
  int<lower=0, upper = n_t> t_X[n_X]; // starting points for stock return realization
  //3. Expert data
  vector[n_M_obs] M_obs; // expert forecast realizations
  int<lower=0, upper = J> j_M_obs[n_M_obs]; // expert ID:s
  int<lower=0, upper = J> j_M_mis[n_M_mis]; // expert ID:s
  int<lower=0, upper = N> i_M_obs[n_M_obs]; // stock asset ID:s
  int<lower=0, upper = N> i_M_mis[n_M_mis]; // stock asset ID:s
  int<lower=0, upper = (n_X+4*N)> ind_M_obs[n_M_obs]; // index of corresponding realized return
  int<lower=0, upper = (n_X+4*N)> ind_M_mis[n_M_mis]; // index of corresponding realized return
  // 4. Parameters for prior distributions
  // 4.1. Market parameters
  real mean_rp0; //prior expected value for industry risk premium
  real sd_rp0; //prior uncertainty of market rp as sd
  real<lower = 0> scale_psi0; // scale for industry t-scale 
  real<lower = 0> scale_psi_star; // scale for average t-scale 
  real<lower = 0> scale_omega; // scale for sd of c
  // Hyperparameters for degrees of freedom
  real<lower = 0> alpha_nu0;
  real<lower = 0> beta_nu0;
  real<lower = 0> alpha_nu;
  real<lower = 0> beta_nu;
   // 4.2. Expert parameters
  real<lower = 0> scale_kappa; // prior std of mean bias
  real<lower = 0> xi; // bias variation
  real<lower = 0> mean_tau; // mean responsiveness
  real<lower = 0> sd_tau; // sd of responsiveness
  real<lower = 0> scale_sigma_star; // average inaccuracy
  real<lower = 0> scale_iota; //inaccuracy variation
  real<lower = 0> shape_Omega; // shape parameter for correlation matrix
  // 5. Settings
  int<lower = 0, upper = 1> use_likelihood;
}

transformed data {
    vector[N] x_vec[n_t]; //Return vectors for each time points 
    // Build return vectors
    for(i in 1:n_X) x_vec[t_X[i]][i_X[i]] = x[i];
}

parameters {
  //Market parameters
  real<lower = 2> nu0;
  vector[n_t+3] eta; // random shock time series
  real<lower=0> psi0;
  real<lower = 2> nu;
  vector[N] eps_init[3];
  vector[N] mu[n_t];
  vector[N] mu_next;
  real<lower=0> psi_star; //shared standard deviations of asset shocks
  real<lower = 0> omega; //sd of shock scale random effect
  vector<lower=0>[N] c_i; //scale random effect for assets
  //Expert parameters
  // Bias
  real kappa; //mean expert bias
  vector[J] b_tilde[N]; //standardized expert biases for each stock
  // Responsiveness
  real<lower=0> tau; //horseshoe global parameter
  vector<lower=0>[J] lambda; //horseshoe local parameter
  vector[J] phi_tilde; //sensitivity to reality for each expert, standardized
  // Inaccuracy
  real<lower=0> sigma_star; //shared standard deviations of experts, aka precision
  real<lower = 0> iota; //sd of precision random effect
  vector<lower=0>[J] d_j; //sd random effect for experts
  cholesky_factor_corr[J] L_Omega;
 
  vector[n_M_mis] M_mis; // Vector containing "stochastic" nodes for filling missing values
}

transformed parameters {
//Market parameters
  vector<lower = 0>[N] psi;
  //Expert parameters
  vector[J] phi; //sensitivity to reality for each expert, aka informativeness,
  vector[J] b[N]; //expert biases for each stock
  vector<lower=0>[J] sigma; //standard deviations of experts, aka precision

    //Define Market parameters
    for(i in 1:N) psi[i] = psi_star/sqrt(c_i[i]);
  // Define Expert parameters
  for(i in 1:N) b[i] = kappa + b_tilde[i]; //biases
  phi = phi_tilde .* lambda * tau; //informativeness
  for(j in 1:J) sigma[j] = sigma_star/sqrt(d_j[j]);
}


model {
    /// Defining parameters that are used within model but not saved
    // Market parameters
    vector[n_t+3] eps[N]; // random shock time series
    vector[N] mu_shock[n_t]; //mu+realized shocks, before the last company shock
    vector[J] M[n_X]; //the standardized "data" with interpolated missing values
      // Current forecasts
  vector[J] M_next[N];
  
      // Fill M with non-missing values 
  for(ind in 1:n_M_obs) {
    if(ind_M_obs[ind] <= n_X) {
        M[ind_M_obs[ind]][j_M_obs[ind]] = M_obs[ind];
    }
    else 
    M_next[i_M_obs[ind]][j_M_obs[ind]] = M_obs[ind];
  }
  // Fill the rest of x with missing value "parameters"
  for(ind in 1:n_M_mis){
    if(ind_M_mis[ind] <= n_X){
       M[ind_M_mis[ind]][j_M_mis[ind]] = M_mis[ind];
    }
    else 
    M_next[i_M_mis[ind]][j_M_mis[ind]] = M_mis[ind];
  }

  // Priors
  nu0 ~ gamma(alpha_nu0,beta_nu0);
  nu ~ gamma(alpha_nu,beta_nu);
  psi_star ~ normal(0, scale_psi_star);
  omega ~ normal(0, scale_omega);
  c_i ~ gamma((1/omega)^2, 1/(omega^2)); 
  for(i in 1:N){
    for(t in 1:n_t) mu[t][i] ~ normal(r[t] + mean_rp0, sd_rp0);
    mu_next[i] ~ normal(r[n_t+4] + mean_rp0, sd_rp0);
  } 
  psi0 ~ normal(0, scale_psi0);
  for(t in 1:(n_t+3)) eta[t] ~ student_t(nu0, 0, psi0);
  for(t in 1:3){
      for(i in 1:N) eps_init[t][i] ~ student_t(nu, 0, psi[i]);
  }

  
  // Expert parameters
  kappa ~ normal(0, scale_kappa);
  for(i in 1:N) b_tilde[i] ~ normal(0,xi);
  phi_tilde ~ normal(0, 1);
  lambda ~ cauchy(0, 1);
  tau ~ gamma((mean_tau/sd_tau)^2,mean_tau/(sd_tau^2));
  sigma_star ~ normal(0, scale_sigma_star); // prior on the standard deviations
  iota ~ normal(0, scale_iota);
  d_j ~ gamma((1/iota)^2, 1/(iota^2));
  L_Omega ~ lkj_corr_cholesky(shape_Omega);
  
  // LIKELIHOODS and imputation model
  if(use_likelihood){
  for(ind in 1:n_X){
  M[ind] ~ multi_normal_cholesky(b[i_X[ind]] + phi*mu[t_X[ind]][i_X[ind]], diag_pre_multiply(sigma, L_Omega)); //prior for missing values, likelihood for non-missing
  }
  
    // Define parameters in the asset model
    for(i in 1:N) for(t in 1:3)  eps[i][t] = eps_init[t][i];
    for(t in 1:n_t){
      for(i in 1:N) {
      mu_shock[t][i] = mu[t][i] + sum(eta[t:(t+3)]) + sum(eps[i][t:(t+2)]);
      x_vec[t][i] ~ student_t(nu, mu_shock[t][i], psi[i]);
      eps[i][t+3] = x_vec[t][i]-mu_shock[t][i];
      }
  }
  //predictions as missing values
  for(i in 1:N) M_next[i] ~ multi_normal_cholesky(b[i] + phi*mu_next[i], diag_pre_multiply(sigma, L_Omega));
  }
}

generated quantities{
    vector[N] x_next; // next values of the returns
    real x0_next; //next values of equally weighted index
    corr_matrix[J] Omega; //correlation matrix between experts
    vector[4] eta_last;
    vector[4] eps_last[N];
  
    for(t in 1:4) eta_last[t] = student_t_rng(nu0 , 0, psi0);
    for(t in 1:4) for(i in 1:N) eps_last[i][t] = student_t_rng(nu, 0, psi[i]);
    for(i in 1:N){
        x_next[i] = mu_next[i] + sum(eta_last[1:4]) + sum(eps_last[i][1:4]);
    } 
    x0_next = sum(x_next)/N;
    Omega = multiply_lower_tri_self_transpose(L_Omega);
}
