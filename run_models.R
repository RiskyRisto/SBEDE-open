# 1. Load libraries ----

library(tidyverse)
library(readxl)
library(lubridate)
library(rstan)

# Set your directory for reading and saving files if relevant
dir_files <- ""
# Set your directory for reading STAN models if relevant
dir_models <- ""

#Industry
indu <- "LifeHealthInsurance"

# 2. Data wrangling----
## 2.1. Load data ----

# Real returns
df_market <- read_excel(paste0(dir_files,"example_data_sim_",indu,".xlsx"), 
                         sheet = "df_market")
# Expert predictions
df_predictions <- read_excel(paste0(dir_files,"example_data_sim_",indu,".xlsx"), 
                         sheet = "df_predictions")

# Dividends
df_dividends <- read_excel(paste0(dir_files,"example_data_sim_",indu,".xlsx"), 
                           sheet = "df_dividends")

# If you have already done Bayesian anlysis and saved posterior iterations, jump to 5.

## 2.2. Settings ----

# Decision moment
# i.e. beginning of the period, we are forecasting
H_date <- "2020-12-31"
H <- 29 

# number realizations before decision
# i.e. the last time point starting a period where we know return of the whole year at the time H
n_t <- H-4 

(N = df_predictions %>% distinct(Company_id) %>% nrow()) # #stocks
(E =  df_predictions %>% select(starts_with("Expert")) %>% ncol()) # #experts

## 2.3. Manipulate data ----

# Manipulate expert data to a long version
df_e_model <- df_predictions %>% 
    mutate(ind = row_number()) %>% # id number for Company related return
    select(ind, t = t_now, everything(),-Company, -t_end) %>% 
    pivot_longer(starts_with("Expert"), 
                 names_to = "Analyst_id", values_to = "Prediction") %>% 
    mutate(Analyst_id = as.numeric(str_sub(Analyst_id,8)))

# Manipulate market data (returns) to a long version
df_x_model <- df_market %>% select(t = t_now, everything(), -rf, -Industry_return, -t_end) %>% 
    pivot_longer(-1, names_to = "Company", values_to = "Return") %>% 
    left_join(df_dividends %>% distinct(Company, Company_id)) %>% #ID numbers for the companies
    arrange(t, Company_id) %>% 
    mutate(ind = row_number()) %>% # ind = id for each time-company combination
    select(ind, t, Company_id, Return) %>% 
    filter(t <= n_t)
df_x_model


# 3. Hyperparameters for Bayesian models ----

## 3.1. Market parameters
sd_rp0 = 0.04 #prior expected value for industry risk premium
sd_rp0 = 0.1 #prior uncertainty of market rp as sd
# psi = scale parameter for t-distribution
scale_psi0 = 0.075
scale_psi_star = 0.075
scale_omega = 1.5 
# nu = degree of freedom parameter for t-distribution
alpha_nu0 = 2
beta_nu0 = 0.1
alpha_nu = 2
beta_nu = 0.1
## 4.2. Expert parameters
scale_kappa = 0.15 # average bias
xi = 0.3 # expert bias
mean_tau = 0.065 # responsiveness
sd_tau = 0.0125 # responsiveness
scale_sigma_star = 0.15 # average inaccuracy
scale_iota = 1.5 # inaccuracy variation
shape_Omega = 1 # correlation

## Other settings
use_likelihood = 1 # 0 if testing model based only on priors
## Special parameters for ee
tau_ee = 0.75 # responsiveness

# 4. Modelling with Stan ----
## 4.1. Prepare data ----

#historical predictions
df_e_past <- df_e_model %>% 
    filter(t <= n_t)
# predictions for the next 4 quarters
df_e_next <- df_e_model %>% 
    filter(t == H)

# Extract the observed values as data frame
(df_ind_e_obs <- df_e_model %>% filter(!is.na(Prediction)) %>% select(ind, Analyst_id, Company_id, Prediction) %>% 
        filter(ind <= N*n_t | ind > (N*n_t+N*3))
)

# Extract the missing values as data
(df_ind_e_mis <- df_e_model %>% filter(is.na(Prediction)) %>% select(ind, Analyst_id, Company_id) %>% 
        filter(ind <= N*n_t | ind > (N*n_t+N*3))
)


## 4.1. Stan run settings ----
    
nchains <- 4
ncores <- nchains
thin = 1

# Test first with much less iterations
warmup = 5000
  iter = 15000
    
r <- nchains *(iter-warmup)/thin
    
    
## 4.2. SBEDE model ----
# (model used in simulating data) 

# Data list for the Stan model
dat_stan_SB <- list(
    n_t = n_t, # Number of realized return time points
    N = N, # Number of stocks assets
    J = E, # // Number of experts
    n_X = N*n_t, #// Number of stock return realizations
    n_M_obs = nrow(df_ind_e_obs),# // Number of experts forecasts
    n_M_mis = N*E*(n_t+1)-nrow(df_ind_e_obs), #nrow(df_ind_e_mis), # // No of missing forecasts
    #market data
    r = df_market %>% filter(t_now <= H)  %>% pull(rf),
    x = df_x_model$Return, #// stock returns realizations
    i_X = df_x_model$Company_id, # // stock asset ID:s
    t_X = df_x_model$t, # // time for stock return realization
    #expert data
    M_obs = df_ind_e_obs$Prediction, #; // expert forecast realizations
    j_M_obs = df_ind_e_obs$Analyst_id, #; // expert ID:s
    j_M_mis = df_ind_e_mis$Analyst_id, # // expert ID:s
    i_M_obs = df_ind_e_obs$Company_id, # // stock asset ID:s
    i_M_mis = df_ind_e_mis$Company_id, #  // stock asset ID:s
    ind_M_obs = df_ind_e_obs$ind, # // index of corresponding realized return
    ind_M_mis= df_ind_e_mis$ind, # // index of corresponding realized return
    #Hyper priors
    ## Market parameters
    mean_rp0 = sd_rp0,
    sd_rp0 = sd_rp0,
    scale_psi0 = scale_psi0, 
    scale_psi_star = scale_psi_star,
    scale_omega = scale_omega, 
    alpha_nu0 = alpha_nu0,
    beta_nu0 = beta_nu0,
    alpha_nu = alpha_nu,
    beta_nu =beta_nu,
    ## Expert parameters
    scale_kappa = scale_kappa, # prior std of average bias
    xi = xi, # std of expert biases
    mean_tau = mean_tau, # responsiveness
    sd_tau = sd_tau, # responsiveness
    scale_sigma_star = scale_sigma_star, # average inaccuracy
    scale_iota = scale_iota, # inaccuracy variation
    shape_Omega = shape_Omega, # prior parameter for correlations between experts
    ## Other settings
    use_likelihood = use_likelihood # 0 if testing model based only on priors
)

# Read and compile model   
code_SB <- stanc(file = paste0(dir_models, "SBEDE.stan"), model_name = "SBEDE")
mod_SB <- stan_model(stanc_ret = code_SB)

# Run iterations
{
start_time <- Sys.time()
stanobj <- sampling(mod_SB, data = dat_stan_SB,
                        iter = iter, chains = nchains, thin = thin, 
                        cores = ncores, warmup = warmup, init_r = 0.5,
                        verbose=FALSE)
end_time <- Sys.time() 
duration <- end_time - start_time
print(paste0("Run SBEDE. Run time: ", duration))
}

fit_SB <- stanobj
# Visual convergence
rstan::traceplot(fit_SB, pars = "phi", inc_warmup = FALSE)
rstan::traceplot(fit_SB, pars = "x_next", inc_warmup = FALSE)
# Future forecasts, x_next
post_df_SB <- as.data.frame(fit_SB, pars = c("x0_next", "x_next")) %>% as_tibble()
# Effective sample sizes of x_next
# (rule of thumb: should be > 400, try to redo mcmc sampling with more iterations if not)
#Bulk ess 
post_df_SB %>% apply(2, ess_bulk)
#Tail ess 
post_df_SB %>% apply(2, ess_tail)

# Parameter estimates and convergence metrics
# Return forecasts
print(fit_SB, c("x0_next","x_next"))
# Features of individual experts
print(fit_SB, c("phi", "sigma","b"))
# Real values, used in the simulation data
#phi[1]      phi[2]      phi[3]      phi[4] 
#1.11026735  0.61022375 -0.08028038  0.02519934 

#sigma[1]   sigma[2]   sigma[3]   sigma[4] 
#0.06903510 0.07638935 0.10830274 0.08600252 

# b_ij, one column for each expert
#Stock  1            2          3         4
#[1,] -0.04825936  0.017400790 0.10430063 0.1777876
#[2,] -0.10719678 -0.005253076 0.12460240        NA
#[3,] -0.04090497  0.255072340 0.01115048        NA

# Common features of experts
print(fit_SB, c( "kappa", "tau", "sigma_star"))
# Stock features
print(fit_SB, c("mu_next","psi0", "psi", "psi_star", "nu0","nu"))
    
run_info_SB <- tibble(nchains = nchains, thin = thin, warmup = warmup, iter = iter,
                       r = r, duration = duration)
    
run_list_SB <- list(dat_stan_SB = dat_stan_SB, post_df_SB = post_df_SB, 
                        run_info_SB = run_info_SB, fit_summary_SB = summary(fit_SB))

# Save results for later use    
saveRDS(run_list_SB, file = paste0(dir_files,"stan_res_SBEDE_", indu, ".rda"))
saveRDS(fit_SB, file = paste0(dir_files,"stanobj_SBEDE_", indu, ".rda"))
    
    
## 4.3. Exchangeable expert model for comparison ----
# (not used for simulating data) 

dat_stan_EE <- list(
    #1. Sample size information
    n_t = n_t, # Number of realized return time points
    N = N, # Number of stocks assets
    J = E, # // Number of experts
    n_X = N*n_t, #// Number of stock return realizations
    n_M_obs = nrow(df_ind_e_obs),# // Number of experts forecasts
    n_M_mis = N*E*(n_t+1)-nrow(df_ind_e_obs), #nrow(df_ind_e_mis), # // No of missing forecasts
    #2. Market data
    r = df_market %>% filter(t_now <= H)  %>% pull(rf),
    x = df_x_model$Return, #// stock returns realizations
    i_X = df_x_model$Company_id, # // stock asset ID:s
    t_X = df_x_model$t, # // time for stock return realization
    #3. Expert data
    M_obs = df_ind_e_obs$Prediction, #; // expert forecast realizations
    j_M_obs = df_ind_e_obs$Analyst_id, #; // expert ID:s
    j_M_mis = df_ind_e_mis$Analyst_id, # // expert ID:s
    i_M_obs = df_ind_e_obs$Company_id, # // stock asset ID:s
    i_M_mis = df_ind_e_mis$Company_id, #  // stock asset ID:s
    ind_M_obs = df_ind_e_obs$ind, # // index of corresponding realized return
    ind_M_mis= df_ind_e_mis$ind, # // index of corresponding realized return
    #4. Parameters for hyperpriors
    ## 4.1. Market parameters
    mean_rp0 = sd_rp0,
    sd_rp0 = sd_rp0,
    scale_psi0 = scale_psi0, 
    scale_psi_star = scale_psi_star,
    scale_omega = scale_omega, 
    alpha_nu0 = alpha_nu0,
    beta_nu0 = beta_nu0,
    alpha_nu = alpha_nu,
    beta_nu =beta_nu,
    ## 4.2. Expert parameters
    scale_kappa = scale_kappa, # average bias
    xi = xi, # bias variation
    tau = tau_ee, # sd of responsiveness
    scale_sigma_star = scale_sigma_star, # average inaccuracy
    ## 5. Settings
    use_likelihood = use_likelihood # 0 if testing model based only on priors
)


# Read and compile model
code_EE <- stanc(file = paste0(dir_models, "EE_SBEDE.stan"), model_name = "model_EE")
mod_EE <- stan_model(stanc_ret = code_EE)

# Run iterations

{
start_time <- Sys.time()
stanobj <- sampling(mod_EE, data = dat_stan_EE,
                        iter = iter, chains = nchains, thin = thin, 
                        cores = ncores, warmup = warmup, init_r = 0.5,
                        verbose=FALSE)
end_time <- Sys.time() 
duration <- end_time - start_time
print(paste0("Run EE. Run time: ", duration))
}
    
fit_EE <- stanobj
# Visual convergence
rstan::traceplot(fit_EE, pars = "phi_star", inc_warmup = FALSE)
rstan::traceplot(fit_EE, pars = "x_next", inc_warmup = FALSE)

# Future forecasts, x_next
post_df_EE <- as.data.frame(fit_EE, pars = c("x0_next", "x_next")) %>% as_tibble()
# Effective sample sizes of x_next
# (rule of thumb: should be > 400, try to redo mcmc sampling with more iterations if not)
#Bulk ess 
post_df_EE %>% apply(2, ess_bulk)
#Tail ess 
post_df_EE %>% apply(2, ess_tail)

# Parameter estimates and convergence metrics
print(fit_EE, c("x0_next","mu_next", "psi0", "psi", "nu0","nu", "kappa", "b", "phi_star", "sigma_star", "rho"))


run_info_EE <- tibble(nchains = nchains, thin = thin, warmup = warmup, iter = iter,
                       r = r, duration = duration)
    
run_list_EE <- list(dat_stan_EE = dat_stan_EE, post_df_EE = post_df_EE,
                        run_info_EE = run_info_EE, fit_summary_EE = summary(fit_EE))

# Save results for later use     
saveRDS(run_list_EE, file = paste0(dir_files,"stan_res_EE_", indu, ".rda"))
saveRDS(fit_EE, file = paste0(dir_files,"stanobj_EE_", indu, ".rda"))

## 4.4. Visualize results ----
require(bayesplot)
# Select fitted model
fit <- fit_SB

# Experts' responsivenesses to reality
mcmc_areas(rstan::extract(fit, pars = "phi", permuted = FALSE), prob = 0.9) + 
    ggtitle("Posterior distribution for phi","with medians and 90% intervals") +
    lims(x = c(-0.5,1.5))

# Experts' biases for each stock
mcmc_areas(rstan::extract(fit, pars = "b", permuted = FALSE), prob = 0.9) + 
    ggtitle("Posterior distribution for personal bias","with medians and 90% intervals") +
    lims(x = c(-0.5,1))

# Future returns
mcmc_areas(rstan::extract(fit, pars = "x_next", permuted = FALSE), prob = 0.9) + 
    ggtitle("Posterior distribution for future returns","with medians and 90% intervals") +
    lims(x = c(-1,1))

# Select fitted model
fit <- fit_EE

# Expert's common resposivenees to reality
mcmc_areas(rstan::extract(fit, pars = "phi_star", permuted = FALSE), prob = 0.9) + 
    ggtitle("Posterior distribution for phi","with medians and 90% intervals") +
    lims(x = c(-0.5,1))
# Future returns
mcmc_areas(rstan::extract(fit, pars = "x_next", permuted = FALSE), prob = 0.9) + 
    ggtitle("Posterior distribution for future returns","with medians and 90% intervals") +
    lims(x = c(-1,1))

df_H = df_e_model %>% filter(t == H) 

# Consensus judgements 
df_H %>% 
    drop_na(Prediction) %>% 
    group_by(Company_id) %>% 
    summarise(mean = mean(Prediction), sd = sd(Prediction), med = median(Prediction), n = n())

# SE model mean and sd of future returns
run_list_SB$post_df_SB %>% summarise(across(everything(), mean))
run_list_SB$post_df_SB %>% summarise(across(everything(), sd))
# EE model mean and sd of future returns
run_list_EE$post_df_EE %>% summarise(across(everything(), mean))
run_list_EE$post_df_EE %>% summarise(across(everything(), sd))

# Explanations for differences
# Individual current targets

df_H %>% 
    ggplot(aes(x = Prediction, y = Analyst_id)) +
    geom_point()+
    facet_wrap(~Company_id) +
    geom_vline(xintercept = 0, col ="red") +
    scale_y_continuous(breaks = seq(0, max(df_H$Analyst_id), by = 1)) +
    ggtitle("Expert predictions of future returns for each stock")

# Historical performance
lim_fix <- 1
df_x_model %>% 
    select(-ind) %>% 
    left_join(df_e_model) %>% 
    drop_na(Prediction) %>%
    mutate(Company = as.factor(Company_id)) %>% 
    ggplot(aes(x = Return, y = Prediction, col = Company)) +
    #geom_abline(intercept = 0, slope = 1) +
    geom_abline(intercept = 0, slope = 0, col = "black") +
    geom_vline(xintercept = 0, col = "black") +
    #geom_smooth(method = "lm", col = "gray40", fill = NA) +
    geom_smooth(method="lm", fill = NA) +
    geom_point(size = 0.2) +
    facet_wrap(~Analyst_id) +
    lims(x = c(-lim_fix,lim_fix), y = c(-lim_fix,lim_fix))+
    ylab("Target Return")+
    ggtitle("Historical performance of the experts")

df_x_model %>% 
    select(-ind) %>% 
    left_join(df_e_model) %>%
    drop_na(Prediction) %>%
    mutate(Company = as.factor(Company_id)) %>% 
    ggplot(aes(x = Return, y = Prediction)) +
    #geom_abline(intercept = 0, slope = 1) +
    geom_abline(intercept = 0, slope = 0, col = "black") +
    geom_vline(xintercept = 0, col = "black") +
    #geom_smooth(method = "lm", col = "gray40", fill = NA) +
    geom_smooth(method="lm", fill = NA) +
    geom_point(size = 0.2) +
    facet_wrap(~Company) +
    lims(x = c(-lim_fix,lim_fix), y = c(-lim_fix,lim_fix))+
    ylab("Prediction")+
    ggtitle("Collective performance of estimating different companies")

## 4.5. Combine results ----
df_posterior <- post_df_SB %>%
    mutate(Scenario = row_number()) %>% 
    pivot_longer(-Scenario, names_to = "Asset") %>% 
    mutate(Method = "SB") %>% 
    bind_rows(
        post_df_EE %>% 
            mutate(Scenario = row_number()) %>% 
            pivot_longer(-Scenario, names_to = "Asset") %>% 
            mutate(Method = "EE")
        ) %>% 
    mutate(Asset = str_replace(Asset,"x0_next", "x_next[0]"))

df_posterior

df_posterior_div <- df_posterior %>% 
    left_join(
        df_dividends %>% 
            mutate(Asset = paste0("x_next[",Company_id,"]")) %>% 
            #predicting dividends with the latest div_pr at the decision moment
            filter(Date == as.Date(H_date)-years(1)) %>% 
            select(Div_pr_est = Div_pr, Asset)
        
    ) %>%
    rename(x = value) %>% 
    mutate(x_exp = exp(x), y_exp = x_exp + Div_pr_est, y = log(y_exp)) %>% 
    mutate(Asset = str_replace(Asset, "x_next\\[", "Asset_") %>% str_replace("\\]",""))

df_posterior_div

## 2.3. Save posterior predictions if you want to optimize in different session 
saveRDS(df_posterior_div, file = paste0(dir_files, "df_posterior_div.rds"))


# 5. Portfolio decisions ----

## 5.1. Optimization with posteriors (x_next) and dividend expectations----
# Load posterior predictions
df_posterior_div <- readRDS(file = paste0(dir_files, "df_posterior_div.rds"))
(N <- df_posterior_div %>% filter(Asset != "Asset_0") %>% distinct(Asset) %>% nrow())
H <- 29

source(paste0(dir_files,"functions_optimization_example.R"))

# Two-step method (set two_step = TRUE): elegant method that is reported in article
## -> slow and the result include some uncertainty
# Reference-method (set two_step = FALSE): brute force -style method that optimizes separatery every asset and filters the best
## -> with a low number of assets within industry faster and but not elegant for general use
# Check that both methods give same result to be sure about the solutions
two_step <- T

opt_df <- tibble(Method = NULL, Asset = NULL, w_opt = NULL, opt_val = NULL, convg = NULL)

for(method in c("SB", "EE")){
    if(two_step){
        require("DEoptim")
        de_f <- function(w) -objective_Eg_industry_N_const(w, eps = 0.01)
        n_assets = N
    
        print(paste("Method", method))
  
        opt_dat_df <- df_posterior_div %>% 
            filter(Method == method) %>% 
            pivot_wider(Scenario, names_from = "Asset", values_from = "y") %>% 
            rename(Index = "Asset_0")
        
        np <- 10*n_assets
        
        init_pop = matrix(
            rep(rep(0,n_assets), np*0.2)
            ,byrow=T, ncol = n_assets
        )
        
        for(i in 1:n_assets){
            init_pop <- init_pop %>% 
                rbind(
                    matrix(
                        rep(c(rep(0,i-1),0.5,rep(0,n_assets-i)),8),
                        byrow = T, ncol = n_assets)
                )
        }
        
        
        sol_de <- DEoptim(fn = de_f, 
                          control = list(
                              strategy = 2,
                              itermax=60, #30
                              initialpop = init_pop,
                              NP = np
                          ),
                          lower = rep(0, n_assets),
                          upper = rep(1, n_assets)
        )
        
        (init_sol <- sol_de$optim$bestmem)
        (init_val <- sol_de$optim$bestval)
        
        lb_sol <- optim(par = init_sol, fn = de_f,
                        lower = rep(0, n_assets), upper = rep(2, n_assets), method = "L-BFGS-B")
        
        print(lb_sol)
        
        
        opt_df <- opt_df %>% 
            bind_rows(
                tibble(Method = method, Asset = which.max(lb_sol$par), 
                       w_opt =  max(lb_sol$par), opt_val = -lb_sol$value, convg = lb_sol$convergence)
            )
        
    }
    if(!two_step)
        for(i in 1:N){
            opt_dat_df <- df_posterior_div %>% 
                filter(Method == method) %>% 
                pivot_wider(Scenario, names_from = "Asset", values_from = "y") %>% 
                rename(Index = "Asset_0", X = paste0("Asset_",i))
            
            
            lb_sol <- optim(par = 0.5, fn = function(w){-objective_Eg_industry1(w)},
                            lower = 0, upper = 1, method = "L-BFGS-B")
            opt_df <- opt_df %>% 
                bind_rows(
                    tibble(Method = method, Asset = i, 
                           w_opt = lb_sol$par, opt_val = -lb_sol$value, convg = lb_sol$convergence)
                )
        }
}

opt_df

if(!two_step){
    opt_df_best <- opt_df %>% arrange(Method, desc(opt_val)) %>% 
        distinct(Method, .keep_all = TRUE)
    opt_df_all <- opt_df
}

if(two_step) opt_df_best2 <- opt_df %>% arrange(Method)

# # If both methods run, check that both methods gave the same results
# opt_df_best %>% left_join(
#     opt_df_best2 %>%
#         rename(Asset2 = Asset, w_opt2 = w_opt, opt_val2=opt_val, convg2 = convg)
#     ) %>% 
#     mutate(across(where(is.double),round, digits = 3)) %>% 
#     mutate(SAME = 1*(Asset == Asset2)) %>% 
#     as.data.frame()


# Select opt_df_best or opt_df_best2
opt_df_best_select <- opt_df_best2

## 5.2 Performance of the portfolios based on optimization----

# Realizations at the following year after H
df_H <- df_market %>% filter(t_now == H) %>% 
    select(-t_end, -rf) %>% 
    pivot_longer(-c(1), names_to = "Company", values_to = "Return") %>% 
    left_join(
        df_dividends %>% 
            select(t_now, Div_pr, Company, Company_id)
    )

# Different return measurements
df_returns_next <- df_H %>% 
    # exp = e^x 
    mutate(x_exp_real = exp(Return), y_exp_real = x_exp_real + Div_pr, y_real = log(y_exp_real)) %>% 
    arrange(Company_id) %>% 
    mutate(Asset = as.integer(Company_id)) %>% 
    select(-Company_id) %>% 
    rename(x_real = Return)


# Expectations at the decision time
df_E <- df_posterior_div %>% 
    mutate(G = exp(y)) %>% #capital growth rate
    mutate(g = log(G)) %>% # logarithmic capital growth rate
    group_by(Method, Asset) %>% 
    summarize(Eg = mean(g), EG = mean(G), .groups = "drop") %>% 
    mutate(Asset = as.integer(str_sub(Asset,7)))

df_E

# Realizations versus expectations at the decision time
decision_tbl <- opt_df_best_select %>% select(Method, Asset, w_opt) %>% 
    left_join(df_returns_next, by = "Asset") %>% 
    # Index realizations (asset 0)
    mutate(
        x_real0 = df_returns_next %>% filter(Asset == "0") %>% pull(x_real),
        y_real0 = df_returns_next %>% filter(Asset == "0") %>% pull(y_real)
    ) %>% 
    bind_rows(
        df_returns_next %>% filter(Asset == "0") %>% 
            mutate(Method = "Index", w_opt = 1, x_real0 = x_real, y_real0 = y_real)
        ) %>% 
    mutate(G_real = w_opt*exp(y_real) + (1-w_opt)*exp(y_real0)) %>% 
    left_join(df_E %>% select(Method, Asset, EG)) %>% 
    # Index expectations based on both models
    left_join(df_E %>% filter(Asset == 0) %>% select(Method, EG0 = EG))

decision_tbl



