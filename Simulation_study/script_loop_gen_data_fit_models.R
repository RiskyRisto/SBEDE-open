library(tidyverse)
library(tidyquant)
library(readxl)
library(writexl)
library(rstan)

source("Simulation_study/functions_simulator.R")

# 1 Settings ----
## 1.1. Set Scenarios ----
df_scen <- tibble(nT_hi = rep(c(20,40), each = 2), J = rep(c(4,8), 2)) %>% 
    mutate(scen_id = row_number())

## 1.2. Fixed settings ----
# Number of companies within the industry
N = 5 
# # of chains in MCMC algorithm
nchains <- 4 
# # of warmup iterations = # saved iterations in one chain
warmup = 1000 #7500 in the real study but takes time
#Number of repetition
study_size <- 5 #500 in the real study but it takes time

# Parameter values for simulating data can be manipulated within this Excel - file
df_fixed_parameters <- read_xlsx("Simulation_study/parameter_values_for_sim.xlsx",sheet = "Fixed_parameters")
df_varying_parameters <- read_xlsx("Simulation_study/parameter_values_for_sim.xlsx",sheet = "Varying_parameters")

# File for saving simulated data. Create if the file does not exist
dir_data_save <- "Simulation_study/Simulated_data/"
# Set your directory for reading STAN models
dir_models <- ""
# # File for saving forecasted scenarios. Create if the file does not exist
dir_res_save <- "Simulation_study/Sim_forecasts/"

## 1.3. Read and compile Bayes models ----
code_SB <- stanc(file = paste0(dir_models, "SBEDE.stan"), model_name = "SBEDE")
mod_SB <- stan_model(stanc_ret = code_SB)
code_EE <- stanc(file = paste0(dir_models, "EE_SBEDE.stan"), model_name = "model_EE")
mod_EE <- stan_model(stanc_ret = code_EE)
code_Me <- stanc(file = paste0(dir_models, "Merkle.stan"), model_name = "model_EE")
mod_Me <- stan_model(stanc_ret = code_Me)

## 1.3. Define set ids ----

for(set in 1:study_size){
for(sce in df_scen$scen_id){
    start_time_sim <- Sys.time()
    J = df_scen %>% filter(scen_id == sce) %>% pull(J)
    nT_hi = df_scen %>% filter(scen_id == sce) %>% pull(nT_hi)
    H = nT_hi+3
    
    generate_sim_set(N = N, J = J, nT_hi = nT_hi, set = set, df_fixed_parameters, df_varying_parameters, dir_data_save)
    
    # 2. Fit Bayesian models with Stan ----
    ## 2.1. Data preparations ----
    # Real returns
    df_market <- read_excel(paste0(dir_data_save,"sim_N",N,"_J",J,"_nT",nT_hi,"_set",set,".xlsx"), 
                            sheet = "df_market")
    # Expert predictions
    df_predictions <- read_excel(paste0(dir_data_save,"sim_N",N,"_J",J,"_nT",nT_hi,"_set",set,".xlsx"), 
                                 sheet = "df_predictions")
    
    # Manipulate expert data to a long version
    df_e_model <- df_predictions %>% 
        #mutate(ind = row_number()) %>% # id number for Company related return
        select(ind, t, everything(), -t_now, -t_end, -mu) %>% 
        pivot_longer(starts_with("Expert"), 
                     names_to = "Analyst_id", values_to = "Prediction") %>% 
        mutate(Analyst_id = as.numeric(str_sub(Analyst_id,8)))
    
    # Manipulate market data (returns) to a long version
    df_x_model <- df_market %>% select(t, everything(), -rf, -Industry_return,-t_now, -t_end) %>% 
        pivot_longer(-1, names_to = "Company", values_to = "Return") %>% 
        mutate(Company_id = as.numeric(str_sub(Company,9))) %>% 
        #left_join(df_dividends %>% distinct(Company, Company_id)) %>% #ID numbers for the companies
        arrange(t, Company_id) %>% 
        mutate(ind = row_number()) %>% # ind = id for each time-company combination
        select(ind, t, Company_id, Return) %>% 
        filter(t <= nT_hi)
    
    ## 2.2. Set prior parameters ----
    ### Market parameters
    prior_list <- list(
        mean_rp0 = 0.04, #prior expected value for industry risk premium
        sd_rp0 = 0.1, #prior uncertainty of market rp as sd
        # psi, = scale parameter for t-distribution
        scale_psi0 = 0.075,
        scale_psi_star = 0.075,
        scale_omega = 1.5 ,
        # nu = degree of freedom parameter for t-distribution
        alpha_nu0 = 2,
        beta_nu0 = 0.1,
        alpha_nu = 2,
        beta_nu = 0.1,
        ## Expert parameters
        scale_kappa = 0.15, # average bias
        xi = 0.3, # expert bias
        mean_tau = 0.065, # responsiveness
        sd_tau = 0.0125, # responsiveness
        scale_sigma_star = 0.15, # average inaccuracy
        scale_iota = 1.5, # inaccuracy variation
        shape_Omega = 1, # correlation
        ## Other settings
        use_likelihood = 1, # 0 if testing model based only on priors
        ## Special parameters for ee and Merkle
        tau_ee = 0.75, # responsiveness,
        phi_star_me = 0,
        tau_me = 1,
        iota_me = sqrt(10), # Based on Merkle et al (2020)
        kappa_me = 0
    )
    
    ## 2.3. SBEDE model ----
    fit_SB <- fit_SBEDE_model(df_market = df_market, 
                              df_e_model = df_e_model,
                              df_x_model = df_x_model,
                              model_obj = mod_SB,
                              N = N, J = J, nT_hi = nT_hi, H = H, 
                              nchains = nchains, warmup = warmup)
    
    # Future forecasts, x_next
    post_df_SB <- as.data.frame(fit_SB, pars = c("x0_next", "x_next")) %>% as_tibble()
    post_df_SB %>% apply(2, ess_bulk) %>% print()
    post_df_SB %>% apply(2, ess_tail) %>% print()
    
    ## 3.4. Exchangeable expert model for comparison ----
    # (not used for simulating data) 
    
    fit_EE <- fit_EE_model(df_market = df_market, 
                           df_e_model = df_e_model,
                           df_x_model = df_x_model,
                           model_obj = mod_EE,
                           N = N, J = J, nT_hi = nT_hi, H = H, 
                           nchains = nchains, warmup = warmup)
    
    # Future forecasts, x_next
    post_df_EE <- as.data.frame(fit_EE, pars = c("x0_next", "x_next")) %>% as_tibble()
    post_df_EE %>% apply(2, ess_bulk) %>% print()
    post_df_EE %>% apply(2, ess_tail) %>% print()
    
    ## 3.5. Merkle model for comparison ----
    # (not used for simulating data) 
    
    fit_Me <- fit_Merkle_model(df_market = df_market, 
                               df_e_model = df_e_model,
                               df_x_model = df_x_model,
                               model_obj = mod_Me,
                               N = N, J = J, nT_hi = nT_hi, H = H, 
                               nchains = nchains, warmup = warmup)
    
    # Future forecasts, x_next
    post_df_Me <- as.data.frame(fit_Me, pars = c("x0_next", "x_next")) %>% as_tibble()
    post_df_Me %>% apply(2, ess_bulk) %>% print()
    post_df_Me %>% apply(2, ess_tail) %>% print()
    
    ## 3.6. Combine results ----
    
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
        bind_rows(
            post_df_Me %>% 
                mutate(Scenario = row_number()) %>% 
                pivot_longer(-Scenario, names_to = "Asset") %>% 
                mutate(Method = "ME")
        ) %>% 
        
        mutate(Asset = str_replace(Asset,"x0_next", "x_next[0]"))
    
    saveRDS(df_posterior, file = paste0(dir_res_save,"posterior_N",N,"_J",J,"_nT",nT_hi,"_set",set,".rds"))
    end_time_sim <- Sys.time()
    timedif <- end_time_sim - start_time_sim
    
    print(paste0("Scenario ", sce, " set ",set, " ready. Running time: ",timedif))
}
}

