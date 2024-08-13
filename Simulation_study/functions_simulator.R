simulate_M <- function(mu, b_vec, df_phi_sigma, Omega_mat, n = 1){
    sigma_vec <- df_phi_sigma %>% pull(sigma)
    mat_Cov <- diag(sigma_vec) %*% Omega_mat %*% diag(sigma_vec)
    phi_vec <- df_phi_sigma %>% pull(phi)
    
    E_M <- as.vector(phi_vec)*mu + b_vec
    
    require(MASS)
    sim <- mvrnorm(n = n, E_M, mat_Cov)
    detach("package:MASS")
    
    out <- as_tibble(matrix(nrow = 0, ncol = length(sim)), .name_repair = ~ paste0("M_",1:length(sim)))
    out[1,] <- t(sim)
    
    out
}

#M_sim_test <- simulate_M(mu = df_mu$mu[1], b_vec = b_mat[1,], df_phi_sigma, Omega_mat)

generate_sim_set <- function(N, J, nT_hi, set=1, df_fixed_parameters, df_varying_parameters, dir_data_save){
    ## 1. Settings ----
    #H = nT_hi+4
    
    ## 2. Define mu values ----
    #df_par_collect_ind
    #df_mean_mu <- tibble(i = rep(1:N,(nT_hi+4)), t = rep(1:(nT_hi+4), each = N)) %>% 
    #    mutate(ind = row_number()) %>% left_join(
    #        df_varying_parameters %>% 
    #            filter(Scenario == paste0("N",N), Parameter == "mean_mu") %>% 
    #            select(mean_mu = value, i)
    #    ) 
    df_mu <- tibble(i = rep(1:N,(nT_hi+4)), t = rep(1:(nT_hi+4), each = N)) %>% 
        mutate(ind = row_number()) %>% 
        mutate(mu = rnorm(
            (nT_hi+4)*N, 
            mean = df_fixed_parameters %>% filter(Parameter=="mean_mu") %>% pull(),
            sd = df_fixed_parameters %>% filter(Parameter=="sd_mu") %>% pull()
            )
            )
    
    ## 4. Define expert parameters ----
    iota <- df_fixed_parameters %>% filter(Parameter == "iota") %>% pull(value)
    sigma_star <- df_fixed_parameters %>% filter(Parameter == "sigma_star") %>% pull(value)
    kappa <- df_fixed_parameters %>% filter(Parameter == "kappa") %>% pull(value)
    xi <- df_fixed_parameters %>% filter(Parameter == "xi") %>% pull(value)
    mean_rho <- df_fixed_parameters %>% filter(Parameter == "mean_rho") %>% pull(value)
    # Half-interval for randomizing with uniform distribution
    hi <- df_fixed_parameters %>% filter(Parameter == "hi") %>% pull(value)
    
    d = rgamma(J, shape = 1/iota^2, rate = 1/iota^2)
    sigma <- sigma_star/sqrt(d)
    
    #df_par_collect_j
    df_phi_sigma <- df_varying_parameters %>% 
        filter(Scenario == paste0("J",J), Parameter == "phi") %>% 
        select(phi = value, i) %>% 
        mutate(sigma = sigma)
    
    
    #df_par_collect_ij 
    df_b <- tibble(i = rep(1:N,each = J), j = rep(1:J, N)) %>% 
        mutate(kappa = kappa) %>% 
        mutate(b = rnorm(
            J*N, 
            mean = kappa,
            sd = xi
        )
        ) 
    
    b_mat <- df_b %>% select(i, j, b) %>%
        arrange(j) %>% 
        pivot_wider(names_from = "j", values_from = "b") %>% 
        arrange(i) %>%  
        select(-i) %>% 
        as.matrix()
    

    
    df_Omega <- tibble(j1 = rep(1:J,J), j2 = rep(1:J, each = J)) %>% 
        #mutate(value = ifelse(j1==j2, 1, rho))
        mutate(rho = runif(
            J*J, 
            min = mean_rho - hi,
            max = mean_rho + hi
        )
        ) %>% 
        mutate(rho = ifelse(j1==j2, 1, rho)) %>% #correlation with itself = 1
        filter(j1<=j2)
    
    
    Omega_mat <- df_Omega %>%
        #making matrix symmetric
        bind_rows(df_Omega %>% rename(j2=j1, j1 = j2) %>% filter(j1 != j2)) %>%  
        pivot_wider(names_from = j2, values_from = "rho") %>% 
        select(-j1) %>% 
        as.matrix()
    
    
    ## 5. Define asset parameters ----
    
    #df_par_collect_glob
    df_asset <- df_fixed_parameters %>%
        select(Par = Parameter, value) %>% 
        filter((str_sub(Par, 1,2) == "nu")|(str_sub(Par, 1,3) == "psi")|(str_sub(Par, 1,3) == "ome"))
    
    
    ## 6. Simulate new Expert data ---
    
    
    # Repeating for different time points
    b_vecs_for_mu  <- vector("list", length = nrow(df_mu))
    for(k in 1:nrow(df_mu)) b_vecs_for_mu[[k]] <- b_mat[df_mu$i[k],]
    
    dat_M_sim <- map2_dfr(.x = df_mu$mu, .y = b_vecs_for_mu,
                          .f = simulate_M,
                          df_phi_sigma=df_phi_sigma,
                          Omega_mat = Omega_mat)
    
    ## 6. Build expert data for Stan analysis ----
    
    df_M_sim <- df_mu %>% bind_cols(dat_M_sim) %>% 
        pivot_longer(starts_with("M_"), names_to = "j", values_to = "M") %>% 
        mutate(j = as.numeric(str_sub(j,3)))
    
    df_predictions <- df_mu %>% left_join(df_M_sim %>% select(i,j,t,M), multiple = "all") %>% 
        pivot_wider(names_from = j, values_from = M, names_prefix = "Expert_") %>% 
        #select(Company_id = i, t, all_of(paste0("Expert_",1:ncol(dat_M_sim)))) %>% 
        rename(Company_id = i) %>% 
        mutate(t_now = t-1) %>% 
        mutate(t_end = t_now+4) %>% 
        select(t_now, t, t_end, everything())
    
    ## 7. Simulate asset data
    nu0 <- df_fixed_parameters %>% filter(Parameter == "nu0") %>% pull(value)
    nu <- df_fixed_parameters %>% filter(Parameter == "nu") %>% pull(value)
    psi0 <- df_fixed_parameters %>% filter(Parameter == "psi0") %>% pull(value)
    psi_star <- df_fixed_parameters %>% filter(Parameter == "psi_star") %>% pull(value)
    omega <- df_fixed_parameters %>% filter(Parameter == "omega") %>% pull(value)
    
    c = rgamma(N, shape = 1/omega^2, rate = 1/omega^2)
    psi <- psi_star/sqrt(c)
    
    
    eta <- rt(n = nT_hi+7, df = nu0)*psi0
    # Should be equal with big n
    #psi0*sqrt(nu0/(nu0-2))
    #sd(eta)
    
    df_eta <- tibble(eta = eta) %>% mutate(t = row_number())
    
    eps_mat <- matrix(ncol = N, nrow = (nT_hi+7))
    for(i in 1:N) eps_mat[,i] <- rt(n = nT_hi+7, df = nu)*psi[i]
    colnames(eps_mat) <- paste0("Company_",1:N)
    
    df_eps <- as_tibble(eps_mat) %>% mutate(t = row_number()) %>% 
        pivot_longer(-t, names_to = "Company", values_to = "eps") %>% 
        mutate(i = as.numeric(str_sub(Company,-1, -1)))
    
    df_x <- df_mu %>% 
        right_join(df_eps %>% select(t,i,eps)) %>% 
        right_join(df_eta) %>% 
        group_by(i) %>% 
        mutate(eps_sum = rollsum(eps,4, align = "left", fill = NA)) %>% 
        ungroup() %>% 
        group_by(i) %>% 
        mutate(eta_sum = rollsum(eta,4, align = "left", fill = NA)) %>% 
        ungroup() %>% 
        mutate(x = mu + eps_sum + eta_sum)
    
    
    df_market <- df_predictions %>% select(t_now, t, t_end, Company_id) %>% 
        left_join(df_x %>% select(t, Company_id = i, x)) %>% 
        pivot_wider(names_from = "Company_id", values_from = "x", names_prefix = "Company_") %>% 
        mutate(rf = 0) %>% 
        mutate(Industry_return = rowMeans(select(., starts_with("Company"))))
    
    write_xlsx(list(df_market = df_market,
                    df_predictions = df_predictions),
               path = paste0(dir_data_save,"sim_N",N,"_J",J,"_nT",nT_hi,"_set",set,".xlsx"))
}

fit_SBEDE_model <- function(df_market, 
                      df_e_model,
                      df_x_model,
                      model_obj,
                      N, J, nT_hi, H, nchains, warmup){
    ## 1 Prepare data ----
    
    #historical predictions
    df_e_past <- df_e_model %>% 
        filter(t <= nT_hi)
    # predictions for the next 4 quarters
    df_e_next <- df_e_model %>% 
        filter(t == H+1)
    
    # Extract the observed values as data frame
    (df_ind_e_obs <- df_e_model %>% filter(!is.na(Prediction)) %>% select(ind, Analyst_id, Company_id, Prediction) %>% 
            filter(ind <= N*nT_hi | ind > (N*nT_hi+N*3))
    )
    
    # Extract the missing values as data
    (df_ind_e_mis <- df_e_model %>% filter(is.na(Prediction)) %>% select(ind, Analyst_id, Company_id) %>% 
            filter(ind <= N*nT_hi | ind > (N*nT_hi+N*3))
    )
    
    
    # 2. Settings ----
    # Set your directory for reading and saving files if relevant
    # dir_files <- ""
    
    
    ncores <- nchains
    thin = 1
    
    # Test first with much less iterations
    iter = 2*warmup
    
    r <- nchains *(iter-warmup)/thin
    
    
    ## SBEDE model 
    # (model used in simulating data) 
    
    # Data list for the Stan model
    dat_stan_SB <- list(
        n_t = nT_hi, # Number of realized return time points
        N = N, # Number of stocks assets
        J = J, # // Number of experts
        n_X = N*nT_hi, #// Number of stock return realizations
        n_M_obs = nrow(df_ind_e_obs),# // Number of experts forecasts
        n_M_mis = N*J*(nT_hi+1)-nrow(df_ind_e_obs), #nrow(df_ind_e_mis), # // No of missing forecasts
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
        mean_rp0 = prior_list$mean_rp0,
        sd_rp0 = prior_list$sd_rp0,
        scale_psi0 = prior_list$scale_psi0, 
        scale_psi_star = prior_list$scale_psi_star,
        scale_omega = prior_list$scale_omega, 
        alpha_nu0 = prior_list$alpha_nu0,
        beta_nu0 = prior_list$beta_nu0,
        alpha_nu = prior_list$alpha_nu,
        beta_nu = prior_list$beta_nu,
        ## Expert parameters
        scale_kappa = prior_list$scale_kappa, # prior std of average bias
        xi = prior_list$xi, # std of expert biases
        mean_tau = prior_list$mean_tau, # responsiveness
        sd_tau = prior_list$sd_tau, # responsiveness
        scale_sigma_star = prior_list$scale_sigma_star, # average inaccuracy
        scale_iota = prior_list$scale_iota, # inaccuracy variation
        shape_Omega = prior_list$shape_Omega, # prior parameter for correlations between experts
        ## Other settings
        use_likelihood = prior_list$use_likelihood # 0 if testing model based only on priors
    )
    
    # 3. Read, compile and fit model ----  
  
    
    # Run iterations
    {
        start_time <- Sys.time()
        stanobj <- sampling(model_obj, data = dat_stan_SB,
                            iter = iter, chains = nchains, thin = thin, 
                            cores = ncores, warmup = warmup, init_r = 0.5,
                            verbose=FALSE)
        end_time <- Sys.time() 
        duration <- end_time - start_time
        print(paste0("Run SBEDE. Run time: ", duration))
    }
    
    stanobj
}

fit_EE_model <- function(df_market, 
                      df_e_model,
                      df_x_model,
                      model_obj,
                      N, J, nT_hi, H, nchains, warmup){
    ## 1 Prepare data ----
    
    #historical predictions
    df_e_past <- df_e_model %>% 
        filter(t <= nT_hi)
    # predictions for the next 4 quarters
    df_e_next <- df_e_model %>% 
        filter(t == H+1)
    
    # Extract the observed values as data frame
    (df_ind_e_obs <- df_e_model %>% filter(!is.na(Prediction)) %>% select(ind, Analyst_id, Company_id, Prediction) %>% 
            filter(ind <= N*nT_hi | ind > (N*nT_hi+N*3))
    )
    
    # Extract the missing values as data
    (df_ind_e_mis <- df_e_model %>% filter(is.na(Prediction)) %>% select(ind, Analyst_id, Company_id) %>% 
            filter(ind <= N*nT_hi | ind > (N*nT_hi+N*3))
    )
    
    
    # 2. Settings ----
    # Set your directory for reading and saving files if relevant
    # dir_files <- ""
    
    
    ncores <- nchains
    thin = 1
    
    # Test first with much less iterations
    iter = 2*warmup
    
    r <- nchains *(iter-warmup)/thin

    
    dat_stan_EE <- list(
        #1. Sample size information
        n_t = nT_hi, # Number of realized return time points
        N = N, # Number of stocks assets
        J = J, # // Number of experts
        n_X = N*nT_hi, #// Number of stock return realizations
        n_M_obs = nrow(df_ind_e_obs),# // Number of experts forecasts
        n_M_mis = N*J*(nT_hi+1)-nrow(df_ind_e_obs), #nrow(df_ind_e_mis), # // No of missing forecasts
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
        mean_rp0 = prior_list$mean_rp0,
        sd_rp0 = prior_list$sd_rp0,
        scale_psi0 = prior_list$scale_psi0, 
        scale_psi_star = prior_list$scale_psi_star,
        scale_omega = prior_list$scale_omega, 
        alpha_nu0 = prior_list$alpha_nu0,
        beta_nu0 = prior_list$beta_nu0,
        alpha_nu = prior_list$alpha_nu,
        beta_nu = prior_list$beta_nu,
        ## 4.2. Expert parameters
        scale_kappa = prior_list$scale_kappa, # average bias
        xi = prior_list$xi, # bias variation
        tau = prior_list$tau_ee, # sd of responsiveness
        scale_sigma_star = prior_list$scale_sigma_star, # average inaccuracy
        ## 5. Settings
        use_likelihood = prior_list$use_likelihood # 0 if testing model based only on priors
    )
    
    
    # Run iterations
    {
        start_time <- Sys.time()
        stanobj <- sampling(model_obj, data = dat_stan_EE,
                            iter = iter, chains = nchains, thin = thin, 
                            cores = ncores, warmup = warmup, init_r = 0.5,
                            verbose=FALSE)
        end_time <- Sys.time() 
        duration <- end_time - start_time
        print(paste0("Run EE. Run time: ", duration))
    }    
    stanobj
}

fit_Merkle_model <- function(df_market, 
                         df_e_model,
                         df_x_model,
                         model_obj,
                         N, J, nT_hi, H, nchains, warmup){
    ## 1 Prepare data ----
    
    #historical predictions
    df_e_past <- df_e_model %>% 
        filter(t <= nT_hi)
    # predictions for the next 4 quarters
    df_e_next <- df_e_model %>% 
        filter(t == H+1)
    
    # Extract the observed values as data frame
    (df_ind_e_obs <- df_e_model %>% filter(!is.na(Prediction)) %>% select(ind, Analyst_id, Company_id, Prediction) %>% 
            filter(ind <= N*nT_hi | ind > (N*nT_hi+N*3))
    )
    
    # Extract the missing values as data
    (df_ind_e_mis <- df_e_model %>% filter(is.na(Prediction)) %>% select(ind, Analyst_id, Company_id) %>% 
            filter(ind <= N*nT_hi | ind > (N*nT_hi+N*3))
    )
    
    
    # 2. Settings ----
    # Set your directory for reading and saving files if relevant
    # dir_files <- ""
    
    
    ncores <- nchains
    thin = 1
    
    # Test first with much less iterations
    iter = 2*warmup
    
    r <- nchains *(iter-warmup)/thin
    
    
    dat_stan_Me <- list(
        #1. Sample size information
        n_t = nT_hi, # Number of realized return time points
        N = N, # Number of stocks assets
        J = J, # // Number of experts
        n_X = N*nT_hi, #// Number of stock return realizations
        n_M_obs = nrow(df_ind_e_obs),# // Number of experts forecasts
        n_M_mis = N*J*(nT_hi+1)-nrow(df_ind_e_obs), #nrow(df_ind_e_mis), # // No of missing forecasts
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
        mean_rp0 = prior_list$mean_rp0,
        sd_rp0 = prior_list$sd_rp0,
        scale_psi0 = prior_list$scale_psi0, 
        scale_psi_star = prior_list$scale_psi_star,
        scale_omega = prior_list$scale_omega, 
        alpha_nu0 = prior_list$alpha_nu0,
        beta_nu0 = prior_list$beta_nu0,
        alpha_nu = prior_list$alpha_nu,
        beta_nu = prior_list$beta_nu,
        ## 4.2. Expert parameters
        kappa = prior_list$kappa_me, # average bias
        xi = prior_list$xi, # bias variation
        phi_star = prior_list$phi_star_me, #mean responisveness
        tau = prior_list$tau_me, # sd of responsiveness
        iota = prior_list$iota_me, # average inaccuracy
        ## 5. Settings
        use_likelihood = prior_list$use_likelihood # 0 if testing model based only on priors
    )
    
    # Run iterations
    {
        start_time <- Sys.time()
        stanobj <- sampling(model_obj, data = dat_stan_Me,
                            iter = iter, chains = nchains, thin = thin, 
                            cores = ncores, warmup = warmup, init_r = 0.5,
                            verbose=FALSE)
        end_time <- Sys.time() 
        duration <- end_time - start_time
        print(paste0("Run Merkle. Run time: ", duration))
    }    
    stanobj
}

# This function allows only one X to be included in order to get positive growth
# this version constrains itself
objective_Eg_industry_N_const <- function(w, eps = 0.02){
    #only one weights can be other than zero
    w_grid <- expand.grid(w1 = w, w2 = w)
    zerocheck <- w_grid %>% apply(1,prod)
    zerocheck2 <- max(zerocheck[-which.max(zerocheck)]) #only multiplyed the biggest by itself is OK
    c1 <- zerocheck2 - eps
    c2 <- sum(w)-1
    if((c1 > 0)|(c2 > 0)) 
        return(-10)
    
    exp_x_matrix <- opt_dat_df %>% 
        select(-Scenario, -Index) %>% 
        mutate(across(everything(), exp)) %>% 
        as.matrix()
    if(ncol(exp_x_matrix) != length(w)) stop("Wrong size w")
    #weighted <- sweep(x_matrix, MARGIN=2, w, `*`)
    weighted <- exp_x_matrix[,w!=0]*w[w!=0]
    index_w <- max((1-sum(w)),0) #shorting index not possible
    exp_index_vec <- (opt_dat_df %>% pull(Index) %>% exp())*index_w
    
    G_vec <- cbind(weighted, exp_index_vec) %>% apply(1,sum)
    return(mean(log(G_vec)))
} 

objective_Eg_industry1 <- function(w){
    opt_dat_df%>%
        mutate(G = w*exp(X)+(1-w)*exp(Index)) %>% 
        mutate(G = ifelse(G > 0, G, 10^-20)) %>% #log(0) = -infty
        mutate(g = log(G)) %>% 
        summarize(Eg = mean(g)) %>% 
        pull(Eg)
} 

objective_Sharpe_industry1 <- function(w){
    opt_dat_df%>%
        mutate(G = w*exp(X)+(1-w)*exp(Index)) %>% 
        mutate(ret = G-1) %>% 
        summarise(mu = mean(ret-0), sigma = sd(ret)) %>% #rf=0.001 31.12.2020
        mutate(SR = mu/sigma) %>% 
        pull(SR)
} 

objective_U3_industry1 <- function(w){
    opt_dat_df %>%
        mutate(G = w*exp(X)+(1-w)*exp(Index)) %>% 
        mutate(ret = G-1) %>% 
        summarise(mu = mean(ret), sigma2 = var(ret)) %>% 
        mutate(U = mu-3/2*sigma2) %>% 
        pull(U)
} 


optimize_decision <- function(df_posterior_div, df_market, N, H, two_step = F, model_vec = c("SB", "EE"), obj = "KELLY" ){
    require("DEoptim")
    opt_df <- tibble(Method = NULL, Asset = NULL, w_opt = NULL, opt_val = NULL, convg = NULL)
    
    for(method in model_vec){
        if(two_step){
            
            de_f <- function(w) -objective_Eg_industry_N_const(w, eps = 0.01)
            n_assets = N
            
            print(paste("Method", method))
            
            opt_dat_df <<- df_posterior_div %>% 
                filter(Method == method) %>% 
                pivot_wider(id_cols = Scenario, names_from = "Asset", values_from = "y") %>% 
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
                opt_dat_df <<- df_posterior_div %>% 
                    filter(Method == method) %>% 
                    pivot_wider(id_cols = Scenario, names_from = "Asset", values_from = "y") %>% 
                    rename(Index = "Asset_0", X = paste0("Asset_",i))
                
                if(obj == "KELLY")
                lb_sol <- optim(par = 0.5, fn = function(w){-objective_Eg_industry1(w)},
                                lower = 0, upper = 1, method = "L-BFGS-B")
                if(obj == "SHARPE")
                    lb_sol <- optim(par = 0.5, fn = function(w){-objective_Sharpe_industry1(w)},
                                    lower = 0, upper = 1, method = "L-BFGS-B")
                if(obj == "U3")
                    lb_sol <- optim(par = 0.5, fn = function(w){-objective_U3_industry1(w)},
                                    lower = 0, upper = 1, method = "L-BFGS-B")
                
                opt_df <- opt_df %>% 
                    bind_rows(
                        tibble(Method = method, Asset = i, 
                               w_opt = lb_sol$par, opt_val = -lb_sol$value, convg = lb_sol$convergence)
                    )
            }
    }
    
    opt_df
}

# function for calculating mean/median differences between portfolio growth and index growth 
## for a single optimization solutions
# row = one row from optimization result (selected stock and its weight)
calc_g_dif <- function(row, df_posterior_wider){
    if(nrow(row) != 1) return("Number of rows has to be 1")
    row %>% left_join(
        df_posterior_wider, multiple = "all"
    ) %>% 
        mutate(G_opt = w_opt*exp(y)+(1-w_opt)*exp(y_index),
               G_ind = exp(y_index)) %>% 
        mutate(G_dif = G_opt-G_ind) %>% 
        mutate(g_dif = log(G_opt)-log(G_ind)) %>% 
        group_by(Industry_id, Method) %>% 
        summarize(med_g_dif = median(g_dif), med_G_dif = median(G_dif), 
                  mean_g_dif = mean(g_dif), mean_G_dif = mean(G_dif),
                  .groups = "drop")
}

# Function for filtering a row and pushing it to the function above
calc_g_dif_all <- function(i,j, opt_df, df_posterior_div) {
    
    # Index return as own column
    
    df_posterior_div_0 <- df_posterior_div %>% 
        filter(Asset == "Asset_0") %>% 
        select(-Asset) %>%
        select(Scenario, Industry_id, Method, y_index = y)
    
    df_posterior_wider <- df_posterior_div %>% 
        filter(Asset != "Asset_0") %>% 
        left_join(
            df_posterior_div_0
        ) %>% 
        mutate(Asset = as.integer(str_sub(Asset, 7)))
    
    opt_df %>% 
        filter(Industry_id == i,  Method == j) %>% 
        calc_g_dif(df_posterior_wider)
}
