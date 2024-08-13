library(tidyverse)
library(readxl)
library(writexl)


source("Simulation_study/functions_simulator.R")

# 1 Settings ----
## Set Scenarios 
df_scen <- tibble(nT_hi = rep(c(20,40), each = 2), J = rep(c(4,8), 2)) %>% 
    mutate(scen_id = row_number())

## Folders for data and results
dir_data_save <- "Simulation_study/Simulated_data/"
dir_res_save <- "Simulation_study/Sim_forecasts/"
dir_perf_save <- "Simulation_study/Performance_results/"


Puhti <- FALSE # This is related to outsourced computing in CSC
if(Puhti){
    dir_data_save <- "Simulation_study/Puhti_outputs/Simulated_data/"
    dir_res_save <- "Simulation_study/Puhti_outputs/Sim_forecasts/"
    dir_perf_save <- "Simulation_study/Puhti_outputs/Performance_results/"
}

# Fixed settings
N = 5
sets <- 1:10 # 1:500 in the real study
object_vec <- c("KELLY", "SHARPE", "U3")

# Assuming zero dividends
df_dividends <- tibble(Company_id = 0:N, Div_pr = 0) %>% 
    mutate(Company = paste0("Company_",Company_id))

# 2 Loop predictions and with performance metrics ----
for(sce in df_scen$scen_id){ 
    # Settings based on scenario
    nT_hi <- df_scen %>% filter(scen_id == sce) %>% pull(nT_hi)
    J <- df_scen %>% filter(scen_id == sce) %>% pull(J)
    H = nT_hi+3
    
    # Tables for collecting information in loop
    df_real_now_all <- NULL # realizations
    post_q_tbl <- NULL # quartiles of the realizations in posterior distribution
    raw_est_tbl_all <- NULL # raw consensus estimates
    jasp_est_tbl_all <- NULL # Jaspersen predictions
    
    for(set in sets){
    # Read posteriors and simulated data
    df_posterior <- readRDS(file = paste0(dir_res_save,"posterior_N",N,"_J",J,"_nT",nT_hi,"_set",set,".rds"))
    df_market <- read_excel(paste0(dir_data_save,"sim_N",N,"_J",J,"_nT",nT_hi,"_set",set,".xlsx"), 
                            sheet = "df_market")
    ## 2.1. Combine with dividends ----
    # Assuming zero dividends
    df_dividends <- tibble(Company_id = 0:N, Div_pr = 0) %>% 
        mutate(Company = paste0("Company_",Company_id))
    
    df_posterior_div_set <- df_posterior %>% 
        left_join(
            df_dividends %>% 
                mutate(Asset = paste0("x_next[",Company_id,"]")) %>% 
                select(Div_pr_est = Div_pr, Asset)
            
        ) %>%
        rename(x = value) %>% 
        mutate(x_exp = exp(x), y_exp = x_exp + Div_pr_est, y = log(y_exp)) %>% 
        mutate(Asset = str_replace(Asset, "x_next\\[", "Asset_") %>% str_replace("\\]","")) %>% 
        mutate(Industry_id = set)
    
    ## 2.2. Combining predictions and Realizations for the decision period----
    df_market <- read_excel(paste0(dir_data_save,"sim_N",N,"_J",J,"_nT",nT_hi,"_set",set,".xlsx"), 
                            sheet = "df_market")
    
    ### 2.2.1 Real returns and dividends
    df_now = df_market %>% filter(t_now == H) %>% select(starts_with("Company"), Industry_return) %>% 
        pivot_longer(everything(), values_to = "Return", names_to = "Company") %>% 
        mutate(Company = as.numeric(str_sub(Company,9))) %>% 
        mutate(Company = replace_na(Company, 0))
    
    df_real_now <- df_now %>% distinct(Company, Return) %>% 
        left_join(
            #df_dividends_run %>% filter(Date == as.Date(T_now), Industry_id == i) %>% 
            # Assuming zero dividends
            df_dividends %>% 
                select(Div_pr, Company = Company_id)
        ) %>% 
        # exp = e^x , E(x) = expectations
        mutate(x_exp_real = exp(Return), y_exp_real = x_exp_real + Div_pr, y_real = log(y_exp_real)) %>% 
        arrange(Company) %>% 
        mutate(Asset = paste0("Asset_",Company)) %>% 
        rename(x_real = Return)
    # Collect values to the table
    df_real_now_all <- df_real_now_all %>% bind_rows(df_real_now %>% mutate(Industry_id = set))
    
    ### 2.2.2. Point forecasts ----
    df_predictions <- read_excel(paste0(dir_data_save,"sim_N",N,"_J",J,"_nT",nT_hi,"_set",set,".xlsx"), 
                                 sheet = "df_predictions")
    
    
    df_pred_x_hist <- df_predictions %>% 
        pivot_longer(starts_with("Expert"), names_to = "Expert", values_to = "M") %>% 
        mutate(Expert_id = as.numeric(str_sub(Expert, 8))) %>% 
        left_join(
            df_market %>% 
                pivot_longer(starts_with("Company"), names_to = "Company", values_to = "x") %>% 
                mutate(Company_id = as.numeric(str_sub(Company, 9))) %>% 
                select(t, Company_id, x)
        ) %>% 
        filter(t <= nT_hi)
    
    # Jaspersen forecasts, not used in the real study
    # Delta_iy <- df_pred_x_hist %>% 
    #     group_by(Expert) %>% 
    #     summarise(cor = cor(M,x)) %>% 
    #     arrange(Expert) %>% 
    #     pull(cor) %>% as.matrix()
    # 
    # Delta_ij <- df_predictions %>% filter(t <= nT_hi) %>% 
    #     select(starts_with("Expert")) %>% cov()
    # 
    # w_s <- solve(Delta_ij)%*%Delta_iy
    # 
    # #w_s_std <- w_s/sum(w_s)
    # 
    # pred_now_mat <- df_predictions %>% filter(t_now == H) %>% 
    #     select(starts_with("Expert")) %>% as.matrix()
    # # sw_est = sophisticated weight estimator (see Jaspersen (2022))
    # jasp_est_tbl <- tibble(sw_est = (pred_now_mat%*%w_s)[,1]) %>%  
    #     mutate(Company = row_number()) %>% 
    #     mutate(Name = paste0("Asset_",Company))
    # 
    # jasp_est_tbl_all <- jasp_est_tbl_all %>% 
    #     bind_rows(
    #         jasp_est_tbl %>% select(Company, sw_est) %>% 
    #             mutate(Method = "Jaspersen", Industry_id = set)
    #     )
    
    ### 2.2.3. Raw estimates (equal weight) ----
    df_pred_now = df_predictions %>% filter(t_now == H) %>% 
        select(Company = Company_id, starts_with("Expert")) %>% 
        pivot_longer(-Company, values_to = "Target_return", names_to = "Expert")
    
    raw_est_tbl <- 
        df_pred_now %>% 
        group_by(Company) %>% 
        summarise(mean_tr = mean(Target_return, na.rm = T), med_tr = median(Target_return, na.rm = T), .groups = "drop") %>% 
        mutate(Name = paste0("Asset_",Company))
    
    raw_est_tbl
    
    raw_est_tbl_all <- raw_est_tbl_all %>% 
        bind_rows(
            raw_est_tbl %>% select(Company, mean = mean_tr, med = med_tr) %>% 
                mutate(Method = "Raw", Industry_id = set)
        )
    
    ### 2.2.3. Information fo coverage probabilities ----
    # Posterior for one set/industry with binary information if iteration is lower than the real return
    df_post_indu <- df_posterior_div_set %>% select(Scenario, Asset, Industry_id, Method, x) %>% 
        left_join(
            df_real_now %>% select(Company, x_real, Asset) # y_real out for saving memory
        ) %>% 
        mutate(is_lower = (x <= x_real))
    
    # Calculation proportion of underestimated iterations and posterior means/medians
    post_q_tbl <- post_q_tbl %>% bind_rows(
        df_post_indu %>% group_by(Method, Industry_id, Company) %>% 
            summarize(q = mean(is_lower), mean = mean(x), med = median(x), sd = sd(x), .groups = "drop")
    )
    } # end of a set loop
    
    # average posterior stds
    #post_q_tbl %>% group_by(Method) %>% 
    #    summarize(mean_sd = mean(sd), med_sd = median(sd))
    
    ## 2.3. Calculating coverage probabilities ----
    inc_tbl <- post_q_tbl %>%
        filter(Company != 0) %>% 
        mutate(inc_50 = 1*((q > 0.25) & (q <= 0.75)),
               inc_68 = 1*((q > 0.16) & (q <= 0.84)),
               inc_90 = 1*((q > 0.05) & (q <= 0.95)),
               inc_95 = 1*((q > 0.025) & (q <= 0.975))
        )
    inc_tbl
    
    inc_summary <- inc_tbl %>% group_by(Method) %>% 
        summarize(across(starts_with("inc"), .fns = ~mean(.x)))
    
    ## 2.4. Forecasting accuracy comparison with point forecasts ----
    
    df_forecast_with_real <- post_q_tbl %>% 
        filter(Company != 0) %>% 
        select(Method, Industry_id, Company, med, mean) %>% 
        bind_rows( raw_est_tbl_all) %>% 
        #bind_rows( jasp_est_tbl_all %>% rename(mean = sw_est)) %>% 
        arrange(Industry_id, Company, Method) %>% 
        left_join(df_real_now_all %>% select(-Asset))
    
    df_0_forecast_with_real <-  df_forecast_with_real %>% 
        left_join(
            df_forecast_with_real %>% 
                select(Method, Industry_id, Company, mean, med, x_real) %>% 
                group_by(Method, Industry_id) %>% 
                summarize(mean_0 = mean(mean), mean_0_med = mean(med), x_real_0 = mean(x_real)) %>% 
                ungroup()
        ) %>% 
        mutate(mean_prem = mean - mean_0, med_prem = med - mean_0_med, x_real_prem = x_real - x_real_0)
    
    
    point_est_summary <-  df_forecast_with_real %>% 
        group_by(Method) %>% 
        summarize(
            #MAE = mean(abs(mean-x_real)),
            MAE_med = mean(abs(med-x_real)),
            Bias = mean(mean-x_real)
        )
    
    
    # MAE for premium returns
    
    prem_est_summary <- df_0_forecast_with_real %>% 
        group_by(Method) %>% 
        summarize(
            #MAE = mean(abs(mean_prem-x_real_prem)), 
            MAE_med = mean(abs(med_prem-x_real_prem))
        )
    
    write_xlsx(list(
        df_real_now_all = df_real_now_all,
        point_est_summary = point_est_summary,
        prem_est_summary = prem_est_summary,
        inc_summary = inc_summary), 
               path = paste0(dir_perf_save,"forecastperf_N",N,"_J",J,"_nT",nT_hi,".xlsx"))
    
}


# 3. Looping optimization with performance metrics ----
model_vec <- c("SB", "EE", "ME") # SBEDE, EE and Merkle
    obj <- "U3" #Repeat with followings: "KELLY", "SHARPE", "U3"
        for(sce in df_scen$scen_id){
            nT_hi <- df_scen %>% filter(scen_id == sce) %>% pull(nT_hi)
            J <- df_scen %>% filter(scen_id == sce) %>% pull(J)
            H = nT_hi+3
             
            ## 3.1. Optimize ----
            two_step <- F # Faster if False
            
            opt_df <- NULL
            for(set in sets){
                df_market <- read_excel(paste0(dir_data_save,"sim_N",N,"_J",J,"_nT",nT_hi,"_set",set,".xlsx"), 
                                        sheet = "df_market")
                df_posterior <- readRDS(file = paste0(dir_res_save,"posterior_N",N,"_J",J,"_nT",nT_hi,"_set",set,".rds"))
                df_posterior_div_set <- df_posterior %>% 
                    left_join(
                        df_dividends %>% 
                            mutate(Asset = paste0("x_next[",Company_id,"]")) %>% 
                            select(Div_pr_est = Div_pr, Asset)
                        
                    ) %>%
                    rename(x = value) %>% 
                    mutate(x_exp = exp(x), y_exp = x_exp + Div_pr_est, y = log(y_exp)) %>% 
                    mutate(Asset = str_replace(Asset, "x_next\\[", "Asset_") %>% str_replace("\\]","")) %>% 
                    mutate(Industry_id = set)
                
                opt_df_init <- optimize_decision(df_posterior_div = df_posterior_div_set, df_market, N,H, two_step, model_vec = model_vec, obj) %>% 
                    mutate(Industry_id = set)
                
                opt_df <- opt_df %>% bind_rows(opt_df_init)
            }
            
            
            if(!two_step){
                opt_df_best <- opt_df %>% arrange(Industry_id, Method, desc(opt_val)) %>% 
                    distinct(Industry_id, Method, .keep_all = TRUE) 
                opt_df_all <- opt_df
            }
            if(two_step) opt_df_best2 <- opt_df %>% arrange(Industry_id, Method)
            
            # # If both methods run, check that both methods gave the same results
            # opt_df_best %>% left_join(
            #     opt_df_best2 %>%
            #         rename(Asset2 = Asset, w_opt2 = w_opt, opt_val2=opt_val, convg2 = convg)
            #     ) %>% 
            #     mutate(across(where(is.double),round, digits = 3)) %>% 
            #     mutate(SAME = 1*(Asset == Asset2)) %>% 
            #     as.data.frame()
            
            
            # Select opt_df_best or opt_df_best2
            if(two_step) opt_df_best_select <- opt_df_best2
            if(!two_step) opt_df_best_select <- opt_df_best
            
            ## 3.2. Expectations for beating the index ----
            df_E_ind <- NULL
            df_E_dif <- NULL
            for(set in sets){
                df_posterior_div_set <- readRDS(file = paste0(dir_res_save,"posterior_N",N,"_J",J,"_nT",nT_hi,"_set",set,".rds")) %>%
                    left_join(
                        df_dividends %>% 
                            mutate(Asset = paste0("x_next[",Company_id,"]")) %>% 
                            select(Div_pr_est = Div_pr, Asset)
                        
                    ) %>%
                    rename(x = value) %>% 
                    mutate(x_exp = exp(x), y_exp = x_exp + Div_pr_est, y = log(y_exp)) %>% 
                    mutate(Asset = str_replace(Asset, "x_next\\[", "Asset_") %>% str_replace("\\]","")) %>% 
                    mutate(Industry_id = set) %>% select(Scenario, Asset, Industry_id, Method, y)
                
                
                # Expectations for the index
                df_E_ind <- df_E_ind %>% bind_rows(
                    df_posterior_div_set %>% filter(Asset == "Asset_0") %>% 
                    mutate(G = exp(y)) %>% #capital growth rate
                    mutate(g = log(G)) %>% # logarithmic return
                    group_by(Method, Industry_id) %>% 
                    summarize(Eg = mean(g), EG = mean(G), .groups = "drop") 
                )
                
                
                
                # Expected premium growths
                
                for(j in model_vec){
                df_E_dif <- df_E_dif %>% bind_rows(
                    calc_g_dif_all(set,j,opt_df_best_select, 
                                          df_posterior_div_set %>%
                                              filter(Method == j) 
                                          )
                )
                }
            }
            df_E_dif <- df_E_dif %>% arrange(Industry_id, Method)
            
            
            #Summarize by mean/med
            tbl_E_dif <- df_E_dif %>% group_by(Method) %>% 
                summarize(mean_E_Prem_g = mean(mean_g_dif), mean_E_Prem_G = mean(mean_G_dif),
                          med_E_Prem_g = median(med_g_dif), med_E_Prem_G = median(med_G_dif))
            
            tbl_E_dif
            
            
            ## 3.3. Decision Performance for each object----
            df_real_now_all  <- read_excel(path = paste0(dir_perf_save,"forecastperf_N",N,"_J",J,"_nT",nT_hi,".xlsx"),
                                             sheet = "df_real_now_all")
            
            decision_tbl <- opt_df_best_select %>% 
                left_join(df_real_now_all %>% select(-Asset), 
                          by = c("Industry_id" = "Industry_id", "Asset" = "Company")) %>% 
                left_join(
                    df_real_now_all %>% select(-Asset) %>% filter(Company == 0) %>% 
                        select(Industry_id, x_real0 = x_real, y_real0 = y_real)
                ) %>% 
                bind_rows(
                    df_real_now_all %>% select(-Asset) %>% filter(Company == 0) %>% 
                        rename(Asset = Company) %>% 
                        mutate(Method = "Index", w_opt = 1, x_real0 = x_real, y_real0 = y_real) %>% 
                        left_join(
                            df_E_ind %>% select(Method, Industry_id, opt_val = Eg)
                        )
                ) %>% 
                arrange(Industry_id, Method) %>% 
                mutate(G = w_opt*exp(y_real) + (1-w_opt)*exp(y_real0))
            
            decision_tbl 
            
            
            # Performance summary, capital growth and logarithmic returns, Sharpe and Utility
            tbl_perf <- decision_tbl %>% 
                group_by(Method) %>% 
                summarize(meanG = mean(G), medG = median(G), mean_ret = mean(G-1), sd_ret = sd(G-1)) %>% 
                mutate(exp_mean_ret_pr = exp(mean_ret), Sharpe = mean_ret/sd_ret, U3 = mean_ret-3/2*sd_ret^2)
            
            
            ## 3.8. Optimizers curse ----
            oc_tbl <- decision_tbl %>% filter(Method != "Index") %>% 
                left_join(
                    df_E_ind %>% 
                        select(Method,Industry_id, ind_Eg = Eg)
                ) %>% 
                mutate(E_ret_dif = opt_val - ind_Eg, Prem_ret = log(G)-y_real0) %>%
                mutate(E_G_dif = exp(opt_val) - exp(ind_Eg), Prem_G = G-exp(y_real0)) %>%
                mutate(OC_G = Prem_G-E_G_dif) %>% # OC for individual industry, not used
                mutate(OC_g = Prem_ret-E_ret_dif) %>% # OC for individual industry, not used
                group_by(Method) %>% 
                summarize(
                    mean_E_ret = mean(opt_val),
                    mean_E_G = mean(exp(opt_val)),
                    mean_real_ret = mean(log(G)),
                    mean_real_G = mean(G),
                    mean_E_ret_index = mean(ind_Eg),
                    mean_real_ret_index = mean(y_real0),
                    mean_real_G_index = mean(exp(y_real0)),
                    mean_OC_g = mean(OC_g),
                    mean_OC_G = mean(OC_G))
            
            ## 3.8. Save results
            
            write_xlsx(list( decision_tbl = decision_tbl,
                             tbl_perf = tbl_perf,
                             oc_tbl = oc_tbl), path = paste0(dir_perf_save,"performance_N",N,"_J",J,"_nT",nT_hi,"_obj",obj,".xlsx"))
        }
    
    
# 4 Report results ----
table1_all <- NULL
table1_2_all <- NULL
table2_all <- NULL
table3_all <- NULL
decision_tbl_all <- NULL


for(sce in df_scen$scen_id){
    #sce <- 1
    nT_hi <- df_scen %>% filter(scen_id == sce) %>% pull(nT_hi)
    J <- df_scen %>% filter(scen_id == sce) %>% pull(J)
    H = nT_hi+3


    ## 4.1. Forecasting ----
    point_est_summary  <- read_excel(path = paste0(dir_perf_save,"forecastperf_N",N,"_J",J,"_nT",nT_hi,".xlsx"),
                                     sheet = "point_est_summary")
    prem_est_summary  <- read_excel(path = paste0(dir_perf_save,"forecastperf_N",N,"_J",J,"_nT",nT_hi,".xlsx"),
                                     sheet = "prem_est_summary")
    
    inc_summary <- read_excel(path = paste0(dir_perf_save,"forecastperf_N",N,"_J",J,"_nT",nT_hi,".xlsx"),
                              sheet = "inc_summary")
    
    
table1 <- point_est_summary %>% left_join(inc_summary) %>% 
    rename(MAE = MAE_med) %>% 
    mutate(Method = factor(str_replace(Method, "SB", "SBEDE") %>% str_replace("ME", "Merkle"),
                           levels=c("SBEDE", "EE", "Merkle", "Raw", "Jaspersen"))) %>% 
    arrange(Method) %>% 
    mutate(J = J, nT = nT_hi)

names(table1) <- names(table1) %>% str_replace("inc_", "CP ")

table1_all <- table1_all %>% bind_rows(table1)

table1_2 <- prem_est_summary %>% left_join(inc_summary) %>% 
    mutate(Method = factor(str_replace(Method, "SB", "SBEDE") %>% str_replace("ME", "Merkle"),
                           levels=c("SBEDE", "EE", "Merkle", "Raw"))) %>% 
    arrange(Method) %>% 
    mutate(J = J, nT = nT_hi)

names(table1_2) <- names(table1_2) %>% str_replace("inc_", "CP ")

table1_2_all <- table1_2_all %>% bind_rows(table1_2)



    ## 4.2. Dcision Performance ----
table2 <- NULL
table3 <- NULL
decision_tbl <- NULL
for(o in object_vec){
tbl_perf <- read_excel(path = paste0(dir_perf_save,"performance_N",N,"_J",J,"_nT",nT_hi,"_obj",o,".xlsx"),
                       sheet = "tbl_perf")
table2 <- table2 %>% bind_rows(
    tbl_perf %>% select(Method, GmeanG = exp_mean_ret_pr, Sharpe, U3) %>%
    mutate(Method = factor(str_replace(Method, "SB", "SBEDE") %>% str_replace("ME", "Merkle"),
                           levels=c("SBEDE", "EE", "Merkle", "Index"))) %>% 
    mutate(across(all_of(c("GmeanG")), function(x) (x-1)*100*100)) %>%  #to basis points
    arrange(Method) %>% 
    mutate(J = J, nT = nT_hi) %>% 
    mutate(Obj = o)
)

oc_tbl <- read_excel(path = paste0(dir_perf_save,"performance_N",N,"_J",J,"_nT",nT_hi,"_obj",o,".xlsx"),
                     sheet = "oc_tbl")
table3 <- table3 %>% bind_rows(
    oc_tbl %>% mutate(#Method,
    "GMean G" = exp(mean_real_ret),
    "GMean G Index" = exp(mean_real_ret_index),
    Premium=`GMean G`-`GMean G Index`,
    "Premium Exp." = exp(mean_E_ret)-exp(mean_E_ret_index),
    OC = Premium-`Premium Exp.`) %>% 
    select(Method, "GMean G", "GMean G Index", "Premium","Premium Exp.", OC) %>% 
    mutate(`GMean G` = ((`GMean G`-1)*100*100) %>% round(0)) %>% 
    mutate(`GMean G Index` = ((`GMean G Index`-1)*100*100) %>% round(0)) %>% 
    mutate(`Premium` = ((`Premium`)*100*100) %>% round(0))  %>% 
    mutate(`Premium Exp.` = ((`Premium Exp.`)*100*100) %>% round(0)) %>% 
    mutate(OC = (OC*100*100) %>% round(0)) %>% 
    mutate(Method = factor(str_replace(Method, "SB", "SBEDE")%>% str_replace("ME", "Merkle"),
                           levels=c("SBEDE", "EE", "Merkle"))) %>% 
    arrange(Method) %>% 
    mutate(J = J, nT = nT_hi) %>% 
    mutate(Obj = o)
    
    #U3 = mean(exp(x)-1) - 3/2*var(exp(x)-1)
    # Sharpe = (mean(exp(x)-1)-r)/sd(exp(x)-1)
)

decision_tbl <- decision_tbl %>% bind_rows(
    read_excel(path = paste0(dir_perf_save,"performance_N",N,"_J",J,"_nT",nT_hi,"_obj",o,".xlsx"),
                           sheet = "decision_tbl") %>% 
    mutate(J = J, nT = nT_hi) %>% 
    mutate(Obj = o)
)
}

table2_all <- table2_all %>% bind_rows(table2)



table3_all <- table3_all %>% bind_rows(table3)


decision_tbl_all <- decision_tbl_all %>% bind_rows(decision_tbl)
}

## 4.3. Save some figures/tables ----
dir_report_figures <- "Simulation_study/Saved_figures/"

fig_mae <- table1_all %>% 
    rename(n_t = nT) %>% 
    #filter(Method!="Jaspersen") %>% 
    ggplot(aes(y = Method, x = MAE)) +
    geom_bar(stat = "identity") +
    #theme_minimal() +
    facet_grid(J ~ n_t, labeller = label_both) +
    xlim(c(0,0.3)) +
    ggtitle(paste0("A. Forecasting accuracy MAE"))

fig_mae
ggsave(paste0(dir_report_figures,"table_MAE_500.eps"),device = "eps", width = 6, height = 6)

fig_mae_prem <- table1_2_all %>% 
    rename(n_t = nT) %>% 
    #filter(Method!="Jaspersen") %>% 
    ggplot(aes(y = Method, x = MAE_med)) +
    geom_bar(stat = "identity") +
    #theme_minimal() +
    facet_grid(J ~ n_t, labeller = label_both) +
    xlim(c(0,0.3)) +
    xlab("MAE Premium") +
    ggtitle(paste0("B. Forecasting accuracy MAE Premium"))
fig_mae_prem

ggsave(paste0(dir_report_figures,"table_MAE_prem_500.eps"), width = 6, height = 6)

CP_table <- table1_all %>% 
    rename(n_t = nT) %>% 
    filter(Method!="Jaspersen", Method!="Raw") %>%
    pivot_longer(starts_with("CP"), names_to = "IV", values_to = "CP") %>%
    mutate(IV = as.numeric(str_sub(IV, 4))) %>% 
    mutate(Interval = as.factor(IV))

CP_table %>% 
    ggplot(aes(x = Interval, y = CP, fill = Method)) +
    geom_hline(yintercept = c(0.5,0.68,0.9,0.95)) +
    #geom_point(stat = "identity",  size = 0.6) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    scale_fill_grey() +
    facet_grid(J ~ n_t) +
    xlab("Interval (%)")

##

library(xtable)

CP1 <- CP_table %>% filter(n_t==20, J == 4) %>% 
    select(Method, Interval, CP) %>% 
    arrange(Interval) %>% 
    pivot_wider(names_from = "Interval", values_from = "CP", names_prefix = "CP ")

print(xtable(CP1, type = "latex", digits = 3), file = paste0(dir_report_figures,"sim_stu_CP1_500.tex"))

CP2 <- CP_table %>% filter(n_t==20, J == 8) %>% 
    select(Method, Interval, CP) %>% 
    arrange(Interval) %>% 
    pivot_wider(names_from = "Interval", values_from = "CP", names_prefix = "CP ")
print(xtable(CP2, type = "latex", digits = 3), file = paste0(dir_report_figures,"sim_stu_CP2_500.tex"))

CP3 <- CP_table %>% filter(n_t==40, J == 4) %>% 
    select(Method, Interval, CP) %>% 
    arrange(Interval) %>% 
    pivot_wider(names_from = "Interval", values_from = "CP", names_prefix = "CP ")
print(xtable(CP3, type = "latex", digits = 3), file = paste0(dir_report_figures,"sim_stu_CP3_500.tex"))

CP4 <- CP_table %>% filter(n_t==40, J == 8) %>% 
    select(Method, Interval, CP) %>% 
    arrange(Interval) %>% 
    pivot_wider(names_from = "Interval", values_from = "CP", names_prefix = "CP ")
print(xtable(CP4, type = "latex", digits = 3), file = paste0(dir_report_figures,"sim_stu_CP4_500.tex"))
    

obj <- "KELLY"
p2_1 <- table2_all %>% 
    rename(n_t = nT) %>% 
    filter(Obj == obj) %>% 
    mutate(GmeanG_pr = GmeanG/100) %>% 
    ggplot(aes(y = Method, x = GmeanG_pr)) +
    geom_bar(stat = "identity") +
    facet_grid(J ~ n_t, labeller = label_both) +
    ggtitle(paste0("A. Objective: ", obj))

obj <- "SHARPE"
p2_2 <- table2_all %>% 
    rename(n_t = nT) %>% 
    filter(Obj == obj) %>% 
    ggplot(aes(y = Method, x = Sharpe)) +
    geom_bar(stat = "identity") +
    facet_grid(J ~ n_t, labeller = label_both) +
    ggtitle(paste0("B. Objective: ", obj))

obj <- "U3"
p2_3 <- table2_all %>% 
    rename(n_t = nT) %>% 
    filter(Obj == obj) %>% 
    ggplot(aes(y = Method, x = U3)) +
    geom_bar(stat = "identity") +
    facet_grid(J ~ n_t, labeller = label_both) +
    ggtitle(paste0("C. Objective: ", obj))

require(patchwork)

p2_1 | p2_2 | p2_3

ggsave(paste0(dir_report_figures,"performance_fig_500.eps"), width = 13, height = 4)

fig_mae | fig_mae_prem
ggsave(paste0(dir_report_figures,"MAE_fig_500.eps"),  width = 13, height = 4)

# 5. Some additional results ----
#dif to index

premium_tbl <- table2_all %>% filter(Method!="Index") %>% 
    left_join(
        table2_all %>% filter(Method=="Index") %>% select(J, nT, Obj, GmeanGi = GmeanG)
    ) %>% 
    mutate(PremiumG = GmeanG-GmeanGi, PremiumG_pr = GmeanG/100-GmeanGi/100) 

o <- "KELLY"
premium_tbl %>% 
    filter(Obj == o) %>% 
    ggplot(aes(x = Method, y = PremiumG_pr)) +
    geom_bar(stat = "identity") +
    facet_grid(J ~ nT) +
    ggtitle(paste0("Decision objective: ", o))

Gmean_G <- function(x) (exp(mean(log(x/10000+1)))-1)*10000

decision_tbl_all %>% 
    mutate(G = (G-1)*100*100) %>% #to basis points
    mutate(Method = factor(str_replace(Method, "SB", "SBEDE")%>% str_replace("ME", "Merkle"),
                           levels=c("SBEDE", "EE", "Merkle", "Index"))) %>% 
    ggplot(aes(y = G, x = Method)) +
    geom_hline(yintercept = 0) +
    geom_boxplot() +
    #stat_summary(fun=mean, geom='point', shape=1, size=2) +
    stat_summary(fun = Gmean_G, geom = "text", col = "black",     # Add text to plot
                 vjust = 1.9, size = 3,
                 aes(label = paste("GMean:", round(after_stat(y), digits = 0)))) +
    facet_grid(J ~ nT)






  

