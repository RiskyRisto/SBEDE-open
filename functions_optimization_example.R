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

