# function to simulate predicted values not using  random effects
simulate_pred_no_re = function(model, data, contrast, nsim = 1000) {
    
    t = data.table()
    # simulate posterior distribution
    sim_data <- inla.posterior.sample(n=nsim, result=model)
    sim_names <- rownames(sim_data[[1]]$latent)
    beta_names <- rownames(model$summary.fixed) # get name of fixed effects
    
    # names dataset 
    names_data = names(data)
    
    # get random effects
    re = rownames(bri.hyperpar.summary(model))
    re= gsub('SD for |SD for the Gaussian observations', '',  re)
    re = re[nchar(re)>0]
    
    # filtering random effects
    regex = "^Pred|^obs"
    

    if (length(re)>0) {
            for (i in re) {
              regex = paste0(regex, paste0('|^', i))
            } 
    }
    
    # data matrix
    vars_to_omit = unique(c(re,  names_data[!names_data %in% sim_names]))
    f = formula(paste0('~ ', paste0(names(suppressWarnings(data[, !vars_to_omit, with=FALSE])), collapse = ' + ')))
    X = model.matrix(f, dat = suppressWarnings(data[, !vars_to_omit, with=FALSE]))
    N = nrow(data)
    
    # loop over simulation
    for (i in 1:nsim) {
        
        # fixed effects
        betas = sim_data[[i]]$latent[-grep(regex, sim_names)]
        names(betas) = beta_names 
        mt = matrix(rep(betas, N), ncol = length(beta_names), nrow = N, byrow = TRUE)
        Ysim = matrix(nrow = N, ncol = 1)
        for (j in 1:N) {
            Ysim[j, 1] <- X[j, ] %*% mt[j,]
        }
        
        s = data.table(Ysim)
        setnames(s, 'pred')
        s[, sim := i]
        new_vars = vars_to_omit[vars_to_omit %in% names_data]
        s = cbind(s, data[, c(contrast, new_vars), with=FALSE])
        t = rbind(t, s)
     }
    return(t)
}