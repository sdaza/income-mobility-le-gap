simulate_pred_re = function(model, data, nsim = 1000, contrast='variable', 
                                random_intercept = 'rnd_var') {
    
    t = data.table()
    # simulate posterior distribution
    sim_data <- inla.posterior.sample(n=nsim, result=model)
    sim_names <- rownames(sim_data[[1]]$latent)
    beta_names <- rownames(model$summary.fixed) # get name of fixed effects
    
    # names dataset 
    names_data = names(data)

    regex = "^Pred|^obs"
    
    for (i in random_intercept) {
      regex = paste0(regex, paste0('|^', i))
    }

    # data matrix
    vars_to_omit = unique(c(random_intercept,  names_data[!names_data %in% sim_names]))
    f = formula(paste0('~ ', paste0(names(data[, !vars_to_omit, with=FALSE]), collapse = ' + ')))
    X = model.matrix(f, dat = data[, !vars_to_omit, with=FALSE])
    N = nrow(data)
    

    # loop
    for (i in 1:nsim) {

        # get random values
        for (v in random_intercept) {
          assign('code', as.numeric(gsub(paste0(v, ':'), '', grep(paste0('^', v), sim_names, value=TRUE))))
          assign('values', sim_data[[i]]$latent[grep(v, sim_names)])

          assign(paste0(v, '_rnd'), data.table(code,values))
          setnames(get(paste0(v, '_rnd')), c(v, paste0(v, '_values')))
          assign('temp_data',  merge(data, get(paste0(v, '_rnd')), by=v, all.x=TRUE))
        }
        
        intercept_score = apply(temp_data[, paste0(random_intercept, '_values'), with=FALSE], 1, sum)
        temp_data = NULL
        
        # fixed effects
        betas = sim_data[[i]]$latent[-grep(regex, sim_names)]
        names(betas) = beta_names 
        mt = matrix(rep(betas, N), ncol = length(beta_names), nrow = N, byrow = TRUE)

        # update intercept
        indi = grep('Intercept', beta_names) # get name of intercept
        
        # sum random effects
        mt[,indi] = mt[,indi] + intercept_score

        Ysim = matrix(nrow = N, ncol = 1)
        for (j in 1:N) {
            Ysim[j, 1] <- X[j, ] %*% mt[j,]
        }
        
        s = data.table(Ysim)
        setnames(s, 'pred')
        s[, sim := i]
        s = cbind(s, data[, c(contrast, vars_to_omit), with=FALSE])
        t = rbind(t, s)
    }
    return(t)
}
