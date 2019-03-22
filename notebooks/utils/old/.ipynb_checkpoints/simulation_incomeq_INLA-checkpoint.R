# function to simulate predicted values

simulate_predictions = function(model, data, nsim = 1000, contrast='z_relative_mob', 
                               random = 'q_mob') {
    
    # simulate posterior distribution
    simData <- inla.posterior.sample(n=nsim, result=model)
    
    # define matrix model
    f = formula(paste0('~ ', paste0(names(data), collapse = ' + ')))
    X = model.matrix(f, dat = data)
    N = nrow(data)
    Ysim = matrix(nrow = N, ncol = nsim) 
    
    # loop
    for (i in 1:nsim) {
        rnames <- rownames(simData[[i]]$latent)
        rndQinc <- simData[[i]]$latent[grep('income_qr', rnames)]
        rndIncCoef <- simData[[i]]$latent[grep(random, rnames)]
        coefNames <- rownames(model$summary.fixed)
        Betas = simData[[i]]$latent[-grep("^Pred|^state|^cty|^obs|^income_qr|q_mob|q_gini", rnames)]
        names(Betas) = coefNames
        mt = matrix(rep(Betas, N), ncol = length(coefNames), nrow = N, byrow = TRUE)
        indc = grep(contrast, coefNames)
        indi = grep('Intercept', coefNames)
        mt[, indc] = mt[,indc] + rep(rndIncCoef, 2)
        # mt[,indi] = mt[,indi] + rep(rndQinc, 2)
        for (j in 1:N) {
            Ysim[j, i] <- X[j, ] %*% mt[j,]
        }
    }
    
    t = data.table(Ysim)
    t[, q := rep(1:4, times = 2)]
    t[, (contrast) := rep(c(0.0, 1.0), times = 1 , each = 4)]
    return(melt(t, id.vars = c('q', contrast)))
}