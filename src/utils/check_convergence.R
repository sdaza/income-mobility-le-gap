###################################
# simple function to check convergence stan models
# author: sebastian daza
# #################################

check_convergence = function(model) {
    s = summary(model)
    fixed = s$fixed[,'Rhat']
    random = do.call(rbind,s$random)[, 'Rhat']
    restimates = c(fixed, random)
    check = (restimates >= 1.05 | restimates <= 0.90)
    if (sum(check) > 0) {
        stop('::::::::: Please, check model convergence!')
    }
}
