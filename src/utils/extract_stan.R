# texreg function for brms
library(texreg)

extract.brms = function(model, include.r2 = TRUE, include.loo = FALSE, ...) {

  s = summary(model)

  # fixed
  coefficient.names = names(s$fixed[,1])
  coefficients = s$fixed[,1]
  ci.low = s$fixed[, "l-95% CI"]
  ci.upper = s$fixed[, "u-95% CI"]


  # random
  if ('random' %in% names(s)) {
   r = s$random[[1]]
   random.names = stringr::str_replace_all(rownames(r), stringr::fixed('\\_'), "\\_")
   random.estimates = r[,1]
   random.lower = r[,'l-95% CI']
   random.upper = r[, 'u-95% CI']

   coefficient.names = c(coefficient.names, random.names)
   coefficients = c(coefficients, random.estimates)
   ci.low = c(ci.low, random.lower)
   ci.upper = c(ci.upper, random.upper)
  }

  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()

  gof = c(gof, s$nobs)
  gof.names <- c(gof.names, "Num.\ obs.")
  gof.decimal <- c(gof.decimal, FALSE)

  if ('ngrps' %in% names(s)) {
    for (i in seq_along(s$ngrps)) {
          gof = c(gof, s$ngrps[[i]])
          gof.names <- c(gof.names, paste('Num.\ obs. ',
            stringr::str_replace_all(names(s$ngrps[i]), stringr::fixed('_'), '\\_')))
          gof.decimal <- c(gof.decimal, FALSE)
    }
  }

  if (include.loo == TRUE) {
    loo = loo::loo(model, reloo=TRUE)
    gof <- c(gof, loo$estimates[3,1])
    gof.names <- c(gof.names, "LOO Information Criterion")
    gof.decimal <- c(gof.decimal, FALSE)
  }

  if (include.r2 == TRUE) {
    r2 = bayes_R2(model)
    gof <- c(gof, round(r2[1, 1], 2))
    gof.names <- c(gof.names, "Bayes $R^2$")
    gof.decimal <- c(gof.decimal, TRUE)
  }


  tr = createTexreg(
      coef.names = coefficient.names,
      coef = coefficients,
      ci.low = ci.low,
      ci.up = ci.upper,
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
  )
  return(tr)
}

setMethod("extract", signature = className("brmsfit", "brms"),
    definition = extract.brms)