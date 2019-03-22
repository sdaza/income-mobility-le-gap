# texreg extract function for INLA
extract.inla <- function(model, include.dic = FALSE,
    include.waic = FALSE, ...) {

fixed <-  model$summary.fixed
contrib <- bri.hyperpar.summary(model)

coefnames <- c(rownames(fixed), rownames(contrib))
coef <- c(fixed[, "mean"], contrib[, "mean"])
ci.low <- c(fixed[, "0.025quant"], contrib[, "q0.025"])
ci.up <- c(fixed[, "0.975quant"], contrib[, "q0.975"])

gof <- numeric()
gof.names <- character()
gof.decimal <- logical()
  if (include.dic == TRUE) {
    gof <- c(gof, model$dic$dic)
    gof.names <- c(gof.names, "DIC")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.waic == TRUE) {
    gof <- c(gof, model$waic$waic)
    gof.names <- c(gof.names, "WAIC")
    gof.decimal <- c(gof.decimal, FALSE)
  }

tr <- createTexreg(
    coef.names = coefnames,
    coef = coef,
    ci.low = ci.low,
    ci.up = ci.up,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )

}

setMethod("extract", signature = className("inla", "inla"),
    definition = extract.inla)