
fit_rlm <- function(m) {
  ## get summary
  s <- summary(m)
  ## get sum of square stats
  a <- anova(m)
  r <- m$residuals
  n <- length(r)
  w <- m$weights
  if (is.null(w)) {
    rss <- sum(r^2)
  } else {
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  rdf <- length(m$residuals) - length(m$coefficients)
  resvar <- rss/rdf
  df.int <- attr(m$terms, "intercept")
  ## calculate r square
  mss <- sum(a$`Mean Sq`[-nrow(a)], na.rm = TRUE)
  s$r.squared <- mss / (mss + rss)
  s$adj.r.squared <- 1 - (1 - s$r.squared) * ((n -
      df.int)/rdf)
  ## calculate f stat
  s$fstatistic <- c(mss / (m$rank - df.int) / resvar,
    m$rank - df.int, rdf)
  ## f and its p value
  f <- s$fstatistic[1]
  fp <- do.call("pf", as.list(c(unname(s$fstatistic), lower.tail = FALSE)))
  ## root mean square error
  rmse <- rmse(m)
  ## deviance
  #ll <- -2 * logLik(m)
  #lln <- as.integer(attr(ll, "df")
  # AIC/BIC
  aic <- AIC(m)
  bic <- BIC(m)
  ## stat name and estimate
  fit_statistic <- c("F", "R^2", "Adj R^2", "RMSE", "AIC", "BIC")
  estimate <- c(f, s$r.squared, s$adj.r.squared, rmse, aic, bic)
  ## degrees of freedom
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[c(1)], fit_statistic)] <- c(as.integer(s$fstatistic[2]))
  #n <- nobs(m)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(c("F"), fit_statistic)] <- fp
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_stat = fit_statistic, n, df,
    estimate, p.value, stars)
}

fit_lm <- function(m) {
  if (inherits(m, "aov")) {
    s <- summary.lm(m)
  } else {
    s <- summary(m)
  }
  ## f and its p value
  f <- s$fstatistic[1]
  fp <- do.call("pf", as.list(c(unname(s$fstatistic), lower.tail = FALSE)))
  ## root mean square error
  rmse <- rmse(m)
  ## deviance
  #ll <- -2 * logLik(m)
  #lln <- as.integer(attr(ll, "df")
  # AIC/BIC
  aic <- AIC(m)
  bic <- BIC(m)
  ## stat name and estimate
  fit_statistic <- c("F", "R^2", "Adj R^2", "RMSE", "AIC", "BIC")
  estimate <- c(f, s$r.squared, s$adj.r.squared, rmse, aic, bic)
  ## degrees of freedom
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[c(1)], fit_statistic)] <- c(as.integer(s$fstatistic[2]))
  n <- nobs(m)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(c("F"), fit_statistic)] <- fp
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_stat = fit_statistic, n, df,
    estimate, p.value, stars)
}

fit_glm <- function(m) {
  s <- summary(m)
  devn <- s$df.residual
  devp <- pchisq(s$deviance, devn, lower.tail = FALSE)
  nulln <- s$df.null
  nullp <- pchisq(s$null.deviance, nulln, lower.tail = FALSE)
  chisq <- s$null.deviance - s$deviance
  chisqn <- nulln - devn
  chisqp <- pchisq(chisq, chisqn, lower.tail = FALSE)
  aic <- AIC(m)
  bic <- BIC(m)
  rmse <- rmse(m)
  r2nag <- nagelkerke(m)
  #r2cox <- coxsnell(m)
  r2mcf <- mcfadden(m)
  ##mcfadden.adj(m)
  ## names of fit statistics
  fit_statistic <- c("χ2","Δχ2", "Nagelkerke R^2",
    "McFadden R^2", "RMSE", "AIC", "BIC")
  ## estimates
  estimate <- c(s$deviance, chisq, r2nag, r2mcf, rmse, aic, bic)
  ## degrees of freedom
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[1:2], fit_statistic)] <- c(devn, chisqn)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(fit_statistic[1:2], fit_statistic)] <- c(devp, chisqp)
  ## number of obs
  n <- nobs(m)
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_stat = fit_statistic, n, df,
    estimate, p.value, stars)
}

fit_glmRob <- function(m) {
  s <- summary(m)
  devn <- m$df.residual
  devp <- pchisq(m$deviance, devn, lower.tail = FALSE)
  nulln <- length(m$residuals) - attr(m$terms, "intercept")
  nullp <- pchisq(m$null.deviance, nulln, lower.tail = FALSE)
  chisq <- m$null.deviance - m$deviance
  chisqn <- nulln - devn
  chisqp <- pchisq(chisq, chisqn, lower.tail = FALSE)
  m$aic <- AIC(structure(m, class = "lm"))
  m$bic <- BIC(structure(m, class = "lm"))
  aic <- m$aic
  bic <- m$bic
  rmse <- rmse(m)
  r2nag <- nagelkerke(m)
  #r2cox <- coxsnell(m)
  r2mcf <- mcfadden(m)
  ##mcfadden.adj(m)
  ## names of fit statistics
  fit_statistic <- c("χ2","Δχ2", "Nagelkerke R^2",
    "McFadden R^2", "RMSE", "AIC", "BIC")
  ## estimates
  estimate <- c(s$deviance, chisq, r2nag, r2mcf, rmse, aic, bic)
  ## degrees of freedom
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[1:2], fit_statistic)] <- c(devn, chisqn)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(fit_statistic[1:2], fit_statistic)] <- c(devp, chisqp)
  ## number of obs
  n <- tryCatch(nobs(m), error = function(e) NULL)
  if (is.null(n)) {
    n <- length(m$residuals)
  }
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_stat = fit_statistic, n, df,
    estimate, p.value, stars)
}


rmse <- function(m) {
  x <- unname(m$residuals)
  n <- length(x)
  p <- length(variable.names(m))
  x <- (1 / (n - p)) * sum(x^2)
  sqrt(x)
}

#' nagelkerke r squared
#'
#' Estimate R^2 approximation for model object
#'
#' @param m A GLM model object.
#' @return R^2 estimate.
#' @details Equation taken from the following study:
#' Nagelkerke, N. (1991). A Note on a General Definition of the Coefficient of Determination. Biometrika, 78(3), 691-692. doi:10.2307/2337038
#' @export
nagelkerke <- function(m) UseMethod("nagelkerke")

#' @export
nagelkerke.default <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  n <- length(m$residuals)
  1 - exp((-(2/n) * (ll1 - ll0)))
}

mcfadden <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  1 - ll1 / ll0
}

mcfadden.adj <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  1 - (ll1 - ncol(m$model) - 1) / ll0
}

coxsnell <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  n <- length(m$residuals)
  1 - ((ll0 / ll1)^(2 / n))
}
