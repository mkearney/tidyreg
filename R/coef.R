coef_default <- function(x) {
  ## broom the coef table, rename, and add stars column
  d <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(d)[2:4] <- c("est", "s.e.", "est.se")
  d <- add_stars(d)
  ## estimate/add standardized solution estimates and return
  add_std_est(d)
}

#' @importFrom MASS rlm
#' @importFrom robust glmRob
coef_lm <- function(x) {
  ## estimate standardized solution
  data <- standardize_inputs(x)
  ## store df.residual if available
  if ("df.residual" %in% names(x)) {
    dfr <- length(x$residuals) - length(x$coefficients)
  } else {
    dfr <- NULL
  }
  ## sometimes it won't converge, so tryCatch returns NULL
  if (attr(x$terms, "intercept") == 1) {
    ## try to update model
    s <- tryCatch(suppressWarnings(
      update(x, . ~ . - 1, data = data)),
      error = function(e) NULL)
    ## if null try again this time w/o removing the intercept
    if (is.null(s)) {
      s <- tryCatch(update(x, . ~ ., data = data), error = function(e) NULL)
    }
  } else {
    ## update model
    s <- tryCatch(update(x, . ~ ., data = data), error = function(e) NULL)
  }
  ## broom the coef table, rename, and add stars column
  x <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(x)[2:4] <- c("est", "s.e.", "est.se")
  if (ncol(x) == 4 && !is.null(dfr)) {
    x$p.value <- 2 * pt(abs(x[[4]]), dfr, lower.tail = FALSE)
  }
  x <- add_stars(x)
  ## if the standardized solution worked, append the stardardized estimates
  ## otherwise return NA vector
  if (!is.null(s)) {
    s <- tibble::as_tibble(broom::tidy(s), validate = FALSE)
    if (nrow(x) > nrow(s)) {
      x$std.est <- c(0, s[[2]])
    } else {
      x$std.est <- s[[2]]
    }
  } else {
    x$std.est <- NA_real_
  }
  x
}

coef_aov <- function(x) {
  ## broom the coef table and add stars column
  d <- as_tbl(broom::tidy(x))
  add_stars(d)
}

standardize_inputs <- function(x) {
  x <- model.frame(x)
  if (ncol(x) < 2L) return(as_tbl(x))
  y <- x[1]
  x <- x[-1]
  chars <- vapply(x, function(x) is.character(x) | is.factor(x), logical(1))
  x[, !chars] <- scale(x[, !chars])
  as_tbl(cbind(y, x))
}

add_std_est <- function(d, m) {
  ## estimate standardized solution
  data <- standardize_inputs(x)
  ## sometimes it won't converge, so tryCatch returns NULL
  s <- tryCatch(update(m, . ~ . - 1, data = data), error = function(e) NULL)
  ## if the standardized solution worked, append the stardardized estimates
  ## otherwise return NA vector
  if (!is.null(s)) {
    s <- tibble::as_tibble(broom::tidy(s), validate = FALSE)
    if (nrow(d) > nrow(s)) {
      d$std.est <- c(0, s[[2]])
    } else {
      d$std.est <- s[[2]]
    }
  } else {
    d$std.est <- NA_real_
  }
  d
}
