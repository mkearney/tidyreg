
#' @export
tidy_summary <- function(m) {
  print(get_tidycall(m))
  tidy_model(m)
}

tidy_model <- function(m) {
  new_tidy_model(
    fit  = model_fit(m),
    coef = model_coef(m),
    data = model_data(m)
  )
}

new_tidy_model <- function(fit, coef, data) {
  stopifnot(is.data.frame(fit))
  stopifnot(is.data.frame(coef))
  if (!is.data.frame(data)) {
    data <- tbl_frame()
  }
  structure(
    list(
      fit = fit,
      coef = coef,
      data = data
    ),
    class = "tidy_model"
  )
}



##----------------------------------------------------------------------------##
##                                 MODEL COEF                                 ##
##----------------------------------------------------------------------------##

model_coef <- function(x) UseMethod("model_coef")

model_coef.default <- function(x) coef_default(x)

model_coef.lm <- function(x) coef_lm(x)

model_coef.aov <- function(x) coef_lm(x)

model_coef.glm <- function(x) coef_lm(x)

model_coef.glmRob <- function(x) coef_lm(x)

##----------------------------------------------------------------------------##
##                                  MODEL FIT                                 ##
##----------------------------------------------------------------------------##

model_fit <- function(x) UseMethod("model_fit")

model_fit.lm <- function(x) fit_lm(x)

model_fit.rlm <- function(x) fit_rlm(x)

model_fit.aov <- function(x) fit_lm(x)

model_fit.glm <- function(x) fit_glm(x)

model_fit.glmRob <- function(x) fit_glmRob(x)

##----------------------------------------------------------------------------##
##                                 MODEL DATA                                 ##
##----------------------------------------------------------------------------##

model_data <- function(x) UseMethod("model_data")

model_data.default <- function(x) {
  x <- tryCatch(broom::augment(x), error = function(e) data.frame())
  as_tbl(x)
}

model_data.tidy_model <- function(x) x$data
