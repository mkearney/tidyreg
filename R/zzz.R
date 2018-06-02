

tidyselector <- function(data, ...) {
  vars <- tidyselect::vars_select(names(data), ...)
  if (length(vars) > 0) {
    data <- data[vars]
  }
  data
}

all_numeric <- function(x) {
  x[1:ncol(x)] <- lapply(x, coerce_numeric)
  x
}

#' @export
coerce_numeric <- function(x) UseMethod("coerce_numeric")

#' @export
coerce_numeric.default <- function(x) {
  as.numeric(x)
}

#' @export
coerce_numeric.character <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) {
    pstop("Data contains a character column (a textual variable without",
      "set, or fixed, levels), but variables must be either numeric",
      "integer, or factor. Try converting character columns into factor,",
      "which you can do with something like the following code:\n",
      "`dplyr::mutate_if(data, is.character, as.factor)`")
  }
  x
}


add_stars <- function(x) {
  x$stars <- make_stars(x)
  ## round p.value
  x$p.value <- round(x$p.value, 6)
  x
}


make_stars <- function(x) UseMethod("make_stars")

make_stars.data.frame <- function(x) {
  if ("p.value" %in% names(x)) {
    x <- x$p.value
  } else if (any(grepl("^p$|^pval$|^pvalue$", names(x)))) {
    x <- x[[grep("^p$|^pval$|^pvalue$", names(x))[1]]]
  }
  make_stars(x)
}

make_stars.numeric <- function(x) {
  ifelse(
    is.na(x),            "",
  ifelse(
    x < .10 & x >=  .05, "+",
  ifelse(
    x < .05 & x >=  .01, "*",
  ifelse(
    x < .01 & x >= .001, "**",
  ifelse(
    x <= .001          , "***", ""
  )))))
}

make_stars.character <- function(x) {
  x <- as.numeric(x)
  make_stars(x)
}

breaklines <- function(x, n = getOption("width") - 5) {
  if (n < 15) {
    n <- 15
  }
  if (n > 70) {
    n <- 70
  }
  if (length(x) == 0L) return("")
  x <- strsplit(x, "[ ]{1,}")[[1]]
  cs <- cumsum(nchar(x) + 1L)
  out <- paste(x[cs <= n], collapse = " ")
  continue <- TRUE
  x <- x[cs > n]
  while (continue) {
    if (length(x) == 0) return(out)
    out <- paste0(out, "\n")
    cs <- cumsum(nchar(x) + 1L)
    out <- paste0(
      out, paste(x[cs <= n], collapse = " ")
    )
    x <- x[cs > n]
    if (length(x) == 1L && nchar(x) >= n) break
  }
  if (length(x) > 0) {
    out <- paste0(out, "\n", x)
  }
  out
}

pstop <- function(..., call. = FALSE) {
  msg <- paste(list(...), collapse = " ")
  msg <- gsub("[ ]{2}", " ", msg)
  msg <- strsplit(msg, "\n")[[1]]
  msg <- lapply(msg, breaklines)
  msg <- paste(msg, collapse = "\n")
  stop(msg, call. = call.)
}
