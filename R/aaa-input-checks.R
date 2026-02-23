.check_index_df <- function(df) {
  if (!is.data.frame(df)) stop("`df` must be a data.frame.", call. = FALSE)
  if (ncol(df) < 1) stop("`df` must have at least 1 column.", call. = FALSE)

  bad <- vapply(df, function(x) !(is.numeric(x) || is.integer(x)), logical(1))
  if (any(bad)) {
    stop("All columns must be numeric/integer. Bad columns: ",
         paste(names(df)[bad], collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}
