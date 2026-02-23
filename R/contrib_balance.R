#' Contribution balance via variance-share importance
#'
#' Computes a variance-share style contribution for each variable to a constructed index:
#' score_j = |w_j| * sd(x_j) * |cor(x_j, index)|
#' share_j = score_j / sum(score_j)
#'
#' Index is computed as a weighted sum with row-wise NA handling:
#' weights are renormalized over non-missing variables for each row.
#'
#' @param df A data.frame of numeric variables. NAs allowed.
#' @param weights Optional numeric vector of weights. If NULL, equal weights are used.
#'   If named, names must match df column names (order can differ).
#'   If unnamed, length must equal ncol(df) and is assumed to follow df column order.
#'
#' @return A list with:
#'   \item{summary}{A 1-row data.frame with top contributor and concentration metrics.}
#'   \item{table}{A data.frame of per-variable contributions (variable, weight, sd, cor, score, share).}
#'   \item{index}{The computed index vector.}
#'
#' The function also prints a short summary and the contribution table.
#'
#' @export
contrib_balance <- function(df, weights = NULL) {
  .check_index_df(df)

  p <- ncol(df)
  vars <- names(df)
  if (p < 2) stop("`df` must have at least 2 columns.", call. = FALSE)

  # ----- weights -----
  if (is.null(weights)) {
    w <- rep(1 / p, p)
    names(w) <- vars
  } else {
    if (!is.numeric(weights) || anyNA(weights)) {
      stop("`weights` must be a numeric vector with no NA values.", call. = FALSE)
    }

    if (!is.null(names(weights)) && any(nzchar(names(weights)))) {
      # named weights: must match df columns
      missing_names <- setdiff(vars, names(weights))
      extra_names <- setdiff(names(weights), vars)
      if (length(missing_names) > 0) {
        stop("`weights` is missing names for: ", paste(missing_names, collapse = ", "), call. = FALSE)
      }
      if (length(extra_names) > 0) {
        stop("`weights` has names not in df: ", paste(extra_names, collapse = ", "), call. = FALSE)
      }
      w <- weights[vars]
    } else {
      # unnamed weights: must align by position
      if (length(weights) != p) {
        stop("If `weights` is unnamed, it must have length equal to ncol(df).", call. = FALSE)
      }
      w <- as.numeric(weights)
      names(w) <- vars
    }

    if (all(w == 0)) stop("`weights` cannot be all zeros.", call. = FALSE)
    # normalize to sum to 1 for interpretability (sign preserved)
    w <- w / sum(abs(w))
  }

  # ----- build index with row-wise NA renormalization -----
  X <- as.matrix(df)
  w_vec <- as.numeric(w)

  # For each row, zero-out missing and renormalize weights over observed entries
  obs <- !is.na(X)
  w_mat <- matrix(rep(w_vec, each = nrow(X)), nrow = nrow(X))
  w_obs <- w_mat * obs

  # row sums of abs weights over observed entries
  denom <- rowSums(abs(w_obs))
  # Avoid division by 0 (rows with all NA)
  denom[denom == 0] <- NA_real_

  w_norm <- w_obs / denom
  index <- rowSums(X * w_norm, na.rm = TRUE)

  # ----- per-variable stats -----
  sd_j <- vapply(df, stats::sd, numeric(1), na.rm = TRUE)

  cor_j <- vapply(vars, function(v) {
    suppressWarnings(stats::cor(df[[v]], index, use = "pairwise.complete.obs", method = "pearson"))
  }, numeric(1))

  # scores and shares
  score <- abs(w) * sd_j * abs(cor_j)

  # handle cases where score is all NA/0 (e.g., constant vars)
  score[is.na(score)] <- 0
  total <- sum(score)

  if (total == 0) {
    share <- rep(0, length(score))
  } else {
    share <- score / total
  }

  tab <- data.frame(
    variable = vars,
    weight = as.numeric(w),
    sd = as.numeric(sd_j),
    cor_with_index = as.numeric(cor_j),
    score = as.numeric(score),
    share = as.numeric(share),
    stringsAsFactors = FALSE
  )

  tab <- tab[order(tab$share, decreasing = TRUE), , drop = FALSE]
  rownames(tab) <- NULL

  # concentration metrics
  top_var <- tab$variable[1]
  top_share <- tab$share[1]
  hhi <- sum(tab$share^2)

  summary <- data.frame(
    top_variable = top_var,
    top_share = top_share,
    hhi = hhi,
    n_over_0.20 = sum(tab$share >= 0.20),
    n_over_0.10 = sum(tab$share >= 0.10),
    stringsAsFactors = FALSE
  )

  # ----- print summary + table -----
  cat(sprintf("TOP CONTRIBUTOR: %s (%.0f%%)\n", top_var, top_share * 100))
  cat(sprintf("HHI (CONCENTRATION): %.3f\n", hhi))
  print(tab, row.names = FALSE)

  list(
    summary = summary,
    table = tab,
    index = index
  )
}
