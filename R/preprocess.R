#' @importFrom stats cor lm
#' @importFrom utils install.packages
#' @importFrom car vif
NULL

#' Normalize numeric columns using min-max scaling
#'
#' Converts numeric columns in a data frame to a 0–1 range.
#' Non-numeric columns are excluded.
#'
#' @param df A data frame
#' @return A data frame with normalized numeric columns
#' @export
normalize_minmax <- function(df) {
  df_numeric <- data.frame(lapply(df, function(x) {
    if (is.numeric(x)) {
      return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    } else {
      return(NULL)
    }
  }))
  df_numeric <- df_numeric[, sapply(df_numeric, is.numeric), drop = FALSE]
  return(df_numeric)
}

#' Compute correlation matrix of numeric columns
#'
#' Returns a correlation matrix for numeric columns in a data frame.
#'
#' @param df A data frame
#' @param method Correlation method: "pearson", "kendall", or "spearman"
#' @return A correlation matrix
#' @export
get_correlation_matrix <- function(df, method = "pearson") {
  df_num <- df[sapply(df, is.numeric)]
  return(cor(df_num, use = "complete.obs", method = method))
}

#' Check for multicollinearity using Variance Inflation Factor (VIF)
#'
#' Calculates VIF values for each numeric predictor.
#'
#' @param df A data frame with numeric columns
#' @return A named vector of VIF values
#' @export
get_vif <- function(df) {
  if (!requireNamespace("car", quietly = TRUE)) {
    install.packages("car")
  }
  df_num <- df[sapply(df, is.numeric)]
  model <- lm(df_num[[1]] ~ ., data = df_num)
  car::vif(model)
}
