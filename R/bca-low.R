calc_boot_bca <- function(estimate, apparent_estimate, a, alpha = 0.05) {

  po <- mean(estimate <= apparent_estimate, na.rm = TRUE)

  Z0 <- stats::qnorm(po)
  Za <- stats::qnorm(1 - alpha / 2)

  Zu <- (Z0 + Za) / (1 - a * (Z0 + Za)) + Z0
  Zl <- (Z0 - Za) / (1 - a * (Z0 - Za)) + Z0

  lower_percentile <- stats::pnorm(Zl, lower.tail = TRUE)
  upper_percentile <- stats::pnorm(Zu, lower.tail = TRUE)

  ci_bca <- as.numeric(quantile(estimate, c(lower_percentile, upper_percentile)))

  tibble::tibble(
    lower = min(ci_bca),
    estimate = apparent_estimate,
    upper = max(ci_bca),
    alpha = alpha,
    .method = "BCa"
  )

}
