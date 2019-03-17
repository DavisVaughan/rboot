calc_boot_t <- function(estimate, variance, apparent_estimate, apparent_variance, alpha = 0.05) {
  rsample:::t_single(estimate, variance, apparent_estimate, apparent_variance, alpha)
}
