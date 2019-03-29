boot_pctl_df <- function(rset_computed, ..., alpha = 0.05, times = 1000) {

  summarise_exprs <- attr(rset_computed, "boot_dots")

  original_data <- rsample::analysis(purrr::pluck(rset_computed, "splits", 1))

  data_bootstrapped <- strapgod::bootstrapify(original_data, times = times)

  estimates <- dplyr::summarise(data_bootstrapped, !!! summarise_exprs)

  syms_groups <- dplyr::groups(original_data)
  vars_estimates <- tidyselect::vars_select(names(estimates), ...)

  # keep all
  if (length(vars_estimates) == 0L) {
    vars_estimates <- names(summarise_exprs)
  }

  syms_estimates <- rlang::syms(vars_estimates)

  estimates <- dplyr::select(estimates, !!!syms_groups, !!!syms_estimates)

  estimates <- tidyr::gather(estimates, ".statistic", ".estimate", !!!syms_estimates)

  estimates <- dplyr::group_by(estimates, .statistic, add = TRUE)

  out <- dplyr::group_map(estimates, ~calc_boot_pctl(.x$.estimate, alpha = alpha))
  out <- dplyr::ungroup(out)

  out
}
