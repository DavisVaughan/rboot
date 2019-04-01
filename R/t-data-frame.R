boot_t_df <- function(data, ..., values = NULL, std_error_vars = vars(), alpha = 0.05, times = 1000) {

  std_error_vars <- purrr::map(std_error_vars, rlang::as_label)

  summarise_exprs <- rlang::enquos(...)

  data_bootstrapped <- strapgod::bootstrapify(data, times = times)

  .result <- dplyr::summarise(data_bootstrapped, !!!summarise_exprs)

  syms_groups <- dplyr::groups(data)
  vars_result <- tidyselect::vars_select(names(.result), !!rlang::enquo(values))
  vars_result <- setdiff(vars_result, ".bootstrap")

  # keep all
  if (length(vars_result) == 0L) {
    vars_result <- names(summarise_exprs)
  }

  std_error_vars <- std_error_vars[vars_result %in% names(std_error_vars)]

  .result <- purrr::imap_dfr(std_error_vars, ~{
    sym_estimate <- rlang::sym(.y)
    sym_std_error <- rlang::sym(.x)
    .single_estimate <- dplyr::select(.result, !!!syms_groups, !!sym_estimate, !!sym_std_error)
    .single_estimate <- dplyr::rename(.single_estimate, .estimate = !!sym_estimate, .std_error = !!sym_std_error)
    tibble::add_column(.single_estimate, .statistic = .y)
  })

  .result <- dplyr::group_by(.result, .statistic, add = TRUE)

  .f <- function(x) {
    .result <- dplyr::summarise(x, !!!summarise_exprs)

    .result <- purrr::imap_dfr(std_error_vars, ~{
      sym_estimate <- rlang::sym(.y)
      sym_std_error <- rlang::sym(.x)
      .single_estimate <- dplyr::select(.result, !!!syms_groups, !!sym_estimate, !!sym_std_error)
      .single_estimate <- dplyr::rename(.single_estimate, .estimate = !!sym_estimate, .std_error = !!sym_std_error)
      tibble::add_column(.single_estimate, .statistic = .y)
    })

    boot_result(.result)
  }

  .result_apparent <- .f(data)$.result

  .result_apparent <- dplyr::ungroup(.result_apparent)

  .apparent_groups <- dplyr::select(.result_apparent, !!!dplyr::groups(.result))

  out <- dplyr::group_map(.result, ~{

    .x_apparent <- dplyr::filter(.result_apparent, vctrs::vec_equal(.apparent_groups, .y))

    calc_boot_t(
      .x$.estimate,
      .x$.std_error,
      .x_apparent$.estimate,
      .x_apparent$.std_error,
      alpha = alpha
    )
  })

  out <- dplyr::ungroup(out)

  out
}
