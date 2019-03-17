boot_bca_df <- function(rset_computed, ..., alpha = 0.05, times = 1000) {

  summarise_exprs <- attr(rset_computed, "boot_dots")

  original_split <- purrr::pluck(rset_computed, "splits", 1)
  original_data <- rsample::analysis(original_split)

  data_bootstrapped <- strapgod::bootstrapify(original_data, times = times)

  .result <- dplyr::summarise(data_bootstrapped, !!!summarise_exprs)

  syms_groups <- dplyr::groups(original_data)
  vars_result <- tidyselect::vars_select(names(.result), ...)

  # keep all
  if (length(vars_result) == 0L) {
    vars_result <- names(summarise_exprs)
  }

  syms_result <- rlang::syms(vars_result)

  .result <- dplyr::select(.result, !!!syms_groups, !!!syms_result)

  .result <- tidyr::gather(.result, ".statistic", ".estimate", !!!syms_result)

  .result <- dplyr::group_by(.result, .statistic, add = TRUE)

  .f <- function(x) {
    x <- analysis(x)
    .result <- dplyr::summarise(x, !!!summarise_exprs)
    .result <- dplyr::select(.result, !!!syms_groups, !!!syms_result)
    .result <- tidyr::gather(.result, ".statistic", ".estimate", !!!syms_result)
    boot_result(.result)
  }

  .result_apparent <- .f(original_split)$.result
  acceleration_tbl <- jacknife_acceleration(original_data, .f)

  .apparent_groups <- dplyr::select(.result_apparent, !!!dplyr::groups(.result))
  acceleration_groups <- dplyr::select(acceleration_tbl, !!!dplyr::groups(.result))

  calc_single_bca <- function(.x, .y) {

    .x_apparent <- dplyr::filter(.result_apparent, vctrs::vec_equal(.apparent_groups, .y))
    .x_acceleration <- dplyr::filter(acceleration_tbl, vctrs::vec_equal(acceleration_groups, .y))

    calc_boot_bca(
      .x$.estimate,
      .x_apparent$.estimate,
      .x_acceleration$a,
      alpha = alpha
    )

  }

  out <- dplyr::group_map(.result, calc_single_bca)

  out <- dplyr::ungroup(out)

  out
}
