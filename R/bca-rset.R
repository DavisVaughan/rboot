boot_bca_rset <- function(rset_mapped, alpha = 0.05) {

  # validate_is_mapped_rset(rset_mapped)
  # validate_has_apparent(rset_mapped)

  .f_callr <- attr(rset_mapped, ".f_callr")

  apparent  <- extract_apparent(rset_mapped)
  rset_mapped <- remove_apparent(rset_mapped)

  original_data <- rsample::analysis(purrr::pluck(apparent, "splits", 1))

  # patch because it expects a split
  .f_patched_callr <- function(x) {
    .f_callr(rsample::apparent(x)$splits[[1]])
  }

  acceleration_tbl <- jacknife_acceleration(original_data, .f_patched_callr)

  .result <- purrr::pluck(rset_mapped, ".result")
  .result <- dplyr::bind_rows(.result)
  .result <- dplyr::group_by(.result, .statistic, add = TRUE)

  .result_apparent <- purrr::pluck(apparent, ".result", 1)

  .result_apparent <- dplyr::ungroup(.result_apparent)

  .apparent_groups <- dplyr::select(.result_apparent, !!!dplyr::groups(.result))
  acceleration_groups <- dplyr::select(acceleration_tbl, !!!dplyr::groups(.result))

  fn <- function(.x, .y) {

    .x_apparent <- dplyr::filter(.result_apparent, vctrs::vec_equal(.apparent_groups, .y))
    .x_acceleration <- dplyr::filter(acceleration_tbl, vctrs::vec_equal(acceleration_groups, .y))

    calc_boot_bca(
      .x$.estimate,
      .x_apparent$.estimate,
      .x_acceleration$a,
      alpha = alpha
    )

  }

  out <- dplyr::group_map(.result, fn)

  out <- dplyr::ungroup(out)

  out
}

jacknife_acceleration <- function(data, .f) {

  rset_loo <- rsample::loo_cv(data)

  .results <- purrr::map_dfr(rset_loo[["splits"]], ~.f(rsample::analysis(.x))$.result)

  .results <- dplyr::group_by(.results, .statistic, !!!dplyr::groups(data))

  .results <- dplyr::mutate(.results, .estimate_mean = mean(.estimate, na.rm = TRUE))

  acceleration <- .results %>%
    dplyr::summarise(
      a = sum((.estimate_mean - .estimate)^3) / (6 * (sum((.estimate_mean - .estimate)^2))^(3/2))
    )

  acceleration <- dplyr::ungroup(acceleration)

  acceleration
}
