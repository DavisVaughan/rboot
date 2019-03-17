boot_bca_rset <- function(rset_mapped, alpha = 0.05) {

  # validate_is_mapped_rset(rset_mapped)
  # validate_has_apparent(rset_mapped)

  .f_callr <- attr(rset_mapped, ".f_callr")

  apparent  <- extract_apparent(rset_mapped)
  rset_mapped <- remove_apparent(rset_mapped)

  original_data <- analysis(purrr::pluck(apparent, "splits", 1))

  acceleration_tbl <- jacknife_acceleration(original_data, .f_callr)

  .result <- purrr::pluck(rset_mapped, ".result")
  .result <- dplyr::bind_rows(.result)
  .result <- dplyr::group_by(.result, .statistic)

  .result_apparent <- purrr::pluck(apparent, ".result", 1)

  out <- dplyr::group_map(.result, ~{

    .x_apparent <- dplyr::filter(.result_apparent, .statistic == .y$.statistic)
    .x_acceleration <- dplyr::filter(acceleration_tbl, .statistic == .y$.statistic)

    calc_boot_bca(
      .x$.estimate,
      .x_apparent$.estimate,
      .x_acceleration$a,
      alpha = alpha
    )

  })

  out <- dplyr::ungroup(out)

  out
}

jacknife_acceleration <- function(data, .f) {

  rset_loo <- rsample::loo_cv(data)

  .results <- purrr::map_dfr(rset_loo[["splits"]], ~.f(.x)$.result)

  .results <- dplyr::group_by(.results, .statistic, !!!dplyr::groups(data))

  .results <- dplyr::mutate(.results, .estimate_mean = mean(.estimate, na.rm = TRUE))

  acceleration <- .results %>%
    dplyr::summarise(
      a = sum((.estimate_mean - .estimate)^3) / (6 * (sum((.estimate_mean - .estimate)^2))^(3/2))
    )

  acceleration <- dplyr::ungroup(acceleration)

  acceleration
}
