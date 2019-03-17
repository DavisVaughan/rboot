boot_t_rset <- function(rset_mapped, alpha = 0.05) {

  # validate_is_mapped_rset(rset_mapped)
  # validate_has_apparent(rset_mapped)

  # or we could go into jacknife!
  # validate_has_std_error(rset_mapped)

  apparent  <- extract_apparent(rset_mapped)
  rset_mapped <- remove_apparent(rset_mapped)

  .result <- purrr::pluck(rset_mapped, ".result")
  .result <- dplyr::bind_rows(.result)
  .result <- dplyr::group_by(.result, .statistic)

  .result_apparent <- purrr::pluck(apparent, ".result", 1)

  out <- dplyr::group_map(.result, ~{

    .x_apparent <- dplyr::filter(.result_apparent, .statistic == .y$.statistic)

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
