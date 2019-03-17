boot_pctl_rset <- function(rset_mapped, alpha = 0.05) {

  # validate_is_mapped_rset(rset_mapped)

  # remove apparent
  rset_mapped <- remove_apparent(rset_mapped)

  .result <- rset_mapped[[".result"]]
  .result <- dplyr::bind_rows(.result)
  .result <- dplyr::group_by(.result, .statistic)

  out <- dplyr::group_map(.result, ~calc_boot_pctl(.x$.estimate, alpha = alpha))
  out <- dplyr::ungroup(out)

  out
}
