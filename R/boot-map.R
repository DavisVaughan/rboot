boot_result <- function(.result, .extra = NULL) {
  list(.result = .result, .extra = .extra)
}

boot_map <- function(.rset, .f, ...) {

  .f_callr <- function(x) {
    x_data <- rsample::analysis(x)
    res <- .f(x_data, ...)

    # TODO validation

    res
  }

  out <- dplyr::select(.rset, splits, id)

  .f_out <- purrr::map(out[["splits"]], .f_callr)

  .f_result <- purrr::map(.f_out, ~.x[[".result"]])
  .f_extra <- purrr::map(.f_out, ~.x[[".extra"]])

  out[[".result"]] <- .f_result
  out[[".extra"]] <- .f_extra

  attr(out, ".f_callr") <- .f_callr

  out
}
