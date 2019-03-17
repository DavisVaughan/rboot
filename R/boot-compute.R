boot_compute <- function(data, ...) {

  # makes it resiliant to dplyr commands
  # stripping attributes
  data <- rsample::apparent(data)

  dots <- rlang::enquos(..., .named = TRUE)
  attr(data, "boot_dots") <- dots

  data
}
