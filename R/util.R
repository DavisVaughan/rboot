remove_apparent <- function(data) {
  if (!("id" %in% colnames(data))) {
    return(data)
  }

  if (!is.character(data$id)) {
    return(data)
  }

  data <- dplyr::filter(data, id != "Apparent")

  attr(data, "apparent") <- FALSE

  data
}

extract_apparent <- function(data) {
  dplyr::filter(data, id == "Apparent")
}
