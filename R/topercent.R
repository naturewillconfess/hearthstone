topercent <- function(x) {
  if (is.numeric(x)) {
    per <- paste0(as.character(round(x, digits = 4) * 100), "%")
  } else {
    per <- x
  }
  per
}
