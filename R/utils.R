R6Object <- R6::R6Class("QObject", lock_objects = FALSE)

index_of <- function(x, value) {
  UseMethod("index_of")
}

index_of.list <- function(x, value) {
  index <- -1
  for (i in seq_along(x)) {
    if (identical(x[[i]], value)) {
      index <- i
    }
  }
  index
}
