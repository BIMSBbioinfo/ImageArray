.validate_ImageArray <- function(object) {
  # check axes
  if (!"axes" %in% names(meta(object))) {
    stop(
      "'axes' should not be provided in the metadata of the ",
       "ImageArray object, as they are stored separately in ",
       "the 'axes' slot."
    )
  }

  # check default axes
  if (!all(axes(object) %in% .AXES)) {
    stop(
      sprintf(
        "The axes of the ImageArray object should be a subset of %s%"
      ),
      .AXES
    )
  }

  # check all dim vs axes
  all_dim <- vapply(object@levels, function(x) length(dim(x)), integer(1))
  if (!all(all_dim == length(axes(object)))) {
    stop(
      "The number of dimensions of all levels should match the number of axes."
    )
  }

  TRUE
}

setValidity("ImageArray", .validate_ImageArray)
