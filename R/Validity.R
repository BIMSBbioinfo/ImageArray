.validate_ImageArray <- function(object) {

  # check default axes
  if (!all(axes(object) %in% .AXES)) {
    stop("The axes of the ImageArray object should be a subset of ",  
         deparse(.AXES))
  }


  # check all dim vs axes
  all_length <- vapply(object@levels, function(x) length(dim(x)), integer(1))
  if (!all(all_length == length(axes(object)))) {
    stop(
      "The number of dimensions of all levels should match the number of axes."
    )
  }
  
  # check all dim vs scales
  all_dim <- lapply(object@levels, function(x) dim(x))
  if(length(object@scales) != length(all_dim))
    stop("scales should be of the same length as levels!")
  for(ad in all_dim){
    if(!all(names(ad) == axes(object))) stop("scale names do not match axes")
    if(!all(is.numeric(ad) & is.finite(ad))) 
      stop("scale entries are not numeric")
  }

  TRUE
}

setValidity("ImageArray", .validate_ImageArray)
