.validate_ImageArray <- function(object) {

  # check default axes
  ax <- axes(object)
  if (!all(ax %in% .AXES)) {
    stop("The axes of the ImageArray object should be a subset of ",  
         deparse(.AXES))
  }
  
  # check scales
  sc <- scales(object)
  for(s in sc){
    if(!all(names(s) %in% ax)) stop("scale names do not match axes")
    if(!all(is.numeric(s) & is.finite(s))) 
      stop("scale entries are not numeric")
  }

  # check all dim vs axes
  all_length <- vapply(object@levels, function(x) length(dim(x)), integer(1))
  if (!all(all_length == length(ax))) {
    stop(
      "The number of dimensions of all levels should match the number of axes."
    )
  }
  
  # check all dim vs scales
  all_dim <- lapply(object@levels, function(x) dim(x))
  if(length(sc) != length(all_dim))
    stop("scales should be of the same length as levels!")

  TRUE
}

setValidity("ImageArray", .validate_ImageArray)

.validate_BFPath<- function(object) {
  
  # check series
  if(length(series(object)) > 1)
    stop("series cannot be longer than 1!")
  if(series(object) %% 1 != 0)
    stop("series should be an integer!")
  
  # check resolution
  lapply(resolution(object), 
         \(.) {
           if(. %% 1 != 0)
             stop("resolution values should be integers!")
         })
  
  TRUE
}

setValidity("BFPath", .validate_BFPath)
