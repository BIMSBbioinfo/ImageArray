.validate_ImageArray <- function(object) {

  # check scales vs levels, this is done first since the axes are
  # given by the names of the scales
  sc <- scales(object)
  if(length(sc) != length(object@levels))
    stop("scales should be of the same length as levels!")

  # check default axes
  ax <- axes(object)
  if (!all(ax %in% .AXES))
    stop("The axes of the ImageArray object should be a subset of ",
         deparse(.AXES))
    
  # check duplicate axes
  ind_dup <- which(table(ax) > 1)
  if (length(ind_dup))
    stop("Duplicated axes are detected: ",
         paste(names(ind_dup), collapse = ","))

  # check scales, all levels should carry the same axes in the same order
  for(s in sc){
    if(!identical(names(s), ax)) stop("scale names do not match axes")
    if(!all(is.numeric(s) & is.finite(s)))
      stop("scale entries are not numeric")
  }

  # check all dim vs axes
  all_length <- vapply(object@levels, function(x) length(dim(x)), integer(1))
  if (!all(all_length == length(ax)))
    stop(
      "The number of dimensions of all levels should match the number of axes."
    )

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

.validate_BFArraySeed <- function(object) {
  
  # check resolutions
  if(length(series(object)) > 1)
    stop("resolutions cannot be longer than 1!")
  lapply(resolution(object), 
         \(.) {
           if(. %% 1 != 0)
             stop("resolution values should be integers!")
         })
  
  TRUE
}

setValidity("BFArraySeed", .validate_BFArraySeed)
