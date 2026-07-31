#' @importFrom BiocGenerics scale

# generics of ImageArray
setGeneric("crop", function(object, ...) standardGeneric("crop"))
setGeneric("negate", function(object, ...) standardGeneric("negate"))
setGeneric("modulate", function(object, ...) standardGeneric("modulate"))
setGeneric("meta", function(object, ...) standardGeneric("meta"))
setGeneric("axes", function(object, ...) standardGeneric("axes"))
setGeneric("extent", \(x, ...) standardGeneric("extent"))
setGeneric(".extent", \(x, ...) standardGeneric(".extent"))

# generics from EBImage
setGeneric("rotate")
setGeneric("flip")
setGeneric("flop")

# transformations
setGeneric("affine", \(x, ...) standardGeneric("affine"))
setGeneric("translation", \(x, ...) standardGeneric("translation"))
setGeneric("rotate_transform",
           \(x, ...) standardGeneric("rotate_transform"))