# generics of ImageArray
setGeneric("crop", function(object, ...) standardGeneric("crop"))
setGeneric("negate", function(object, ...) standardGeneric("negate"))
setGeneric("modulate", function(object, ...) standardGeneric("modulate"))
setGeneric("meta", function(object, ...) standardGeneric("meta"))
setGeneric("axes", function(object, ...) standardGeneric("axes"))

# generics from EBImage
setGeneric("rotate")
setGeneric("flip")
setGeneric("flop")

# transformations
setGeneric("scale_transform", \(x, ...) standardGeneric("scale_transform"))
setGeneric("affine_transform", \(x, ...) standardGeneric("affine_transform"))
setGeneric("translate_transform", \(x, ...) standardGeneric("translate_transform"))