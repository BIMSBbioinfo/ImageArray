#' @importMethodsFrom BiocGenerics scale path
#' @importFrom EBImage rotate flip flop
NULL

# generics of ImageArray manipulation
setGeneric("createImageList", 
           function(image, ...) standardGeneric("createImageList"))
setGeneric("scales", function(object, ...) standardGeneric("scales"))
setGeneric("axes", function(object, ...) standardGeneric("axes"))
setGeneric("read_image", 
           function(image, engine) standardGeneric("read_image"))

# generics of ImageArray manipulation
setGeneric("crop", function(object, ...) standardGeneric("crop"))
setGeneric("negate", function(object, ...) standardGeneric("negate"))
setGeneric("modulate", function(object, ...) standardGeneric("modulate"))

# generics for BFPath (BioFormats)
setGeneric("series", function(x, ...) standardGeneric("series"))
setGeneric("resolution", function(x, ...) standardGeneric("resolution"))

# generics from EBImage
setGeneric("rotate")
setGeneric("flip")
setGeneric("flop")

# transformations
setGeneric("affine", \(x, ...) standardGeneric("affine"))
setGeneric("translation", \(x, ...) standardGeneric("translation"))
setGeneric("extent", \(x, ...) standardGeneric("extent"))
setGeneric(".extent", \(x, ...) standardGeneric(".extent"))