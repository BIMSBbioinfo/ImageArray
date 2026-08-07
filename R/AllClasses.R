#' @importFrom S4Vectors SimpleList
#' @importFrom methods setClass setClassUnion setOldClass
#' @importClassesFrom S4Arrays Array
NULL

# magick classes
setOldClass("magick-image")
setOldClass("bitmap")
setClassUnion(c("magick_class"), 
              c("magick-image", "bitmap"))

# array and matrix classes
setClassUnion("matrix_array_Array", 
              c("matrix", "array", "Array"))

.ImageList <- setClass(
  Class="ImageList",
  contains="SimpleList",
  prototype=prototype(elementType="matrix_array_Array"))

.ImageArray <- setClass(
  Class = "ImageArray",
  slots = c(
    levels = "ImageList",
    axes = "character",
    scales = "list"
  )
)

.BFPath <- setClass(
  Class = "BFPath",
  slots = c(
    filepath = "character", 
    series = "numeric",
    resolution = "numeric"
  )
)

.BFArraySeed <- setClass(
  "BFArraySeed",
  contains = c("Array", "BFPath"),
  slots = c(
    axes = "character",
    dim = "numeric",
    type = "character"
  )
)

.BFArray <- setClass(
  Class = "BFArray",
  contains = c("DelayedArray"),
  slots = c(seed = "BFArraySeed")
)

.OZPath <- setClass(
  Class = "OZPath",
  slots = c(
    filepath = "character",
    resolution = "character"
  )
)