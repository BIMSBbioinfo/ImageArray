#' @importFrom S4Vectors SimpleList
#' @importFrom methods setClass setClassUnion setOldClass
#' @importClassesFrom S4Arrays Array
NULL

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
    axes = "character"
  )
)

.BFArraySeed <- setClass(
  "BFArraySeed",
  contains = "Array",
  slots = c(
    filepath = "character",
    series = "numeric",
    resolution = "numeric",
    shape = "numeric",
    type = "character"
  )
)

.BFArray <- setClass(
  Class = "BFArray",
  contains = c("DelayedArray"),
  slots = c(seed = "BFArraySeed")
)
