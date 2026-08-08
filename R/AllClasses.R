#' @importFrom S4Vectors SimpleList
#' @importFrom methods setClass setClassUnion setOldClass
#' @importClassesFrom S4Arrays Array
NULL

# magick classes
setOldClass("magick-image")
setOldClass("bitmap")

# array and matrix classes
setClassUnion("matrix_array_Array", 
              c("matrix", "array", "Array"))

.ImageList <- setClass(
  Class="ImageList",
  contains="SimpleList",
  prototype=prototype(elementType="matrix_array_Array"))

#' @title ImageArray class
#'
#' @description
#' An S4 container for a multi-resolution (pyramidal) image, holding the
#' pyramid levels together with their scales. Objects are created with
#' \code{\link{ImageArray}}.
#'
#' @slot levels an \code{ImageList} of pyramid levels, ordered from the
#'   highest to the lowest resolution
#' @slot scales a list of named numeric vectors, one per level, where values
#'   are the scales of these axes. The names of these vectors define the axes
#'   of the object, hence they are a subset of \code{c("c", "y", "x", "z", "t")}
#'   and are shared, in the same order, by all levels. See
#'   \url{https://ngff.openmicroscopy.org/} for more information.
#'
#' @keywords internal
#' @exportClass ImageArray
.ImageArray <- setClass(
  Class = "ImageArray",
  slots = c(
    levels = "ImageList",
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