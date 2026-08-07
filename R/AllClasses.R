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

#' @title ImageArray class
#'
#' @description
#' An S4 container for a multi-resolution (pyramidal) image, holding the
#' pyramid levels together with their axis names and scales. Objects are
#' created with \code{\link{ImageArray}}.
#'
#' @slot levels an \code{ImageList} of pyramid levels, ordered from the
#'   highest to the lowest resolution
#' @slot axes a character vector of axis names, a subset of
#'   \code{c("c", "y", "x", "z", "t")}, of the same length as the number of
#'   dimensions of every level
#' @slot scales a list of named numeric vectors, one per level, where names
#'   are a subset of \code{axes} and values are the scales of these axes. See
#'   \url{https://ngff.openmicroscopy.org/} for more information. 
#'
#' @keywords internal
#' @exportClass ImageArray
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