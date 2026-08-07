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

#' @title ImageList class
#'
#' @description
#' A \code{SimpleList} holding the levels of an \code{ImageArray} object.
#'
#' @keywords internal
#' @noRd
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
#' @noRd
.ImageArray <- setClass(
  Class = "ImageArray",
  slots = c(
    levels = "ImageList",
    axes = "character",
    scales = "list"
  )
)

#' @title BFPath class
#'
#' @description
#' A pointer to a single series and resolutions of an image file read with
#' \code{RBioFormats}, e.g. an OME-TIFF or a QPTIFF. Objects are created with
#' \code{\link{BFPath}}.
#'
#' @slot filepath the path to the image read by \code{RBioFormats}
#' @slot series the series ID of the pyramidal image, a single integer
#'   typically starting from 1
#' @slot resolution the resolution IDs of the pyramidal image, integers
#'   typically starting from 1
#'
#' @keywords internal
#' @noRd
.BFPath <- setClass(
  Class = "BFPath",
  slots = c(
    filepath = "character",
    series = "numeric",
    resolution = "numeric"
  )
)

#' @title BFArraySeed class
#'
#' @description
#'  The \code{DelayedArray} seed backing a \code{BFArray}. It extends
#' \code{BFPath}, hence it also carries the \code{filepath}, \code{series} and
#' \code{resolution} slots pointing to the image on disk.
#'
#' @slot axes a character vector of upper case axis names of the selected
#'   series and resolution.
#' @slot dim the dimension lengths of the selected series and resolution,
#'   matching \code{axes}
#' @slot type the storage type reported to \code{DelayedArray}.
#'
#' @keywords internal
#' @noRd
.BFArraySeed <- setClass(
  "BFArraySeed",
  contains = c("Array", "BFPath"),
  slots = c(
    axes = "character",
    dim = "numeric",
    type = "character"
  )
)

#' @title BFArray class
#'
#' A \code{DelayedArray} giving lazy access to a single series and resolution
#' of an image read with \code{RBioFormats}. Pixels are only read from disk
#' when the array is realized. Objects are created with \code{\link{BFArray}}.
#'
#' @slot seed a \code{BFArraySeed} object
#'
#' @keywords internal
#' @noRd
.BFArray <- setClass(
  Class = "BFArray",
  contains = c("DelayedArray"),
  slots = c(seed = "BFArraySeed")
)

#' @title OZPath class
#'
#' @description
#' A pointer to the resolutions of an OME-Zarr store. Objects are created with
#' \code{\link{OZPath}}.
#'
#' @slot filepath the path to the OME-Zarr store
#' @slot resolution the names of the resolution directories of the pyramidal
#'   image, e.g. "s0". Subdirectories of the store that are not zarr arrays,
#'   such as the OME-NGFF "labels" group, are never resolutions
#'
#' @keywords internal
#' @noRd
.OZPath <- setClass(
  Class = "OZPath",
  slots = c(
    filepath = "character",
    resolution = "character"
  )
)
