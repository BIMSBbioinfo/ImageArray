#' BFArray constructor method
#'
#' A function for creating objects of BFArray class
#'
#' @param x A BFArray object
#' @param image.file the path to the image read by
#' RBioFormats
#' @param series the series IDs of the pyramidal image,
#' typical an integer starting from 1
#' @param resolution the resolution IDs of the
#' pyramidal image, typical an integer starting from 1
#'
#' @name BFArray-methods
#' @rdname BFArray-methods
#'
#' @export
#' @return A BFArray object
#'
#' @examples
#' # get image
#' library(RBioFormats)
#' img.file <- system.file("extdata",
#'                         "xy_12bit__plant.ome.tiff",
#'                         package = "ImageArray")
#' bfa <- BFArray(img.file, series = 1, resolution = 2)
#' dim(bfa)
#' type(bfa)
BFArray <- function(image.file, series, resolution) {
  # check RBioFormats
  if (!requireNamespace("RBioFormats")) {
    stop("Please install RBioFormats: BiocManager::install('RBioFormats')")
  }

  # get metadata
  meta.data <- RBioFormats::read.metadata(
    file = image.file,
    filter.metadata = TRUE,
    proprietary.metadata = TRUE
  )
  len_meta <- lengths(meta.data@.Data)
  meta.data@.Data <- meta.data@.Data[which(len_meta > 0)]

  # get shape
  series_res_meta <- vapply(
    meta.data@.Data,
    function(x) {
      if (!is.null(cm <- x$coreMetadata)) {
        x <- cm
      }
      c(x$series, x$resolutionLevel)
    },
    integer(2)
  )
  series_index <-
    which(
      series_res_meta[1, ] == series &
        series_res_meta[2, ] == resolution
    )
  if (length(series_index) > 0) {
    shape <- vapply(
      c("sizeX", "sizeY", "sizeC"),
      function(x) {
        md <- meta.data@.Data[[series_index]]
        if (!is.null(cm <- md$coreMetadata)) {
          md <- cm
        }
        md[[x]]
      },
      integer(1),
      USE.NAMES = FALSE
    )

    seed <- BFArraySeed(
      filepath = image.file,
      series = series,
      resolution = resolution,
      shape = shape,
      type = "double"
    )
    .BFArray(seed = seed)
  } else {
    stop("Specified resolution was not found in the image!")
  }
}

#' @importFrom S4Vectors new2
BFArraySeed <- function(filepath, series, resolution, shape, type) {
  S4Vectors::new2(
    "BFArraySeed",
    filepath = filepath,
    series = series,
    resolution = resolution,
    shape = shape,
    type = type
  )
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dim() getter
###

#' @describeIn BFArray-methods dim function for BFArray objects
setMethod("dim", "BFArraySeed", function(x) x@shape)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### type() getter
###

#' @describeIn BFArray-methods type function for BFArray objects
setMethod("type", "BFArraySeed", function(x) x@type)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extract_array
###

#' @importFrom EBImage imageData
.extract_array_from_BFArraySeed <- function(x, index) {
  # check RBioFormats
  if (!requireNamespace("RBioFormats")) {
    stop("Please install RBioFormats: BiocManager::install('RBioFormats')")
  }

  # check for index length
  if (length(index) > 3) {
    stop("You cannot get BFArray slices more than 2 dimensions!")
  }

  # create slices
  ind <- mapply(
    function(x, y) {
      if (is.null(x)) {
        seq_len(y)
      } else if (length(x) == 0) {
        integer(0)
      } else if (length(x) > 0) {
        x
      }
    },
    index,
    x@shape,
    SIMPLIFY = FALSE
  )

  # get slices
  len_ind <- lengths(ind)
  if (any(len_ind == 0)) {
    res <- array(dim = len_ind)
    type(res) <- x@type
  } else {
    subset_list <- list(X = ind[[1]], Y = ind[[2]])
    if (length(len_ind) == 3) {
      subset_list <- c(subset_list, list(C = ind[[3]]))
    }
    res <- RBioFormats::read.image(
      file = x@filepath,
      series = x@series,
      resolution = x@resolution,
      subset = subset_list
    )
    res <- EBImage::imageData(res)
    if (length(dim(res)) != 3) {
      res <- array(res, dim = c(dim(res), 1))
    }
  }

  res
}

setMethod("extract_array", "BFArraySeed", .extract_array_from_BFArraySeed)

### - - - - - - - - - - - - - - - - - -
### Constructor
###

setMethod("DelayedArray", "BFArraySeed", function(seed) {
  new_DelayedArray(seed, Class = "BFArray")
})
