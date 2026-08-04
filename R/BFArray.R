### - - - - - - - - - - - - - - - - - -
### BFArray
###

#' BFArray constructor method
#'
#' A function for creating objects of BFArray class
#'
#' @param image A BFPath object
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
#' bfp <- BFPath(img.file, series = 1, resolution = 2)                         
#' bfa <- BFArray(bfp)
#' dim(bfa)
#' type(bfa)
BFArray <- function(image) {
  
  # check RBioFormats
  if (!requireNamespace("RBioFormats")) {
    stop("Please install RBioFormats: BiocManager::install('RBioFormats')")
  }

  # get metadata
  meta.data <- RBioFormats::read.metadata(
    file = path(image),
    filter.metadata = TRUE,
    proprietary.metadata = TRUE
  )
  len_meta <- lengths(meta.data@.Data)
  meta.data@.Data <- meta.data@.Data[which(len_meta > 0)]

  # full image axes from metadata (always lowercase)
  if( "coreMetadata" %in% names(meta.data)) {
    axes <-  meta.data$coreMetadata$dimensionOrder
  } else if ("coreMetadata" %in% names(meta.data[[1]])) {
    axes <- meta.data@.Data[[1]]$coreMetadata$dimensionOrder
  }
  axes <- tolower(unlist(strsplit(axes, "")))

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
      series_res_meta[1, ] == series(image) &
        series_res_meta[2, ] == resolution(image)
    )
  if (length(series_index) > 0) {
    shape <- vapply(
      sprintf("size%s", toupper(axes)),
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
    
    # remove dimensions with size 1 to mimic RBioFormats::read.image behavior
    axes <- toupper(axes[shape > 1])
    shape <- shape[shape > 1]

    seed <- BFArraySeed(
      filepath = path(image),
      series = series(image),
      resolution = resolution(image),
      dim = shape,
      axes = axes,
      type = "double"
    )
    .BFArray(seed = seed)
  } else {
    stop("Specified resolution was not found in the image!")
  }
}

### - - - - - - - - - - - - - - - - - -
### BFArraySeed
###

#' @importFrom S4Vectors new2
BFArraySeed <- function(filepath, series, resolution, dim, axes, type) {
  S4Vectors::new2(
    "BFArraySeed",
    filepath = filepath,
    series = series,
    resolution = resolution,
    dim = dim,
    axes = axes,
    type = type
  )
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dim() getter
###

#' @describeIn BFArray-methods dim function for BFArray objects
setMethod("dim", "BFArraySeed", function(x) x@dim)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### axes() getter
###

#' @describeIn BFArray-methods axes function for BFArray objects
#' @exportMethod axes
setMethod("axes", "BFArray", function(object) axes(seed(object)))

#' @describeIn BFArray-methods axes function for BFArray objects
#' @exportMethod axes
setMethod("axes", "BFArraySeed", function(object) object@axes)

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
    x@dim,
    SIMPLIFY = FALSE
  )
  
  # get slices
  len_ind <- lengths(ind)
  if (any(len_ind == 0)) {
    res <- array(dim = len_ind)
    type(res) <- x@type
  } else {
    subset_list <- setNames(ind, x@axes)
    res <- RBioFormats::read.image(
      file = x@filepath,
      series = x@series,
      resolution = x@resolution,
      subset = subset_list
    )
    res <- EBImage::imageData(res)
    dim(res) <- len_ind
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

### - - - - - - - - - - - - - - - - - -
### BFPath
###

#' BFPath constructor method
#'
#' A function for creating objects of BFPath class
#'
#' @param filepath the path to the image read by RBioFormats
#' @param series the series IDs of the pyramidal image,
#' typical an integer starting from 1
#' @param resolution the resolution IDs of the
#' pyramidal image, typical an integer starting from 1
#'
#' @name BFArray-methods
#' @rdname BFArray-methods
#'
#' @importFrom S4Vectors new2
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
#' bfp <- BFPath(img.file, series = 1, resolution = 2)                         
#' series(bfp)
#' resolution(bfp)
BFPath <- function(filepath, series = NULL, resolution = NULL) {
  
  # check RBioFormats
  if (!requireNamespace("RBioFormats")) {
    stop("Please install RBioFormats: BiocManager::install('RBioFormats')")
  }
  
  # get metadata
  meta.data <- RBioFormats::read.metadata(
    file = filepath,
    filter.metadata = TRUE,
    proprietary.metadata = TRUE
  )
  len_meta <- lengths(meta.data@.Data)
  meta.data@.Data <- meta.data@.Data[which(len_meta > 0)]
  
  # series and resolution metadata
  meta <- as.data.frame(t(vapply(
    meta.data@.Data,
    function(x) {
      if (!is.null(cm <- x$coreMetadata)) 
        x <- cm
      c(series = x$series,
        resolutionLevel = x$resolutionLevel)
    },
    c(series = 0L, resolutionLevel = 0L)
  )))

  # check for nulls
  if (is.null(series)) {
    series <- 1
  } else {
    if(!series %in% meta$series) stop("series not found")
  }
  res_list <- meta$resolutionLevel[meta$series == series]
  if (is.null(resolution)) {
    resolution <- res_list
  } else {
    if(!all(resolution %in% res_list)) stop("resolutions not found")
  }
  
  S4Vectors::new2(
    "BFPath",
    filepath = filepath,
    series = series,
    resolution = resolution
  )
}

#' @importFrom S4Vectors coolcat
#' @noRd
setMethod(
  f = "show",
  signature = c("BFPath"),
  definition = function(object) {
    cat(class(x = object), "Object", "\n")
    cat("Path: ", path(object), "\n")
    cat("Series: ", series(object), "\n")
    cat("Resolutions: ", paste(resolution(object), collapse = ","), "\n")
  }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### getters
###

#' @exportMethod series
setMethod("series", "BFPath", function(x) x@series)

#' @exportMethod resolution
setMethod("resolution", "BFPath", function(x) x@resolution)

#' @exportMethod path
setMethod("path", "BFPath", function(object, ...) object@filepath)