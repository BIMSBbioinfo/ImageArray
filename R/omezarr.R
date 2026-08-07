### - - - - - - - - - - - - - - - - - -
### OZPath
###

#' the shape of a zarr array
#'
#' Returns the shape of the zarr array at \code{path}, or NULL if \code{path}
#' is not a zarr array, e.g. it is a zarr group or a plain directory.
#'
#' @param path the path to a candidate zarr array
#'
#' @importFrom Rarr zarr_overview
#' @noRd
.zarr_array_dim <- function(path) {

  # no zarr metadata file at all, so not an array
  zarr_meta <- c(".zarray", "zarr.json")
  if (!any(file.exists(file.path(path, zarr_meta))))
    return(NULL)

  # zarr_overview() errors on groups, both the v2 .zgroup and the
  # v3 zarr.json with a "group" node_type
  meta <- tryCatch(
    Rarr::zarr_overview(path, as_data_frame = TRUE),
    error = function(e) NULL
  )
  if (is.null(meta) || nrow(meta) != 1L)
    return(NULL)

  as.integer(meta$dim[[1]])
}

#' OZPath constructor method
#'
#' A function for creating objects of OZPath class
#'
#' @param filepath the path to the OME-Zarr store
#' @param resolution the resolution IDs of the pyramidal image, either the
#' names of the resolution directories, e.g. "s0", or integers starting from 1
#' indexing into them. Subdirectories of the store that are not zarr arrays,
#' such as the OME-NGFF "labels" group, are never resolutions.
#'
#' @name OZPath
#' @rdname OZPath
#'
#' @aliases
#' resolution
#' resolution,OZPath-method
#' path,OZPath-method
#'
#' @importFrom S4Vectors new2
#'
#' @export
#' @return An OZPath object
#'
#' @examples
#' # get image
#' library(utils)
#' omezarrzip <- system.file("extdata", 
#'                           "test_ngff_image_v04.ome.zarr.zip", 
#'                           package = "ImageArray")
#' dir.create(omezarr <- tempfile())
#' unzip(omezarrzip, exdir = omezarr)
#' oz <- OZPath(omezarr, resolution = 2)                         
#' resolution(oz)
#' path(oz)
OZPath <- function(filepath, resolution = NULL) {

  # check if zarr exists
  zarr_meta <- c(".zgroup", "zarr.json")
  if(!any(file.exists(file.path(filepath, zarr_meta))))
    stop(filepath, " is not a zarr store!")

  # keep only the subdirectories that are zarr arrays, dropping zarr groups
  # such as the OME-NGFF "labels" group
  candidates <- list.dirs(filepath, recursive = FALSE, full.names = FALSE)
  dims <- lapply(file.path(filepath, candidates), .zarr_array_dim)
  is_array <- !vapply(dims, is.null, logical(1))
  res_list <- candidates[is_array]
  dims <- dims[is_array]
  if(!length(res_list))
    stop("no zarr arrays found in ", filepath)

  # get/check resolutions
  if (is.null(resolution)) {
    index <- seq_along(res_list)
  } else if (is.character(resolution)) {
    if(!all(resolution %in% res_list)) stop("resolutions not found")
    index <- match(resolution, res_list)
  } else {
    if(!all(resolution %in% seq_along(res_list))) stop("resolutions not found")
    index <- as.integer(resolution)
  }
  resolution <- res_list[index]

  # all resolutions of a pyramid should have the same number of dimensions,
  # their shapes of course differ across the pyramid
  ndim <- lengths(dims[index])
  if(length(unique(ndim)) > 1)
    stop("resolutions have differing dimensionality: ",
         paste0(resolution, " (", ndim, "D)", collapse = ", "))

  S4Vectors::new2(
    "OZPath",
    filepath = filepath,
    resolution = resolution
  )
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### resolution() and path() getters
###

#' @describeIn OZPath resolution metadata of OZPath object
#' @exportMethod resolution
setMethod("resolution", "OZPath", function(x) x@resolution)

#' @describeIn OZPath path method for OZPath object
#' @exportMethod path
setMethod("path", "OZPath", function(object, ...) object@filepath)

#' @importFrom S4Vectors coolcat
#' @noRd
setMethod(
  f = "show",
  signature = c("OZPath"),
  definition = function(object) {
    cat(class(x = object), "Object", "\n")
    cat("Path:", path(object), "\n")
    cat("Resolutions:", paste(resolution(object), collapse = ","), "\n")
  }
)