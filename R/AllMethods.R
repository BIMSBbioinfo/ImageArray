#' @export
#' 
#' @examples
#' library(rome)
#' x <- ome_read(
#'   system.file("extdata", "ome-v0.4", "10501752.zarr", package = "rome"),
#'   lazy = TRUE
#' )
#' as.ImageArray(x)
setMethod(
  "as.ImageArray",
  "ome_zarr",
  function(object, ...) {
    new(
      "ImageArray",
      meta = list(
        axes = names(dimnames(object[[1]])) %||% c("c", "x", "y", "z", "t")[seq_along(dim(object[[1]]))]
      ),
      levels = unclass(object)
    )
  }
)