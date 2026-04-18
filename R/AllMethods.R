#' @export
#' 
#' @examples
#' if(!requireNamespace("rome", quietly = TRUE))
#'   devtools::install_github("Huber-group-EMBL/rome")
#' library(rome)
#' x <- ome_read(
#'   system.file("extdata", "10501752.zarr", package = "ImageArray"),
#'   lazy = TRUE
#' )
#' as.ImageArray(x)
setMethod(
  "as.ImageArray",
  "ome_zarr",
  function(object, ...) {
    axes = names(dimnames(object[[1]])) %||% .AXES[seq_along(dim(object[[1]]))]
    new(
      "ImageArray",
      meta = list(axes = axes),
      levels = unclass(object)
    )
  }
)