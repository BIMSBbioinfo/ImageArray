#' @export
#' 
#' @examples
#' if(!requireNamespace("rome", quietly = TRUE))
#' devtools::install_github("Huber-group-EMBL/rome")
#' omezarrfile <- system.file("extdata", "10501752.zarr.zip", 
#'                            package = "ImageArray")
#' dir.create(td <- tempfile())
#' utils::unzip(omezarrfile, exdir=td)
#' library(rome)
#' omezarrimg <- ome_read(path = file.path(td, "10501752.zarr"), lazy = TRUE)
#' imgarray <- as.ImageArray(x)
#' imgarray
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