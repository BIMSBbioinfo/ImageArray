#' getImageInfo
#'
#' get information of an ImageArray object
#'
#' @param object an ImageArray object
#'
#' @importFrom stats setNames
#'
#' @export
#' @returns a data frame of width and height info
#'
#' @examples
#' # get image
#' library(EBImage)
#' img.file <- system.file("images", "sample.png", package="EBImage")
#'
#' # create ImageArray
#' dir.create(td <- tempfile())
#' output_h5 <- tempfile(fileext = ".h5")
#' imgarray <- writeImageArray(img.file,
#'                           output = output_h5,
#'                           name = "image",
#'                           verbose = FALSE)
#' getImageInfo(imgarray)
#'
#' # create ImageArray
#' imgarray <- ImageArray(img.file, n.levels = 3)
#' imgarray_raster <- as.raster(imgarray, max.pixel.size = 300)
#' getImageInfo(imgarray)
#'
getImageInfo <- function(object) {
  ax <- axes(object)
  dim_image <- stats::setNames(dim(object[[1]]), ax)
  imginfo <- list(width = dim_image["x"], height = dim_image["y"])
  as.data.frame(imginfo, row.names = NULL)
}

#' read_image
#'
#' @param image the image
#' @param engine the package to use for each image layer: either
#' \code{ebimage} or \code{magick}
#'
#' @importFrom magick image_read
#' @importFrom EBImage readImage
#'
#' @noRd
#' @keywords internal
NULL

#' @describeIn read_image read image
#' @exportMethod read_image
setMethod("read_image", "character", function(image, engine) {
  switch(
    engine,
    `magick-image` = magick::image_read(image),
    `EBImage` = EBImage::readImage(image)
  )
})

#' @describeIn read_image read image
#' @exportMethod read_image
setMethod("read_image", "array", function(image, engine) {
  if(inherits(image, "bitmap"))
    engine <- "magick-image"
  switch(
    engine,
    `magick-image` = magick::image_read(image),
    `EBImage` = EBImage::Image(image)
  )
})

