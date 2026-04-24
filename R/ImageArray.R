#' Methods for ImageArray
#'
#' Methods for \code{ImageArray} objects
#'
#' @param x,a,object An ImageArray object
#' @param i,j,value Depends on the usage
#' \describe{
#'  \item{\code{[[}, \code{[[<-}}{
#'    Here \code{i} is the level of the image pyramid.
#'    You can use the \code{length} function to get the
#'    number of the layers in the pyramid.
#'    When used with \code{crop}, arguments \code{i} and \code{j} are
#'    associated with indices of image dimensions (e.g. width, height)
#'  }
#' }
#' @param drop ignored
#' @param angle value between 0 and 360 for degrees to rotate
#' @param brightness the brightness of the new image in percentage, e.g. 120
#' @param perm perm
#' @param index a named or unnamed list of indices for cropping/subsetting the
# image, e.g. list(x = 1:100, y = 1:100) or list(1:100, 1:100)
#' @param max.pixel.size maximum pixel size
#' @param min.pixel.size minimum pixel size
#' @param level level
#' @param ... Arguments passed to other methods
#'
#' @name ImageArray-methods
#' @rdname ImageArray-methods
#'
#' @aliases
#' [[,ImageArray,numeric-method
#' [[<-,ImageArray,numeric-method
#' rotate
#' rotate,ImageArray-method
#' crop
#' crop,ImageArray-method
#' flip
#' flip,ImageArray-method
#' flop
#' flop,ImageArray-method
#' negate
#' negate,ImageArray-method
#' modulate
#' modulate,ImageArray-method
#' meta
#' meta,ImageArray-method
#' axes
#' axes,ImageArray-method
#' realize
#' realize,ImageArray-method
#' as.raster
#' as.raster,ImageArray-method
#' path
#' path,ImageArray-method
#'
#' @examples
#' # get image
#' library(EBImage)
#' img.file <- system.file("images", "sample.png", package="EBImage")
#'
#' # create ImageArray
#' imgarray <- createImageArray(img.file, n.levels = 3)
#'
#' # access layers
#' imgarray[[1]]
#' imgarray[[2]]
#'
#' # dimensions and length
#' dim(imgarray)
#' length(imgarray)
#'
#' # manipulate images
#' imgarray <- crop(imgarray, ind = list(100:200, 100:200))
#' imgarray <- crop(imgarray, ind = list(x = 10:20, y = 10:20))
#' imgarray <- rotate(imgarray, angle = 90)
#' imgarray <- flip(imgarray)
#' imgarray <- flop(imgarray)
#'
#' # create ImageArray on disk as HDF5 format
#' dir.create(td <- tempfile())
#' output_h5 <- tempfile(fileext = ".h5")
#' imgarray <- writeImageArray(img.file,
#'                           output = output_h5,
#'                           name = "image",
#'                           verbose = FALSE)
#'
#' # as.raster
#' imgarray_raster <- as.raster(imgarray)
#'
#' # realize
#' imgarray <- realize(imgarray)
NULL

#' @describeIn ImageArray-methods subset and crop
#' for \code{ImageArray} objects
#'
#' @export
setMethod(
  f = "[",
  signature = c("ImageArray"),
  function(x, i, j, ..., drop = FALSE) {
    if (missing(x)) {
      stop("'x' is missing")
    }
    if (!.isTRUEorFALSE(drop)) {
      stop("'drop' must be TRUE or FALSE")
    }
    Nindex <- S4Arrays:::extract_Nindex_from_syscall(sys.call(), parent.frame())
    crop(x, index = Nindex)
  }
)

#' @describeIn ImageArray-methods Layer access
#' for \code{ImageArray} objects
#'
#' @export
setMethod(
  f = "[[",
  signature = c("ImageArray", "numeric"),
  definition = function(x, i) {
    .check_level(i, x)
    x@levels[[i]]
  }
)

#' @describeIn ImageArray-methods Layer access
#' for \code{ImageArray} objects
#'
#' @export
setMethod(
  f = "[[<-",
  signature = c("ImageArray", "numeric"),
  definition = function(x, i, ..., value) {
    .check_level(i, x)
    x@levels[[i]] <- value
    x
  }
)

#' @importFrom S4Vectors coolcat
#' @noRd
setMethod(
  f = "show",
  signature = c("ImageArray"),
  definition = function(object) {
    cat(
      class(x = object),
      "Object",
      paste0(
        "(",
        paste(axes(object), collapse = ","),
        ")"
      ),
      "\n"
    )
    scales <- sprintf(
      "(%s)",
      vapply(
        object@levels,
        \(x) paste(dim(x), collapse = ","),
        character(1)
      )
    )
    S4Vectors::coolcat("Scales (%d): %s", scales)
  }
)

#' @describeIn ImageArray-methods dimensions of an ImageArray
#' @export
#' @returns dim of the first level of the ImageArray object
setMethod("dim", "ImageArray", function(x) dim(x[[1]]))

#' @describeIn ImageArray-methods dimensions of an ImageArray
#' @export
#' @returns type of ImageArray object
setMethod("type", "ImageArray", function(x) type(x[[1]]))

#' @describeIn ImageArray-methods length of an ImageArray
#' @export
#' @returns length of ImageArray object
setMethod("length", signature = "ImageArray", function(x) length(x@levels))

#' @describeIn ImageArray-methods ImageArray constructor method
#'
#' A function for creating objects of ImageArray class
#'
#' @param meta the metadata of the ImageArray object.
#' @param levels levels of the pyramid image, typically a vector of integers
#' starting with 1
#'
#' @importFrom S4Vectors new2
#' @export
#' @return An ImageArray object
ImageArray <- function(meta, levels) {
  S4Vectors::new2("ImageArray", meta = meta, levels = levels)
}

#' createBFArray
#'
#' creates an object of BFArray class
#'
#' @param image the image
#' @param series the number of series if the image supposed to be
#' pyramidal, or the the series IDs of the pyramidal image,
#' typical an integer starting from 1
#' @param resolution the resolution IDs of the pyramidal
#' image, typical an integer starting from 1
#' @param verbose verbose
#'
#' @noRd
createBFArray <- function(
  image,
  series = NULL,
  resolution = NULL,
  verbose = FALSE
) {
  # check for nulls
  if (is.null(series)) {
    series <- 1
  }
  if (is.null(resolution)) {
    resolution <- 1
  }

  # make list
  image_list <- lapply(resolution, function(res) {
    BFArray(image, series = series, resolution = res)
  })
  ImageArray(meta = list(axes = c("x", "y", "c")), levels = image_list)
}

#' createMagickArray
#'
#' creates an object of ImageArray class from magick image
#'
#' @param image the image
#' @param n.levels the number of levels of the pyramidal image,
#' typical an integer starting from 1
#' @param max.pixel.threshold the maximum width
#' and height pixel dimension that the lowest level of the image pyramid
#' should have, thus the image will be downscaled two folds until both width
#' and height is below the threshold. Default is 700 pixels.
#' If \code{n.levels} is provided, this parameter will be ignored.
#' @param verbose verbose
#'
#' @importFrom magick image_read
#' @importFrom magick image_info
#' @importFrom magick image_resize
#' @importFrom magick image_data
#' @importFrom magick geometry_size_percent
#'
#' @noRd
createMagickArray <- function(
  image,
  n.levels = NULL,
  max.pixel.threshold = 700,
  verbose = FALSE
) {
  # check image
  if (inherits(image, "bitmap")) {
    image <- magick::image_read(image)
  }

  # get image info
  image_info <- magick::image_info(image)
  dim_image <- c(image_info$width, image_info$height)

  # levels
  if (is.null(n.levels)) {
    # get image size and resolution
    image_maxsize_id <- which.max(dim_image)
    image_maxsize <- dim_image[image_maxsize_id]

    # get number of levels
    # how many levels of power of 2 required to
    # get a maximum pixel size of 700 on either width or height
    n.levels <- ceiling(log2(image_maxsize / max.pixel.threshold)) + 1
  } else if (n.levels < 1) {
    stop("'n.levels' has to be 1 or a larger integer value!")
  }

  # create image levels
  if (verbose) {
    .img_create_msg(dim(image), 1)
  }
  image_data <- magick::image_data(image, channels = "rgb")
  storage.mode(image_data) <- "integer"
  image_list <- list(DelayedArray::DelayedArray(as.array(image_data)))
  if (n.levels > 1) {
    cur_image <- image
    for (i in 2:n.levels) {
      dim_image <- ceiling(dim_image / 2)
      if (verbose) {
        .img_create_msg(dim_image, 1)
      }
      cur_image <- magick::image_resize(
        cur_image,
        geometry = magick::geometry_size_percent(50),
        filter = "Gaussian"
      )
      image_data <- magick::image_data(cur_image, channels = "rgb")
      storage.mode(image_data) <- "integer"
      image_list[[i]] <-
        DelayedArray::DelayedArray(as.array(image_data))
    }
  }

  # return
  ImageArray(meta = list(axes = c("c", "x", "y")), levels = image_list)
}

#' createMagickArray
#'
#' creates an object of ImageArray class from magick image
#'
#' @param image the image
#' @param n.levels the number of levels of the pyramidal image,
#' typical an integer starting from 1
#' @param max.pixel.threshold the maximum width
#' and height pixel dimension that the lowest level of the image pyramid
#' should have, thus the image will be downscaled two folds until both width
#' and height is below the threshold. Default is 700 pixels.
#' If \code{n.levels} is provided, this parameter will be ignored.
#' @param verbose verbose
#'
#' @importFrom EBImage readImage
#' @importFrom EBImage resize
#'
#' @noRd
createEBImageArray <- function(
  image,
  n.levels = NULL,
  max.pixel.threshold = 700,
  verbose = FALSE
) {
  # get and image info
  image_info <- dim(image)
  dim_image <- c(image_info[1], image_info[2])

  # levels
  if (is.null(n.levels)) {
    # get image size and resolution
    image_maxsize_id <- which.max(dim_image)
    image_maxsize <- dim_image[image_maxsize_id]

    # get number of levels
    # how many levels of power of 2 required to
    # get a maximum pixel size of 700 on either width or height
    n.levels <- ceiling(log2(image_maxsize / max.pixel.threshold)) + 1
  } else if (n.levels < 1) {
    stop("'n.levels' has to be 1 or a larger integer value!")
  }

  # check dim
  .check_dim(image)

  # create image levels
  meta <- list(axes = c("x", "y", "c"))
  if (verbose) {
    .img_create_msg(dim_image, 1)
  }
  img_perm <- if (length(dim(image)) == 2) c(1, 2) else c(1, 2, 3)
  meta[["axes"]] <- meta[["axes"]][img_perm]
  img_perm <- stats::setNames(img_perm, meta[["axes"]])
  img <- aperm(image, img_perm)
  image_list <- list(DelayedArray::DelayedArray(img))
  if (n.levels > 1) {
    cur_image <- image
    for (i in 2:n.levels) {
      dim_image <- ceiling(dim_image / 2)
      if (verbose) {
        .img_create_msg(dim_image, i)
      }
      cur_image <- EBImage::resize(
        cur_image,
        w = dim_image[1],
        h = dim_image[2]
      )
      cur_img <- aperm(cur_image, img_perm)
      image_list[[i]] <-
        DelayedArray::DelayedArray(cur_img)
    }
  }

  # return
  ImageArray(meta = meta, levels = image_list)
}

#' createImageArray
#'
#' creates an object of ImageArray class
#'
#' @param image the image
#' @param n.levels the number of levels of the pyramidal image,
#' typical an integer starting from 1
#' @param series the series IDs of the pyramidal image,
#' typical an integer starting from 1.
#' @param resolution the resolution IDs of the pyramidal image,
#' typical an integer starting from 1.
#' @param max.pixel.threshold the maximum width
#' and height pixel dimension that the lowest level of the image pyramid
#' should have, thus the image will be downscaled two folds until both width
#' and height is below the threshold. Default is 700 pixels.
#' If \code{n.levels} is provided, this parameter will be ignored.
#' @param engine the package to use for each image layer: either
#' \code{EBImage} or \code{magick-image}
#' @param verbose verbose
#'
#' @importFrom methods new
#' @importFrom DelayedArray DelayedArray
#'
#' @export
#' @return An ImageArray object
#'
#' @examples
#' # get image
#' library(EBImage)
#' img.file <- system.file("images", "sample.png", package="EBImage")
#'
#' # create ImageArray
#' imgarray <- createImageArray(img.file, n.levels = 3)
#' imgarray_raster <- as.raster(imgarray, max.pixel.size = 300)
#' plot(imgarray_raster)
#'
createImageArray <- function(
  image,
  n.levels = NULL,
  series = NULL,
  resolution = NULL,
  max.pixel.threshold = 700,
  engine = "EBImage",
  verbose = FALSE
) {
  # convert to bitmap array if integer
  if (is.integer(image)) {
    if (engine == "magick-image") {
      image <- array(as.raw(image), dim = c(3, 2, 1))
    }
    image <- read_image(image, engine = engine)
  }

  # create ImageArray from magick
  if (inherits(image, c("magick-image", "bitmap"))) {
    return(createMagickArray(
      image,
      n.levels = n.levels,
      max.pixel.threshold = max.pixel.threshold,
      verbose = verbose
    ))
  }

  # create ImageArray from EBImage
  if (inherits(image, c("Image"))) {
    return(createEBImageArray(
      image,
      n.levels = n.levels,
      max.pixel.threshold = max.pixel.threshold,
      verbose = verbose
    ))
  }

  # check image format
  if (inherits(image, "character")) {
    if (grepl(".ome.tiff$|.ome.tif$|.qptiff$|.qptif$", image)) {
      image <- createBFArray(image, series = series, resolution = resolution)
    } else {
      image <- read_image(image, engine = engine)
      if (inherits(image, "magick-image")) {
        createMagickArray(
          image,
          n.levels = n.levels,
          max.pixel.threshold = max.pixel.threshold,
          verbose = verbose
        )
      } else if (inherits(image, "Image")) {
        createEBImageArray(
          image,
          n.levels = n.levels,
          max.pixel.threshold = max.pixel.threshold,
          verbose = verbose
        )
      }
    }
  }
}

#' writeImageArray
#'
#' Writing image arrays on disk
#'
#' @param image an Image object (EBImage), a magick object or the path
#' to an image file,
#' @param output output file name
#' @param name name of the group
#' @param format on disk format, either "h5" for HDF5 format, "zarr" for
#' zarr format, or "in-memory" for in-memory ImageArray object.
#' If not provided, the format will be inferred from the file extension of
#' the output path.
#' @param replace Should the existing file be
#' removed or not
#' @param n.levels the number of levels if the image supposed to be
#' pyramidal.
#' @param chunkdim The dimensions of the chunks
#' to use for writing the data to disk.
#' @param level The compression level to use for
#' writing the data to disk.
#' @param engine the package to use for each image layer: either
#' \code{EBImage} or \code{magick-image}
#' @param verbose verbose
#' @param ... additional parameters passed to
#' \link[ImageArray]{createImageArray}.
#'
#' @importFrom HDF5Array writeHDF5Array
#' @importFrom ZarrArray writeZarrArray
#' @importFrom rhdf5 h5createFile h5createGroup
#' @importFrom tools file_ext
#' @import DelayedArray
#'
#' @export
#' @returns An ImageArray object
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
#' imgarray_raster <- as.raster(imgarray)
#' plot(imgarray_raster)
#'
writeImageArray <- function(
  image,
  output = "my_image",
  name = "",
  format = NULL,
  replace = FALSE,
  n.levels = NULL,
  chunkdim = NULL,
  level = NULL,
  engine = "EBImage",
  verbose = FALSE,
  ...
) {
  # verbose
  verbose <- DelayedArray:::normarg_verbose(verbose)

  # make Image Array
  if (!inherits(image, "ImageArray")) {
    image_list <- createImageArray(
      image,
      n.levels = n.levels,
      verbose = verbose,
      engine = engine
    )
  } else {
    image_list <- image
  }
  
  # create or replace output folder
  if (!.isTRUEorFALSE(replace)) {
    stop("'replace' must be TRUE or FALSE")
  }

  # check format
  fileext <- tools::file_ext(output)
  if (is.null(format)){
    format <- fileext
  } else {
    if(format == "hdf5") format <- "h5"
    if(fileext != format && format != "in-memory") {
      warning(
        sprintf(
          paste(
            "The file extension of the output path%s does", 
            "not match the specified format (%s),", 
            "The object will be written as (%s).", 
            sep = " "
          ),
          if (fileext == "") "" else paste0(" (", fileext, ")"),
          format,
          format
        )
      )
    }
  }
  if (!format %in% .FORMATS) {
    stop(
      sprintf(
        "Invalid format: %s. Currently supported formats are %s.",
        format,
        toString(sprintf('"%s"', .FORMATS))
      )
    )
  }
  
  # remove files or folders if needed
  if (replace && format != "in-memory") {
    unlink(output, recursive = TRUE)
  }

  # open ondisk store
  switch(
    format,
    h5 = {
      if (!file.exists(output)) {
        rhdf5::h5createFile(output)
      }
      # TODO: is there a better way to check existing groups
      if (!name %in% c("", "/")) {
        rhdf5::h5createGroup(output, group = name)
      }
    },
    zarr = {
      if (!dir.exists(output)) {
        create_zarr(store = output)
      }
      if (!name %in% c("", "/")) {
        create_zarr_group(output, name)
      }
    },
    `in-memory` = {
      message(
        "The format is defined as 'in-memory', thus ImageArray will be saved ", 
        "to memory and the output path will be ignored."
      )
    }
  )

  # write all levels
  ax <- axes(image_list)
  for (i in seq_along(image_list@levels)) {
    img <- image_list[[i]]

    # write array
    switch(
      format,
      h5 = {
        image_list[[i]] <-
          HDF5Array::writeHDF5Array(
            img,
            filepath = output,
            name = paste0(name, "/", i),
            chunkdim = chunkdim,
            level = level,
            as.sparse = FALSE,
            with.dimnames = FALSE,
            verbose = verbose
          )
      },
      zarr = {
        chunk_dim <- stats::setNames(dim(img), ax)
        chunk_dim["x"] <- min(chunk_dim["x"], 2000)
        chunk_dim["y"] <- min(chunk_dim["y"], 2000)
        image_list[[i]] <-
          ZarrArray::writeZarrArray(
            img,
            zarr_path = file.path(output, name, i),
            chunkdim = chunk_dim
          )
      },
      "in-memory" = {
        image_list[[i]] <- img
      }
    )
  }

  # return
  image_list
}
