####
# Methods ####
####

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
#' crop
#' crop,ImageArray-method
#' negate
#' negate,ImageArray-method
#' modulate
#' modulate,ImageArray-method
#' meta
#' meta,ImageArray-method
#' axes
#' axes,ImageArray-method
#' axes<-
#' axes<-,ImageArray-method
#' scales
#' scales,ImageArray-method
#' scales<-
#' scales<-,ImageArray-method
#' realize
#' realize,ImageArray-method
#' as.raster
#' as.raster,ImageArray-method
#' path
#' path,ImageArray-method
#' extent
#' extent,ImageArray-method
#'
#' @examples
#' # get image
#' library(EBImage)
#' img.file <- system.file("images", "sample.png", package="EBImage")
#'
#' # create ImageArray
#' imgarray <- ImageArray(img.file, n.levels = 3)
#'
#' # access layers
#' imgarray[[1]]
#' imgarray[[2]]
#'
#' # dimensions and length
#' dim(imgarray)
#' length(imgarray)
#' 
#' # extent
#' extent(imgarray)
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
#' # path
#' path(imgarray)
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

#' @describeIn ImageArray-methods get axes metadata of the ImageArray object
#' @exportMethod axes
setMethod("axes", "ImageArray", function(object)
  .get_axes_from_scales(scales(object)))

#' @describeIn ImageArray-methods replace axes metadata of the ImageArray
#' object, the replacement can only be a permutation of the existing axes
#' @exportMethod axes<-
setReplaceMethod("axes", "ImageArray", function(object, ..., value){
  ax <- axes(object)
  
  # check axes, should be a permutation
  if(!is.character(value) || length(value) != length(ax) ||
     anyDuplicated(value) || !setequal(value, ax))
    stop("axes can only be replaced by a permutation of the existing axes: ",
         paste(ax, collapse = ","), "!")

  # update axes
  scales(object) <- lapply(scales(object), \(.) .[value])
  object
})

#' @describeIn ImageArray-methods get scales metadata of the ImageArray object
#' @exportMethod scales
setMethod("scales", "ImageArray", function(object) object@scales)

#' @describeIn ImageArray-methods replace scales metadata of the ImageArray
#' object, each vector should be named by a permutation of the existing axes
#' @importFrom methods validObject
#' @exportMethod scales<-
setReplaceMethod("scales", "ImageArray", function(object, ..., value) {
  if(!is.list(value)) stop("scales must be a list!")

  # the axes of an ImageArray object can only be permuted, all remaining
  # checks are done by the validity of the class below
  ax <- axes(object)
  for(s in value){
    if(is.null(names(s)) || length(s) != length(ax) ||
       anyDuplicated(names(s)) || !setequal(names(s), ax))
      stop("names of each vector in scales should be a permutation of ",
           "the existing axes: ", paste(ax, collapse = ","), "!")
  }

  object@scales <- value
  methods::validObject(object)
  object
})

####
# Create/Write ####
####

#' createListFromBFPath
#'
#' creates an object of BFArray class
#'
#' @param image a BFPath object
#' @param series the number of series if the image supposed to be
#' pyramidal, or the the series IDs of the pyramidal image,
#' typical an integer starting from 1
#' @param resolution the resolution IDs of the pyramidal
#' image, typical an integer starting from 1
#' @param verbose verbose
#'
#' @noRd
createListFromBFPath <- function(
  image,
  axes = NULL,
  scales = NULL,
  n.levels = NULL,
  max.pixel.threshold,
  verbose = FALSE
) {
  # make list
  image_list <- lapply(resolution(image), function(res) {
    BFArray(
      BFPath(path(image), series = series(image), resolution = res)
    )
  })
  return(list(levels = image_list, 
         axes = tolower(axes(image_list[[1]]))))
}

#' createListFromMagick
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
createListFromMagick <- function(
  image,
  axes = NULL,
  scales = NULL,
  n.levels = NULL,
  max.pixel.threshold,
  verbose = FALSE
) {
  # get axes, magick only accepts XY or CXY
  image_data <- magick::image_data(image)
  axes <- .check_axes(image_data, axes = axes, engine = "magick-image")
  temp_axes <- c(axes, if(!"c" %in% axes) "c" else NULL)
  img_perm_forward <- match(temp_axes, .MAGICK_AXES)
  img_perm_backward <- match(.MAGICK_AXES, temp_axes)
  
  # check image
  if (inherits(image, "bitmap"))
    image <- magick::image_read(image)

  # get image info
  image_info <- magick::image_info(image)
  dim_image <- c(image_info$width, image_info$height)

  # number of levels
  scales <- .check_scales(scales,
                          axes,
                          dim_image,
                          n.levels, 
                          max.pixel.threshold)
  
  # remaining levels
  if (verbose) .img_create_msg(dim_image, 1)
  storage.mode(image_data) <- "integer"
  new_dim <- setNames(dim(image_data), .MAGICK_AXES)
  image_data <- as.array(image_data)
  image_data <- array(image_data, dim = unname(new_dim[axes]))
  image_list <- list(DelayedArray::DelayedArray(image_data))
  if (length(scales) > 1) {
    cur_image <- image
    for (i in 2:length(scales)) {
      sc <- .magick_resize_scale(dim_image, scales[[i]])
      if (verbose) .img_create_msg(sc, i)
      cur_image <- magick::image_resize(
        cur_image,
        geometry = sc,
        filter = "Gaussian"
      )
      image_data <- as.array(magick::image_data(cur_image))
      storage.mode(image_data) <- "integer"
      image_data <- aperm(image_data, perm = img_perm_forward)
      new_dim <- setNames(dim(image_data), temp_axes)
      image_data <- array(image_data, dim = unname(new_dim[axes]))
      image_list[[i]] <-
        DelayedArray::DelayedArray(image_data)
    }
  }

  # return
  return(list(levels = image_list, axes = axes))
}

#' createListFromMagick
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
createListFromEBImage <- function(
  image,
  axes = NULL,
  scales = NULL,
  n.levels = NULL,
  max.pixel.threshold,
  verbose = FALSE
) {
  
  # get axes, EBImage accepts images starting with XY
  axes <- .check_axes(image, axes = axes)
  EBImage_axes <- c("x", "y", axes[!axes %in% c("x", "y")])
  img_perm_forward <- match(axes, EBImage_axes)
  img_perm_backward <- match(EBImage_axes, axes)
  
  # get and image info
  image_info <- setNames(dim(image), axes)
  dim_image <- c(image_info["x"], image_info["y"])

  # number of levels
  scales <- .check_scales(scales,
                          axes,
                          dim_image,
                          n.levels, 
                          max.pixel.threshold)

  # create image levels
  if (verbose) .img_create_msg(dim_image, 1)
  image_list <- list(DelayedArray::DelayedArray(as.array(image)))
  if (length(scales) > 1) {
    cur_image <- aperm(image, perm = img_perm_forward)
    for (i in 2:length(scales)) {
      if (verbose) .img_create_msg(dim_image, i)
      cur_image <- EBImage::resize(
        cur_image,
        w = dim_image["x"]*scales[[i]]["x"],
        h = dim_image["y"]*scales[[i]]["y"]
      )
      cur_img <- aperm(cur_image, perm = img_perm_backward)
      image_list[[i]] <-
        DelayedArray::DelayedArray(cur_img)
    }
  }

  # return
  return(list(levels = image_list, axes = axes))
}

createListFromList <- function(image,
                               axes = NULL,
                               scales = NULL,
                               n.levels = NULL, 
                               max.pixel.threshold,
                               engine = "EBImage",
                               verbose = FALSE){
  
  # check arrays
  img_dims <- lapply(image, function(img){
    if(!is.array(img) && !is(img, "Array"))
      stop("Each element of the list should be an array or Array object")
    length(dim(img))
  })
  all_equal <- all(
    vapply(img_dims, identical, logical(1), length(dim(image[[1L]])))
  )
  if(!all_equal)
    stop("All images must have identical dimensions")
  
  # check axes
  axes <- lapply(image, \(.) .check_axes(., axes, engine = engine))
  all_equal <- all(vapply(axes, identical, logical(1), axes[[1L]]))
  if(!all_equal)
    stop("All images must have identical axes")
  
  return(list(levels = image, axes = axes[[1L]]))
}

#' @noRd
setMethod("createImageList", "magick_class", createListFromMagick)

#' @noRd
setMethod("createImageList", "Image", createListFromEBImage)

#' @noRd
setMethod("createImageList", "BFPath", createListFromBFPath)

#' @noRd
setMethod("createImageList", "list", createListFromList)

#' ImageArray
#'
#' creates an object of ImageArray class
#'
#' @param image a path to a file, a single array or a list of arrays
#'  containing the pixel intensities of an image.
#' @eval paste0("@param axes a character vector of axes names for images. 
#'  Should be a subset of ", deparse(.AXES))
#' @param n.levels the number of levels of the pyramidal image,
#'  typical an integer starting from 1. Will be ignored if \code{scales} is 
#'  provided
#' @param max.pixel.threshold the maximum width
#'  and height pixel dimension that the lowest level of the image pyramid
#'  should have, thus the image will be downscaled two folds until both width
#'  and height is below the threshold. Default is 700 pixels.
#'  Will be ignored if \code{n.levels} is provided
#' @param scales a list of named numeric vectors where names are a  
#'  subset of \code{axes} and values are associated with scales 
#'  of these axes. See \url{https://ngff.openmicroscopy.org/} for more 
#'  information. When provided, \code{axes}, \code{n.levels} and 
#'  \code{max.pixel.threshold} will be overridden.  
#' @param engine the package to use for each image layer: either
#'  \code{EBImage} or \code{magick-image}
#' @param series the series IDs of the pyramidal image,
#'  typical an integer starting from 1.
#' @param resolution the resolution IDs of the pyramidal image,
#'  typical an integer starting from 1.
#' @param verbose verbose
#'
#' @name ImageArray
#' @rdname ImageArray
#' 
#' @aliases 
#' createImageArray
#' createImageArray,ImageArray-method
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
#' imgarray <- ImageArray(img.file, n.levels = 3)
#' imgarray_raster <- as.raster(imgarray, max.pixel.size = 300)
#' plot(imgarray_raster)
ImageArray <- function(
    image,
    axes = NULL, 
    n.levels = NULL,
    max.pixel.threshold = 700,
    scales = NULL,
    engine = "EBImage",
    series = NULL,
    resolution = NULL,
    verbose = FALSE
) {
  
  # overwrite axes if scales are given
  if(!is.null(scales))
    axes <- .get_axes_from_scales(scales)
  
  # create ImageArray from file path
  if (inherits(image, "character")) {
    pyramid_formats <- paste0(gsub("\\.", "\\\\.", .PYRAMID_FORMATS), 
                              "$", collapse = "|")
    if (grepl(pyramid_formats, image, ignore.case = TRUE)) {
      image <- BFPath(image, series, resolution)
    } else {
      image <- read_image(image, engine = engine)
    }
    
  # read arrays as magick or EBImage
  } else if(is.array(image)){
    axes <- .check_axes(image, axes = axes, engine = engine)
    image <- read_image(image, axes = axes, engine = engine)
  } 
  
  # read image list
  image <- createImageList(
    image,
    axes = axes,
    scales = scales,
    n.levels = n.levels,
    max.pixel.threshold = max.pixel.threshold,
    verbose = verbose
  )
  
  # construct ImageArray object
  image$levels <- S4Vectors:::new_SimpleList_from_list("ImageList", 
                                                       image$levels)
  
  # get scales from list if not provided
  if(is.null(scales))
    scales <- .get_scales_from_levels(image$levels, image$axes)
  
  # create class
  S4Vectors::new2(
    "ImageArray", 
    levels = image$levels,
    scales = scales
  )
}

#' @describeIn ImageArray deprecated function
#' @export
createImageArray <- function(
    image,
    n.levels = NULL,
    series = NULL,
    resolution = NULL,
    max.pixel.threshold = max.pixel.threshold,
    engine = "EBImage",
    verbose = FALSE
) {
  warning("'createImageArray' function is deprecated. ", 
          "Please use 'ImageArray' instead!")
  ImageArray(image,
             n.levels = NULL,
             max.pixel.threshold = max.pixel.threshold,
             engine = "EBImage",
             series = NULL,
             resolution = NULL,
             verbose = FALSE)
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
#' @param chunkdim The dimensions of the chunks
#' to use for writing the data to disk.
#' @param level The compression level to use for
#' writing the data to disk.
#' @param verbose verbose
#' @param ... additional parameters passed to \link[ImageArray]{ImageArray}.
#'
#' @importFrom HDF5Array writeHDF5Array
#' @importFrom ZarrArray writeZarrArray
#' @importFrom rhdf5 h5createFile h5createGroup
#' @importFrom Rarr write_zarr_group
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
  chunkdim = NULL,
  level = NULL,
  verbose = FALSE,
  ...
) {
  # verbose
  verbose <- DelayedArray:::normarg_verbose(verbose)

  # make Image Array
  if (!inherits(image, "ImageArray")) {
    image_list <- ImageArray(
      image,
      verbose = verbose,
      ...
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
        "The file extension of the output path", 
        if (fileext == "") "" else sprintf(" '%s'", fileext),
        " does not match the specified format (", format, "). ", 
        "The object will be saved as ", format, " format. "
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
        Rarr::write_zarr_group(zarr_path = output, 
                               group = "", 
                               zarr_version = 2L)
      }
      if (!name %in% c("", "/")) {
        Rarr::write_zarr_group(zarr_path = output, 
                               group = name, 
                               zarr_version = 2L)
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

####
# utils ####
####


#' read_image
#' 
#' read an image using magick or EBImage
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
setMethod("read_image", 
          "character", 
          function(image, engine = "EBImage") {
  switch(
    engine,
    `magick-image` = magick::image_read(image),
    `EBImage` = EBImage::readImage(image)
  )
})

#' @describeIn read_image read image
setMethod("read_image", 
          "array", 
          function(image, axes = NULL, engine = "EBImage") {
  switch(
    engine,
    `magick-image` = magick::image_read(.as_magick_bitmap(image, axes)),
    `EBImage` = EBImage::Image(image)
  )
})

#' @describeIn read_image read image
setMethod("read_image", 
          "bitmap", 
          function(image, axes = NULL, engine) {
  magick::image_read(image)
})

.as_magick_bitmap <- function(x, axes = NULL) {
  d <- dim(x)
  msg <- "Expected an (x, y) or (c, x, y) image with 1,3,4 channels."
  
  # guess axes if not provided
  if(is.null(axes))
    axes <- .check_axes(x, axes = axes, engine = "magick-image")
  
  # get permutation to read as magick
  if(!length(axes) %in% c(2L, 3L) || !length(d) %in% c(2L, 3L)) 
    stop(msg, call. = FALSE)
  magick_axes <- c(if(length(d) == 3L) "c" else NULL, "x", "y")
  img_perm <- match(magick_axes, axes)
  x <- aperm(x, perm = img_perm)
  
  # add extra dimension if two dimensional
  if (length(d) == 2L)
    dim(x) <- c(1L, d)
  
  # check dimension
  if (length(dim(x)) != 3L || !dim(x)[1L] %in% c(1,3,4))
    stop(
      "Expected an (x, y) or (c, x, y) image with 1,3,4 channels.",
      call. = FALSE
    )
  
  # conversions
  if (is.logical(x))
    x <- x * 255L
  if (!is.raw(x)) {
    if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)))
      stop("Pixel values must be finite.", call. = FALSE)
    
    if (is.double(x) && all(x >= 0 & x <= 1))
      x <- round(x * 255)
    
    if (any(x < 0 | x > 255))
      stop("Pixel values must be in [0, 1] or [0, 255].", call. = FALSE)
    
    storage.mode(x) <- "raw"
  }
  
  x
}

#' @keywords internal
#' @noRd
.magick_resize_scale <- function(dim_img, scales){
  paste0(paste(round(dim_img*scales[c("x", "y")]), collapse = "x"),"!")
}


#' @keywords internal
#' @noRd
.get_scales_from_levels <- function(levels, axes){
  msg <- "axes length does not match the dim of the array"
  d <- dim(levels[[1]])
  if(length(d) != length(axes)) stop(msg)
  first_dim <- setNames(dim(levels[[1]]),axes)
  scaled_axes <- intersect(c("x", "y", "z"), axes)
  lapply(levels, \(.){
    d <- dim(.)
    if(length(d) != length(axes)) stop(msg)
    d <- setNames(d, axes)
    sc <- .TEMPLATE_SCALES[axes]
    sc[scaled_axes] <- d[scaled_axes]/first_dim[scaled_axes]
    sc
  })
}

#' @keywords internal
#' @noRd
.get_scales_from_nlevels <- function(n.levels, axes) {
  if(!all(c("x", "y") %in% axes))
    stop("axes should have at least x and y dimensions!")
  lapply(seq_len(n.levels), \(i){
    ax <- .TEMPLATE_SCALES[axes]
    ax[c("x", "y")] <- ax[c("x", "y")] / 2^(i-1)
    ax
  })
}

#' @keywords internal
#' @noRd
.get_axes_from_scales <- function(scales){
  if(!is.list(scales))
    stop("scales must be a list!")
  if(length(scales) < 1)
    stop("scales should have at least one vector of axes scales")
  
  for(sc in scales){
    if(is.null(names(sc))) stop("Each vector in scales should be named!")
  }
  names(scales[[1]])
}

#' @keywords internal
#' @noRd
.check_axes <- function(
    image,
    axes = NULL, 
    engine = "EBImage"
) {
  # We can guess axes for images, labels if 2D (with/without channels)
  d <- dim(image)
  ndim <- length(d)
  if (is.null(axes)) {
    if (ndim %in% c(2, 3)) {
      if(inherits(image, "bitmap")){
        axes <- c("c", "x", "y") 
      } else {
        axes <- switch(
          engine,
          `magick-image` = c(if (ndim == 3) "c" else NULL, "x", "y"),
          `EBImage` = c("x", "y", if (ndim == 3) "c" else NULL)
        ) 
      }
    } else {
      stop(
        "axes must be provided. Can't be guessed beyond 2D images ",
        "(or 3D with channels)!",
        call. = FALSE
      )
    }
  }
  
  # should have XY at least
  if(!all(c("x", "y") %in% axes))
    stop("axes should include at least both x and y dimensions!")
  
  # axes length should match # of dim, also consider the image is bitmap
  if(inherits(image, "bitmap")){
    if(!any(axes %in% .MAGICK_AXES))
      stop("axes should have at least xy or cxy dimensions ", 
           "when used with magick!")
  } else if (!is.null(ndim) && length(axes) != ndim) {
    stop(
      sprintf(
        "axes length (%d) must match number of dimensions (%d)",
        length(axes),
        ndim
      ),
      call. = FALSE
    )
  }
  
  # invalid axes
  diff_axes <- setdiff(axes, .AXES)
  if (length(diff_axes)) {
    stop("Some axes are invalid: ", paste(diff_axes, collapse = ","))
  }
  
  # check magick axes
  other_axes <- setdiff(axes, .MAGICK_AXES)
  if(engine == "magick-image" && length(other_axes) > 1)
    stop("magick images should have only the c, x, and y axes!")
  
  # duplicated axes
  ind_dup <- which(table(axes) > 1)
  if (length(ind_dup)) {
    stop(
      "Duplicated axes are detected: ",
      paste(names(ind_dup), collapse = ",")
    )
  }
  
  axes
}

#' @keywords internal
#' @noRd
.check_scales <- function(scales = NULL,
                          axes,
                          dim_image,
                          n.levels = NULL, 
                          max.pixel.threshold){
  
  # if scales are given, return
  if(!is.null(scales)) {
    scales
  } else {
    # get number of levels
    # how many levels of power of 2 required to
    # get a maximum pixel size of 700 on either width or height
    if (is.null(n.levels)) {
      image_maxsize_id <- which.max(dim_image)
      image_maxsize <- dim_image[image_maxsize_id]
      log2_ratio <- log2(image_maxsize / max.pixel.threshold)
      n.levels <- pmax(0, ceiling(log2_ratio)) + 1
      # if provided, should be an integer
    } else if (n.levels < 1 || n.levels %% 1 != 0) {
      stop("'n.levels' has to be 1 or a larger integer value!")
    }
    .get_scales_from_nlevels(n.levels, axes)
  }
}