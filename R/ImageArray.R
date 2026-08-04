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

####
# Create/Write ####
####

#' createListFromBFPath
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
createListFromBFPath <- function(
  image,
  axes = NULL,
  n.levels = NULL,
  max.pixel.threshold = 700,
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

  # get axes, EBImage accepts XY or XYC
  axes <- c("c", "x", "y")
  if (verbose) {
    .img_create_msg(dim(image), 1)
  }
  
  # create image levels
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

  # get axes, EBImage accepts XY or XYC
  axes <- c("x", "y", "c")
  if (verbose) .img_create_msg(dim_image, 1)
  img_perm <- if (length(dim(image)) == 2) c(1, 2) else c(1, 2, 3)
  axes <- axes[img_perm]
  img_perm <- stats::setNames(img_perm, axes)
  img <- aperm(image, img_perm)
  
  # create image levels
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
  return(list(levels = image_list, axes = axes))
}

createListFromList <- function(image,
                               axes = NULL,
                               n.levels = NULL, 
                               max.pixel.threshold = 700,
                               engine = engine,
                               verbose = FALSE){
  
  # check arrays
  img_dims <- lapply(image, function(img){
    if(!is.array(img))
      stop("Each element of the list should be an array")
    length(dim(img))
  })
  all_equal <- all(
    vapply(img_dims, identical, logical(1), length(dim(image[[1L]])))
  )
  if(!all_equal)
    stop("All images must have identical dimensions")
  
  # check axes
  axes <- lapply(image, \(.) .guess_axes(., axes, engine = engine))
  all_equal <- all(vapply(axes, identical, logical(1), axes[[1L]]))
  if(!all_equal)
    stop("All images must have identical axes")
  
  return(list(levels = image, axes = axes[[1L]]))
}
  
setOldClass("magick-image")
setOldClass("bitmap")
setClassUnion(c("magick_class"), 
              c("magick-image", "bitmap"))

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
#'  typical an integer starting from 1
#' @param max.pixel.threshold the maximum width
#'  and height pixel dimension that the lowest level of the image pyramid
#'  should have, thus the image will be downscaled two folds until both width
#'  and height is below the threshold. Default is 700 pixels.
#'  If \code{n.levels} is provided, this parameter will be ignored.
#' @param scales a list of named numeric vectors where names are a  
#'  subset of \code{axes} and values are associated with scales 
#'  of these axes. See \link{https://ngff.openmicroscopy.org/} for more 
#'  information. When provided, \code{axes} will be overwritten. 
#' @param engine the package to use for each image layer: either
#'  \code{EBImage} or \code{magick-image}
#' @param series the series IDs of the pyramidal image,
#'  typical an integer starting from 1.
#' @param resolution the resolution IDs of the pyramidal image,
#'  typical an integer starting from 1.
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
  
  # override axes if scale is given
  if(!is.null(scales))
    axes <- .get_axes_from_scales(scales)
  
  # create ImageArray from file path
  if (inherits(image, "character")) {
    if (grepl(".ome.tiff$|.ome.tif$|.qptiff$|.qptif$", image)) {
      image <- BFPath(image, series, resolution)
    } else {
      image <- read_image(image, engine = engine)
    }
  # read arrays as magick or EBImage
  } else if(is.array(image)){
    # guess the axes for arrays
    image <- read_image(image, engine = engine)
  } 
  
  # read image list
  image <- createImageList(
    image,
    axes = axes,
    n.levels = n.levels,
    max.pixel.threshold = max.pixel.threshold,
    verbose = verbose
  )
  
  # construct ImageArray object
  image$levels <- S4Vectors:::new_SimpleList_from_list("ImageList", 
                                                       image$levels)
  if(is.null(scales))
    scales <- .get_scales(image$levels, image$axes)
  S4Vectors::new2(
    "ImageArray", 
    levels = image$levels,
    axes = image$axes,
    scales = scales
  )
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
#' \link[ImageArray]{ImageArray}.
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
    image_list <- ImageArray(
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

####
# Utils ####
####


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
setMethod("read_image", "character", function(image, engine = "EBImage") {
  switch(
    engine,
    `magick-image` = magick::image_read(image),
    `EBImage` = EBImage::readImage(image)
  )
})

#' @describeIn read_image read image
#' @exportMethod read_image
setMethod("read_image", "array", function(image, engine = "EBImage") {
  # read array
  switch(
    engine,
    `magick-image` = magick::image_read(.as_magick_bitmap(image)),
    `EBImage` = EBImage::Image(image)
  )
})

#' @describeIn read_image read image
#' @exportMethod read_image
setMethod("read_image", "bitmap", function(image, engine) {
  magick::image_read(image)
})

.as_magick_bitmap <- function(x) {
  d <- dim(x)
  
  if (length(d) == 2L)
    dim(x) <- c(1L, d)
  
  if (length(dim(x)) != 3L || !dim(x)[1L] %in% c(1,3,4))
    stop(
      "Expected an (x, y) or (c, x, y) image with 1,3,4 channels.",
      call. = FALSE
    )
  
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
.get_scales <- function(levels, axes){
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
.get_axes_from_scales <- function(scales){
  if(!is.list(scales))
    stop("scales must be a list!")
  for(sc in scales){
    if(is.null(names(sc))) stop("Each vector in scales should be named!")
  }
  names(scales[[1]])
}

#' @keywords internal
#' @noRd
.guess_axes <- function(
    image,
    axes = NULL, 
    engine = "EBImage"
) {
  # We can guess axes for images, labels if 2D (with/without channels)
  ndim <- length(dim(image))
  if (is.null(axes)) {
    if (ndim %in% c(2, 3)) {
      axes <- switch(
        engine,
        `magick-image` = c(if (ndim == 3) "c" else NULL, "x", "y"),
        `EBImage` = c("x", "y", if (ndim == 3) "c" else NULL)
      )
    } else {
      stop(
        "axes must be provided. Can't be guessed beyond 2D images ",
        "(or 3D with channels)!",
        call. = FALSE
      )
    }
  } else {
    if (is.character(axes) && length(axes) == 1L) {
      axes <- strsplit(axes, "", fixed = TRUE)[[1]]
    }
    if (length(axes) != ndim) {
      stop(
        sprintf(
          "axes length (%d) must match number of dimensions (%d)",
          length(axes),
          ndim
        ),
        call. = FALSE
      )
    }
  }
  
  # axes length should match # of dim
  if (!is.null(ndim) && length(axes) != ndim) {
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