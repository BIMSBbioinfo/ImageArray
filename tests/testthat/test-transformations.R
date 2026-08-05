library(magick)
library(HDF5Array)
library(ZarrArray)

# images
ome.tiff.file <- system.file("extdata", "xy_12bit__plant.ome.tiff",
                             package = "ImageArray")
f <- system.file("images", "sample.png", package = "EBImage")

# array list
img_list <- list()

# OME TIFF
imgarray <- ImageArray(ome.tiff.file, series = 1, resolution = 1:2)
img_list[["tiff"]] <- imgarray

# HDF5
img <- image_read(f)
output_h5 <- tempfile(fileext = ".h5")
imgarray <- writeImageArray(img, output = output_h5)
img_list[["hdf5"]] <- imgarray

# Zarr
img <- image_read(f)
output_h5 <- tempfile(fileext = ".zarr")
imgarray <- writeImageArray(img, output = output_h5)
img_list[["zarr"]] <- imgarray

for(ni in names(img_list)) {
  
  imgarray <- img_list[[ni]]
  xy <- match(c("x", "y"), axes(imgarray))
  
  test_that(paste0("translation transformation for ", ni), {
    
    # translate image
    shift <- c(100,20)
    imgarray_trans <- translation(imgarray, shift = shift)
    
    # check seed
    expect_s4_class(imgarray_trans, "ImageArray")
    for(i in seq_along(imgarray_trans)){
      expect_s4_class(imgarray_trans[[i]]@seed, "DelayedTranslateSeed")
      expect_s4_class(imgarray_trans[[i]]@seed, "DelayedUnaryOp")
    }
    
    # check extent
    expect_equal(
      extent(imgarray_trans),
      mapply(\(i,j) i+j, extent(imgarray), shift, SIMPLIFY = FALSE)
    )
  })
  
  test_that(paste0("scaling transformation for ", ni), {

    # scale image
    output.dim <- c(200,300)
    imgarray_scale <- scale(imgarray, output.dim = output.dim)
    
    # check seed
    expect_s4_class(imgarray_scale, "ImageArray")
    for(i in seq_along(imgarray_scale)){
      expect_s4_class(imgarray_scale[[i]]@seed, "DelayedAffineSeed")
      expect_s4_class(imgarray_scale[[i]]@seed, "DelayedUnaryOp")
    }
    
    # check dim
    actual.dim <- dim(imgarray)
    od <- actual.dim
    od[xy] <- output.dim
    expect_equal(dim(imgarray_scale), od)
    for(i in seq_along(imgarray)){
      od[xy] <- od[xy] / 2^(i-1)
      expect_equal(dim(imgarray_scale[[i]]), od)
    }
    
    # check scale
    img <- realize(imgarray)
    img_scale <- test_scale(img, axes(imgarray), output.dim)
    dimnames(img_scale) <- vector("list", length(dim(img_scale)))
    expect_equal(
      realize(imgarray_scale[[1]]), 
      img_scale
    )
    
    # check subset
    index <- list(x = 100:140, y = 230:240)
    imgarray_subset <- crop(imgarray_scale, index = index)
    img_subset <- test_subset(img_scale, axes(imgarray), index)
    dimnames(img_subset) <- NULL
    expect_equal(
      realize(imgarray_subset[[1]]), 
      img_subset
    )
  })
  
  test_that(paste0("affine transformation for ", ni), {
    
    # affine transform image
    m <- matrix(c(1, -.5, 128, 0, 1, 0), nrow=3, ncol=2)
    imgarray_affine <- affine(imgarray, m = m)
    
    # check seed
    expect_s4_class(imgarray_affine, "ImageArray")
    for(i in seq_along(imgarray_affine)){
      expect_s4_class(imgarray_affine[[i]]@seed, "DelayedAffineSeed")
      expect_s4_class(imgarray_affine[[i]]@seed, "DelayedUnaryOp")
    }
    
    # check affine
    img <- realize(imgarray)
    img_affine <- test_affine(img, axes(imgarray), m)
    dimnames(img_affine) <- vector("list", length(dim(img_affine)))
    expect_equal(
      realize(imgarray_affine[[1]]), 
      img_affine
    )
    
    # check subset
    index <- list(x = 100:140, y = 230:240)
    imgarray_subset <- crop(imgarray_affine, index = index)
    img_subset <- test_subset(img_affine, axes(imgarray), index)
    dimnames(img_subset) <- NULL
    expect_equal(
      realize(imgarray_subset[[1]]), 
      img_subset
    )
  })
  
  # test a lot of angles
  for(angle in c(seq(2, 360, 20), c(90, 180, 270))){
    
    # test a lot of angles
    test_that(paste0("rotate (", angle, " degrees) transformation for ", ni), {
      
      # rotate image
      imgarray_rotate <- ImageArray::rotate(imgarray, angle = angle)
      
      # check seed
      expect_s4_class(imgarray_rotate, "ImageArray")
      for(i in seq_along(imgarray_rotate)){
        expect_s4_class(imgarray_rotate[[i]]@seed, "DelayedAffineSeed")
        expect_s4_class(imgarray_rotate[[i]]@seed, "DelayedUnaryOp")
      }
      
      # check affine
      img <- realize(imgarray)
      img_rotate <- test_rotate(img, axes(imgarray), angle)
      dimnames(img_rotate) <- vector("list", length(dim(img_rotate)))
      expect_equal(
        realize(imgarray_rotate[[1]]), 
        img_rotate
      )
      
      # check subset
      index <- list(x = 100:140, y = 230:240)
      imgarray_subset <- crop(imgarray_rotate, index = index)
      img_subset <- test_subset(img_rotate, axes(imgarray), index)
      dimnames(img_subset) <- NULL
      expect_equal(
        realize(imgarray_subset[[1]]), 
        img_subset
      )
    })
  }
  
  test_that(paste0("sequence transformation for ", ni), {
    m <- matrix(c(1, -.5, 128, 0, 1, 0), nrow=3, ncol=2)
    output.dim <- c(200,300)
    shift <- c(100,20)
    img <- realize(imgarray)
    
    # affine
    imgarray_affine <- affine(imgarray, m = m)
    img_affine <- test_affine(img, axes(imgarray), m)
    
    # scale
    imgarray_scale <- scale(imgarray_affine, output.dim = output.dim)
    img_scale <- test_scale(img_affine, axes(imgarray), output.dim = output.dim)
    
    # check equal
    dimnames(img_scale) <- vector("list", length(dim(img_scale)))
    expect_equal(
      realize(imgarray_scale[[1]]), 
      img_scale
    )
    
    # translate
    imgarray_trans <- translation(imgarray_scale, shift = shift)
    
    # check extent
    expect_equal(
      extent(imgarray_trans),
      mapply(\(i,j) i+j, extent(imgarray_scale), shift, SIMPLIFY = FALSE)
    )
    
    # check subset
    index <- list(x = 30:47, y = 53:58)
    imgarray_subset <- crop(imgarray_trans, index = index)
    img_subset <- test_subset(img_scale, axes(imgarray), index)
    dimnames(img_subset) <- NULL
    expect_equal(
      realize(imgarray_subset[[1]]), 
      img_subset
    )
  })
}