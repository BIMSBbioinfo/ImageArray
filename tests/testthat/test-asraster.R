library(magick)
skip_if_not_installed("ggplot2")
library(ggplot2)
library(EBImage)

# image file
img.file <- system.file("images", "sample.png", package = "EBImage")

test_that("levels", {
  # create image
  img <- magick::image_read(img.file)
  img <- magick::image_data(img)

  # create ImageArray
  imgarray <- ImageArray(img, n.levels = 2)

  # check as.raster
  imgarray2 <- as.raster(imgarray, max.pixel.size = 300)
  expect_true(all(dim(imgarray2) == dim(imgarray[[2]])[3:2]))
  imgarray2 <- as.raster(imgarray, level = 2)
  expect_true(all(dim(imgarray2) == dim(imgarray[[2]])[3:2]))
  expect_error(as.raster(imgarray, level = 3))
  expect_error(as.raster(imgarray, level = 1.2))
})

test_that("levels (3D)", {
  
  # create ImageArray
  imgarray <- ImageArray(image = list(array(1:2197, dim = c(13,13,13)),
                                      array(1:1000, dim = c(10,10,10)),
                                      array(1:216, dim = c(6,6,6))),
                         axes = c("x", "y", "z"))
  
  # check as.raster
  expect_error(
    imgarray_vis <- as.raster(imgarray), 
    regexp = "Rasterable ImageArray objects should have axes"
  )
})

