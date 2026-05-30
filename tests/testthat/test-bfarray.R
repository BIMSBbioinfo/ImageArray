library(magick)
skip_if_not_installed("ggplot2")
skip_if_not_installed("RBioFormats")
library(ggplot2)
library(RBioFormats)

# image file
img.file <- system.file(
  "extdata",
  "xy_12bit__plant.ome.tiff",
  package = "ImageArray"
)
img.file2 <- system.file(
  "extdata",
  "single-channel.ome.tiff",
  package = "ImageArray"
)

test_that("bfarray object", {
  
  # create array
  bfa <- BFArray(img.file, series = 1, resolution = 2)
  expect_equal(dim(bfa), c(256, 256))
  bfa <- BFArray(img.file, series = 1, resolution = 1)
  expect_equal(dim(bfa), c(512, 512))

  # methods
  bfa2 <- aperm(bfa, c(2, 1))
  expect_equal(bfa2[1, 2], bfa[2, 1])

  # construct imagearray
  img <- createImageArray(img.file, series = 1, resolution = 1:2)
  expect_equal(length(img), 2)
  img <- createImageArray(img.file2, series = 1, resolution = 1)
  expect_equal(length(img), 1)
  expect_error(img <- createImageArray(img.file2, series = 1, resolution = 1:2))

  # single channel modulate
  img_modulated <- modulate(img, brightness = 200)
  orig <- realize(img[1:10, 1:10]) * 2
  orig[orig > 1] <- 1
  newmat <- realize(img_modulated[1:10, 1:10])
  expect_equal(orig, newmat)
})

test_that("bfarray based ImageArray", {
  # create array
  img <- createImageArray(img.file, series = 1, resolution = 1:2)
  expect_equal(length(img), 2)

  # get image info
  expect_equal(getImageInfo(img), data.frame(width = 512, height = 512))

  # construct imagearray
  bfa.raster <- as.raster(img)
  plot(bfa.raster)
})
