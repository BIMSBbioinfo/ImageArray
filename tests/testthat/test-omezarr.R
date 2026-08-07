library(utils)

# image file
omezarrzip <- system.file("extdata",
                          "test_ngff_image_v04.ome.zarr.zip",
                          package = "ImageArray")
dir.create(omezarr <- tempfile(fileext = ".ome.zarr"))
unzip(omezarrzip, exdir = omezarr)

test_that("OZPath", {

  # create OZPath object, the "labels" group is not a resolution
  oz <- OZPath(omezarr)
  expect_equal(path(oz), omezarr)
  expect_equal(resolution(oz), c("s0", "s1", "s2", "s3", "s4"))

  # resolutions by index and by name
  expect_equal(resolution(OZPath(omezarr, resolution = 2)), "s1")
  expect_equal(resolution(OZPath(omezarr, resolution = 1:2)), c("s0", "s1"))
  expect_equal(resolution(OZPath(omezarr, resolution = "s0")), "s0")

  # faulty resolutions
  expect_error(OZPath(omezarr, resolution = "labels"),
               "resolutions not found")
  expect_error(OZPath(omezarr, resolution = "s9"),
               "resolutions not found")
  expect_error(OZPath(omezarr, resolution = 99),
               "resolutions not found")

  # faulty store
  dir.create(notzarr <- tempfile())
  expect_error(OZPath(notzarr), "is not a zarr store")
})

test_that("OZPath resolutions have the same dimensionality", {

  # copy the store and make one resolution three dimensional
  dir.create(badzarr <- tempfile())
  file.copy(list.files(omezarr, full.names = TRUE, all.files = TRUE,
                       no.. = TRUE),
            badzarr, recursive = TRUE)
  zarray <- readLines(file.path(badzarr, "s4", ".zarray"))
  zarray <- sub('"shape": [', '"shape": [1,',
                paste(zarray, collapse = ""), fixed = TRUE)
  zarray <- sub('"chunks": [', '"chunks": [1,', zarray, fixed = TRUE)
  writeLines(zarray, file.path(badzarr, "s4", ".zarray"))

  # the 3D level is only rejected when it is among the resolutions
  expect_error(OZPath(badzarr), "differing dimensionality")
  expect_error(OZPath(badzarr, resolution = c("s3", "s4")),
               "differing dimensionality")
  expect_equal(resolution(OZPath(badzarr, resolution = 1:4)),
               c("s0", "s1", "s2", "s3"))
})

test_that("OME-ZARR based ImageArray", {
  
  # construct imagearray
  img <- ImageArray(omezarr, resolution = 1:2)
  expect_equal(length(img), 2)
  img <- ImageArray(omezarr, resolution = c("s0", "s1"))
  expect_equal(length(img), 2)
  img <- ImageArray(omezarr, resolution = c("s0", "s2"))
  expect_equal(length(img), 2)
  img <- ImageArray(omezarr)
  expect_equal(length(img), 5)
  
  # single channel modulate
  img_modulated <- negate(img)
  orig <- realize(img[1:10, 1:10])
  newmat <- realize(img_modulated[1:10, 1:10])
  expect_equal(unique(c(orig+newmat)), 255)
  
  # get image info
  expect_equal(getImageInfo(img), data.frame(width = 512, height = 512))
  
  # construct imagearray
  bfa.raster <- as.raster(img)
  plot(bfa.raster)
})

