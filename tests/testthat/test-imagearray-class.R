test_that("image array class", {
  
  # correct image array construction
  imgarray <- ImageArray(meta = list(axes = c("c", "y", "x")), 
                         levels = list(array(1:75, dim = c(3,5,5))))
  imgarray <- ImageArray(meta = list(axes = c("y", "x")), 
                         levels = list(array(1:25, dim = c(5,5))))
  
  # incorrect axes names
  expect_error(
    imgarray <- ImageArray(meta = list(axes = c("b", "a", "d")), 
                           levels = list(array(1:75, dim = c(3,5,5))))
  )
  
  # incorrect dimensions
  expect_error(
    imgarray <- ImageArray(meta = list(axes = c("c", "y", "x")), 
                           levels = list(array(1:75, dim = c(3,5,5)),
                                         array(1:75, dim = c(3,6,6,2))))
  )
})

test_that("show method displays backend class label", {
  library(EBImage)
  img.file <- system.file("images", "sample.png", package = "EBImage")

  # in-memory: shows DelayedArray (use magick engine to get RGB / 3D array)
  imgarray <- createImageArray(img.file, n.levels = 2, engine = "magick-image")
  expect_output(show(imgarray), "\\[DelayedArray\\]")
  expect_output(show(imgarray), "ImageArray Object")
  expect_output(show(imgarray), "Scales \\(2\\):")

  # HDF5: shows HDF5Array
  output_h5 <- tempfile(fileext = ".h5")
  imgarray_h5 <- writeImageArray(img.file, output = output_h5,
                                 name = "image", format = "h5",
                                 n.levels = 2, engine = "magick-image",
                                 verbose = FALSE)
  expect_output(show(imgarray_h5), "\\[HDF5Array\\]")

  # Zarr: shows ZarrArray
  output_zarr <- tempfile(fileext = ".zarr")
  imgarray_zarr <- writeImageArray(img.file, output = output_zarr,
                                   name = "image", format = "zarr",
                                   n.levels = 2, engine = "magick-image",
                                   verbose = FALSE)
  expect_output(show(imgarray_zarr), "\\[ZarrArray\\]")
})
