library(magick)
library(EBImage)

# image file
img.file <- system.file("images", "sample.png", package = "EBImage")
img <- magick::image_read(img.file)

test_that("format is given (h5, hdf5)", {
  
  # with extension
  output_h5 <- tempfile(fileext = ".h5")
  imgarray <- writeImageArray(
    img,
    output = output_h5,
    format = "h5",
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(output_h5))
  
  # replace path
  temp_output <- tempdir()
  path(imgarray) <- temp_output
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(temp_output))
  
  # without extension
  output_h5 <- tempfile(fileext = "")
  expect_warning(
    imgarray <- writeImageArray(
      img,
      output = output_h5,
      format = "h5",
      replace = TRUE
    ) 
  )
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(output_h5))
  
  # replace path
  temp_output <- tempdir()
  path(imgarray) <- temp_output
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(temp_output))
  
  # format is hdf5, extension is h5
  output_h5 <- tempfile(fileext = ".h5")
  imgarray <- writeImageArray(
    img,
    output = output_h5,
    format = "hdf5",
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(output_h5))
  
  # format is hdf5, no extension
  output_h5 <- tempfile(fileext = "")
  expect_warning(
    imgarray <- writeImageArray(
      img,
      output = output_h5,
      format = "hdf5",
      replace = TRUE
    ) 
  )
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(output_h5))
  
})

test_that("format is given (zarr)", {

  # with extension
  output_zarr <- tempfile(fileext = ".zarr")
  imgarray <- writeImageArray(
    img,
    output = output_zarr,
    format = "zarr",
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(.collapse_slashes(output_zarr)))
  
  # replace path
  temp_output <- tempfile(fileext = ".zarr")
  path(imgarray) <- temp_output
  expect_equal(
    suppressWarnings(normalizePath(path(imgarray))),
    suppressWarnings(normalizePath(temp_output)))
  
  # without extension
  output_zarr <- tempfile(fileext = "")
  expect_warning(
    imgarray <- writeImageArray(
      img,
      output = output_zarr,
      format = "zarr",
      replace = TRUE
    )
  )
  expect_equal(normalizePath(path(imgarray)),
               normalizePath(output_zarr))
})

test_that("format is not given", {
  
  # h5
  output_file <- tempfile(fileext = ".h5")
  imgarray <- writeImageArray(
    img,
    output = output_file,
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)),
               normalizePath(output_file))
  
  # zarr
  output_file <- tempfile(fileext = ".zarr")
  imgarray <- writeImageArray(
    img,
    output = output_file,
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)),
               normalizePath(output_file))
  
  # random file throws an error since format cannot be inferred
  output_file <- tempfile()
  expect_error(
    imgarray <- writeImageArray(
      img,
      output = output_file,
      replace = TRUE
    ) 
  )
  
})

test_that("format and file extension differ", {
  
  # h5 vs zarr, will be written as h5 but with a warning
  output_file <- tempfile(fileext = ".zarr")
  expect_warning(
    imgarray <- writeImageArray(
      img,
      output = output_file,
      format = "h5",
      replace = TRUE
    ) 
  )
  expect_equal(normalizePath(path(imgarray)),
               normalizePath(output_file))
  
  # zarr vs h5, will be written as h5 but with a warning
  output_file <- tempfile(fileext = ".h5")
  expect_warning(
    imgarray <- writeImageArray(
      img,
      output = output_file,
      format = "zarr",
      replace = TRUE
    ) 
  )
  expect_equal(normalizePath(path(imgarray)),
               normalizePath(output_file))
  
})

test_that("in-memory", {
  
  # 'in-memory' writes to memory
  expect_message(
    imgarray <- writeImageArray(
      img,
      format = "in-memory",
      replace = TRUE
    )  
  )
  expect_error(path(imgarray))
  
  # 'in-memory' overwrites output path with a message
  expect_message(
    imgarray <- writeImageArray(
      img,
      output = tempfile(fileext = ".h5"),
      format = "in-memory",
      replace = TRUE
    )  
  )
  expect_error(path(imgarray))
  
})