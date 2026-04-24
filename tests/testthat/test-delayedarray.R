library(magick)
library(rhdf5)
library(HDF5Array)
library(Rarr)

output_h5 <- tempfile(fileext = ".h5")
output_zarr <- tempfile(fileext = ".zarr")

# build image array
set.seed(1)
mat <- array(
  data = sample(1:13, 20 * 50 * 3, replace = TRUE),
  dim = c(20, 50, 3)
)
mat_raster <- as.raster(mat, max = 255)

# read as magick object
mat_image <- magick::image_read(mat_raster)

test_that("path hdf5", {
  # h5
  mat_list <- writeImageArray(
    mat_image,
    output = output_h5,
    name = "image",
    format = "h5",
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(mat_list), 1)
  expect_true(file.exists(path(mat_list)))

  # change path
  output_h5_replace <- gsub(".h5$", "2.h5", path(mat_list))
  file.rename(path(mat_list), output_h5_replace)
  expect_true(file.exists(output_h5_replace))
  path(mat_list) <- output_h5_replace
  expect_true(file.exists(path(mat_list)))
  expect_equal(path(mat_list), output_h5_replace)
})

test_that("path zarr", {
  # zarr
  mat_list <- writeImageArray(
    mat_image,
    output = output_zarr,
    name = "",
    format = "zarr",
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(mat_list), 1)
  expect_true(dir.exists(path(mat_list)))

  # change path
  output_zarr_replace <- gsub(".zarr", "2.zarr", path(mat_list))
  system(paste('mkdir -p', 
               output_zarr_replace))
  system(paste('mv', 
               path(mat_list),
               output_zarr_replace))
  expect_true(dir.exists(output_zarr_replace))
  path(mat_list) <- output_zarr_replace
  expect_true(dir.exists(path(mat_list)))
  expect_equal(
    normalizePath(path(mat_list)),
    normalizePath(output_zarr_replace)
  )
})
