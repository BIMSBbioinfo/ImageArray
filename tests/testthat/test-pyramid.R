library(magick)
library(rhdf5)
library(HDF5Array)
library(Rarr)
skip_if_not_installed("ggplot2")
library(ggplot2)

output_h5 <- tempfile(fileext = ".h5")
output_zarr <- tempfile(fileext = ".zarr")

# build image array
set.seed(1)
mat <- array(
  data = sample(1:255, 2000 * 5000 * 3, replace = TRUE),
  dim = c(2000, 5000, 3)
)
mat_raster <- as.raster(mat, max = 255)

# read as magick object
mat_image <- magick::image_read(mat_raster)

test_that("validate pyramid level", {
  imgarray <- createImageArray(mat_image, n.levels = 3)
  dim_img <- dim(imgarray)
  expect_equal(dim(imgarray[[1]]), dim_img)
  expect_equal(dim(imgarray[[2]]), 
               c(dim_img[1], dim_img[2]/2, dim_img[3]/2))
  expect_error(imgarray[[-1]])
  expect_error(imgarray[[1.2]])
  expect_error(imgarray[[0]])
})

test_that("visualize h5 ImageArray", {
  # create image array
  mat_list <- writeImageArray(
    mat_image,
    output = output_h5,
    name = "image",
    format = "h5",
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(mat_list), 4)

  # create raster array
  img_raster <- as.raster(mat_list, max.pixel.size = 2000)
  expect_equal(dim(img_raster), c(500, 1250))

  # visualize
  info <- list(width = dim(img_raster)[2], height = dim(img_raster)[1])
  ggplot2::ggplot(
    data.frame(x = 0, y = 0),
    ggplot2::aes(.data[["x"]], .data[["y"]])
  ) +
    ggplot2::geom_blank() +
    ggplot2::theme_void() +
    ggplot2::coord_fixed(
      expand = FALSE,
      xlim = c(0, info$width),
      ylim = c(0, info$height)
    ) +
    ggplot2::annotation_raster(
      img_raster,
      0,
      info$width,
      info$height,
      0,
      interpolate = FALSE
    )
})

test_that("visualize zarr ImageArray", {
  # create image array
  unlink(output_zarr, recursive = TRUE)
  mat_list <- writeImageArray(
    mat_image,
    output = output_zarr,
    name = "image",
    format = "zarr",
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(mat_list), 4)

  # create raster array
  img_raster <- as.raster(mat_list, max.pixel.size = 2000)
  expect_equal(dim(img_raster), c(500, 1250))

  # visualize
  info <- list(width = dim(img_raster)[2], height = dim(img_raster)[1])
  ggplot2::ggplot(
    data.frame(x = 0, y = 0),
    ggplot2::aes(.data[["x"]], .data[["y"]])
  ) +
    ggplot2::geom_blank() +
    ggplot2::theme_void() +
    ggplot2::coord_fixed(
      expand = FALSE,
      xlim = c(0, info$width),
      ylim = c(0, info$height)
    ) +
    ggplot2::annotation_raster(
      img_raster,
      0,
      info$width,
      info$height,
      0,
      interpolate = FALSE
    )
})
