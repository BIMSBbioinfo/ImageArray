library(magick)
library(HDF5Array)
library(ZarrArray)
skip_if_not_installed("ggplot2")
library(ggplot2)

# images
mat2d <- array(data = 1:(20*50), dim = c(20, 50))
mat3d <- array(data = 1:(20*50*3), dim = c(20, 50, 3))
mat4d <- array(data = 1:(20*50*3*2), dim = c(20, 50, 3, 2))

test_that("guess axes 2D (no axes)", {
  axes <- .check_axes(mat2d)
  expect_equal(axes, c("x", "y"))
  axes <- .check_axes(mat2d, engine = "magick-image")
  expect_equal(axes, c("x", "y"))
})

test_that("guess axes 3D (no axes)", {
  axes <- .check_axes(mat3d)
  expect_equal(axes, c("x", "y", "c"))
  axes <- .check_axes(mat3d, engine = "magick-image")
  expect_equal(axes, c("c", "x", "y"))
})

test_that("guess axes 2D (with axes)", {
  axes <- .check_axes(mat2d, c("x", "y"))
  expect_equal(axes, c("x", "y"))
  axes <- .check_axes(mat2d, c("y", "x"))
  expect_equal(axes, c("y", "x"))
})

test_that("guess axes 3D (with axes)", {
  axes <- .check_axes(mat3d, c("x", "y", "c"))
  expect_equal(axes, c("x", "y", "c"))
  axes <- .check_axes(mat3d, c("c", "x", "y"))
  expect_equal(axes, c("c", "x", "y"))
})

test_that("axes cannot be guess", {
  expect_error(
    axes <- .check_axes(mat4d), 
    "axes must be provided"
  )
})

test_that("axes non-matching array", {
  expect_error(
    axes <- .check_axes(mat2d, c("c", "x", "y")),
    "must match number of dimensions"
  )
  expect_error(
    axes <- .check_axes(mat3d, c("x", "y", "c", "t")),
    "must match number of dimensions"
  )
})

test_that("invalid axes", {
  expect_error(
    axes <- .check_axes(mat3d, c("c", "p", "y")),
    "axes should include at least both x and y dimensions!"
  )
})

test_that("duplicate axes", {
  expect_error(
    axes <- .check_axes(mat3d, c("c", "x", "x")),
    "axes should include at least both x and y dimensions!"
  )
})

test_that("get axes from scales", {
  
  scales <- list(c(x = 1, y = 1),
                 c(x = 0.6, y = 0.6))
  axes <- .get_axes_from_scales(scales)
  expect_equal(axes, c("x", "y"))
  
  # scales should have a length
  expect_error(
    .get_axes_from_scales(list()),
    "scales should have at least one vector of axes scales"
  )
  
  # each element of scales should be named
  expect_error(
    .get_axes_from_scales(list(c())),
    "Each vector in scales should be named!"
  )
})