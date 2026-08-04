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
  axes <- .guess_axes(mat2d)
  expect_equal(axes, c("x", "y"))
  axes <- .guess_axes(mat2d, engine = "magick-image")
  expect_equal(axes, c("x", "y"))
})

test_that("guess axes 3D (no axes)", {
  axes <- .guess_axes(mat3d)
  expect_equal(axes, c("x", "y", "c"))
  axes <- .guess_axes(mat3d, engine = "magick-image")
  expect_equal(axes, c("c", "x", "y"))
})

test_that("guess axes 2D (with axes)", {
  axes <- .guess_axes(mat2d, c("x", "y"))
  expect_equal(axes, c("x", "y"))
  axes <- .guess_axes(mat2d, c("y", "x"))
  expect_equal(axes, c("y", "x"))
})

test_that("guess axes 3D (with axes)", {
  axes <- .guess_axes(mat3d, c("x", "y", "c"))
  expect_equal(axes, c("x", "y", "c"))
  axes <- .guess_axes(mat3d, c("c", "x", "y"))
  expect_equal(axes, c("c", "x", "y"))
})

test_that("axes cannot be guess", {
  expect_error(
    axes <- .guess_axes(mat4d), 
    "axes must be provided"
  )
})

test_that("axes non-matching array", {
  expect_error(
    axes <- .guess_axes(mat2d, c("c", "x", "y")),
    "must match number of dimensions"
  )
  expect_error(
    axes <- .guess_axes(mat3d, c("x", "y", "c", "t")),
    "must match number of dimensions"
  )
})

test_that("invalid axes", {
  expect_error(
    axes <- .guess_axes(mat3d, c("c", "p", "y")),
    "Some axes are invalid"
  )
})

test_that("duplicate axes", {
  expect_error(
    axes <- .guess_axes(mat3d, c("c", "x", "x")),
    "Duplicated axes are detected"
  )
})