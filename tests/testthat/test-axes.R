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
  expect_error(
    axes <- .check_axes(mat4d, c("x", "y", "c", "c")),
    "Duplicated axes are detected: c"
  )
})

test_that("get axes from scales", {
  
  scales <- list(c(x = 1, y = 1),
                 c(x = 1.33, y = 1.33))
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

test_that("get axes of an ImageArray", {

  # the axes of an ImageArray are given by the names of its scales
  imgarray <- ImageArray(image = list(array(1:75, dim = c(3,5,5)),
                                      array(1:12, dim = c(3,2,2))),
                         axes = c("c", "y", "x"))
  expect_equal(axes(imgarray), c("c", "y", "x"))
  expect_equal(axes(imgarray), names(scales(imgarray)[[1]]))

  # axes are also picked up from user provided scales
  imgarray <- ImageArray(image = list(array(1:25, dim = c(5,5)),
                                      array(1:9, dim = c(3,3))),
                         scales = list(c(y = 1, x = 1),
                                       c(y = 0.6, x = 0.6)))
  expect_equal(axes(imgarray), c("y", "x"))
})

test_that("replace axes of an ImageArray", {

  imgarray <- ImageArray(image = list(array(1:75, dim = c(3,5,5)),
                                      array(1:12, dim = c(3,2,2))),
                         axes = c("c", "y", "x"))

  # axes can be permuted, all levels are permuted alike
  axes(imgarray) <- c("y", "x", "c")
  expect_equal(axes(imgarray), c("y", "x", "c"))
  for(s in scales(imgarray))
    expect_equal(names(s), c("y", "x", "c"))

  # the scales follow their axes
  expect_equal(scales(imgarray)[[2]], c(y = 2.5, x = 2.5, c = 1))

  # the object remains valid
  expect_true(validObject(imgarray))

  # levels are untouched by the permutation
  expect_equal(dim(imgarray[[1]]), c(3, 5, 5))
})

test_that("replace axes of an ImageArray with invalid axes", {

  imgarray <- ImageArray(image = list(array(1:75, dim = c(3,5,5)),
                                      array(1:12, dim = c(3,2,2))),
                         axes = c("c", "y", "x"))
  msg <- "axes can only be replaced by a permutation of the existing axes"

  # only permutations of the existing axes are allowed
  expect_error(axes(imgarray) <- c("z", "y", "x"), msg)
  expect_error(axes(imgarray) <- c("y", "x"), msg)
  expect_error(axes(imgarray) <- c("c", "y", "x", "t"), msg)
  expect_error(axes(imgarray) <- c("x", "x", "y"), msg)
  expect_error(axes(imgarray) <- seq_len(3), msg)

  # the object is left untouched
  expect_equal(axes(imgarray), c("c", "y", "x"))
})
