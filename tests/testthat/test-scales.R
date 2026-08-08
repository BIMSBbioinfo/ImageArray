test_that("get scales from levels", {
  
  # .get_scales_from_levels collects scales of x,y,z axes
  sc <- .get_scales_from_levels(levels = list(array(1:75, dim = c(3,5,5)),
                                array(1:75, dim = c(3,2,2))),
                    axes = c("c", "y", "x"))
  expect_equal(length(sc), 2)
  for(s in sc){
    expect_equal(names(s), c("c", "y", "x"))
    expect_equal(s[["c"]], 1)
  }
  
  # axes do not match array dim
  expect_error({
    sc <- .get_scales_from_levels(levels = list(array(1:75, dim = c(3,5,5)),
                                  array(1:75, dim = c(3,6))),
                      axes = c("c", "y", "x"))
  }, regexp = "length does not match the dim of the array")
})

test_that("get scales from number of levels", {
  
  # .get_scales_from_levels collects scales of x,y,z axes
  sc <- .get_scales_from_nlevels(n.levels = 4,
                                 axes = c("c", "y", "x"))
  expect_equal(length(sc), 4)
  expect_equal(
    vapply(sc, \(.) .[["x"]], numeric(1)),
    c(1, 0.5, 0.25, 0.125)
  )
  expect_equal(
    vapply(sc, \(.) .[["y"]], numeric(1)),
    c(1, 0.5, 0.25, 0.125)
  )
  expect_equal(
    vapply(sc, \(.) .[["c"]], numeric(1)),
    c(1, 1, 1, 1)
  )
  
  # wrong axes
  expect_error(
    .get_scales_from_nlevels(n.levels = 2, axes = c("c", "x")),
    "axes should have at least x and y dimensions!"
  )
})

test_that("get scales of an ImageArray", {

  imgarray <- ImageArray(image = list(array(1:75, dim = c(3,5,5)),
                                      array(1:12, dim = c(3,2,2))),
                         axes = c("c", "y", "x"))

  # one named vector of scales per level
  sc <- scales(imgarray)
  expect_equal(length(sc), length(imgarray))
  expect_equal(sc[[1]], c(c = 1, y = 1, x = 1))
  expect_equal(sc[[2]], c(c = 1, y = 0.4, x = 0.4))
})

test_that("replace scales of an ImageArray", {

  imgarray <- ImageArray(image = list(array(1:75, dim = c(3,5,5)),
                                      array(1:12, dim = c(3,2,2))),
                         axes = c("c", "y", "x"))

  # scales can be replaced with new values
  scales(imgarray) <- list(c(c = 1, y = 1, x = 1),
                           c(c = 1, y = 0.5, x = 0.5))
  expect_equal(scales(imgarray)[[2]], c(c = 1, y = 0.5, x = 0.5))
  expect_true(validObject(imgarray))

  # the names of the scales may be a permutation of the existing axes,
  # which in turn permutes the axes of the object
  scales(imgarray) <- list(c(y = 1, x = 1, c = 1),
                           c(y = 0.5, x = 0.5, c = 1))
  expect_equal(axes(imgarray), c("y", "x", "c"))
})

test_that("replace scales of an ImageArray with invalid scales", {

  imgarray <- ImageArray(image = list(array(1:75, dim = c(3,5,5)),
                                      array(1:12, dim = c(3,2,2))),
                         axes = c("c", "y", "x"))
  msg <- paste0("names of each vector in scales should be a permutation of ",
                "the existing axes")

  # scales should be a list
  expect_error(
    scales(imgarray) <- c(c = 1, y = 1, x = 1),
    "scales must be a list!"
  )

  # each vector should be named by a permutation of the existing axes
  expect_error(scales(imgarray) <- list(c(1, 1, 1), c(1, 0.5, 0.5)), msg)
  expect_error(
    scales(imgarray) <- list(c(c = 1, y = 1, z = 1),
                             c(c = 1, y = 0.5, z = 0.5)),
    msg
  )
  expect_error(
    scales(imgarray) <- list(c(y = 1, x = 1), c(y = 0.5, x = 0.5)),
    msg
  )
  expect_error(
    scales(imgarray) <- list(c(c = 1, y = 1, y = 1),
                             c(c = 1, y = 0.5, y = 0.5)),
    msg
  )

  # all levels should carry the same axes in the same order
  expect_error(
    scales(imgarray) <- list(c(c = 1, y = 1, x = 1),
                             c(y = 0.5, x = 0.5, c = 1)),
    "scale names do not match axes"
  )

  # one vector of scales per level
  expect_error(
    scales(imgarray) <- list(c(c = 1, y = 1, x = 1)),
    "scales should be of the same length as levels!"
  )

  # scales should be finite numbers
  expect_error(
    scales(imgarray) <- list(c(c = 1, y = 1, x = 1),
                             c(c = 1, y = Inf, x = 0.5)),
    "scale entries are not numeric"
  )
  expect_error(
    scales(imgarray) <- list(c(c = "1", y = "1", x = "1"),
                             c(c = "1", y = "0.5", x = "0.5")),
    "scale entries are not numeric"
  )

  # the object is left untouched
  expect_equal(axes(imgarray), c("c", "y", "x"))
  expect_equal(scales(imgarray)[[2]], c(c = 1, y = 0.4, x = 0.4))
})