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