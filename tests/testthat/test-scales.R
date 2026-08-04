test_that("get level scales", {
  
  # .get_scales collects scales of x,y,z axes
  sc <- .get_scales(levels = list(array(1:75, dim = c(3,5,5)),
                                  array(1:75, dim = c(3,2,2))),
                    axes = c("c", "y", "x"))
  expect_equal(length(sc), 2)
  for(s in sc){
    expect_equal(names(s), c("c", "y", "x"))
    expect_equal(s[["c"]], 1)
  }
  
  # axes do not match array dim
  expect_error({
    sc <- .get_scales(levels = list(array(1:75, dim = c(3,5,5)),
                                    array(1:75, dim = c(3,6))),
                      axes = c("c", "y", "x"))
  }, regexp = "length does not match the dim of the array")
})

test_that("get axes from scales", {
  
  # .get_scales collects scales of x,y,z axes
  sc <- .get_scales(levels = list(array(1:75, dim = c(3,5,5)),
                                  array(1:75, dim = c(3,2,2))),
                    axes = c("c", "y", "x"))
  expect_equal(length(sc), 2)
  for(s in sc){
    expect_equal(names(s), c("c", "y", "x"))
    expect_equal(s[["c"]], 1)
  }
  
  # axes do not match array dim
  expect_error({
    sc <- .get_scales(levels = list(array(1:75, dim = c(3,5,5)),
                                    array(1:75, dim = c(3,6))),
                      axes = c("c", "y", "x"))
  }, regexp = "length does not match the dim of the array")
})