test_that("image array class", {
  
  # correct image array construction
  imgarray <- ImageArray(levels = list(array(1:75, dim = c(3,5,5))),
                         axes = c("c", "y", "x"))
  imgarray <- ImageArray(levels = list(array(1:25, dim = c(5,5))),
                         axes = c("y", "x"))
  
  # incorrect axes names
  expect_error(
    imgarray <- ImageArray(levels = list(array(1:75, dim = c(3,5,5))),
                           axes = c("b", "a", "d"))
  )
  
  # incorrect dimensions
  expect_error(
    imgarray <- ImageArray(levels = list(array(1:75, dim = c(3,5,5)),
                                         array(1:75, dim = c(3,6,6,2))), 
                           axes = c("c", "y", "x"))
  )
})
