test_that("image array class", {
  
  # correct image array construction
  imgarray <- ImageArray(image = list(array(1:75, dim = c(3,5,5))),
                         axes = c("c", "y", "x"))
  imgarray <- ImageArray(image = list(array(1:25, dim = c(5,5))),
                         axes = c("y", "x"))
  imgarray <- ImageArray(image = list(array(1:25, dim = c(5,5))),
                         scales = list(c(x = 1, y = 1)))
  imgarray <- ImageArray(image = list(array(1:25, dim = c(5,5)),
                                       array(1:25, dim = c(3,3))),
                         scales = list(c(x = 1, y = 1),
                                       c(x = 0.6, y = 0.6)))
  
  # incorrect axes names
  expect_error(
    imgarray <- ImageArray(image = list(array(1:75, dim = c(3,5,5))),
                           axes = c("b", "a", "d"))
  )
  
  # incorrect dimensions
  expect_error(
    imgarray <- ImageArray(image = list(array(1:75, dim = c(3,5,5)),
                                         array(1:75, dim = c(3,6,6,2))), 
                           axes = c("c", "y", "x"))
  )
  
  # incorrect scales
  expect_error(
    imgarray <- ImageArray(image = list(array(1:25, dim = c(5,5)),
                                         array(1:25, dim = c(3,3))),
                           scales = list(c(1, 1),
                                         c(x = 0.6, y = 0.6))), 
    regexp = "Each vector in scales should be named!"
  )
  expect_error(
    imgarray <- ImageArray(image = list(array(1:25, dim = c(5,5))),
                           scales = list(c(x = 1, y = 11),
                                         c(x = 0.6, y = 0.6))), 
    regexp = "scales should be of the same length as levels!"
  )
  expect_error(
    imgarray <- ImageArray(image = list(array(1:25, dim = c(5,5)),
                                         array(1:25, dim = c(3,3))),
                           scales = list(c(b = 1, c = 1),
                                         c(x = 0.6, y = 0.6))), 
    regexp = "Some axes are invalid"
  )
  
})

