####
# Methods ####
####

#' @describeIn ImageArray-methods permute image
#' @exportMethod aperm
setMethod("aperm", signature = "ImageArray", function(a, perm) {
  for (i in seq_along(a@levels)) {
    a[[i]] <- aperm(a[[i]], perm = perm)
  }
  axes(a) <- axes(a)[perm]
  a
})

#' @describeIn ImageArray-methods negate image
#' @exportMethod negate
setMethod("negate", signature = "ImageArray", function(object) {
  for (i in seq_along(object@levels)) {
    object[[i]] <- 255L - object[[i]]
  }
  object
})

#' @describeIn ImageArray-methods modulate image
#' @exportMethod modulate
setMethod("modulate", signature = "ImageArray", function(object, brightness) {
  if (brightness < 0) {
    stop("Brightness should be more than 0, typically more than 100")
  }
  for (i in seq_along(object@levels)) {
    tmp <- ceiling(object[[i]] * (brightness / 100))
    max <- if (type(object[[i]]) == "double") 1 else 255
    tmp[tmp > max] <- max
    if (max == 255) {
      type(tmp) <- "integer"
    }
    object[[i]] <- tmp
  }
  object
})

#' @describeIn ImageArray-methods cropping image
#' @importFrom stats setNames
#' @exportMethod crop
setMethod("crop", signature = "ImageArray", function(object, index) {

  # get axes
  ax <- axes(object)
  dim_img <- stats::setNames(dim(object), ax)
  
  # get scaled axes
  scaled_axes <- intersect(ax, c("x", "y", "z"))

  # check indices
  if (missing(index)) {
    index <- vector(mode = "list", length = length(ax))
  }
  index <- .check_indices(index = index, dim = dim_img, ax = ax)

  # check sequential
  check_sequential <- all(vapply(index[scaled_axes], is.sequential, logical(1)))
  if (!check_sequential)
    stop("'index' should be a list of sequantial integer vectors!")

  # crop all images
  sc <- scales(object)
  for (i in seq_along(object@levels)) {
    img <- object[[i]]
    # scale only space axes
    cur_ind <- index
    cur_scale <- sc[[i]]
    selected_dim <- stats::setNames(dim(img), ax)[scaled_axes]
    cur_ind[scaled_axes] <-
      lapply(seq_along(index[scaled_axes]), function(j) {
        ind <- index[scaled_axes][[j]]
        ind <- c(
          floor(ind[1] / cur_scale[scaled_axes][j]),
          ceiling(ind[length(ind)] / cur_scale[scaled_axes][j])
        )
        seq(max(ind[1], 1), min(ind[2], selected_dim[j]))
      })
    # subset after scaling indices of space axes
    object[[i]] <- .subset_array(img, cur_ind, drop = FALSE)
  }

  object
})

####
# Utils ####
####

#' @noRd
.subset_array <- function(x, idx, drop = FALSE) {
  d <- dim(x)
  if (is.null(d)) {
    stop("x must be an array or matrix.")
  }
  if (length(idx) > length(d)) {
    stop("Too many index dimensions provided.")
  }
  
  # pad missing dimensions with full slices
  while (length(idx) < length(d)) {
    idx[[length(idx) + 1]] <- seq_len(d[length(idx) + 1])
  }
  
  if (length(idx) == 3) {
    x[idx[[1]], idx[[2]], idx[[3]], drop = drop]
  } else {
    x[idx[[1]], idx[[2]], drop = drop]
  }
}
