#' @describeIn ImageArray-methods path of an ImageArray object
#' @importFrom DelayedArray path
#' @importFrom methods is
#' @export
setMethod("path", signature = "ImageArray", function(object) {
  
  # check one layer
  obj <- object[[1]]
  
  # check out of memory seed objects
  if (!inherits(seed(obj), c("HDF5ArraySeed", "ZarrArraySeed"))) {
    stop(
      "The path method is only applicable to ImageArray objects ",
      "whose layers with DelayedArray seeds."
    )
  }
  
  # get path
  file_path <- DelayedArray::path(obj)

  # check if path is a zarr path
  # the path could have a zarr extension with no associated zarr group/array
  if (methods::is(obj, "ZarrArray")) {
    file_path <- normalizePath(dirname(file_path), winslash = "\\")
  }

  file_path
})

#' @describeIn ImageArray-methods replace method for path(ImageArray)
#' @importFrom methods slotNames slot slot<- is
#' @export
setReplaceMethod(
  "path",
  signature = "ImageArray",
  function(object, value) {
    # update all paths
    for (i in seq_along(object)) {
      object[[i]] <-
        modify_seeds(
          object[[i]],
          function(x) {
            
            # check zarr and update value for each layer
            # this also requires normalizing the path to ensure correct
            # replacement on Windows
            file_path <- path(x)
            if (methods::is(x, "ZarrArraySeed")) {
              value <- gsub(
                normalizePath(dirname(file_path), winslash = "\\"),
                value,
                normalizePath(file_path, winslash = "\\"),
                fixed = TRUE
              )
            }

            # replace path slot
            ind <- grepl("path", slotNames(x))
            path.name <- methods::slotNames(x)[ind]
            methods::slot(x, name = path.name) <- value

            x
          }
        )
    }

    object
  }
)

#' @noRd
.collapse_slashes <- function(x) {
  gsub("/+", "/", x)
}

#' @noRd
.isTRUEorFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' is.sequential
#' @noRd
is.sequential <- function(x) {
  all(abs(diff(x)) == 1)
}

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

.swap <- function(x, i, j) {
  x[c(i, j)] <- x[c(j, i)]
  x
}

.check_dim <- function(object) {
  if (!(length(dim(object)) %in% c(2, 3))) {
    stop(
      "For now, ImageArray only supports only 2D images ",
      "(and 3D images with channels) !!"
    )
  }
}

# based on check_index() from Huber-group-EMBL/Rarr
#' @keywords internal
.check_indices <- function(index, dim, ax) {
  ## check list
  if (!is.list(index)) {
    stop("'index' should be a list")
  }

  ## check if named
  if (is.null(names(index))) {
    ## check we have the correct number of dimensions
    if (isFALSE(length(index) == length(dim))) {
      stop(
        "The number of dimensions provided to 'index' ",
        "does not match the shape of the array"
      )
    }

    # name indices
    names(index) <- ax
  } else {
    # check names
    if (!all(names(index) %in% ax)) {
      stop(
        sprintf(
          paste0(
            "The names of the provided indices should ",
            "be among axes %s of the ImageArray object"
          ),
          paste(
            paste0("'", ax, "'"),
            collapse = ","
          )
        )
      )
    }

    # adjust indices
    new_indices <- setNames(rep(list(NULL), length(ax)), ax)
    new_indices[names(index)] <- index
    index <- new_indices
  }

  ## If any dimensions are NULL transform into the entirety of that dimension
  ## Otherwise check provided indices are valid
  failed <- rep_len(FALSE, length(index))
  for (i in seq_along(index)) {
    # check if null
    if (is.null(index[[i]])) {
      index[[i]] <- seq_len(dim[[i]])
      next
    }

    # check if valid
    failed[i] <- !(is.numeric(index[[i]]) &&
      length(index[[i]]) > 0 &&
      all(index[[i]] %% 1 == 0) &&
      all(index[[i]] >= 0) &&
      all(index[[i]] <= dim[[i]]))
  }

  # message if failed
  if (any(failed)) {
    stop(sprintf(
      "Selected indices for axes %s are out of range or invalid.",
      paste(
        paste0("'", ax[which(failed)], "'"),
        collapse = " & "
      )
    ))
  }

  index
}

#' @keywords internal
#' @noRd
.img_create_msg <- function(dim_img, i) {
  message(paste0(
    "Creating level ",
    i,
    " ",
    paste0("(", paste(dim_img, collapse = ","), ")"),
    "\n"
  ))
}

#' @keywords internal
#' @noRd
.check_level <- function(i, x) {
  if (i %% 1 != 0) {
    stop("Level should be an integer!")
  }
  if (i < 1 || length(x) < i) {
    stop("Level is outside of range")
  }
}

#' @keywords internal
#' @noRd
.FORMATS <- c("in-memory", "h5", "hdf5", "zarr")

#' @keywords internal
#' @noRd
.AXES <- c("c", "y", "x")
