#' The mask is a vector that will be used to filter the inner vector
#' @export
setClass("MaskedArray", slots=c(value = "ANY", masks="list", dynamic_mask="logical"))

setValidity("MaskedArray", function(object){
  # These are lists of either TRUE (when a validation passes), or a character
  # string (when the validation fails)
  checks = list(
    if (!is.null(gdim(object@value))) TRUE else "The array is a type that has no dimensions!",
    if (identical(length(gdim(object@value)), length(object@masks))) TRUE else "The number of vectors in the mask do not match the dimensions of the array!"
  )
  mask_checks = sapply(seq_along(object@masks), function(i){
    if (is.vector(object@masks[[i]])){
      TRUE
    } else {
      paste0("Entry ", i, " of the mask is not a vector!")
    }
  })

  all_checks = c(checks, mask_checks)
  failures = all_checks[vapply(all_checks, is.character, logical(1))]
  if (length(failures) == 0) TRUE else failures
})

#' MaskedArray constructor function
#' @param value An array-like object (vector, matrix, array, data.frame etc) to
#'   mask
#' @param mask A vector to use as the initial mask. It must be a vector that can
#'   be used as a vector, so a character vector can only be used if `value` is
#'   named. This will only work if `value` is a 1 dimensional type such as a
#'   vector or 1D array.
#' @param masks A list of vectors to use as the initial mask, one for each
#'   dimension of `value`. `mask` and `masks` cannot both be specified. The
#'   requirements for the `mask` parameter also apply to each constituent vector
#' @param dynamic_mask A scalar logical. If `TRUE`, indexing the `MaskedArray`
#'   will return a new `MaskedArray` which contains the exact same `data` with
#'   no elements removed, but with a mask that excludes elements that were
#'   unselected.  If `FALSE`, indexing the `MaskedArray` will return a new
#'   `MaskedArray` with a reduced `value`, and only the parts of the mask that
#'   overlap the new slice will persist. Refer to the vignette for an example.
#' @export
MaskedArray = function(value, mask=NULL, masks=NULL, dynamic_mask=TRUE){
    if(is.null(mask) && is.null(masks)){
      # The default mask list (if none are provided), is to not mask anything
      masks = lapply(gdim(value), seq_len)
    }
    else if (is.null(masks)){
      masks = list(mask)
    }
    else if (!is.null(mask)){
      stop("mask and masks cannot both be specified!")
    }

    ret = new("MaskedArray", value=value, masks=masks, dynamic_mask = dynamic_mask)
    validObject(ret)
    ret
}


#' @export
setMethod("[", signature = c(x = "MaskedArray"), function(x, i, j, ..., drop){
  if (missing(j)){
    indices = list(i)
  }
  else {
    indices = list(i, j, ...)
  }
  union_masks(x, indices)
})

#' Returns a new object whose mask is the union of the existing mask, and a new mask
#' @param indices A list of index vectors
setGeneric("union_masks", function(x, indices){})
setMethod("union_masks", signature = c(x="MaskedArray"), function(x, indices){
  dims = gdim(x@value)
  dimnames =
  new_mask = lapply(seq_along(x@masks), function(i){
    # For each dimension
    if (is.null(indices[[i]])){
      # If a given axis isn't being sliced, just preserve the existing mask
      x@masks[[i]]
    }
    else {
      # Generate a vector whose length is the length of the current dimension.
      # To this vector, apply the current mask, then apply the new index.
      # The result will be an integer vector whose values are the indices
      # to preserve.
      all_indices = seq_len(dims[i])
      names(all_indices) = dimnames(x@value)[[i]]
      unname(all_indices[x@masks[[i]]][indices[[i]]])
    }
  })

  MaskedArray(
      value=x@value,
      masks = new_mask
  )
})

setGeneric("unmask", function(x){})
setMethod("unmask", signature = c(x="MaskedArray"), function(x){
    MaskedArray(value = x@value, mask = integer())
})

#' Removes all masked entries from the array and returns the result
#' @export
setGeneric("apply_mask", function(x){})
setMethod("apply_mask", signature = c(x="MaskedArray"), function(x){
  do.call(`[`, args=c(list(x@value), x@masks))
})

# Methods that can be implemented by just applying the mask
# TODO, accept argnames other than x to dispatch on
lapply(c(
  "as.numeric",
  "as.double",
  "as.logical",
  "as.character",
  "as.data.frame",
  "as.matrix",
  "as.array",
  "as.vector",
  "length",
  "dim",
  "dimnames"
), function(generic_name){
  # To implement the function, we copy the signature, and then edit the
  # body to dispatch on the result of `apply_mask()`
  impl = args(match.fun(generic_name))
  body(impl) = expression({
    x = apply_mask(x)
    callGeneric()
  })
  setMethod(generic_name, c(x = "MaskedArray"), impl)
})

#' Generalised dimensions
#'
#' @param x An object to find the dimensions of
gdim = function(x){
  if (is.vector(x)){
    length(x)
  }
  else {
    dim(x)
  }
}

#' Given a vector that will be used as an index,
#' returns a vector of the same type that returns
#' the inverse of the original vector when used as an index
invert_index = function(index){
    if(is.numeric(index)){
        -index
    }
    else if (is.logical(index)){
        !index
    }
    else {
        stop("Only a logical or numeric vector can be passed to this function!")
    }
}
