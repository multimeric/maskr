setClass("MaskedArray", slots=c(value="ANY", masks="list", dynamic_mask="logical"))

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
MaskedArray = function(value, masks = default_mask(value), dynamic_mask=TRUE){
  new("MaskedArray", value=value, masks=masks, dynamic_mask=dynamic_mask)
}

setValidity("MaskedArray", function(object){
  # These are lists of either TRUE (when a validation passes), or a character
  # string (when the validation fails)
  checks = list(
    if (is.null(gdim(object@value))) "The array is a type that has no dimensions!" else TRUE,
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
  failures = unlist(all_checks[vapply(all_checks, is.character, logical(1))])
  if (length(failures) == 0) TRUE else failures
})

setMethod("[", signature=c(x="MaskedArray"), function(x, i, j, ...){
  indices = if (missing(j)){
    list(i)
  } else {
    list(i, j, ...)
  }

  dims = gdim(x@value)
  dim_names = gdimnames(x@value)

  if (length(indices) != length(dims)){
    # When the dimensionality requested is different to the current
    # dimensionality, we don't bother persisting the mask
    return(do.call(`[`, c(list(x@value), indices)))
  }

  if (x@dynamic_mask){
    MaskedArray(
      value = x@value,
      masks = lapply(seq_along(x@masks), function(i){
        # Generate a vector whose length is the length of the current dimension.
        # To this vector, apply the current mask, then apply the new index.
        # The result will be an integer vector whose values are the indices
        # to preserve.
        all_indices = seq_len(dims[[i]])
        if (is.character(indices[[i]])){
          names(all_indices) = dim_names[[i]]
          unname(all_indices[x@masks[[i]]][indices[[i]]])
        }
        else {
          all_indices[x@masks[[i]]][indices[[i]]]
        }
      }),
      dynamic_mask = x@dynamic_mask
    )
  }
  else {
    MaskedArray(
      # Apply the indices to the array like it was unmasked
      value = do.call(`[`, c(list(x@value), indices)),
      masks = lapply(seq_along(x@masks), function(i){
        x@masks[[i]][indices[[i]]]
      }),
      dynamic_mask = x@dynamic_mask
    )
  }
})

#' Maps indices applied to a masked array back to the original array
map_indices = function(index, mask, axis_len){
  # Make a vector with the same length as the unmasked axis,
  # which is 0 everywhere except where an element is masked out
  masked_out = rep_len(0, axis_len)
  masked_out[invert_integer_indices(mask, axis_len)] = 1
  # desync maps an index to the number of masked elements that precede that index
  desync = cumsum(masked_out)
  # desync[mask] is a post-mask array showing how each element of the array
  # has been shifted.
  # We correct the indices by adding on the amount each index has been shifted
  index + desync[mask][index]
}

invert_integer_indices = function(indices, len){
  seq_len(len)[-indices]
}

default_mask = function(value){
  lapply(gdim(value), seq_len)
}

gdim = function(x){
  if (is.vector(x)){
    length(x)
  }
  else {
    dim(x)
  }
}

gdimnames = function(x){
  if (is.vector(x)){
    names(x)
  }
  dimnames(x)
}

setGeneric("unmask", function(x){})
setMethod("unmask", signature = c(x="MaskedArray"), function(x){
  MaskedArray(value = x@value, mask = default_mask(x@value))
})

#' Removes all masked entries from the array and returns the result
#' @export
setGeneric("apply_mask", function(x){})
setMethod("apply_mask", signature = c(x="MaskedArray"), function(x){
  do.call(`[`, args=c(list(x@value), x@masks))
})

# Methods that can be implemented by just applying the mask
# TODO, accept argnames other than x to dispatch on
APPLIED_MASK_FUNCS = c(
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
)

# Implement each of the above methods
lapply(APPLIED_MASK_FUNCS, function(generic_name){
  # To implement the function, we copy the signature, and then edit the
  # body to dispatch on the result of `apply_mask()`
  impl = args(match.fun(generic_name))
  body(impl) = expression({
    x = apply_mask(x)
    callGeneric()
  })
  setMethod(generic_name, c(x = "MaskedArray"), impl)
})


