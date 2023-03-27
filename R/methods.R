# Public methods for the MaskedArray class

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
    })
  )
})

#' Removes the mask of a `MaskedArray`, exposing the full array
#' @param x A `MaskedArray` object
#' @return The same type as `x@value`: some array-like object.
#' @export
setGeneric("unmask", function(x){})
setMethod("unmask", signature = c(x="MaskedArray"), function(x){
  x@value
})

#' Resets the mask of a `MaskedArray`, returning a new `MaskedArray` which
#' has no masked/hidden elements
#' @param x A `MaskedArray` object
#' @return A `MaskedArray` object
#' @export
setGeneric("reset_mask", function(x){})
setMethod("reset_mask", signature = c(x="MaskedArray"), function(x){
  MaskedArray(value = x@value, mask = default_mask(x@value))
})

#' Removes all masked entries from the array and returns the result
#' @param x A `MaskedArray` object
#' @return The same type as `x@value`, but shorter
#' @export
setGeneric("apply_mask", function(x){})
setMethod("apply_mask", signature = c(x="MaskedArray"), function(x){
  do.call(`[`, args=c(list(x@value), x@masks))
})

#' @export
setMethod("[[", signature = c(x="MaskedArray"), function(x, i, j, ..., drop){
  # We can't use the below hack to generate this method, because it's some
  # kind of special method whose arguments are impossible to access dynamically
  x = apply_mask(x)
  callGeneric()
})

###
# Methods that can be implemented by just applying the mask
###
# TODO, accept argnames other than x to dispatch on

APPLIED_MASK_S3 = c(
  "as.matrix",
  "as.array",
  "mean"
)
# Implement each of the above methods
lapply(APPLIED_MASK_S3, function(generic_name){
  name = paste0(generic_name, ".MaskedArray")
  package = Filter(function(env){
    environmentName(env) == "maskr"
  }, sys.frames())[[1]]
  assign(name, envir = package, value = function(...){
    # Setting x allows us to dispatch on its new value
    x = apply_mask(x)
    # NextMethod will use the arguments that actually exist in the function
    # environment, even though we can't see them
    NextMethod()
  })
})
# This exports all dynamic S3 methods:
#' @evalNamespace { paste0("S3method(", APPLIED_MASK_S3, ", MaskedArray)") }
NULL

APPLIED_MASK_S4 = c(
  "as.numeric",
  "as.double",
  "as.logical",
  "as.character",
  "as.integer",
  "as.data.frame",
  "as.vector",
  "length",
  "dim",
  "dimnames"
)
# Implement each of the above methods
lapply(APPLIED_MASK_S4, function(generic_name){
  # S4 automagically fixes the function signature to include the real arguments
  # so we don't have to list them here
  setMethod(generic_name, c(x = "MaskedArray"), function(x){
    # Setting x allows us to dispatch on its new value
    x = apply_mask(x)
    # callGeneric will use the arguments that actually exist in the function
    # environment, even though we can't see them
    callGeneric()
  })
})

