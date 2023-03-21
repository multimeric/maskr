setClass("MaskedArray", slots=c(value="array", masks="list", dynamic_mask="logical"))

MaskedArray = function(value, masks = lapply(dim(value), seq_len), dynamic_mask=TRUE){
  new("MaskedArray", value=value, masks=masks, dynamic_mask=dynamic_mask)
}

setMethod("[", signature=c(x="MaskedArray"), function(x, i, j, ...){
  indices = if (missing(j)){
    list(i)
  } else {
    list(i, j, ...)
  }

  if (length(indices) != length(dim(x))){
    # When the dimensionality requested is different to the current
    # dimensionality, we don't bother persisting the mask
    return(do.call(`[`, c(list(x@value), indices)))
  }

  dims = dim(x@value)
  dim_names = dimnames(x@value)
  if (x@dynamic_mask){
    MaskedArray(
      value = x@value,
      masks = lapply(seq_along(x@masks), function(i){
        # Generate a vector whose length is the length of the current dimension.
        # To this vector, apply the current mask, then apply the new index.
        # The result will be an integer vector whose values are the indices
        # to preserve.
        all_indices = seq_len(dims[[i]])
        names(all_indices) = dim_names[[i]]
        unname(all_indices[x@masks[[i]]][indices[[i]]])
      })
    )
  }
  else {
    MaskedArray(
      value = do.call(`[`, x@value, indices),
      mask = lapply(seq_along(x@masks), function(i){
        x@masks[[i]][indices[[i]]]
      })
    )
  }
})
