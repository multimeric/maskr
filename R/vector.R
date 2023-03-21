setClass("MaskedVector", slots=c(value="vector", mask="vector"))

MaskedVector = function(value, mask = seq_along(value), dynamic_mask=TRUE){
  new("MaskedVector", value=value, mask=mask, dynamic_mask=dynamic_mask)
}

setMethod("[", signature=c(x="MaskedVector", i="vector"), function(x, i){
  if (x@dynamic_mask){
    MaskedVector(
      value = x@value,
      mask = union_mask(x@mask, i)
    )
  }
  else {
    MaskedVector(
      value = x@value[i],
      mask = x@mask[i]
    )
  }
})
