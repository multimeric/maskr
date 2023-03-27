# Internal utility methods

#' Returns the default mask for an array like
#' @param value An array-like object
#' @noRd
default_mask = function(value){
  lapply(gdim(value), seq_len)
}

#' Generalised version of [base::dim()], which also works on vectors
#' @noRd
gdim = function(x){
  if (is.vector(x)){
    length(x)
  }
  else {
    dim(x)
  }
}

#' Generalised version of [base::dimnames()], which also works on vectors
#' @noRd
gdimnames = function(x){
  if (is.vector(x)){
    names(x)
  }
  dimnames(x)
}
