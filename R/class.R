# Class, constructor, and validator definition

#' @export
setClass("MaskedArray", slots=c(value="ANY", masks="list"))

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
#' @export
MaskedArray = function(value, masks = default_mask(value)){
  new("MaskedArray", value=value, masks=masks)
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
