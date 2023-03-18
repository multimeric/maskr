# setOldClass("numeric")
# setOldClass("character")
# setOldClass("logical")
# setOldClass("integer")
# setOldClass("array")
# setOldClass("data.frame")

# setClassUnion("VectorIndex", c("data.frame", "vector"))

#' @export
#' The mask is a vector that will be used to filter the inner vector
setClass("MaskedNumeric", slots=c(value = "vector", mask="integer"))#, contains="numeric")

MaskedNumeric = function(value, mask=seq_along(value)){
    structure(new("MaskedNumeric", value=value, mask=mask), class=c("MaskedNumeric", "numeric"))
}

#' @export
setMethod("as.numeric", signature = c(x = "MaskedNumeric"), function(x){
    x@value[x@mask]
})
# as.numeric.MaskedNumeric = function(x, ...){
#     x@value[x@mask]
# }
#' @export
setMethod("is.numeric", signature = c(x = "MaskedNumeric"), function(x){
    TRUE
})
# is.numeric.MaskedNumeric = function(x){
#     TRUE
# }
#' @export
setMethod("[", signature = c(x = "MaskedNumeric"), function(x, i){
    combine_masks(x, i)
})

#' Returns a new object whose mask is the union of the existing mask, and a new mask
setGeneric("combine_masks", function(x, mask){})
setMethod("combine_masks", signature = c(x="MaskedNumeric"), function(x, mask){
    MaskedNumeric(
        value=x@value,
        mask = seq_along(x@value)[x@mask][mask]
    )
})


setGeneric("unmask", function(x){})
setMethod("unmask", signature = c(x="MaskedNumeric"), function(x){
    MaskedNumeric(value = x@value, mask = integer())
})

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
