
#' @export

new_modelmatrix <- function(object, ...) UseMethod("new_modelmatrix")

#' @export

new_modelmatrix.lm <- function(object, data, ...) {
    tt <- delete.response(object$terms)
    m <- model.frame(tt, data, na.action = na.pass, xlev = object$xlevels)
    X <- model.matrix(tt, m, contrasts.arg = object$contrasts)
    return(X)
}