#' This is my constructor function
#'
#'
#'This function will create a list output
#'
#'The constructor makes an object filled with valuable material
#'Need to be more specific here.
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
myconstr = function(x,y){
  ylm = lm(y~x)
  obj = list(data = list(x = x, y = y), ylm = ylm)
  class(obj) = "constr"
  obj
}
