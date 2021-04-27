#' This is my constructor function
#'
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
