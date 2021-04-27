#' This is my constructor function
#'
#'
#'This function will create a list output
#'
#'The constructor makes an object filled with valuable material
#'Need to be more specific here.
#' @param x vector of independent variable
#' @param y vector of dependent variable
#'
#' @return list of data and lm objects
#' @export
#'
#' @examples
#' myconstr(x,y)
myconstr = function(x,y){
  ylm = lm(y~x)
  obj = list(data = list(x = x, y = y), ylm = ylm)
  class(obj) = "constr"
  obj
}
print("hello")
