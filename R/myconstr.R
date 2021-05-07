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
myttest = function(x,y){
  ylm = lm(y~x)
  obj = list(data = list(x = x, y = y), ylm = ylm)
  class(obj) = "constr"
  obj
}

mynewf = function(x,y){
  ylm = lm(y~x)
  obj = list(data = list(x = x, y = y), ylm = ylm)
  class(obj) = "constr"
  obj
}


new_difftime <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))

  structure(x,
            class = "difftime",
            units = units
  )
}

new_difftime(c(1, 10, 3600), "secs")
#> Time differences in secs
#> [1]    1   10 3600
new_difftime(52, "weeks")
#> Time difference of 52 weeks
