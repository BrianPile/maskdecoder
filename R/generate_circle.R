#' Generate points along a circle
#'
#' Generates `npoints` evenly spaced points along the circumference of a circle
#' defined by its center and diameter.
#'
#' @param center Numeric vector of length 2 giving the circle center as
#'   `c(x, y)`.
#' @param diameter Numeric scalar giving the circle diameter.
#' @param npoints Integer scalar giving the number of points to generate along
#'   the circle.
#'
#' @returns A data frame with `npoints` rows and two columns:
#'   \describe{
#'     \item{x}{x-coordinate of each point}
#'     \item{y}{y-coordinate of each point}
#'   }
#'
#' @export
#'
#' @examples
#' generate_circle()
#'
#' generate_circle(center = c(1, 2), diameter = 4, npoints = 50)
generate_circle = function(center = c(0, 0), diameter = 1, npoints = 100) {
  r = diameter / 2
  tt = seq(0, 2*pi, length.out = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
