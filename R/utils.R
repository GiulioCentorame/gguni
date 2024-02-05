#' Validate vector of colours
#'
#' Adapted from [this StackOverflow post](https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation)
#' @param x A vector of objects that can be coerced as a colour by `col2rgb()` (see `?col2rgb` for more information)
#'
#' @return Named vector of logical values
#' @importFrom grDevices col2rgb
#' @export
#'
#' @examples
#' my_colours <- c("Colour1" = "#FFFFFF", "Colour2" = "#2", "Colour3" = "snow1", "Colour4" = "2")
#'
#' areColours(my_colours)
areColours <- function(x) {
  vapply(x, function(element) {
    # Try to split colour into RGB componennt.
    # If it fails, return FALSE
    tryCatch(is.matrix(grDevices::col2rgb(element)),
             error = function(e) FALSE)
  },
  FUN.VALUE = TRUE
  )
}

#' Construct palettes from a vector of colours
#'
#' Returns either a function with the exact discrete values for the palette (for `type = "discrete"`), or interpolated values for the missing colours (for `type = "continuous"`; 512 by default).
#'
#' The function returned by `type = "discrete"` uses the format `palette(n)`, where `n` specifies the range `1:n` of colours to show.
#'
#' The reason why the outputs are different is that the discrete value function is used by `ggplot2::discrete_scale`, while the continuous values are used by `ggplot2::scale_{fill,colour}_gradientn()`.
#' @param colours Vector of colours (as Hex or another object )
#' @param type Either `discrete` for discrete scales, or `continuous` for continuous scales
#' @param reverse Logical; if `TRUE` reverses the order of the colours in `colours`. Default: `FALSE`
#' @param n Optional; `integer` value with the number of colours to interpolate (if `type = "continuous"`). Default: 512
#' @param ... Optional; additional arguments to pass to `grDevices::colorRampPalette()` (if `type = "continuous"`)
#'
#' @return Vector of colours
#' @importFrom grDevices colorRampPalette
#' @importFrom scales manual_pal
#' @export
#'
#' @examples
#' library(scales)
#'
#' my_palette <- c("My black" = "black", "My white" = "white")
#'
#' # Construct a discrete palette
#' d1 <-
#' palette_constructor(colours = my_palette,
#'                     type = "discrete")
#'
#' # Show two colours
#' show_col(d1(2))
#'
#' # Show only one colour
#' show_col(d1(1))
#'
#' # Construct a continuous palette (my 2 colours + 8 intermediate colours)
#' c1 <-
#' palette_constructor(colours = my_palette,
#'                     type = "continuous",
#'                     n = 10)
#'
#' show_col(c1)
#'
#' # Reverse palette order
#' c2 <-
#' palette_constructor(colours = my_palette,
#'                     type = "continuous",
#'                     n = 10,
#'                     reverse = TRUE)
#'
#' show_col(c2)
palette_constructor <- function(colours = NULL,
                                type  = c("discrete",
                                          "continuous"),
                                reverse = FALSE,
                                n = 512L,
                                ...){
  if(is.null(colours)) {
    stop("You must define the colours for the palette")
  }

  type <- match.arg(type)

  # Reverse colour order
  if(reverse) {
    colours <- rev(colours)
  }

  if(type == "continuous"){

    palette_func <- grDevices::colorRampPalette(colours, ...)

    palette_func(n)

  } else if (type == "discrete") {
    scales::manual_pal(colours)
  }
}
