#' Retrieve UQ colour palettes, apply transparency
#'
#' @param ... (Optional) Specify subset of palette colours to include
#' @param palette Palette name (one of `main`, `secondary`, `bw`, `discrete`, `continuous`, or `neutral`). Default: `main`
#' @param alpha Colour transparency. Default: 1 (no transparency)
#'
#' @return Named vector of colours
#' @importFrom grDevices col2rgb rgb
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # No alpha, default palette (`main`)
#' ggplot(mtcars, aes(x = cyl, y = mean(qsec))) +
#'  geom_bar(stat = "identity",
#'           fill = colours_uq())
#'
#' # You can change the transparency using `alpha`
#' ggplot(mtcars, aes(x = cyl, y = mean(qsec))) +
#'  geom_bar(stat = "identity",
#'           fill = colours_uq(alpha = 0.5))
#'
#' # You can also use specific colours from a palette
#' ggplot(mtcars, aes(x = cyl, y = mean(qsec))) +
#'  geom_bar(stat = "identity",
#'           fill = colours_uq("Light purple", palette = "secondary"))
colours_uq <- function(...,
                       palette = c("main",
                                   "secondary",
                                   "bw",
                                   "discrete",
                                   "continuous",
                                   "neutral"),
                       alpha = 1
                       ) {
  cols <- c(...)
  palette <- match.arg(palette)

  if (alpha > 1L | alpha <= 0L){
    stop("alpha must be within (0, 1]")
  }

  if(is.null(cols)){
    raw_palette <- colour_data[["uq"]][[palette]]
  } else {
    raw_palette <- colour_data[["uq"]][[palette]][cols]
  }

  rgb_palette <- grDevices::col2rgb(raw_palette)

  alpha_palette <- grDevices::rgb(
    rgb_palette[1L, ], rgb_palette[2L, ], rgb_palette[3L, ],
    alpha = alpha * 255L,
    names = names(raw_palette),
    maxColorValue = 255L
  )

  return(alpha_palette)
}

#' Discrete ggplot2 palette for `fill`
#'
#' @param palette Palette name (one of `main`, `secondary`, `bw`, `discrete`, `continuous`, or `neutral`). Default: `main`
#' @param alpha Colour transparency. Default: 1 (no transparency)
#' @param reverse If `TRUE`, reverse the colour palette order. Default: `FALSE`
#' @param ... Additional arguments to pass to `ggplot2::discrete_scale()`
#'
#' @return A discrete ggplot2 palette for `fill`
#' @importFrom ggplot2 discrete_scale
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' example_plot <-
#' ggplot(diamonds, aes(x = cut, y = price, fill = cut)) +
#'  stat_summary(fun = "mean", geom = "bar", colour = "black")
#'
#' # plain palette
#' example_plot + scale_fill_uq_d(palette = "discrete")
#'
#' # with transparency
#' example_plot + scale_fill_uq_d(palette = "discrete",
#'                                alpha = 0.6)
#'
#' # reverse palette order
#' example_plot + scale_fill_uq_d(palette = "discrete",
#'                                reverse = TRUE)
scale_fill_uq_d <- function(palette = c("main",
                                        "secondary",
                                        "bw",
                                        "discrete",
                                        "continuous",
                                        "neutral"),
                            alpha = 1,
                            reverse = FALSE,
                            ...){
  palette <- match.arg(palette)

  palette_colours <-
    palette_constructor(colours = colours_uq(palette = palette,
                                             alpha = alpha),
                        type = "discrete",
                        reverse = reverse)

  ggplot2::discrete_scale("fill", "uq", palette_colours, ...)
}

#' Discrete ggplot2 palette for `colour`
#'
#' @param palette Palette name (one of `main`, `secondary`, `bw`, `discrete`, `continuous`, or `neutral`). Default: `main`
#' @param alpha Colour transparency. Default: 1 (no transparency)
#' @param reverse If `TRUE`, reverse the colour palette order. Default: `FALSE`
#' @param ... Additional arguments to pass to `ggplot2::discrete_scale()`
#'
#' @return A discrete ggplot2 palette for `colour`
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' example_plot <-
#' ggplot(diamonds, aes(x = cut, y = price, colour = cut)) +
#'   stat_summary(fun = "mean", geom = "bar", fill = "white")
#'
#' # plain palette
#' example_plot + scale_colour_uq_d(palette = "discrete")
#'
#' # with transparency
#' example_plot + scale_colour_uq_d(palette = "discrete",
#'                                  alpha = 0.6)
#'
#' # reverse palette order
#' example_plot + scale_colour_uq_d(palette = "discrete",
#'                                  reverse = TRUE)
scale_colour_uq_d <- function(palette = c("main",
                                        "secondary",
                                        "bw",
                                        "discrete",
                                        "continuous",
                                        "neutral"),
                            alpha = 1,
                            reverse = FALSE,
                            ...){
  palette <- match.arg(palette)

  palette_colours <-
    palette_constructor(colours = colours_uq(palette = palette,
                                             alpha = alpha),
                        type = "discrete",
                        reverse = reverse)

  ggplot2::discrete_scale("colour", "uq", palette_colours, ...)

}


#' Continuous ggplot2 palette for `fill`
#'
#' @param palette Palette name (one of `main`, `secondary`, `bw`, `discrete`, `continuous`, or `neutral`). Default: `main`
#' @param alpha Colour transparency. Default: 1 (no transparency)
#' @param reverse If `TRUE`, reverse the colour palette order. Default: `FALSE`
#' @param ... Additional arguments to pass to `ggplot2::discrete_scale()`
#'
#' @return A continuous ggplot2 palette for `fill`
#' @importFrom ggplot2 scale_fill_gradientn
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' example_plot <-
#' ggplot(faithfuld,
#'        aes(waiting, eruptions, fill = density)) +
#'        geom_tile()
#'
#' # plain palette
#' example_plot + scale_fill_uq_c(palette = "continuous")
#'
#' # with transparency
#' example_plot + scale_fill_uq_c(palette = "continuous",
#'                                alpha = 0.6)
#'
#' # reverse palette order
#' example_plot + scale_fill_uq_c(palette = "continuous",
#'                                reverse = TRUE)
scale_fill_uq_c <- function(palette = c("main",
                                        "secondary",
                                        "bw",
                                        "discrete",
                                        "continuous",
                                        "neutral"),
                            alpha = 1,
                            reverse = FALSE,
                            ...){
  palette <- match.arg(palette)

  palette_colours <-
    palette_constructor(colours = colours_uq(palette = palette,
                                             alpha = alpha),
                        type = "continuous",
                        reverse = reverse)

  ggplot2::scale_fill_gradientn(colours = palette_colours, ...)
}

#' Continuous ggplot2 palette for `colour`
#'
#' @param palette Palette name (one of `main`, `secondary`, `bw`, `discrete`, `continuous`, or `neutral`). Default: `main`
#' @param alpha Colour transparency. Default: 1 (no transparency)
#' @param reverse If `TRUE`, reverse the colour palette order. Default: `FALSE`
#' @param ... Additional arguments to pass to `ggplot2::discrete_scale()`
#'
#' @return A continuous ggplot2 palette for `colour`
#' @importFrom ggplot2 scale_fill_gradientn
#' @export
#'
#' @examples
#' example_plot <-
#' ggplot(diamonds, aes(carat, depth, colour = price)) +
#'  geom_point()
#'
#' # normal palette
#' example_plot + scale_colour_uq_c(palette = "continuous")
#'
#' # add transparency
#' example_plot + scale_colour_uq_c(palette = "continuous",
#'                                  alpha = 0.6)
#' # reverse palette order
#' example_plot + scale_colour_uq_c(palette = "continuous",
#'                                  reverse = TRUE)
scale_colour_uq_c <- function(palette = c("main",
                                          "secondary",
                                          "bw",
                                          "discrete",
                                          "continuous",
                                          "neutral"),
                              alpha = 1,
                              reverse = FALSE,
                              ...){
  palette <- match.arg(palette)

  palette_colours <-
    palette_constructor(colours = colours_uq(palette = palette,
                                             alpha = alpha),
                        type = "continuous",
                        reverse = reverse)

  ggplot2::scale_colour_gradientn(colours = palette_colours, ...)
}

# British to American spellings

#' @export
#' @usage NULL
colors_uq <- colours_uq

#' @export
#' @usage NULL
scale_color_uq_d <- scale_colour_uq_d

#' @export
#' @usage NULL
scale_color_uq_c <- scale_colour_uq_c
