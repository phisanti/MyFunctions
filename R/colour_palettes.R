#' @title My color set.
#'
#' @description My own personalised set of colours. Original code from Simon Jackson (twitter:
#' [at]drsimonj, https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2).
#'
#' @param `...` `character`: One of my defined coulurs in the palette. Currently, "light grey",
#' "blue", "green", "orange", "yellow", "red", "dark grey"
#' @author Santiago Caño-Muñiz
#'
#' @export

my_cols <- function(...) {
  cols <- c(...)
  color_set <- c(
    `light grey` = "#cccccc",
    `blue`       = "#00aedb",
    `green`      = "#00b159",
    `orange`     = "#f37735",
    `yellow`     = "#ffc425",
    `red`        = "#d11141",
    `dark grey`  = "#8c8c8c")

  if (is.null(cols))
    return (color_set)

  color_set[cols]
}

#' @export

my_palettes <- list(
  `contrast` = my_cols("light grey", "blue", "green", "organge", "yellow", "red"),

  `main`  = my_cols("blue", "green", "yellow"),

  `cool`  = my_cols("blue", "green"),

  `hot`   = my_cols("yellow", "orange", "red"),

  `mixed` = my_cols("blue", "green", "yellow", "orange", "red"),

  `grey`  = my_cols("light grey", "dark grey")
  )

#' @title Get my palette.
#'
#' @description Wrapper function to call the colours from my palettes. Original code from Simon Jackson (twitter:
#' [at]drsimonj, https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2)
#'
#' @author Santiago Caño-Muñiz
#'
#' @inheritDotParams grDevices::colorRampPalette
#' @param palette char: Name of the colour set. Currently: "contrast", "main", "cool", "hot", "mixed" and "grey".
#' @param reverse Logical. Should the order of the colors be reversed?
#'
#' @export
#'

get_mypalette <- function(palette = "main", reverse = FALSE, ...) {
  pal <- my_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' @title My palette wrapper for ggplot2.
#'
#' @description See link[MyFunctions]{get_mypalette}
#'
#' @inheritDotParams ggplot2::scale_color_gradientn
#' @param palette `character`: Name of the colour set. Currently: "contrast", "main", "cool", "hot", "mixed" and "grey".
#' @param reverse `logical`: Should the order of the colors be reversed?
#' @param discrete `logical`: selects palette for continuous or discrete data
#' @author Santiago Caño-Muñiz
#' @rdname scale_color_santi
#' @export
#'

scale_color_santi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- get_mypalette(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname scale_color_santi
#' @export

scale_fill_santi <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- get_mypalette(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
