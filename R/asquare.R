#' asquare
#'
#' Draws a 100px x 100px square in a given colour
#'
#' @import htmlwidgets
#'
#' @export
asquare <- function(rgb_string, width = NULL, height = NULL) {

  # forward options using x
  x = list(
    fill = rgb_string
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'asquare',
    x,
    width = width,
    height = height,
    package = 'rcljsdemo'
  )
}

#' Shiny bindings for asquare
#'
#' Output and render functions for using asquare within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a asquare
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name asquare-shiny
#'
#' @export
asquareOutput <- function(outputId, width = '120px', height = '120px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'asquare', width, height, package = 'rcljsdemo')
}

#' @rdname asquare-shiny
#' @export
renderAsquare <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, asquareOutput, env, quoted = TRUE)
}

