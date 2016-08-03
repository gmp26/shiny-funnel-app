#' fsquare
#'
#' Draws a 100px x 100px square in a given colour
#'
#' @import htmlwidgets
#'
#' @export
fsquare <- function(message, width = NULL, height = NULL) {

  # forward options using x
  x = list(
    fill = message
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'fsquare',
    x,
    width = width,
    height = height,
    package = 'rcljsdemo'
  )
}

#' Shiny bindings for fsquare
#'
#' Output and render functions for using fsquare within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates an fsquare
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name fsquare-shiny
#'
#' @export
fsquareOutput <- function(outputId, width = '120px', height = '120px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'filled_square', width, height, package = 'rcljsdemo')
}

#' @rdname fsquare-shiny
#' @export
renderfsquare <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, fsquareOutput, env, quoted = TRUE)
}

