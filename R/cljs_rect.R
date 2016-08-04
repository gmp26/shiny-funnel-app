#' cljs_rect
#'
#' Draws a filled cljs_rectangle
#'
#' @import htmlwidgets
#'
#' @export
cljs_rect <- function(fill = "#fa0", width = NULL, height = NULL) {

  # forward options using x
  x = list(
    fill = fill
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'cljs_rect',
    x,
    width = width,
    height = height,
    package = 'rcljsdemo'
  )
}

#' Shiny bindings for cljs_rect
#'
#' Output and render functions for using cljs_rect within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a cljs_rect
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name cljs_rect-shiny
#'
#' @export
cljs_rectOutput <- function(outputId, width = '120px', height = '120px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'cljs_rect', width, height, package = 'rcljsdemo')
}

#' @rdname cljs_rect-shiny
#' @export
renderCljs_rect <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, cljs_rectOutput, env, quoted = TRUE)
}

