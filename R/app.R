######################
#' Run a shiny app to plot funnels or sliced funnels from uploaded mortality data
#' @export
demo1 <- function() {
  shiny::shinyApp(ui = ui, server = server)
}
