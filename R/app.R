######################
#' Run a shiny app to plot funnels or sliced funnels from uploaded mortality data
#' @export
run_app <- function() {
  shiny::shinyApp(ui = ui, server = server)
}
