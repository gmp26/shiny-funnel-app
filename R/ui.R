#
# user interface:
#
#' @import "shiny"
ui <- shiny::fluidPage(
  titlePanel("Funnel or Slice Plot"),

  sidebarLayout(
    sidebarPanel(
      
      # tag$div("Hello"),
      
      # tanglekit::tk_drag(1, " pies"),
      
      fileInput("upload", "Upload a CSV file",
                multiple = FALSE,
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'),
                width = NULL),

      textInput("title", "Chart title", value = "NY Cardiac Surgery", width = NULL, placeholder = "title"),
      textInput("ylabel", "Observed label", value = "Survival rate", width = NULL, placeholder = "y-axis label"),
      textInput("xlabel", "Number of operations label", value = "Number of operations per hospital", width = NULL, placeholder = "x-axis label"),
      fluidRow(
        column(6,
               selectInput("plot", "Plot type:",
                           c("funnel" = "funnel",
                             "slice" = "slice"))),
        conditionalPanel(
          condition = "input.plot == 'slice'",
          column(6,
                 selectInput("rank", "Rank by:",
                             c("precision" = "precision",
                               "name" = "none"
                               #,"outcome" = "outcome"
                             ))))),
      fluidRow(
        conditionalPanel(
          condition = "input.plot == 'slice'",
          column(6,
                 checkboxInput("riskadj", "Risk adjusted", FALSE)),
          column(6,
                 conditionalPanel(
                   condition = "input.riskadj",
                   checkboxInput("RASRplot", "RASR plot", FALSE))))),

      fluidRow(
        column(6,
               checkboxInput("mean.target", "Mean target", TRUE)),
        column(6,
               checkboxInput("plot.target", "Plot target", FALSE))),

      fluidRow(
        column(6,
               checkboxInput("ypercent", "y percent", TRUE))
        #column(6,
        #      checkboxInput("plot.target", "Plot target", FALSE))
      ),

      fluidRow(
        column(6,
               numericInput("tail.low", "low tail", 0.001, min = 0, max = 1, step = 0.001)),
        column(6,
               numericInput("tail.high", "high tail", 0.025, min = 0, max = 1, step = 0.001))
      ),

      width = 4),

    # Create a spot for the barplot
    mainPanel(
      plotOutput("funnelPlot", height = "600px", width = "100%"),
      width = 8
    )
  )
)
