
#
# user interface:
#
ui <- fluidPage(
  titlePanel("Funnel Plot Inputs"),

  sidebarLayout(
    sidebarPanel(
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

###################### server.R
# read csv file and provide defaults for core funnel plotter
wrapfunnel <- function(datapath,
                       title="NY Cardiac Surgery", #change when adjusted
                       plot="funnel",
                       xlabel="Number of operations per hospital", #change when adjusted
                       ylabel="Survival rate (%)",
                       rank="precision",
                       riskadj=F,
                       RASRplot=F,
                       mean.target=F,
                       plot.target=F,
                       ypercent=T,
                       tails=c(0.001,0.025)) {

  # todo: add some error handling here
  x <- read.csv(datapath,sep=",")
  N<- x$Cases
  R<- N-x$Deaths
  P = N -x$EMR*N/100
  xrange<-c(0,max(N))
  yrange<-c(min(R/N )-0.01, 1)
  names= as.character(x$Hospital)

  # test using slices
  funnel4(obs.prop=R/N,  denom=N, pred.prop=P/N, names=names,
          plot=plot, rank=rank, riskadj=riskadj, RASRplot=RASRplot,
          mean.target=mean.target, plot.target=plot.target, title=title,xrange=xrange,
          yrange=yrange, tails=tails,xlab=xlabel,ylab=ylabel,ypercent=ypercent,
          bandcols=c("white","cyan","cyan3")
  )
}

# Define server logic required to draw funnel plot
server <- function(input, output, session) {
  # Expression that generates a funnel. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  output$funnelPlot <- renderPlot({
    upload <- input$upload

    plot <- input$plot
    riskadj <- input$riskadj
    if(plot == "funnel" && riskadj) {
      updateCheckboxInput(session, "riskadj", value = FALSE)
    }

    title <- input$title
    if(riskadj) {
      title <- paste(title, "(adjusted)")
    }

    ylabel <- input$ylabel
    ypercent <- input$ypercent
    if(ypercent) {
      ylabel <- paste(ylabel, "(%)")
    }

    if (is.null(upload))
      return(NULL)

    tail.low <- input$tail.low
    tail.high <- max(input$tail.high, tail.low)
    if(tail.high != input$tail.high) {
      updateNumericInput(session, "tail.high", value = tail.high)
    }

    wrapfunnel(datapath=upload$datapath,
               title=title,
               xlabel=input$xlabel,
               ylabel=ylabel,
               plot=plot,
               rank=input$rank,
               riskadj = riskadj,
               RASRplot = input$RASRplot,
               mean.target = input$mean.target,
               plot.target = input$plot.target,
               ypercent = ypercent,
               tails = c(tail.low, tail.high)
    )

  })
}


######################
shiny::shinyApp(ui = ui, server = server)
