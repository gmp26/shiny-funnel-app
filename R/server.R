###################### server.R

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
    
    x <- read.csv(upload$datapath,sep=",")
    funnel::funnelslice(x,
                        plot_title=title,
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
