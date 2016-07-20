library("shiny")
source(file="funnel4.R")

# x<-read.csv(file="CABG-hospitals-03.csv",header=T,sep=",")

funnel <- function(datapath,
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
  #browser()
  x <- read.csv(datapath,sep=",")
  N<- x$Cases
  R<- N-x$Deaths
  P = N -x$EMR*N/100
  #xlabel<-"Number of operations per hospital"
  #xlabel<-"Number of operations per hospital (adjusted)"
  xrange<-c(0,max(N))
  # ylabel<-"Mortality rate (%)"
  #yrange<-c(0,max( R/N ))
  yrange<-c(min(R/N )-0.01, 1)
  #yrange=c(0,1)
  names= as.character(x$Hospital)
  #tails=c(0.001,0.025)
  
  # test using slices
  
  # 1.  not risk-adjusted
  #title<-"NY Cardiac Surgery - not risk-adjusted"
  #ylabel<-"Survival rate (%)"
  funnel4(obs.prop=R/N,  denom=N, pred.prop=P/N, names=names,
          plot=plot, rank=rank, riskadj=riskadj, RASRplot=RASRplot,
          mean.target=mean.target, plot.target=plot.target, title=title,xrange=xrange,
          yrange=yrange, tails=tails,xlab=xlabel,ylab=ylabel,ypercent=ypercent,
          bandcols=c("white","cyan","cyan3")
  ) 
}

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
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
    
    funnel(datapath=upload$datapath,
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
})