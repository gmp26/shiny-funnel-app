# February 2016. New funnel program

#####
#' Core funnel and slice plot generator
#' @importFrom "graphics" "axis" "par" "plot" "points" "polygon"
#' @import "stats"
#' @importFrom "utils" "read.csv"
#' @export
#' @param obs.prop A data frame of mortality observations from a number of hospitals.
#' @param denom Number of cases
#' @param pred.prop Predicted survival proportion
#' @param names Names of hospitals
#' @param plot The plot type
#' @param rank The plot order - by "precision" or alphabetically by "name"
#' @param riskadj Make a risk adjusted plot if TRUE
#' @param RASRplot Work with risk adjusted mortality data if TRUE, else raw mortality rates
#' @param ratedenom Unused
#' @param mean.target Calculate target from mean
#' @param target Given target (?)
#' @param plot.target Plot target if TRUE
#' @param title The plot target
#' @param scale Axis scale
#' @param xrange x axis range
#' @param yrange y axis range
#' @param xlab x axis label (?)
#' @param ylab y axis label (?)
#' @param pointsymbol NUmeric code identifying the plot symbol
#' @param legend Use legend if TRUE
#' @param ypercent Show survival as percentage if true
#' @param tails A pair of p-values determining the inner and outer confidence limits (?)
#' @param Npoints The number of bernouilli trials used in an integer binomial approximation (?)
#' @param bandcols Vector of colours used in bars
#####
funnel4 <-
  function(obs.prop=NA, denom=NA, pred.prop=NA, names=NA,
           plot = "funnel", rank="none", riskadj=F, RASRplot=F,
           ratedenom = NA,	mean.target = T, target = NA, plot.target=F,
           title = "", scale = 0.7, xrange = c(	0, 1000), yrange = c(0, 1),
           tails = c(0.001, 0.025), Npoints = 200, xlab = "", ylab = "",
           pointsymbol=16, legend=1, ypercent=T,   bandcols=c("white","lightblue","azure"))
  {

    #  SET UP INDICATOR AND TARGET

    numer     = obs.prop * denom
    ##########################################################
    #  simple Binoomial with no risk adjustment
    ##########################################################
    if(riskadj==F){
      y <- numer/denom  # do something to check no zeros in denominator?
      if(mean.target == T) {
        target <- sum(numer[])/sum(denom[])  # scalar
      }
      #    ps.and.qs(y,denom,target,names,tails)  #  calculate p and q values

      if(plot=="funnel") {
        # calculate limits for different precisions - need to be integers!
        # have to do this outside as may need to transform
        precisions <- round(xrange[1] + ((1:Npoints) * (xrange[2] - xrange[1]))/Npoints)

        limits= binomial.limits(precisions,rep(target,Npoints),tails)

        # transform limits? Not with basic Binomial
        plot.funnel(y,denom, precisions, target, plot.target,title, xrange, yrange,
                    tails, limits, xlab, ylab,pointsymbol,  legend, ypercent, bandcols  )
      }

      if(plot=="slice") {   # plot slice
        limits= binomial.limits(denom,rep(target,length(denom)),tails)  #
        plot.slice(y, denom, names, target, plot.target, rank, title, xrange, yrange,
                   tails, limits, xlab, ylab,pointsymbol, ypercent, bandcols  )
      }
    }
    ##########################################################
    #  simple Binoomial with  risk adjustment
    ##########################################################
    if(riskadj==T){

      pred = pred.prop * denom  # Predicted numerator
      #    if(meantarget==T){target <- sum(pred[])/sum(denom[])} # standardise around predicted
      #    ps.and.qs( , )  #  calculate p and q values
      y <- numer/denom         #do something to check no zeros in denominator?
      target = pred.prop  # make target a vector with different targets
      mean.pred=sum(pred)/sum(denom)  # overall predicted e

      if(RASRplot==F){
        # work with observed survival rate , with limits around predicted (just slices)
        if(plot=="slice") {   # plot slice
          limits= binomial.limits(denom,target,tails)  #
          plot.slice(y, denom, names, target, plot.target, rank, title, xrange, yrange,
                     tails, limits, xlab, ylab,pointsymbol, ypercent, bandcols  )
        }
      }

      if(RASRplot==T){     # work with RASR, can have funnel or slice
        logit.y.adj = log((numer+0.5)/(denom-numer+0.5)) - log((pred+0.5)/(denom-pred+0.5)) + log(mean.pred/(1-mean.pred))
        y.adj  = 1/(1+ exp(-logit.y.adj))

        if(plot=="funnel") {
          # calculate limits for different precisions - need to be integers!
          denom.adj= round(denom * mean.pred*(1-mean.pred)/(pred.prop*(1-pred.prop) )
                           *( (y-pred.prop)/(y.adj-mean.pred)    )^2 ) # 'adjusted sample size', rounded
          precisions <- round(xrange[1] + ((1:Npoints) * (xrange[2] - xrange[1]))/Npoints)
          limits= binomial.limits(precisions,rep(target,Npoints),tails)
          # transform denoms and y's for plotting
          plot.funnel(y.adj,denom.adj, precisions, target, plot.target,title, xrange, yrange,
                      tails, limits, xlab, ylab,pointsymbol,  legend, ypercent, bandcols  )
        }

        if(plot=="slice") {   # plot slice using transformed 'exact' limits
          limits= binomial.limits(denom,target,tails)  # same limits for non-risk-adjusted
          logit.limits.adj = log(limits/(1-limits)) - log((pred+0.5)/(denom-pred+0.5)) + log(mean.pred/(1-mean.pred)) #limits for risk-adjusted
          limits.adj= 1/(1+ exp(-logit.limits.adj))
          # browser()
          plot.slice(y.adj, denom, names, target, plot.target, rank, title, xrange, yrange,
                     tails, limits.adj, xlab, ylab,pointsymbol, ypercent, bandcols  )
        }
      }
    } # end of riskadj==T

  }  # End of main function


######################################
# function to set Binomial limits
######################################

binomial.limits=function(precisions,target,tails){
  num.limits<-length(tails)
  lowerlimits <- upperlimits  <- matrix(0,length(precisions),num.limits )
  for(j in 1:num.limits) {
    lowerlimits[,j  ] <- qbinom.interp(tails[j], precisions, target,tail="lower")
    upperlimits[,j  ] <- qbinom.interp(tails[j], precisions, target,tail="upper")
  }
  cbind(lowerlimits,upperlimits)
}

# test   binomial.limits(100:150,0.4,c(0.001,0.025))


######################################
#  Function to plot slices
####################################

#   assume labs come in as matrix - then proceed to printing full table later
plot.slice=function(y, precision,names, target, plot.target,  rank, title, xrange, yrange,
                    tails,limits, xlab, ylab,pointsymbol, ypercent, bandcols  ) {
  nunits=length(y)
  scale=1
  ord=1:nunits  # current order
  if(rank=="outcome"){ord=order(y)}
  if(rank=="precision"){ord=order(precision)}
  # set up display

  labs=as.matrix(cbind(names))
  par(mgp = c(2, 0.75, 0))
  par(mar = c(3.5, trunc(max(nchar(labs[, 1]))/2) + 3, 1, 2))
  par(adj = 0.5) # centred
  plot(yrange[1], 1, type = "n", bty="n", ylim = c(0, nunits + 1),
       xlim = yrange, ylab = "",xlab =ylab,main=title,axes=F)


  # plot target  , need to just do single vertical line
  if(plot.target==T){
    for(i in nunits:1){
      par(new=T)
      plot(c(target[ord[i]], target[ord[i]]),c(i,i+1),  type = "l", xlim = yrange,
           ylim = c(0, nunits + 1), ylab = "", xlab = "", lty = 1, main = "",
           cex = scale, axes = F, lwd = 1)
    }
  }

  # horizontal axis at bottom
  xticks <- pretty(yrange)
  axis(1, at = xticks, labels = xticks,  cex.axis = scale)
  # vertical labels/text
  par(mgp = c(10, 0.5, 0)) # sets relative location of axis labels
  par(adj = 1) # right-justified

  axis(2, at = c(nunits:1), labels = labs[ord, 1], cex.axis = scale,
       tick = F,las=1)
  #    par(adj = 0.5)
  #    mtext(side = 1, line = 2, title, cex = cex*1)

  # put in bands
  # ASSUMES TWO limits!!!
  # set up limits
  xx=matrix(0,nunits,6)
  xx[,1]=yrange[1]
  xx[,6]=yrange[2]
  xx[,2]=pmax(xx[,1],limits[,1]) # don't overlap plotting range
  xx[,3]=pmax(xx[,1],limits[,2])
  xx[,4]=pmin(xx[,6],limits[,4])
  xx[,5]=pmin(xx[,6],limits[,3])
  hh=0.3 # height of band
  yy=matrix(0,nunits,2)
  yy[,1] =  (nunits:1) - hh
  yy[,2] =  (nunits:1) + hh
  bandcols[4]=bandcols[2]
  bandcols[5]=bandcols[1]
  for(i in 1:nunits) {
    # first put in central bit
    for(j in 1:5){  # colour in 5 bands
      polygon( c(xx[ord[i],j], xx[ord[i],j+1],
                 xx[ord[i],j+1], xx[ord[i],j]),
               c(yy[i,1] , yy[i,1], yy[i,2] , yy[i,2]),
               col= bandcols[j],density=NA)
    }
  }

  # put in dots
  points(y[ord], nunits:1 ,  pch = pointsymbol)

}

######################################
#  Plotting funnel
####################################

# can I get rid of some of these arguements?
plot.funnel<-
  function(y, n ,  precisions,target, plot.target, title, xrange, yrange,
           tails, limits, xlab, ylab,  pointsymbol, legend, ypercent,  bandcols){

    # set up plotting region  - reset parameters!
    scale=1
    limittype=c(2,3)  # linestyle
    limitwidth=2  # width of lines
    nunits=length(y)
    nlimits=length(tails)
    par(new=F)
    par(mfrow = c(1, 1),mar=c(3,3,3,0),mgp= c(2, 0.75, 0),adj=0.5)

    yticks <- pretty(yrange)
    ylabels <- yticks
    xticks <- pretty(xrange)
    xlabels <- xticks

    if(ypercent==1){
      ylabels<-ylabels*100 # plot as percentage
    }


    symbols<-rep("*",nunits)
    if(length(pointsymbol)==1){symbols<-rep(pointsymbol,nunits)}
    if(length(pointsymbol) ==nunits){symbols<-pointsymbol}


    # set up plot
    # need to reset par parameters!!
    plot(0,0,type="n", ylim = yrange, xlim = xrange, ylab = ylab, xlab = xlab,
         main=title,axes = F)

    #  plot points
    points(n, y, pch = symbols)


    # plot target
    if(plot.target==T){
      par(new=T)
      plot(xrange, c(target, target), type = "l", ylim = yrange,
           xlim = xrange, ylab ="", xlab = "", lty = 1, main = "",
           cex = scale, axes = F, lwd = 2)
    }

    # pretty labels
    axis(1, at = xticks, labels = xlabels, cex = scale, lty = 1)
    axis(2, at =yticks , labels = ylabels, cex = scale, lty = 1,adj=1,las=1)

    #  plot prediction bands
    #browser()
    for(j in 1:nlimits) {
      par(new = T)
      plot(precisions, limits[,j  ], type = "l", ylim = yrange, xlim = xrange, ylab = "", xlab = "",
           lty = limittype[j], cex = scale, axes = F, lwd = limitwidth)
      par(new = T)
      plot(precisions, limits[,nlimits+j  ], type = "l", ylim = yrange, xlim = xrange, ylab = "", xlab = "",
           lty = limittype[j], cex = scale, axes = F, lwd = limitwidth)
    }

    # Need some legend to describe limits
    if(legend==1){
      prange <- paste((1 - 2 * tails) * 100, "% limits")
      xleg<- c(xrange[1] + (xrange[2] - xrange[1]) * 0.65, xrange[1] + (xrange[2]- xrange[1]) * 1.0)
      yleg<-  c(yrange[1] + (yrange[2] - yrange[1]) * 1.00, yrange[1] + (yrange[2] - yrange[1]) * 1.05)
      legend(xleg[1],yleg[1], bty = "n", prange, lty =limittype[1:nlimits],lwd=limitwidth,cex=scale)
    }

  }



#############################################
# function to calculate and print out p and q values
#############################################

#ps.and.qs(R/N,N,target,names, tails)  #  calculate p and q val
#obs.prop=R/N
#denom=N
#tails=c(0.001,0.025)

ps.and.qs=function(obs.prop,denom,target,names,tails){
  nunits=length(denom)
  numer=obs.prop*denom
  p.obs.low = pbinom(numer,denom,target)  # will this work for non-integer values of denom?
  p.obs.high = 1- pbinom(numer-1,denom ,target)

  #print datatable of four extreme bands
  #name, y, n, p-value, q-value
  # First just set up vectors
  q.obs.high=rep(0,nunits)
  q.obs.low=rep(0,nunits)

  low.ord=order(p.obs.low)
  high.ord=order(p.obs.high)
  #p.obs.low[low.ord[]] # lists in increasing order
  q.obs.low[low.ord[nunits]] = p.obs.low[low.ord[nunits]] # same highest value
  q.obs.high[high.ord[nunits]] = p.obs.high[high.ord[nunits]]
  for(i in (nunits-1):1){
    q.obs.low[low.ord[i]] = min(p.obs.low[low.ord[i]]*nunits/i,  q.obs.low[low.ord[i+1]])
    q.obs.high[high.ord[i]] = min(p.obs.high[high.ord[i]]*nunits/i,  q.obs.high[high.ord[i+1]])
  }
  cuts<-c(0,tails,1)
  band.low <-as.numeric(cut(p.obs.low,cuts))
  # write out details of units with evidence of 'low'
  p.low.out =cbind(band.low[low.ord],names[low.ord],round(obs.prop[low.ord],3),denom[low.ord],round(p.obs.low[low.ord],4),round(q.obs.low[low.ord],4))
  print(p.low.out[p.low.out[,1]==2])
  # select those with low p-values
  #write headers for low alarms
  # for(i in  nunits){
  # if(band.low[low.ord][i] ==2 ) {
  #  [write out ith row of p.low.out ]
  #  }
  #   }

  # write out details of units with evidence of 'high'
  #	p.high.out =cbind(names[high.ord],format(p.obs.high[high.ord],4),format(q.obs.high[high.ord],4))
}


####################################################################
# p-percentile of binomial proportion with denom n and prob target.
####################################################################
qbinom.interp<-function(p, denom, target,tail)
  #  p-percentile of binomial proportion with denom n and prob target.
  # NB target is a vector of same length as n
  # different depending on whether p is lower or upper tail
  # need to check can't go above 100 or below 0.
{
  if(tail=="lower"){
    rp = qbinom(p, denom, target) # this is lowest R such that df>p
    alpha = (pbinom(rp,denom,target) - p)/dbinom(rp,denom,target)
    x<-  pmax( (rp - alpha)/denom , 0.00001)
  }
  if(tail=="upper"){
    rp = qbinom(1-p, denom, target) # this is highst x such that P(>=x) >p
    alpha = (pbinom(rp,denom,target) - (1-p)) / dbinom(rp,denom,target)
    x<- pmin(  (rp + 1 - alpha)/denom , 0.99999)
  }
  x
}





###################
# winsorise a vector - limit extreme values
###################
winsorise<-function(Z,winsor){
  #  rank z
  n <- length(Z)
  Z.ord <- Z[order(Z)]
  winlow <- trunc((n * winsor)/100) + 1
  winhigh <- n - winlow+1
  # winsorise
  for(i in 1:winlow) {
    Z.ord[i] <- Z.ord[winlow]
  }
  for(i in winhigh:n) {
    Z.ord[i] <- Z.ord[winhigh]
  }
  Z.winsor<-Z.ord[rank(Z)]
  Z.winsor
}
