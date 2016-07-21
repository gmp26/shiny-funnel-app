# test if I have to pass everything over?  YES I DO

test=function(x,y){sum(x)/y}
y=2
test(x=c(1,2,3),y)


####################################################
# March 2016.  Test new funnel program
####################################################
# Lung data
x<-read.csv(file="~//Dropbox//DB-funnels-HSMR//funnels//lung.csv",header=T,sep=",")

N<- x$Cases
R<- N-x$deaths
xlabel<-"Number of operations per hospital"
scale<-0.8
xrange<-c(0,max(N))
xrange=c(0,600)
# ylabel<-"Mortality rate (%)"
#yrange<-c(0,max( R/N ))
ylabel<-"Survival rate (%)"
yrange<-c(0.94, 1)
#yrange=c(0,1)
names= as.character(x$X)
title<-"LCCOP data"
tails=c(0.001, 0.025)

source(file="~//Dropbox//DB-Rfunctions//funnel4.R")

funnel4(obs.prop=R/N,  denom=N, pred.prop=P/N, names=names,
        plot="slice", rank="precision", riskadj=F, RASRplot=F,
        plot.target=F, title=title,xrange=xrange,
        yrange=yrange, tails=tails,xlab=xlabel,ylab=ylabel,ypercent=T,
        bandcols=c("white","lightcyan","cyan")
) #





####################################################
# Feb 2016.  Test new funnel program
####################################################

# New York data using new function

x<-read.csv(file="~//Dropbox//DB-funnels-HSMR//funnels//CABG-hospitals-03.csv",header=T,sep=",")
N<- x$Cases
R<- N-x$Deaths
P = N -x$EMR*N/100
xlabel<-"Number of operations per hospital"
xlabel<-"Number of operations per hospital (adjusted)"
xrange<-c(0,max(N))
# ylabel<-"Mortality rate (%)"
#yrange<-c(0,max( R/N ))
yrange<-c(min(R/N )-0.01, 1)
#yrange=c(0,1)
names= as.character(x$Hospital)
tails=c(0.001,0.025)

source(file="~//Dropbox//DB-Rfunctions//funnel4.R")

# test using slices

# 1.  not risk-adjusted
source(file="~//Dropbox//DB-Rfunctions//funnel4.R")
title<-"NY Cardiac Surgery - not risk-adjusted"
ylabel<-"Survival rate (%)"
funnel4(obs.prop=R/N,  denom=N, pred.prop=P/N, names=names,
        plot="funnel", rank="precision", riskadj=F, RASRplot=F,
        plot.target=F, title=title,xrange=xrange,
        yrange=yrange, tails=tails,xlab=xlabel,ylab=ylabel,ypercent=T,
        bandcols=c("white","cyan","cyan3")
) #


# 2.  risk-adjusted, using shifted slices
source(file="~//Dropbox//DB-Rfunctions//funnel4.R")
title<-"NY Cardiac Surgery -  risk-adjusted"
ylabel<-"Survival rate (%)"
funnel4(obs.prop=R/N,  denom=N, pred.prop=P/N, names=names,
        plot="slice", rank="precision", riskadj=T, RASRplot=F,
        mean.target=F,plot.target=F, title=title,xrange=xrange,
        yrange=yrange, tails=tails,xlab=xlabel,ylab=ylabel,ypercent=T,
        bandcols=c("white","cyan","cyan3")
) #


# 3.  risk-adjusted, using RASR
source(file="~//Dropbox//DB-Rfunctions//funnel4.R") 

title<-"NY Cardiac Surgery -  risk-adjusted"
ylabel<-"Survival rate (%)"
funnel4(obs.prop=R/N,  denom=N, pred.prop=P/N, names=names,
        plot="funnel", rank="precision", riskadj=T, RASRplot=T,
        mean.target=F,plot.target=F, title=title,xrange=xrange,
        yrange=yrange, tails=tails,xlab=xlabel,ylab=ylabel,ypercent=T,
        bandcols=c("white","cyan","cyan3")
) # 









############################################################
# New York data using older function

x<-read.csv(file="~//Dropbox//DB-funnels-HSMR//funnels//CABG-hospitals-03.csv",header=T,sep=",")

N<- x$Cases
R<- N-x$Deaths
P = N -x$EMR*N/100

xlabel<-"Number of operations per hospital"
scale<-0.8
xrange<-c(0,max(N))

# ylabel<-"Mortality rate (%)"
#yrange<-c(0,max( R/N ))

ylabel<-"Survival rate (%)"

yrange<-c(min(R/N )-0.01, 1)
#yrange=c(0,1)

names= as.character(x$Hospital)

title<-"NY Cardiac Surgery - risk-adjusted"

source(file="~//Dropbox//DB-Rfunctions//funnel-3.R")
funnel.3(datatype="prop", obs.prop=R/N,denom=N, pred.prop=P/N, names=names,
               frame="pos",  slice=T, rank="outcome", riskadj=T, RASRplot=F,
               normapprox=F, logtrans=F,
               plot.target=F,
               random.effects=0,tau=NA,
               title=title,scale=scale,xrange=xrange,
               yrange=yrange, xlab=xlabel,ylab=ylabel,symbolstretch=scale,
         slicecols=c("pink","orange","white") ) #

par(mar=c(5,5,5,5))
par(mgp=c(3,1,1)) 
plot(c(1,0),c(0,1),axes=F,xlab="XXX")
# can I have non-integer binomial?  NO!  Ah, not so good
pbinom(6, 10.5,0.5)

 ####################################################################

# Robin's data
 source(file="~//Dropbox//DB-Rfunctions//funnel-3.R")
 

 # reading in data
 x<-read.csv(file="~//Dropbox//DB-funnels-HSMR//funnels//Robin-EX1.txt",header=T,sep=",")
 summary(x)
 attach(x)
 
 R=Dead
   N=Total
 min(N)
 
 ylabel<-"Mortality rate (%)"
 xlabel<-"Number of operations per surgeon"
 yrange<-c(0,max(R/N ))
 title<-""
 xrange<-c(0,max(N))
 scale<-1
 
# png("~//Dropbox//DB-articles//funnels//NYhospitals.png",width=4000,height=3000, res=600)
 funnel.3(datatype="prop",
                a1=R,b1=N,ratedenom=NA,normapprox=0,logtrans=0,meantarget=1,target=NA,
                overdisp = 0, winsor = 10, phisig = 1,
                random.effects=0,tau=NA,title=title,scale=scale,xrange=xrange,
                yrange=yrange,pvalue=c(0.001,0.025),
                Npoints=200,xlab=xlabel,ylab=ylabel,plottext=0,pointsymbol=16,symbolstretch=scale,
                limittype=c(5,6)  ,legend=1,ylogscale=0,ypercent=1) #

 #dev.off()
 
 # issues with this - most 0, a few extreme, need summaries in list
 # eg p-values, q-values etc.
 
  