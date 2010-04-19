library(alplot)
data(BodyWeight,package="nlme")
attach(BodyWeight)
al <- 
  arraylist(weight=t(narray(weight,Time,Rat)),
            time=narray(unique(Time),Time),
            Diet=narray(factor(Diet[Time==1]),Rat))
detach(BodyWeight)
print(al)
object.size(BodyWeight)/object.size(al)
alplot <- plot(al,weight~time|Diet,groups=Rat,type="l",layout=c(3,1))
print(alplot)
require(latticeExtra)
usual <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type="l",layout=c(3,1))
c(arraylist=alplot,data.frame=usual)
##plot(al,~weight|time,densityplot,groups=Diet)

