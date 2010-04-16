data(BodyWeight,package="nlme")
object.size(BodyWeight)
## thinking of this data set as a list of arrays, we just need to
## store a matrix of weights, time in columns, rats in rows. Then we
## store Diet in a vector.
m <- with(BodyWeight,matrix(weight,byrow=TRUE,
                            ncol=nlevels(as.factor(Time)),
                            nrow=nlevels(Rat)))
dimnames(m) <- list(Rat=NULL,t=NULL)
al <- list(weight=m,
           Time=array(unique(BodyWeight$Time),dimnames=list(t=NULL)),
           Diet=array(subset(BodyWeight,Time==1)$Diet,dimnames=list(Rat=NULL)))
excess <- as.numeric(object.size(BodyWeight)/object.size(al))
print(excess)
ldnames(al)
alldims(al)
gd(al,"weight",list(Rat=5))
gd(al,"weight",list(Diet="2"))
gd(al,"Time",list(Rat=5))
gd(al,"Time",list(Diet="2"))
gd(al,"Rat",list(Diet="1"))
xyplot(weight~Time|Diet,BodyWeight,groups=Rat,layout=c(3,1),type="l")
pdf("long.pdf")
myplot(weight~Time|Diet,al,groups=Rat,layout=c(3,1),type="l")
dev.off()
system("convert long.pdf long.png && display long.png")
