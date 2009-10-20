ldnames <- function(x)
  lapply(x,function(a)structure(dim(a),names=names(dimnames(a))))
alldims <- function(y){
  x <- ldnames(y)
  structure(unlist(x,use.names=FALSE),names=unlist(lapply(x,names)))
}
vnames <- function(x)unique(c(dnames(x),names(x)))
gd <- function # Get data
### Specify variables to query and return
(al,
### The array.
 vars,
### Vector of variable names to return.
 terms
### Query terms.
 ){
  a <- al[[vars]]
  if(is.null(a))return(1:(alldims(al)[vars]))
  for(sub.var in names(terms)){
    val <- terms[[sub.var]]
    i <- which(sub.var==names(dimnames(a)))
    newnames <- dimnames(a)[-i]
    elements <- (1:dim(a)[i])==val
    arglist <- c(list(a),rep(TRUE,length(dim(a))))
    arglist[[i+1]] <- elements
    a <- do.call("[",arglist)
    if(is.null(dim(a)))
      a <- array(a,dim=length(a),dimnames=newnames)
    else
      dimnames(a) <- newnames
  }
  a
}
myplot <- function(x,data,...){
  L1 <- as.list(x)
  yvar <- as.character(L1[[2]])
  L2 <- as.list(L1[[3]])
  xvar <- as.character(L2[[2]])
  cond <- as.character(L2[[3]])
  cond.vals <- gd(data,cond)
  gvar <- as.character(match.call()$groups)
  g.vals <- gd(data,gvar)
  newdata <- adply(cond.vals,1,function(v){
    getsub <- function(VAR)range(gd(data,VAR,structure(list(v),names=cond)))
    data.frame(getsub(xvar),getsub(yvar),g.vals)
  })
  newdata[,1] <- factor(newdata[,1])
  names(newdata) <- c(cond,xvar,yvar,gvar)
  panel.groups <- function(subscripts,...){
    lev <- newdata[subscripts,cond][1]
    gval <- newdata[subscripts,gvar][1]
    lookup <- function(VAR)
      gd(data,VAR,structure(list(lev,gval),names=c(cond,gvar)))
    m <- match.call()
    m$x <- lookup(xvar)
    m$y <- lookup(yvar)
    m[[1]] <- panel.xyplot
    eval(m)
  }
  xyplot(x,newdata,...,panel=panel.superpose,panel.groups=panel.groups)
}
