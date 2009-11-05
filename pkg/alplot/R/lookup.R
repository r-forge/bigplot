ldnames <- function(x)
  lapply(x,function(a)structure(dim(a),names=names(dimnames(a))))
alldims <- function(y){
  x <- ldnames(y)
  structure(unlist(x,use.names=FALSE),names=unlist(lapply(x,names)))
}
gd <- function # Get data
### Specify variables to query and return
(al,
### The array.
 vars,
### Vector of variable names to return.
 terms=list()
### Query terms.
 ){
  a <- al[[vars]]
  for(i in seq_along(dimnames(a)))dimnames(a)[[i]] <- 1:(dim(a)[i])
  if(is.null(a))a <-
    array(1:(alldims(al)[vars]),dimnames=structure(list(NULL),names=vars))
  for(sub.var in names(terms)){
    val <- terms[[sub.var]]
    i <- which(sub.var==names(dimnames(a)))
    if(!length(i)){ ## var not on an array index
      i <- which(names(ldnames(al)[[sub.var]])==names(dimnames(a)))
      if(!length(i)){## return all
        return(a)
      }else{
        elements <- al[[sub.var]]==val
      }
    }else{
      elements <- (dimnames(a)[[i]])==val
    }
    old.dim.len <- length(dim(a))
    newnames <- dimnames(a)[-i]
    arglist <- c(list(a),rep(TRUE,length(dim(a))))
    arglist[[i+1]] <- elements
    a <- do.call("[",arglist)
    if(is.null(dim(a)))
      a <- array(a,dim=length(a),dimnames=newnames)
    else{
      if(length(dim(a))<old.dim.len)dimnames(a) <- newnames
    }
  }
  a
}
myplot <- function(x,data,...){
  L1 <- as.list(x)
  yvar <- as.character(L1[[2]])
  L2 <- as.list(L1[[3]])
  xvar <- as.character(L2[[2]])
  cond <- as.character(L2[[3]])
  cond.vals <- structure(unique(gd(data,cond)),dimnames=NULL)
  gvar <- as.character(match.call()$groups)
  g.vals <- gd(data,gvar)
  newdata <- adply(cond.vals,1,function(v){
    this <- structure(list(v),names=cond)
    getsub <- function(VAR)range(gd(data,VAR,this))
    data.frame(getsub(xvar),getsub(yvar),gd(data,gvar,this))
  })
  newdata[,1] <- factor(newdata[,1])
  names(newdata) <- c(cond,xvar,yvar,gvar)
  panel.groups <- function(subscripts,...){
    lev <- newdata[subscripts,cond][1]
    gval <- newdata[subscripts,gvar][1]
    this <- structure(list(lev,gval),names=c(cond,gvar))
    lookup <- function(VAR)gd(data,VAR,this)
    m <- match.call()
    m$x <- lookup(xvar)
    m$y <- lookup(yvar)
    m[[1]] <- panel.xyplot
    eval(m)
  }
  xyplot(x,newdata,...,panel=panel.superpose,panel.groups=panel.groups)
}
