narray <- function # Named array
### Shortcut function for creating an array with dimnames.
(x, ##<< Data to fill array.
 ...  ##<< dimnames.
 ){
  n <- as.character(match.call()[-(1:2)])
  dn <- sapply(n,function(x)NULL)
  thedim <-
    if(length(n)==1)length(x)
    else sapply(list(...),function(x)nlevels(as.factor(x)))
  a <- array(x,thedim,dn)
  class(a) <- c("narray",class(a))
  a
}
arraylist <- function
### Make a new array list, which is just a special type of list where
### each element has a name that corresponds to the variable's
### name. Each element should be an array with dimnames that match the
### variable names that run along the edges of the array.
(...){
  L <- list(...)
  class(L) <- c("arraylist",class(L))
  L
}
### Print method for narray objects.
print.narray <- function(x,...){
  if(length(dimnames(x))==1)cat(names(dimnames(x)),"\n")
  class(x) <- "array"
  print(x)
}
### Get dimnames of arraylist elements.
ldnames <- function(x)
  lapply(x,function(a)structure(dim(a),names=names(dimnames(a))))
### Get dimnames and sizes for arraylists.
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
plot.arraylist <- function
### Plot an arraylist using efficient lattice panel functions.
(x,
### The arraylist.
 f,
### lattice plot formula.
 lattice.fun=NULL,
### Lattice plot function to call. default will be xyplot.
 ...
### Other arguments to the lattice plot function.
 ){
  lattice.fun.name <-
    if(is.null(lattice.fun))"xyplot"
    else as.character(match.call()$lattice.fun)
  panel.fun <- get(paste("panel.",lattice.fun.name,sep=""))
  lattice.fun <- get(lattice.fun.name)
  parsed <-
    gsub("([^~ ]*) *~ *([^| ]+) *[|] *(\\w+) *","\\1\n\\2\n\\3",deparse(f))
  vars <- strsplit(parsed,split="\n")[[1]]
  yvar <- vars[1]
  xvar <- vars[2]
  cond <- vars[3]
  cond.vals <- structure(unique(gd(x,cond)),dimnames=NULL)
  gvar <- as.character(match.call()$groups)
  newdata <- lapply(seq_along(cond.vals),function(i){
    v <- cond.vals[i]
    this <- structure(list(v),names=cond)
    getsub <- function(VAR)range(gd(x,VAR,this))
    d <- data.frame(v,getsub(xvar),gd(x,gvar,this))
    if(yvar!="")d <- cbind(d,getsub(yvar))
    d
  })
  newdata <- do.call(rbind,newdata)
  newdata[,1] <- factor(newdata[,1])
  newnames <- c(cond,xvar,gvar)
  if(yvar!="")newnames <- c(newnames,yvar)
  names(newdata) <- newnames
  panel.groups <- function(subscripts,...){
    lev <- newdata[subscripts,cond][1]
    gval <- newdata[subscripts,gvar][1]
    this <- structure(list(lev,gval),names=c(cond,gvar))
    lookup <- function(VAR)gd(x,VAR,this)
    m <- match.call()
    m$x <- lookup(xvar)
    if(yvar!="")m$y <- lookup(yvar)
    m[[1]] <- panel.fun
    eval(m)
  }
  lattice.fun(f,newdata,...,panel=panel.superpose,panel.groups=panel.groups)
}
