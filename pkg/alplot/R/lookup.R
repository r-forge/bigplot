dnames <- function(x)unlist(structure(lapply(e,ddnames),names=NULL))
vnames <- function(x)unique(c(dnames(x),names(x)))
ddnames <- function(x){
  y=dim(x[[1]])
  if(is.null(y))y=length(x[[1]])
  structure(y,names=x[[2]])
}
gd <- function # Get data
### Specify variables to query and return
(al,
### The array.
 vars,
### Vector of variable names to return.
 ...
### Query terms.
 ){
  L <- al[[vars]]
  if(is.null(L))return(1:(dnames(x)[vars]))
  a <- L[[1]]
  dim.var.names <- L[[2]]
  terms <- list(...)
  for(sub.var in names(terms)){
    val <- terms[[sub.var]]
    i <- which(sub.var==dim.var.names)
    elements <- (1:dim(a)[i])==val
    arglist <- c(list(a),rep(TRUE,length(dim(a))))
    arglist[[i+1]] <- elements
    a <- do.call("[",arglist)
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
  newdata <- adply(cond.vals,1,function(v){
    getsub <- function(VAR)
      range(do.call("gd",c(list(data,VAR),structure(list(v),names=cond))))
    data.frame(getsub(xvar),getsub(yvar))
  })
  newdata <- c(newdata,1)
  names(newdata) <- c(cond,xvar,yvar,as.character(match.call()$groups))
  xyplot(x,newdata,...)
}
