vnames <- function(x)unique(c(unlist(sapply(x,function(l)l[[2]])),names(x)))
gd <- function # Get data
### Specify variables to query and return
(al,
### The array.
 vars,
### Vector of variable names to return.
 ...
### Query terms.
 ){
  terms <- list(...)
  L <- al[[vars]]
  a <- L[[1]]
  dim.var.names <- L[[2]]
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
