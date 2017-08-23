## require(igraph)

## cover <- function(classes, scale_factor) {
##   ## auxiliar de learn, halla el recubrimiento de la primera clase
##   ## there are only two classes, first is target, second is not target
##   df <- classes[[1]]

##   ## compute radius
##   df[length(df)+1] <- apply(df, 1, function(x) {
##     min(apply(classes[[2]], 1, function(y) distance(x, y, scale_factor)))
##   })

##   ## create the digraph to cover
##   adj <- sapply(rownames(df), function(r)
##     rownames(df[apply(df, 1, function(y)
##       distance(df[r,-length(df)], y, scale_factor) < df[r,length(df)]),]),
##     simplify = FALSE)
##   D <- graph.data.frame(stack(adj)[2:1])
##   V(D)$names <- rownames(df)

##   ## greedy dominating set
##   S <- c()
##   while (length(V(D)) > 0){
##     v <- names(which.max(sapply(V(D), function(x) length(neighbors(D,x,mode="out")))))
##     S <- c(S,v)
##     D <- delete.vertices(D,neighbors(D,v,mode="out"))
##   }
##   df[S,]
## }


## learn <- function(dataset) {
##   ## llamar a esto para obtener clasificacion a partir del training data

##   scale_factor <- get_scale(dataset)
##   sapply(levels(dataset[,length(dataset)]), function(l) {
##     classes <- split(dataset[-length(dataset)],
##                     f = dataset[length(dataset)]!=l)
##     cover(classes, scale_factor)
##   }, simplify = FALSE)
## }

cover <- function(classes, scale_factor) {
  ## auxiliar de learn, halla el recubrimiento de la primera clase
  ## there are only two classes, first is target, second is not target
  df <- classes[[1]]

  ## compute radius
  df[length(df)+1] <- apply(df, 1, function(x) {
    min(apply(classes[[2]], 1, function(y) distance(x, y, scale_factor)))
  })

  ## create the digraph to cover
  adj <- sapply(rownames(df), function(r)
    rownames(df[apply(df, 1, function(y)
      distance(df[r,-length(df)], y, scale_factor) < df[r,length(df)]),]),
    simplify = FALSE)

  ## greedy dominating set / set cover
  C <- c()
  U <- names(adj)[sapply(adj, length) > 0]
  while (length(U) > 0){
    v <- names(which.max(sapply(adj, length)))
    C <- c(C,v)
    U <- setdiff(U, adj[[v]])
    adj <- lapply(adj, function(x) intersect(x, U))
  }
  df[C,]
}


learn <- function(dataset) {
  ## llamar a esto para obtener clasificacion a partir del training data

  scale_factor <- get_scale(dataset)
  sapply(levels(dataset[,length(dataset)]), function(l) {
    classes <- split(dataset[-length(dataset)],
                     f = dataset[length(dataset)]!=l)
    cover(classes, scale_factor)
  }, simplify = FALSE)
}
