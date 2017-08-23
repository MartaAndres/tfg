get_scale <- function(dataset) {
  ## Get scale to be used as reference for normalized distance
  ## Last column is the class, needed for nominal distance

  l <- lapply(dataset[-length(dataset)], function(column) {
    if (is.numeric(column)) {
      sd <- sd(column)
      if (sd == 0) {
        sd <- 1
      }
      sd
    }
    else {
      t <- table(cbind(column, dataset[length(dataset)]))
      t <- t/apply(t, 1, sum)
      ## print(t)
      sapply(rownames(t), function(x) {
        sapply(rownames(t), function(y) {
          sum(apply(t, 2, function(c) (c[x]-c[y])**2))
        })
      })
    }
  })
  l
}

hvdm <- function(x, y, scale_factor, exclude_last = FALSE) {
  ## Distance from x to y
  ## Use exclude_last if the last column is the class
  if (exclude_last) {
    x <- x[-length(x)]
    y <- y[-length(y)]
  }

  d <- sapply(names(x), function(a) {
    if (is.na(x[[a]]) || is.na(y[[a]])) {
      1
    }
    else if (is.numeric(x[[a]])){
      abs(x[[a]]-y[[a]])/scale_factor[[a]]
    }
    else {
      scale_factor[[a]][x[[a]],y[[a]]]
    }
  })
  sqrt(sum(d**2))
}
distance <- hvdm
