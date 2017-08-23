## F1 uses average over attributes or max?
## Comprobar que esté bien
measure_f1_multiclass_avg <- function(dataset) {
  ## Last column is class

  ## Converts al nominal columns except class to discrete values
  dataset[1:(length(dataset)-1)] <- lapply(dataset[-length(dataset)], function(x)
      if (!is.numeric(x)) as.numeric(as.factor(x)) else x)

  classes <- split(dataset[-length(dataset)], dataset[length(dataset)])
  overlap <- sapply(colnames(dataset)[-length(dataset)], function(attr) {
    m <- mean(dataset[[attr]])
    numerator <- sum(sapply(classes, function(c)
      nrow(c)*abs(m-mean(c[[attr]]))))
    denominator <- sum(sapply(classes, function(c) {
      m <- mean(c[[attr]])
      sum(sapply(c[[attr]], function(x) abs(x-m)))
      }))
    numerator/denominator
  })
  mean(overlap)
}

measure_f1_euclidean <- function(dataset) {
    ## Last column is class

    ## Converts al nominal columns except class to discrete values
    dataset[1:(length(dataset)-1)] <- lapply(dataset[-length(dataset)], function(x)
        if (!is.numeric(x)) as.numeric(as.factor(x)) else x)

    classes <- split(dataset[-length(dataset)], dataset[length(dataset)])
    m <- sapply(dataset[-length(dataset)], mean)
    numerator <- sum(sapply(classes, function(c)
        nrow(c)*sqrt(sum((sapply(c, mean) - m)**2))))
    denominator <- sum(sapply(classes, function(c) {
        m_i <- sapply(c, mean)
        sum(apply(c, 1, function(x) sqrt(sum(x-m_i)**2)))
    }))
    numerator/denominator
}

measure_f1_manhattan <- function(dataset) {
    ## Last column is class

    ## Converts al nominal columns except class to discrete values
    dataset[1:(length(dataset)-1)] <- lapply(dataset[-length(dataset)], function(x)
        if (!is.numeric(x)) as.numeric(as.factor(x)) else x)

    classes <- split(dataset[-length(dataset)], dataset[length(dataset)])
    m <- sapply(dataset[-length(dataset)], mean)
    numerator <- sum(sapply(classes, function(c)
        nrow(c)*sum(abs(sapply(c, mean) - m))))
    denominator <- sum(sapply(classes, function(c) {
        m_i <- sapply(c, mean)
        sum(apply(c, 1, function(x) sum(abs(x-m_i))))
    }))
    numerator/denominator
}


measure_fisher <- function(dataset) {
  ## Only two classes
  classes <- split(dataset[-length(dataset)], dataset[length(dataset)])
  max(sapply(colnames(dataset)[-length(dataset)], function(attr) {
    (mean(classes[[1]][[attr]]) - mean(classes[[2]][[attr]]))**2 /
      (var(classes[[1]][[attr]]) + var(classes[[2]][[attr]]))
  }))
}
