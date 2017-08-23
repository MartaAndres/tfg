proposed_cover_size_total <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  sum(sapply(cover, nrow)) / nrow(dataset)
}

proposed_cover_size_class_max <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  instances <- table(dataset[length(dataset)])
  s <- sapply(names(cover), function(n) nrow(cover[[n]]) / instances[[n]])
  max(s)
}

proposed_cover_size_class_min <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  instances <- table(dataset[length(dataset)])
  s <- sapply(names(cover), function(n) nrow(cover[[n]]) / instances[[n]])
  min(s)
}

proposed_cover_size_class_avg <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  instances <- table(dataset[length(dataset)])
  s <- sapply(names(cover), function(n) nrow(cover[[n]]) / instances[[n]])
  mean(s)
}

proposed_points_covered_total <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)

  sum(sapply(1:nrow(dataset), function(i) {
    x <- dataset[i, ]
    cov <- cover[[x[[length(x)]]]]
    sum(sapply(1:nrow(cov), function(j) {
      y <- cov[j, ]
      distance(x[-length(x)], y[-length(y)], scale_factor) < y[length(y)]
    }))
  }))/nrow(dataset)
}

proposed_points_covered_class_avg <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)

  s <- sapply(split(dataset, dataset[length(dataset)]), function(df) {
    cov <- cover[[df[1,length(df)]]]
    sum(sapply(1:nrow(df), function(i) {
      x <- df[i, ]
      sum(sapply(1:nrow(cov), function(j) {
        y <- cov[j, ]
        distance(x[-length(x)], y[-length(y)], scale_factor) < y[length(y)]
      }))
    }))/nrow(df)
  })
  mean(s)
}

proposed_points_covered_class_max <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)

  s <- sapply(split(dataset, dataset[length(dataset)]), function(df) {
    cov <- cover[[df[1,length(df)]]]
    sum(sapply(1:nrow(df), function(i) {
      x <- df[i, ]
      sum(sapply(1:nrow(cov), function(j) {
        y <- cov[j, ]
        distance(x[-length(x)], y[-length(y)], scale_factor) < y[length(y)]
      }))
    }))/nrow(df)
  })
  max(s)
}

proposed_points_covered_class_min <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)

  s <- sapply(split(dataset, dataset[length(dataset)]), function(df) {
    cov <- cover[[df[1,length(df)]]]
    sum(sapply(1:nrow(df), function(i) {
      x <- df[i, ]
      sum(sapply(1:nrow(cov), function(j) {
        y <- cov[j, ]
        distance(x[-length(x)], y[-length(y)], scale_factor) < y[length(y)]
      }))
    }))/nrow(df)
  })
  min(s)
}

proposed_ball_points_total <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)

  classes <- split(dataset, dataset[length(dataset)])
  sum(sapply(names(classes), function(n) {
    df <- classes[[n]]
    cov <- cover[[n]]
    sum(sapply(1:nrow(df), function(i) {
      x <- df[i, ]
      sum(sapply(1:nrow(cov), function(j) {
        y <- cov[j, ]
        distance(x[-length(x)], y[-length(y)], scale_factor) < y[length(y)]
      }))
    }))/nrow(df)
  })) / sum(sapply(cover, nrow))
}

proposed_ball_points_class_avg <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)

  classes <- split(dataset, dataset[length(dataset)])
  s <- sapply(names(classes), function(n) {
    df <- classes[[n]]
    cov <- cover[[n]]
    sum(sapply(1:nrow(df), function(i) {
      x <- df[i, ]
      sum(sapply(1:nrow(cov), function(j) {
        y <- cov[j, ]
        distance(x[-length(x)], y[-length(y)], scale_factor) < y[length(y)]
      }))
    }))/(nrow(df) * nrow(cov))
  })
  mean(s)
}

proposed_ball_points_class_max <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)

  classes <- split(dataset, dataset[length(dataset)])
  s <- sapply(names(classes), function(n) {
    df <- classes[[n]]
    cov <- cover[[n]]
    sum(sapply(1:nrow(df), function(i) {
      x <- df[i, ]
      sum(sapply(1:nrow(cov), function(j) {
        y <- cov[j, ]
        distance(x[-length(x)], y[-length(y)], scale_factor) < y[length(y)]
      }))
    }))/(nrow(df) * nrow(cov))
  })
  max(s)
}

proposed_ball_points_class_min <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)

  classes <- split(dataset, dataset[length(dataset)])
  classes <- split(dataset, dataset[length(dataset)])
  s <- sapply(names(classes), function(n) {
    df <- classes[[n]]
    cov <- cover[[n]]
    sum(sapply(1:nrow(df), function(i) {
      x <- df[i, ]
      sum(sapply(1:nrow(cov), function(j) {
        y <- cov[j, ]
        distance(x[-length(x)], y[-length(y)], scale_factor) < y[length(y)]
      }))
    }))/(nrow(df) * nrow(cov))
  })
  min(s)
}

proposed_ball_radius_total <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)
  diameters <- lapply(
    split(dataset[-length(dataset)],
          dataset[[length(dataset)]]),
    function(df) {
      max(sapply(combn(1:nrow(df), 2, simplify = FALSE),
             function(ix) distance(df[ix[1], ], df[ix[2], ], scale_factor)))
    })
  2 * sum(sapply(names(cover), function(n) {
    sum(cover[[n]][length(cover[[n]])])/diameters[[n]]
  })) / sum(sapply(cover, nrow))
}

proposed_ball_radius_class_avg <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)
  diameters <- lapply(
    split(dataset[-length(dataset)],
          dataset[[length(dataset)]]),
    function(df) {
      max(sapply(combn(1:nrow(df), 2, simplify = FALSE),
             function(ix) distance(df[ix[1], ], df[ix[2], ], scale_factor)))
    })
  s <- sapply(names(cover), function(n) {
    sum(cover[[n]][length(cover[[n]])])/(diameters[[n]]*nrow(cover[[n]]))
  })
  2*mean(s)
}

proposed_ball_radius_class_max <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)
  diameters <- lapply(
    split(dataset[-length(dataset)],
          dataset[[length(dataset)]]),
    function(df) {
      max(sapply(combn(1:nrow(df), 2, simplify = FALSE),
             function(ix) distance(df[ix[1], ], df[ix[2], ], scale_factor)))
    })
  s <- sapply(names(cover), function(n) {
    sum(cover[[n]][length(cover[[n]])])/(diameters[[n]]*nrow(cover[[n]]))
  })
  2*max(s)
}

proposed_ball_radius_class_min <- function(dataset, cover) {
  if (missing(cover)) {
    cover <- learn(dataset)
  }
  scale_factor <- get_scale(dataset)
  diameters <- lapply(
    split(dataset[-length(dataset)],
          dataset[[length(dataset)]]),
    function(df) {
      max(sapply(combn(1:nrow(df), 2, simplify = FALSE),
             function(ix) distance(df[ix[1], ], df[ix[2], ], scale_factor)))
    })
  s <- sapply(names(cover), function(n) {
    sum(cover[[n]][length(cover[[n]])])/(diameters[[n]]*nrow(cover[[n]]))
  })
  2*min(s)
}
