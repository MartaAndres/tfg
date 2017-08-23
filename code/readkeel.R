## Modified version of RKEEL's read.keel function
read.keel <- function (file)
{
  text <- readLines(file)
  i <- 1
  while (!grepl("@attribute", tolower(text[i]))) {
    i <- i + 1
  }
  attributeNames <- c()
  attributeTypes <- c()
  while (grepl("@attribute", tolower(text[i]))) {
    attributeNames <- c(attributeNames, gsub("'", "", strsplit(text[i],
                                                               "[ {]")[[1]][2]))
    if (grepl("\\{", text[i])) {
      attributeTypes <- c(attributeTypes, "categorical")
    }
    else {
      attributeTypes <- c(attributeTypes, strsplit(text[i],
                                                   " ")[[1]][3])
    }
    i <- i + 1
  }
  outputs <- -1
  while (!grepl("@data", tolower(text[i]))) {
    if (grepl("@outputs", tolower(text[i]))) {
      outputAttribute <- gdata::trim(strsplit(text[i],
                                              " ")[[1]][2])
      for (j in 1:length(attributeNames)) {
        if (grepl(outputAttribute, attributeNames[j])) {
          outputs <- j
        }
      }
      if (outputs == -1) {
        stop("Output attribute don't found")
      }
    }
    i <- i + 1
  }
  i <- i + 1
  data <- c()
  row <- 0
  while (!is.na(text[i])) {
    dataWords <- strsplit(text[i], ",")[[1]]
    if (length(dataWords) > 0) {
      dataLine <- c()
      for (j in 1:length(dataWords)) {
        if (dataWords[j] == "?") {
          dataLine <- c(dataLine, "NA")
        }
        else if (grepl("integer", attributeTypes[j])) {
          dataLine <- c(dataLine, strtoi(gdata::trim(dataWords[j])))
        }
        else if (grepl("real", attributeTypes[j])) {
          dataLine <- c(dataLine, as.double(gdata::trim(dataWords[j])))
        }
        else if (grepl("categorical", attributeTypes[j])) {
          dataLine <- c(dataLine, gdata::trim(dataWords[j]))
        }
        else {
          stop("Type not found")
        }
      }
      data <- c(data, dataLine)
      row <- row + 1
    }
    i <- i + 1
  }
  m <- matrix(data, nrow = row, ncol = length(attributeNames),
              byrow = TRUE)
  colnames(m) <- attributeNames
  if ((outputs != -1) && (outputs < length(attributeNames))) {
    m2 <- m
    attributeTypes2 <- attributeTypes
    attributeNames2 <- attributeNames
    j <- 1
    for (i in 1:length(attributeNames)) {
      if (i != outputs) {
        m[, j] <- m2[, i]
        attributeTypes[j] <- attributeTypes2[i]
        attributeNames[j] <- attributeNames2[i]
        j <- j + 1
      }
    }
    m[, length(attributeNames)] <- m2[, outputs]
    attributeTypes[length(attributeNames)] <- attributeTypes2[outputs]
    attributeNames[length(attributeNames)] <- attributeNames2[outputs]
  }
  df <- data.frame(m)

  ## Added to RKEEL's version: convert to numeric or factor
  for (j in 1:length(attributeTypes)) {
    if (grepl("categorical", attributeTypes[j])) {
      df[, j] <- as.factor(df[, j])
    }
    else {
      df[, j] <- as.numeric(levels(df[, j]))[df[, j]]
    }
  }
  df[ncol(df)] <- as.factor(df[ncol(df)][[1]])
  return(df)
}
