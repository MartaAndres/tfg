saveResults <- function(results, filename = "results", directory = "results",
                        older=file.path(directory,"older")) {
  dir.create(older, showWarnings = FALSE, recursive = TRUE)
  dput(results, file.path(directory, paste0(filename, ".txt")))
  file.copy(file.path(directory, paste0(filename, ".txt")),
            file.path(older, paste0(filename, "_",
                                    gsub(" ", "_", format(Sys.time())), ".txt")))
  write.csv(results, file.path(directory, paste0(filename, ".csv")))
}

loadResults <- function(filename = "results.txt",
                        directory = "./results") {
  dget(file.path(directory, filename))
}
