disableActionButton <- function(id, session) {
  session$sendCustomMessage(type = "jsCode", list(code =
                                                    paste0(
                                                      "$('#", id, "').prop('disabled', true)"
                                                    )))
}
enableActionButton <- function(id, session) {
  session$sendCustomMessage(type = "jsCode", list(code =
                                                    paste0(
                                                      "$('#", id, "').prop('disabled', false)"
                                                    )))
  
}
#### Find possible PAM sequences +/- 17 nts around the stop codon depending on the Cpf1 chosen
aroundpam <- reactive({
  function(targetinput) {
    (DNAString(targetinput))[(200 - 17 + 1):(200 + 3 + 17)]
  }
})
aroundstop <- reactive({
  function(targetinput) {
    (DNAString(targetinput))[(200 - 17 - 10 + 1):(200 + 3 + 17 + 10)]
  }
})
#### Function searchingfor PAM sites
lookforpam <-
  function(plasmid,
           pam,
           plasmidchoice,
           target,
           extendedspace)
  {
    if (plasmid  %in% plasmidchoice) {
      dc <-
        matchPattern(pam, DNAString(target)[184:203], fixed = F)
      rc <-
        matchPattern(pam, reverseComplement(DNAString(target)[201:220]), fixed = F)
      de <-
        matchPattern(pam, DNAString(target)[201:(203 + extendedspace)], fixed = F)
      if (extendedspace > 17) {
        re <-
          matchPattern(pam, reverseComplement(DNAString(target)[(200 + 17 + 1):(203 + extendedspace)]), fixed = F)
      } else {
        re <- NULL
      }
      return(list(
        dc = dc,
        rc = rc,
        de = de,
        re = re
      ))
      
    } else {
      return(list(
        dc = NULL,
        rc = NULL,
        de = NULL,
        re = NULL
      ))
    }
  }
##### Function merging the results to lists according to strands (direct/reverse/direct extended/reverse extended)
mergepamlists <- function(i)
  {
  allpamspec <- list(tttv[[i]], tycv[[i]], astttv[[i]], astycv[[i]], mccc[[i]], ratr[[i]], tatv[[i]], otherpams[[i]])
  if (length(tttv[[i]]) == 0 |
      length(tycv[[i]]) == 0 | length(astttv[[i]]) == 0 |
      length(astycv[[i]]) == 0 | length(mccc[[i]]) == 0 | length(ratr[[i]]) == 0 |
      length(tatv[[i]]) == 0 | length(otherpams[[i]]) == 0)
  {
    allpamspec <- allpamspec[which(lengths(allpamspec) > 0)]
  }
  if (length(allpamspec) == 0) {
    allpammspec <- list()
  } else {
    allpammspec <- allpamspec[[1]]
    if (length(allpamspec) > 1) {
      for (j in (1:(length(allpamspec) - 1))) {
        allpammspec <- c(allpammspec, allpamspec[[j + 1]])
      }
    }
  }
  return(allpammspec = allpammspec)
}
##### Save input
saveData <- function(data, outputDir) {
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.table(
    x = rbind(data),
    file = file.path(outputDir, fileName),
    row.names = FALSE, col.names = FALSE, quote = TRUE
  )
}
##### Load saved input
loadData <- function(outputDir) {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.table, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}
##### Set time format
humanTime <- function() format(Sys.time(), "%d.%m.%Y - %H:%M:%OS")
#####
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}