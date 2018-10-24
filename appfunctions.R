'%ni%' <- Negate('%in%')
disableActionButton <- function(id, session) {
  session$sendCustomMessage(type = "jsCode", list(code =
                                                    paste0("$('#", id, "').prop('disabled', true)")))
}
enableActionButton <- function(id, session) {
  session$sendCustomMessage(type = "jsCode", list(code =
                                                    paste0(
                                                      "$('#", id, "').prop('disabled', false)"
                                                    )))
}
#### Find possible PAM sequences +/- 17 nts around the stop codon depending on the Cas12a chosen -----
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
#### Function searchingfor PAM sites -----
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
##### Function merging the results to lists according to strands (direct/reverse/direct extended/reverse extended) -----
mergepamlists <- function(i)
{
  allpamspec <-
    list(tttv[[i]], tycv[[i]], astttv[[i]], astycv[[i]], mccc[[i]], ratr[[i]], tatv[[i]], otherpams[[i]])
  if (length(tttv[[i]]) == 0 |
      length(tycv[[i]]) == 0 | length(astttv[[i]]) == 0 |
      length(astycv[[i]]) == 0 |
      length(mccc[[i]]) == 0 | length(ratr[[i]]) == 0 |
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