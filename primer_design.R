require(Biostrings)
# nts <- paste(sample(DNA_ALPHABET[1:4], size = 203, replace = T))
# nts[101] <- "T"
# nts[102] <- "A"
# nts[103] <- "A"
# # nts - create random single nucleotid sequence (100-100 nts upstream and downstream from the STOP-codon)
# nts <- paste(nts, collapse = "")
# nts <- DNAString(nts)
# nts
nts <- DNAString("GTGGGACGTACTAGATTCCGACGATATGATAATGGGCCCTTCTACGCGCAGCGCTTCCTGTCGTGGTAAGTAACGCGTCACCGCTTTATCTAATCGTTCCTAACAGCCAGGTGCGGAAAAGCGGCGACAAAGCAAACTTGCGCGCTGATTGCGAACAAAGCTGGTCAGGAGCTCGCTTTAACACTCTAGTTCATCCCCACTGA")
ntspam <- nts[((length(nts) - 3) / 2-16):((length(nts) - 3) / 2+3+17)] # find possible PAM-sequences +/- 17 bp around the STOP-codon depending on the Cpf1 chosen
fnpams <- NULL
lbpams <- NULL
aspams <- NULL
mbpams <- NULL
cpf <- c("AsCpf1", "FnCpf1", "LbCpf1", "MbCpf1")
if ("FnCpf1" %in% cpf) {
  fnpams <- list(matchPattern("TTV", ntspam, fixed = F),
                 matchPattern("TYN", ntspam, fixed = F))
}
if (length(fnpams) > 1) {
  fnpams <- c(fnpams[[1]], fnpams[[2]])
}
if ("LbCpf1"  %in% cpf) {
  lbpams <- matchPattern("HYBV", ntspam, fixed = F)
}
if ("AsCpf1"  %in% cpf) {
  aspams <- matchPattern("TTTV", ntspam, fixed = F)
}
if ("MbCpf1"  %in% cpf) {
  mbpams <- matchPattern("TYYV", ntspam, fixed = F)
}
allpam <- list(fnpams, lbpams, aspams, mbpams)
if (is.null(aspams) | is.null(fnpams) | is.null(lbpams) | is.null(mbpams))
  {allpam <- allpam[-which(sapply(allpam, is.null))]}
allpamm <- allpam[[1]]
if (length(allpam) > 1) {
    for (i in (1:(length(allpam) - 1))) {
    allpamm <- c(allpamm, allpam[[i + 1]])
  }
}
# remove sites occuring twice
if (length(allpamm) == 0) {print("No PAM-site found.")
  } else {
single <-
  which(duplicated(cbind(start(allpamm), end(allpamm))) == F)
allpam_single <- allpamm[single]
# rank sites according to proximity to STOP
sortedallpam <-
  allpam_single[order(abs((length(ntspam) - 3) / 2 - allpam_single@ranges@start)), ]
# compute gRNAs for the 5 best PAM-sites
if (length(sortedallpam) < 5) {
  pamnumber <- length(sortedallpam)
} else { 
  pamnumber <- 5
}
grnas <- rep("grna", times = pamnumber)
coneprimer <- as.character(xscat(complement(nts[11:100]), complement(DNAString("SGGGGSGGGGS"))))
dim(grnas) <- c(pamnumber, 1)
colnames(grnas) <- "gRNAs"
for (i in (1:pamnumber)) {
  targetstart <-
    sortedallpam@ranges@start[i] + length(sortedallpam[[i]])
  grnas[i] <-
    as.character(complement(nts[(targetstart+100-17):(targetstart + 19+100-17)]))
}
grnas
summary <-matrix(rep(0, len = pamnumber*5), nrow = pamnumber)
colnames(summary) <- c("Rank", "Cpf1", "PAM", "gRNA-coding sequence", "C2-primer")
summary = as.data.frame(summary)
for (i in (1:pamnumber)) {
    summary$PAM[i] <- as.character(sortedallpam[[i]])
    summary$`C2-primer`[i] <- paste("grnahandle",grnas[i], "TTTTTT", as.character(nts[104:163]), sep = "")
}
for (i in (1:pamnumber)) {
  if (length(DNAString("TTV")) == sortedallpam@ranges@width[i] &
      (countPattern(sortedallpam[[i]], DNAString("TTV"), fixed = F) > 0) & ("FnCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "FnCpf1"
  }
}
for (i in (1:pamnumber)) {
  if (length(DNAString("TYN")) == sortedallpam@ranges@width[i] &
      (countPattern(sortedallpam[[i]], DNAString("TYN"), fixed = F) > 0) & ("FnCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "FnCpf1"
  }
}
for (i in (1:pamnumber)) {
  if (length(DNAString("HYBV")) == sortedallpam@ranges@width[i] &
      (countPattern(sortedallpam[[i]], DNAString("HYBV"), fixed = F) > 0) & ("LbCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "LbCpf1"
  }
}
for (i in (1:pamnumber)) {
  if (length(DNAString("TTTV")) == sortedallpam@ranges@width[i] &
      (countPattern(sortedallpam[[i]], DNAString("TTTV"), fixed = F) > 0) & ("AsCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "AsCpf1"
  }
}
for (i in (1:pamnumber)) {
  if (length(DNAString("TYYV")) == sortedallpam@ranges@width[i] &
      (countPattern(sortedallpam[[i]], DNAString("TYYV"), fixed = F) > 0) & ("MbCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "MbCpf1"
  }
}
for (i in (1:pamnumber)) {
  if (length(DNAString("TYYV")) == sortedallpam@ranges@width[i] &
      (countPattern(sortedallpam[[i]], DNAString("TYYV"), fixed = F) > 0) & ("MbCpf1"  %in% cpf) & (countPattern(sortedallpam[[i]], DNAString("TTTV"), fixed = F) > 0) & ("AsCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "AsCpf1, MbCpf1"
  }
}
for (i in (1:pamnumber)) {
  if (length(DNAString("TYYV")) == sortedallpam@ranges@width[i] &
      (countPattern(sortedallpam[[i]], DNAString("HYBV"), fixed = F) > 0) & ("LbCpf1"  %in% cpf) & (countPattern(sortedallpam[[i]], DNAString("TTTV"), fixed = F) > 0) & ("AsCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "AsCpf1, LbCpf1"
  }
}
for (i in (1:pamnumber)) {
  if (length(DNAString("TYYV")) == sortedallpam@ranges@width[i] &
      (countPattern(sortedallpam[[i]], DNAString("HYBV"), fixed = F) > 0) & ("LbCpf1"  %in% cpf) & (countPattern(sortedallpam[[i]], DNAString("TYYV"), fixed = F) > 0) & ("MbCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "LbCpf1, MbCpf1"
  }
}
for (i in (1:pamnumber)) {
  if ((length(DNAString("TYYV")) == sortedallpam@ranges@width[i]) &
      (countPattern(sortedallpam[[i]], DNAString("TTTV"), fixed = F) > 0) &
      ("AsCpf1"  %in% cpf) & (countPattern(sortedallpam[[i]], DNAString("HYBV"), fixed = F) > 0)
      & ("LbCpf1"  %in% cpf) & (countPattern(sortedallpam[[i]], DNAString("TYYV"), fixed = F) > 0)
      & ("MbCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "AsCpf1, LbCpf1, MbCpf1"
  }
}
for (i in (1:pamnumber)) {
  summary$`gRNA-coding sequence`[i] <- grnas[i]
}
for (i in (1:pamnumber)) {
  summary$Rank[i] <- as.character(i)
summary
  }
  }
