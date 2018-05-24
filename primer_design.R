require(Biostrings)
# nts <- paste(sample(DNA_ALPHABET[1:4], size = 203, replace = T))
# nts[101] <- "T"
# nts[102] <- "A"
# nts[103] <- "A"
# # nts - create random single nucleotid sequence (100-100 nts upstream and downstream from the STOP-codon)
# nts <- paste(nts, collapse = "")
# nts <- DNAString(nts)
# nts
nts <- DNAString("AAACCACAGGAACTGGTACATTCAGGCCACCTGCGCCACCAGCGGCGACGGGCTCTATGAAGGACTGGACTGGCTGTCCAATCAGCTCCGGAACCAGAAGTGAACGCGACCCCCCTCCCTCTCACTCCTCTTGCCCTCTGCTTTACTCTCATGTGGCAAACGTGCGGCTCGTGGTGTGAGTGCCAGAAGCTGCCTCCGTGGTT")
ntspam <- nts[((length(nts) - 3) / 2 - 16):((length(nts) - 3) / 2 + 3 + 17)] # find possible PAM-sequences +/- 17 bp around the STOP-codon depending on the Cpf1 chosen
cpf <- c("AsCpf1", "LbCpf1")
# direct strand - d
fnpamsd <- NULL
lbpamsd <- NULL
aspamsd <- NULL
mbpamsd <- NULL
# reverse strand - r
fnpamsr <- NULL
lbpamsr <- NULL
aspamsr <- NULL
mbpamsr <- NULL
if ("FnCpf1" %in% cpf) {
  fnpamsd <- list(matchPattern("TTV", ntspam[1:20], fixed = F),
                 matchPattern("TYN", ntspam[1:20], fixed = F))
  
  fnpamsr <- list(matchPattern("TTV", reverseComplement(ntspam[18:37]), fixed = F),
                  matchPattern("TYN", reverseComplement(ntspam[18:37]), fixed = F))
  }
if (length(fnpamsd) > 1) {
  fnpamsd <- c(fnpamsd[[1]], fnpamsd[[2]])
}
if (length(fnpamsr) > 1) {
  fnpamsr <- c(fnpamsr[[1]], fnpamsr[[2]])
}
if ("LbCpf1"  %in% cpf) {
  lbpamsd <- matchPattern("HYBV", ntspam[1:20], fixed = F)
  lbpamsr <- matchPattern("HYBV", reverseComplement(ntspam[18:37]), fixed = F)
}
if ("AsCpf1"  %in% cpf) {
  aspamsd <- matchPattern("TTTV", ntspam[1:20], fixed = F)
  aspamsr <- matchPattern("TTTV", reverseComplement(ntspam[18:37]), fixed = F)
  }
if ("MbCpf1"  %in% cpf) {
  mbpamsd <- matchPattern("TYYV", ntspam[1:20], fixed = F)
  mbpamsr <- matchPattern("TYYV", reverseComplement(ntspam[18:37]), fixed = F)
  }
## ----
allpamd <- list(fnpamsd, lbpamsd, aspamsd, mbpamsd)
if (is.null(aspamsd) | is.null(fnpamsd) | is.null(lbpamsd) | is.null(mbpamsd))
  {allpamd <- allpamd[-which(sapply(allpamd, is.null))]}
allpammd <- allpamd[[1]]
if (length(allpamd) > 1) {
    for (i in (1:(length(allpamd) - 1))) {
    allpammd <- c(allpammd, allpamd[[i + 1]])
  }
}
## -------
allpamr <- list(fnpamsr, lbpamsr, aspamsr, mbpamsr)
if (is.null(aspamsr) | is.null(fnpamsr) | is.null(lbpamsr) | is.null(mbpamsr))
{allpamr <- allpamr[-which(sapply(allpamr, is.null))]}
allpammr <- allpamr[[1]]
if (length(allpamr) > 1) {
  for (i in (1:(length(allpamr) - 1))) {
    allpammr <- c(allpammr, allpamr[[i + 1]])
  }
}
# merge the two datasets in one data frame
if (length(allpammd) + length(allpammr)  == 0) {print("No PAM-site found.")
} else {
allpamm <-matrix(rep(0, len = (length(allpammd) + length(allpammr))*4), length(allpammd) + length(allpammr))
colnames(allpamm) <- c("start", "width", "PAM", "strand")
allpamm = as.data.frame(allpamm)
if (length(allpammd) > 0) {
for (i in (1:(length(allpammd)))) {
  allpamm$start[i] <- allpammd@ranges@start[i]
  allpamm$width[i] <- allpammd@ranges@width[i]
  allpamm$PAM[i] <- as.character(allpammd[[i]])
  allpamm$strand[i] <- "direct"
}
}
if (length(allpammr)  > 0) {
for (i in (1:length(allpammr))) {
  allpamm$start[i + length(allpammd)] <- allpammr@ranges@start[i]
  allpamm$width[i + length(allpammd)] <- allpammr@ranges@width[i]
  allpamm$PAM[i + length(allpammd)] <- as.character(allpammr[[i]])
  allpamm$strand[i + length(allpammd)] <- "reverse"
}
}
# remove sites occuring twice
single <- which(duplicated(allpamm) == F)
allpam_single <- allpamm[single, ]
# rank sites according to proximity to STOP
sortedallpam <-
  allpam_single[order(-allpam_single$start), ]
sortedallpam <-
  sortedallpam[order(sortedallpam$strand), ]
# compute gRNAs for the 5 best PAM-sites
if (nrow(sortedallpam) < 5) {
  pamnumber <- nrow(sortedallpam)
} else { 
  pamnumber <- 5
}
endonucleases <- data.frame(
name    = c("FnCpf1", "FnCpf1", "LbCpf1", "AsCpf1", "MbCpf1"),
pattern = c("TTV", "TYN", "HYBV", "TTTV", "TYYV"),
handle  = c(
"GTTGTAGAT",  # FnCpf1
"GTTGTAGAT",  # - " -
"AAGTGTAGAT", # LbCpf1
"CTTGTAGAT",  # AsCpf1
"TTTGTAGAT"  # MbCpf1
),
stringsAsFactors = F
)
handles <- unique(subset(endonucleases, select = -c(pattern)))
handles <- handles[order(handles$name), ]
handle <- rep("handle", times = pamnumber)
forwardoligo <- as.character(xscat(nts[11:100], DNAString("TCAGGTGGAGGAGGTAGTG")))
grnas <- rep("grna", times = pamnumber)
dim(grnas) <- c(pamnumber, 1)
colnames(grnas) <- "gRNAs"
for (i in (1:pamnumber)) {
  if (sortedallpam$strand[i] == "direct") {
    grnastart <- sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 83
    grnas[i] <-
    as.character(reverseComplement(nts[(grnastart):(grnastart + 19)]))
  } else if (sortedallpam$strand[i] == "reverse") {
    grnastart <- sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 * (20 - sortedallpam$start[i]) + 81
    grnas[i] <-
   as.character(nts[(grnastart - 19):(grnastart)])
}
  }
summary <-matrix(rep(0, len = pamnumber*6), nrow = pamnumber)
colnames(summary) <- c("Rank", "Cpf1", "PAM", "Strand", "gRNA-coding sequence", "Reverse oligo")
summary = as.data.frame(summary)
for (i in (1:pamnumber)) {
  if (nchar("TTV") == sortedallpam$width[i] &
      countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TTV"), fixed = F) > 0 & ("FnCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "FnCpf1"
    handle[i] <- handles[handles$name == "FnCpf1", 2]
  }
}
for (i in (1:pamnumber)) {
  if (nchar("TYN") == sortedallpam$width[i] &
      countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TYN"), fixed = F) > 0 & ("FnCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "FnCpf1"
    handle[i] <- handles[handles$name == "FnCpf1", 2]
      }
}
for (i in (1:pamnumber)) {
  if (nchar("HYBV") == sortedallpam$width[i] &
      countPattern(DNAString(sortedallpam$PAM[i]), DNAString("HYBV"), fixed = F) > 0 & ("LbCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "LbCpf1"
    handle[i] <- handles[handles$name == "LbCpf1", 2]
  }
}
for (i in (1:pamnumber)) {
  if (nchar("TTTV") == sortedallpam$width[i] &
      countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TTTV"), fixed = F) > 0 & ("AsCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "AsCpf1"
    handle[i] <- handles[handles$name == "AsCpf1", 2]
  }
}
for (i in (1:pamnumber)) {
  if (nchar("TYYV") == sortedallpam$width[i] &
      countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TYYV"), fixed = F) > 0 & ("MbCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "MbCpf1"
    handle[i] <- handles[handles$name == "MbCpf1", 2]
  }
}
for (i in (1:pamnumber)) {
  if (nchar("TYYV") == sortedallpam$width[i] &
      countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TYYV"), fixed = F) > 0 & ("MbCpf1"  %in% cpf) & countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TTTV"), fixed = F) > 0 & ("AsCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "AsCpf1, MbCpf1"
    handle[i] <- handles[handles$name == "AsCpf1", 2]
  }
}
for (i in (1:pamnumber)) {
  if (nchar("TYYV") == sortedallpam$width[i] &
      countPattern(DNAString(sortedallpam$PAM[i]), DNAString("HYBV"), fixed = F) > 0 & ("LbCpf1"  %in% cpf) & countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TTTV"), fixed = F) > 0 & ("AsCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "AsCpf1, LbCpf1"
    handle[i] <- handles[handles$name == "AsCpf1", 2]
  }
}
for (i in (1:pamnumber)) {
  if (nchar("TYYV") == sortedallpam$width[i] &
      countPattern(DNAString(sortedallpam$PAM[i]), DNAString("HYBV"), fixed = F) > 0 & ("LbCpf1"  %in% cpf) & countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TYYV"), fixed = F) > 0 & ("MbCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "LbCpf1, MbCpf1"
    handle[i] <- handles[handles$name == "LbCpf1", 2]
  }
}
for (i in (1:pamnumber)) {
  if (nchar("TYYV") == sortedallpam$width[i] &
      countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TTTV"), fixed = F) > 0 &
      ("AsCpf1"  %in% cpf) & countPattern(DNAString(sortedallpam$PAM[i]), DNAString("HYBV"), fixed = F) > 0
      & ("LbCpf1"  %in% cpf) & countPattern(DNAString(sortedallpam$PAM[i]), DNAString("TYYV"), fixed = F) > 0
      & ("MbCpf1"  %in% cpf)) {
    summary$Cpf1[i] <- "AsCpf1, LbCpf1, MbCpf1"
    handle[i] <- handles[handles$name == "AsCpf1", 2]
  }
}
for (i in (1:pamnumber)) {
  summary$PAM[i] <- sortedallpam$PAM[i]
  summary$Strand[i] <- as.character(sortedallpam$strand[i])
  summary$`gRNA-coding sequence`[i] <- grnas[i]
  summary$`Reverse oligo`[i] <- paste(as.character(reverseComplement(nts[104:163])), "AAAAAA", grnas[i], reverseComplement(DNAString(handle[i])), "AGTAGAAATTACGGTGTTTCGTC", sep = "") #!!!
  summary$Rank[i] <- as.character(i)
summary
  }
  }