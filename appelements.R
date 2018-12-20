# General server elements - titles, comments, warnings, urls
endonucleases <- data.frame(
  Name    = c("LbCas12a",
              "LbCas12a",
              "LbCas12a",
              "AsCas12a",
              "AsCas12a"),
  `PAM` = c("TTTV", "TYCV", "MCCC", "TATV", "RATR"),
  `Direct repeat`  = c(
    "UAAUUUCUACUAAGUGUAGAU",
    # LbCas12a
    "UAAUUUCUACUAAGUGUAGAU",
    # LbCas12a
    "UAAUUUCUACUAAGUGUAGAU",
    # LbCas12a
    "UAAUUUCUACUCUUGUAGAU",
    # AsCas12a
    "UAAUUUCUACUCUUGUAGAU"
  ),
  stringsAsFactors = F
)
drtable <- data.frame(
  Name    = c("LbCas12a",
              "AsCas12a"),
  `crRNA_dr`  = c("UAAUUUCUACUAAGUGUAGAU",
                  "UAAUUUCUACUCUUGUAGAU"),
  stringsAsFactors = F
)
urldr <-
  a(
    "Direct repeat sequences of Cas12a (Cpf1) orthologs",
    href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4638220/",
    target = "_blank"
  )
urliupac <-
  a("Please use IUPAC Codes for Nucleotides.",
    href = "https://www.bioinformatics.org/sms/iupac.html",
    target = "_blank")
ensinputact <- paste0('<span style = "color:MediumBlue">', "Please enter the ",  '<a href="https://www.ensembl.org/info/genome/genebuild/genome_annotation.html">Ensembl transcript ID</a>',  " of your target:", '</span>')
ensinputpass <- paste0('<span style = "opacity: 0.5">', "Please enter the ",  '<a href="https://www.ensembl.org/info/genome/genebuild/genome_annotation.html">Ensembl transcript ID</a>',  " of your target:", '</span>')
targinputact <- paste0('<span style = "color:MediumBlue">', "Or provide your target sequence in the following way:", '<br>', strong("200 nt before and 200 nt after the target site (stop codon), 403 nt altogether"),  '</span>')
targinputpass <- paste0('<span style = "opacity: 0.5">', "Or provide your target sequence in the following way:", '<br>', strong("200 nt before and 200 nt after the target site (stop codon), 403 nt altogether"),  '</span>')
enstwarning <- paste0('<span style = "color:Red">', "The Ensembl ID format should be ENSTXXXXXXXXXXX or ENSTXXXXXXXXXXX.X",'</span>')
regiontext <-
  paste(h4(em("Search space for PAM sites:")),
        h5(
          em(
            "17 nucleotides upstream on the direct strand and 17 nucleotides downstream on the reverse strand"
          ),
          style = "color:MediumAquaMarine"
        ), sep = '\n')
stopwarning <-
  paste0("Warning! The insertion site (marked in red) is not a stop codon.")
# crRNA header
sumcrRNA <- paste(
  '<span style = "font-weight:bold; font-family: Arial">',
  "crRNA (",
  '<span style = "text-decoration: underline; color:orange">',
  "Cas12a (Cpf1)-variant-specific direct repeat",
  '</span>',
  " + ",
  '<span style = "color:orange">',
  "spacer",
  '</span>',
  ")",
  '</span>',
  sep = ""
)
# Target sequence header - 17 nt search space
sumrtarget <-                 paste0(
  '<span style = "font-weight:bold; font-family: Arial">',
  "Target direct and reverse strand with ",
  '<span style = "color:MediumAquaMarine; font-weight:bold; font-family: Arial">',
  "PAM",
  '</span>',
  " and ",
  '<span style = "color:orange">',
  "target",
  '</span>',
  ", ",
  '<br>',
  '<sup>&#9660;</sup>',
  '<sub>&#9650;</sub>',
  " Cas12a (Cpf1) cleavage sites, ",
  '<span style = "text-decoration: underline">',
  "overhangs",
  '</span>',
  '</span>'
)
# Target sequence header - extended search space
sumetarget <- paste0(
  '<span style = "font-weight:bold; font-family: Arial">',
  "Target direct and reverse strand with ",
  '<span style = "color:MediumVioletRed; font-weight:bold; font-family: Arial">',
  "PAM",
  '</span>',
  " and ",
  '<span style = "color:orange">',
  "target",
  '</span>',
  ", ",
  '<sup>&#9660;</sup>',
  '<sub>&#9650;</sub>',
  " Cas12a (Cpf1) cleavage sites, ",
  '<span style = "text-decoration: underline">',
  "overhangs",
  '</span>',
  '</span>'
)
# Column names, forward oligo - download table
colcsvf <- c("Forward oligo - Suggested name",
             "Sequence",
             "Length",
             "",
             "",
             "",
             "",
             "")
# Column names - download table
colsum <-               c(
  "Suggested name",
  "Sequence",
  "Length",
  "Rank",
  "Target",
  "crRNA (direct repeat + spacer)",
  "Cas12a (Cpf1)",
  "PAM",
  "Strand"
)
# Column names, forward oligo - download table
colsumf <-               c("Suggested name",
                           "Sequence",
                           "Length")
##### Titles and comments
monetitle <- paste0(
  '<br>',
  '<br>',
  '<span style = "font-weight:bold; font-family: Arial; font-size: 24px; color:green">',
  "M1 tagging oligo ",
  '</span>',
  '<span style = "font-weight:bold; font-family: Arial; font-size: 24px">',
  "(one forward oligo)",
  '</span>'
)
monedescr <- paste0(
  '<br>',
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">',
  "Only one M1 tagging oligo is generated, since this oligo is independent from the PAM sites or Cas12a (Cpf1) variants.",
  '<br>',
  '<br>',
  '<br>',
  '</span>'
)
mtwotitle <- paste0(
  '<br>',
  '<br>',
  '<span style = "font-weight:bold; font-family: Arial; font-size: 24px; color:CornflowerBlue ">',
  "M2 tagging oligo(s) ",
  '</span>',
  '<span style = "font-weight:bold; font-family: Arial; font-size: 24px">',
  "(one or more reverse oligos)",
  '</span>'
)
mtwodescr <- paste0(
  '<br>',
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">',
  "Zero, one or multiple M2 tagging oligos are generated, since the oligos depend on the PAM sites and Cas12a (Cpf1) variants.",
  '<br>',
  '<br>',
  '<br>',
  '</span>'
)
mtwoext <- paste0(
  '<br>',
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">',
  "Please continue by selecting extended search space below, which will widen the region to search for suitable PAM sites in the 3",
  "\u0027",
  "UTR of the gene. In this case the homology arm of the M2 tagging oligo is adjusted in such a manner that the binding site of the guide RNA is deleted. Since this is in the 3",
  "\u0027",
  "UTR of the gene after the insertion site of the cassette, we deem this to be non-critical.",
  '<br>',
  '<br>',
  '<br>',
  '<br>',
  '</span>'
)
mtwoextb <- paste0(
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">',
  "The use of oligos marked with an * is not recommended, as they can lead to decreased tag efficiency. If you continue by selecting extended search space below, this will widen the region to search for suitable PAM sites in the 3",
  "\u0027",
  "UTR of the gene.",
  '<br>',
  "The homology arm of the M2 tagging oligo is adjusted in such a manner that the binding site of the guide RNA is deleted. Since this is in the 3",
  "\u0027",
  "UTR of the gene after the insertion site of the cassette, we deem this to be non-critical.",
  '<br>',
  '<br>',
  '</span>'
)
nextstep <-
  paste0(
    '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">',
    "If still zero oligos are predicted, you are interested in one of the rare genes without a PAM site nearby (< 0.5%). Go ahead and keep on increasing the search space, or watch out for new Cas12a (Cpf1) variants (STEP 3).
    ",
    '<br>',
    '<br>',
    '<br>',
    '<br>',
    '</span>'
  )
mtwocomment <-
  paste0(
    '<br>',
    '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumAquaMarine">',
    "No PAM sites found in the +/- 17 nt search space.",
    '<br>',
    '<br>',
    '</span>'
  )
mtwocommenterank <- paste0(
  '<br>',
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">',
  "M2 tagging oligos are ranked according to the following criteria:",
  '<br>',
  '<br>',
  tags$ol(
    tags$li(
      "no potential Pol III terminator encoded in the oligo (reverse oligos containing 4 or more A-s in the crRNA encoding sequence are marked with an * - these are not recommended for gene tagging)"
    ),
    tags$li("quality of PAM site"),
    tags$li("position of Cas12a cleavage site compared to STOP codon"),
    tags$li("(coming soon: potential off-targets)")
  ),
  '<br>',
  '</span>'
)