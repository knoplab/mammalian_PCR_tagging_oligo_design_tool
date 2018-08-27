# General server elements
'%ni%' <- Negate('%in%')
endonucleases <- data.frame(
  Name    = c("LbCpf1",
              "LbCpf1",
              "LbCpf1",
              "AsCpf1",
              "AsCpf1"),
  `PAM` = c("TTTV", "TYCV", "MCCC", "TATV", "RATR"),
  Handle  = c(
    "UAAUUUCUACUAAGUGUAGAU",
    # LbCpf1
    "UAAUUUCUACUAAGUGUAGAU",
    # LbCpf1
    "UAAUUUCUACUAAGUGUAGAU",
    # LbCpf1
    "UAAUUUCUACUCUUGUAGAU",
    # AsCpf1
    "UAAUUUCUACUCUUGUAGAU"  
  ),
  stringsAsFactors = F
)
handletable <- data.frame(
  Name    = c("LbCpf1",
              "AsCpf1"),
  `gRNA_handle`  = c("UAAUUUCUACUAAGUGUAGAU",
                     # LbCpf1
                     "UAAUUUCUACUCUUGUAGAU"  
  ),
  stringsAsFactors = F
)
# output$endo <- renderTable({
#   handletable
# })
#output$pcrmixtable <- renderTable({pcrmix})
#output$pcrtable <- renderTable({pcr})
urlhandle <-
  a("Direct repeat sequences of Cpf1 orthologs",
    href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4638220/",
    target = "_blank")
urliupac <-
  a("Please use IUPAC Codes for Nucleotides.",
    href = "https://www.bioinformatics.org/sms/iupac.html",
    target = "_blank")
####
regiontext <- 
  paste(h4(em("Search space for PAM sites around the insertion site (stop codon):")),
        h5(em(
          "17 nucleotides upstream on the direct strand and 17 nucleotides downstream on the reverse strand"
        ), style = "color:MediumAquaMarine"), sep = '\n')
stopwarning <- "Warning! The insertion site (marked in bold) is not a stop codon."
#### Column names - summary
sumgrna <- paste(
  '<span style = "font-weight:bold; font-family: Arial">',
  "gRNA (",
  '<span style = "color:orange">',
  "Cpf1-variant-specific handle",
  '</span>',
  " + ",
  '<span style = "color:orange">',
  "spacer",
  '</span>',
  ")",
  '</span>',
  sep = ""
)
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
  '<sup>&#9660;</sup>',
  '<sub>&#9650;</sub>',
  '<br>',
  " Cpf1 cleavage sites, ",
  '<span style = "text-decoration: underline">',
  "overhangs",
  '</span>',
  '</span>')
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
  '<br>',
  " Cpf1 cleavage sites, ",
  '<span style = "text-decoration: underline">',
  "overhangs",
  '</span>',
  '</span>')
colcsvf <- c(
  "Forward oligo - Suggested name",
  "Sequence",
  "Length",
  "",
  "",
  "",
  "",
  ""
)
colsum <-               c(
  #"Distance",
  "Suggested name",
  "Sequence",
  "Length",
  "Cpf1",
  "PAM",
  "Strand",
  "gRNA (handle + spacer)",
  "Rank",
  "Target"
)
colsumf <-               c("Suggested name",
                           "Sequence",
                           "Length")
#####
monetitle <- paste0(
  '<span style = "font-weight:bold; font-family: Arial; font-size: 24px; color:green">', "M1-oligo ",
  '</span>',
  '<span style = "font-weight:bold; font-family: Arial; font-size: 24px">', "(one forward oligo)",
  '</span>'
)
monedescr <- paste0(
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">', "Only one M1-oligo is generated, since this oligo is independent from the PAM sites or Cpf1 variants.",
  '</span>'
)
mtwotitle <- paste0(
  '<span style = "font-weight:bold; font-family: Arial; font-size: 24px; color:CornflowerBlue ">', "M2-oligo(s) ",
  '</span>',
  '<span style = "font-weight:bold; font-family: Arial; font-size: 24px">', "(one or more reverse oligos)",
  '</span>'
)
mtwodescr <- paste0(
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">', "Zero, one or multiple M2-oligos are generated, since the oligos depend on the PAM sites and Cpf1 variants.",
  '</span>'
)
mtwoext <- paste0(
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">', "If zero oligos are predicted, please continue by selecting extended search space below, which will widen the region to search for suitable PAM sites in the 3", "\u0027", "UTR of the gene. The homology arm of the M2 oligo is adjusted in such a manner that the binding site of the guide RNA is deleted. Since this is in the 3", "\u0027", "UTR of the gene after the insertion site of the cassette, we deem this to be non-critical.",
  '</span>')
mtwoextb <- paste0(
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">', "If you continue by selecting extended search space below, this will widen the region to search for suitable PAM sites in the 3", "\u0027", "UTR of the gene. The homology arm of the M2 oligo is adjusted in such a manner that the binding site of the guide RNA is deleted. Since this is in the 3", "\u0027", "UTR of the gene after the insertion site of the cassette, we deem this to be non-critical.",
  '</span>')
nextstep <- paste0('<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">', "If still zero oligos are predicted, you are interested in one of the rare genes without a PAM site nearby (< 0.5%). Go ahead and keep on increasing the search space, or watch out for new Cpf1 variants (STEP 3).
                                                        ", '</span>' )
mtwocomment <- paste0('<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumAquaMarine">', "No PAM sites found in the confined search space.", '</span>')
mtwocommenterank <- paste0(
  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumBlue">', "M2-oligos are ranked according to the following criteria:", 
  '<br>',
  '<br>',
  tags$ol(
    tags$li("no potential Pol III terminator encoded in the oligo (reverse oligos containing 4 or more A-s in the gRNA encoding sequence are marked with an *)"),
    tags$li("conventional PAM sites preferred against MCCC or RATR"),
    tags$li("cleavage site proximity to STOP codon"),
    tags$li("(coming soon: potential off-targets)")
  ),
  '</span>'
)