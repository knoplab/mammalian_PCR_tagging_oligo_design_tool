urltemplate <-
  a("here",
    href = "http://schapb.zmbh.uni-heidelberg.de/primerapptest/",
    target = "_blank")
urlplasmid <-
  a("here",
    href = "https://www.addgene.org/69988/",
    target = "_blank")
urlplasmids <-
  a("here",
    href = "https://www.addgene.org/browse/article/28189750/",
    target = "_blank")
urlcellines <-
  a("these",
    href = "http://schapb.zmbh.uni-heidelberg.de/primerapptest/",
    target = "_blank")
panel.intro <- fluidPage(
  fluidPage(
    h4("What is PCR targeting?", style = "color:Crimson; font-weight:bold"),
    tags$ul(
      tags$li("PCR targeting is a one-step procedure for chromosomal gene tagging in mammals"), 
      tags$li("PCR targeting enables the rapid creation of cell lines with targeted large chromosomal insertions, such as GFP")
    )
  ),
  br(),
  splitLayout(cellWidths = c(1000, 500),
              fluidPage(h4("What do I need for PCR targeting?", style = "color:Crimson; font-weight:bold"),
                        
                        tags$ul(
                          tags$li("the sequence of the target locus (e.g. your favorite gene)"), 
                          tags$li(paste0("two oligos", " \u2192 ", "design them with this tool")),
                          tags$li("two helper plasmids"),
                          tags$ol(
                            tags$li(tagList("a PCR template with the desired tag, available", urltemplate)),
                            tags$li(tagList("a Cpf1 expression plasmid for co-transfection, available", urlplasmids, "and", urlplasmid))
                          ),
                          tags$li(tagList("a suitable cell line (so far PCR targeting has been shown to work with", urlcellines,  "cell lines)"))
                        )
              ),
              fluidPage(em(h4("(What do I not need for PCR targeting?)", style = "color:Crimson; font-weight:bold"),
                           tags$ul(
                             tags$li("no RNAs, RNA expression or purification"), 
                             tags$li("no purified protein"),
                             tags$li("no preassembling of RNA/protein complexes"),
                             tags$li("no cloning"))
              )
              )
  ),
  fluidPage(
    h4("How to perform PCR-targeting?", style = "color:Crimson; font-weight:bold"),
    div( 
      tags$ol(style = "list-style-type: upper-roman;",
              tags$li("Design oligos"), 
              tags$li("Order oligos"),
              tags$li("Run tagging PCR"),
              tags$li("Transfect cells"),
              tags$li("Select/enrich positive cells")
      )
    )
  ) 
)
panel.target <- fluidPage(
  # h4(em("Single DNA sequence input")),
  # br(),
  
  splitLayout(
    cellWidths = c(1000, 800),
    #cellArgs = list(style = "padding: 2px"),
    fluidPage(
      h3(strong("Oligo-design - STEP 1: Define your target")),
      br(),
      em(h4(
        "Please insert your target sequence the following way:"),
        h4(strong("200 nts before and 200 nts after the insertion site (stop codon), 403 nts altogether")
        ), style = "color:MediumBlue"),
      
      h4(uiOutput("inp_target_text")),
      br(),
      textAreaInput(
        "inp_target",
        label = NULL,
        width = 900,
        rows = 4,
        resize = "both",
        placeholder = paste0(paste(rep("N", 200), collapse = ""), "TAA", paste(rep("N", 200), collapse = ""))
        
      ),
      tags$head(
        tags$style("#inp_target{font-family: Roboto Mono}")
      ),
      actionButton("example", label = "Click here for an example"),
      br(),
      br(),
      h5(textOutput("inputlength"), style = "color:red"),
      h5(textOutput("inputnuc"), style = "color:red"),
      h4(htmlOutput("inp_target_seq"))
    ),
    fluidPage(
      br(),
      br(),
      img(src='oligos_part.svg', width = 750, align = "right")
    )
  ),
  fluidPage(fluidPage(div(
    style = "width:1750px;",
    fluidRow(verbatimTextOutput("targetsequence")),
    tags$head(
      tags$style(
        "#targetsequence{background: white; font-family: Roboto Mono}"
      )
    )
  )
  )
  )
  # fluidRow(
  #   column(
  #     width = 5,
  #     h4("Search space for PAM sites before the insertion site:"),
  #     h5(
  #       em(
  #         "Number of nucleotides upstream of the insertion sites (stop codon) to search for PAM sites on the direct strand."
  #       ),
  #       style = "color:grey"
  #     )
  #   ),
  #   column(
  #     width = 1,
  #     numericInput(
  #       "pamregionu",
  #       label = NULL,
  #       value = 17,
  #       min = 3,
  #       max = 20,
  #       width = 100
  #     )
  #   ),
  #   tags$head(tags$style(".container-fluid {width: 2000px;}"))
  # ),
  # # fluidPage(column(
  #   width = 6,
  #   h4(
  #     "Number of nucleotides downstream of the stop codon to look for PAM sites on the direct strand:"
  #   )
  # ),
  # column(
  #   width = 1,
  #   numericInput(
  #     "pamregiondd",
  #     label = NULL,
  #     value = 0,
  #     min = 0,
  #     max = 5,
  #     width = 100
  #   )
  # )),
  # fluidRow(column(
  #   width = 5,
  #   h4("Search space for PAM sites after the insertion site:"),
  #   h5(
  #     em(
  #       "Number of nucleotides downstream of the insertion site (stop codon) to search for PAM sites on the reverse strand."
  #     ),
  #     style = "color:grey"
  #   )
  # ),
  # column(
  #   width = 1,
  #   numericInput(
  #     "pamregiond",
  #     label = NULL,
  #     value = 17,
  #     min = 3,
  #     max = 50,
  #     width = 100
  #   )
  # ))
)
# Select CASTLING moduls
panel.genename <- 
  fluidPage(
    h3(strong("Oligo-design - STEP 2: Give a name")),
    br(),
    h4(em("This input will be used to generate meaningful names for the oligos."), style = "color:MediumBlue"),
    br(),
    splitLayout(cellWidths = c(300, 200),
                h4("Provide a name for the gene:"),
                textInput(
                  "genename",
                  label = NULL,
                  width = 100,
                  value = "GENE"
                )
    )
  )
panel.moduls <- 
  fluidPage(
    h3(strong("Oligo-design - STEP 3: Select Cpf1 variant(s)")),
    br(),
    h4(em("PCR targeting makes use of published Cpf1 plasmids available form Addgene."), style = "color:MediumBlue"),
    h4(em("In case you do not have access to one of them, you can unselect the plasmid."), style = "color:MediumBlue"),
    br(),
    
    checkboxGroupInput(
      "inp_cpf",
      label = NULL,
      width = "100%",
      choiceNames = list(
        HTML("LbCpf1(TTTV) - <a href = 'https://www.addgene.org/69988/' target = '_blank'> Addgene: pY016</a>, handle: UAAUUUCUACUAAGUGUAGAU"),
        HTML("LbCpf1(TYCV) - <a href = 'https://www.addgene.org/89355/' target = '_blank'> Addgene: pY230</a>, handle: UAAUUUCUACUAAGUGUAGAU"),
        HTML("AsCpf1(TATV) - <a href = 'https://www.addgene.org/89353/' target = '_blank'> Addgene: pY220</a>, also recognizes RATR and TTTV PAMs, handle: UAAUUUCUACUCUUGUAGAU"),
        HTML("AsCpf1(TYCV) - <a href = 'https://www.addgene.org/89351/' target = '_blank'> Addgene: pY210</a>, also recognizes MCCC PAMs, handle: UAAUUUCUACUCUUGUAGAU"),
        "In case new Cpf1 variants with different PAM site specificities and different handle sequences become available, you can enter these here:"
      ),
      choiceValues = c(
        "LbCpf1_TTTV",
        "LbCpf1_TYCV",
        "AsCpf1_TATV",
        "AsCpf1_TYCV",
        "Other"
      ),
      selected = c(
        "LbCpf1_TTTV",
        "LbCpf1_TYCV",
        "AsCpf1_TATV",
        "AsCpf1_TYCV"
      )
    ),
    
    bootstrapPage(
      div(
        style = "display:inline-block",
        textInput(
          inputId = "inp_pam",
          label = NULL,
          placeholder = "PAM",
          width = "100%"
        )
      ),
      div(
        style = "display:inline-block",
        textInput(
          inputId = "inp_handle",
          label = NULL,
          placeholder = "HANDLE",
          width = "100%"
        )
      ),
      div(
        style = "display:inline-block",
        textInput(
          inputId = "inp_name",
          label = NULL,
          value = "Plasmid name",
          width = "100%"
        )
      )
    ),
    fluidRow(#column(width = 1, h5(uiOutput("pamhandle"), style = "color:red")),
      column(width = 2, uiOutput("pamhandlelink"))),
h5(textOutput("handlecontrol"), style = "color:red"))
panel.region <-            
  
  splitLayout(
    cellWidths = c(1000, 810),
    fluidPage(
      # tags$style(HTML(
      #   '#forwardoligo {font-family: "Roboto Mono"}'
      # )),
      #htmlOutput("forwardoligo"),
      #tags$style(HTML('#forwardoligo th {font-family: "Arial"}')),
      h3(strong("Oligo-design - STEP 4: Define length of homology arms")),
      br(),
      h4(em("Please feel free to choose the desired lengths of the homology arms."), style = "color:MediumBlue"),
      br(),
      splitLayout(
        cellWidths = c(300, 300),
        h4("5'-homology arm for the M1-oligo:", style = "color:green"),
        numericInput(
          "fiveha",
          label = NULL,
          value = 90,
          min = 1,
          max = 200,
          width = 100
        )
      ),
      splitLayout(
        cellWidths = c(300, 300),
        h4("3'-homology arm for the M2-oligos:", style = "color:CornflowerBlue "),
        numericInput(
          "threeha",
          label = NULL,
          value = 55,
          min = 1,
          max = 200,
          width = 100
        )
      ),
      hr(),
      h3(strong("Oligo-design - STEP 5: Compute oligo sequences")),
      br(),
      h4(em("The algorithm will search for possible PAM sites in the colored sequence space around the insertion site"), style = "color:MediumBlue"),
      h4(em("and generate the oligos as outlined in the image on the right."), style = "color:MediumBlue"),
      br(),
      br(),
      actionButton("compute", label = "Find PAM sites and obtain your oligos"),
      br(),
      br(),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css?family=Roboto+Mono"),
      #h5(em("Extended search space*"), style = "color:MediumVioletRed"),
      tags$style(HTML(
        '#nucleotides {font-family: "Roboto Mono"}'
      )),
      tags$style(HTML(
        '#nucleotidesr {font-family: "Roboto Mono"}'
      )),
      br(),
      br(),
      htmlOutput("nucleotides", style = "font-size: 20px"),
      htmlOutput("nucleotidesr", style = "font-size: 20px")
      
    ),
    fluidPage(
      img(src='oligos.svg', width = 800, align = "right"),
      br(),
      img(src='searchspace_c.svg', width = 800, align = "right")
    )
  )

panel.output <- 
  fluidPage(
    h3("RESULTS: Retrieve sequences of M1- and M2-oligos", style = "font-weight:bold"),
    br(),
    br(),
    htmlOutput("monetitle"),
    br(),
    htmlOutput("monedescr"),
    br(),
    br(),
    #h5(textOutput("reversetitle"), style = "font-weight:bold"),
    
    tags$style(HTML('#cpf {table-layout: fixed}')),
    tags$style(HTML('#cpf {font-family: "Roboto Mono"}')),
    tags$style(HTML('#cpff {font-family: "Roboto Mono"}')),
    tags$style(HTML('#cpf td {vertical-align: middle;}')),
    tags$style(HTML('#cpf th {vertical-align: middle;}')),
    tags$style(HTML('#cpf td {text-align: middle;}')),
    tags$style(HTML('#cpf th {text-align: middle;}')),
    tags$style(HTML('#cpff th {font-family: "Arial"}')),
    tags$style(HTML('#cpf th {font-family: "Arial"}')),
    tags$style(HTML('#cpfe th {font-family: "Arial"}')),
    #tags$style(HTML('#cpf tr:last-child td {border-top: 0}')),
    tags$style(HTML('#cpf td:nth-child(4) {border-left: 2px solid grey}')),
    tags$style(HTML('#cpf th:nth-child(4) {border-left: 2px solid grey}')),
    tags$style(HTML('#cpff th:nth-child(2) {width: 1028px}')),
    tags$style(HTML('#cpff td:nth-child(2) {width: 1028px}')),
    tags$style(HTML('#cpff th:first-child {width: 200px}')),
    tags$style(HTML('#cpff td:first-child {width: 200px}')),
    #tags$style(HTML('#cpf tr:nth-child(2) td {border-top: 0}')),
    
    tags$style(HTML('#cpf tr:first-child td {border-top: 0}')),
    #tags$style(HTML('#cpf tr:nth-child(3) {border-top: 2px solid lightgrey}')),
    htmlOutput("cpff"),
    br(),
    br(),
    br(),
    htmlOutput("mtwotitle"),
    br(),
    htmlOutput("mtwodescr"),
    br(),
    br(),
    htmlOutput("cpf"),
    #h4(em(textOutput("nopam"),  style = "color:MediumVioletRed")),
    br(),
    htmlOutput("mtwocomment"),
    br(),
    br()
  )
panel.extended <- 
  
  fluidPage(
    htmlOutput("mtwoext"),
    
    splitLayout(cellWidths = c(1250, 850),
                        
                        fluidPage(
                          br(),
                          br(),
                          br(),
                          splitLayout(cellWidths = c(500, 150, 250),
                                      h4(checkboxInput("extended", "Extended search space, number of nucleotides (max. 100):", width = 500)),
                                      numericInput(
                                        "inp_ext",
                                        label = NULL,
                                        value = 50,
                                        min = 1,
                                        max = 100,
                                        width = 100
                                      ),
                                      actionButton("inp_apply", label = "Apply extended search space")
                          )
                          #htmlOutput("regiontexte")
                        ),
                        uiOutput("searchspacee")
  ),
  tags$link(rel = "stylesheet",
            href = "https://fonts.googleapis.com/css?family=Roboto+Mono"),
  #h5(em("Extended search space*"), style = "color:MediumVioletRed"),
  tags$style(HTML(
    '#nucleotidese {font-family: "Roboto Mono"}'
  )),
  tags$style(HTML(
    '#nucleotidesre {font-family: "Roboto Mono"}'
  )),
  br(),
  br(),
  htmlOutput("nucleotidese", style = "font-size: 20px"),
  htmlOutput("nucleotidesre", style = "font-size: 20px"),
  br(),
  br(),
  h5(textOutput("stopwarning"), style = "color:red"),
  br(),
  tags$style(HTML('#cpfe {font-family: "Roboto Mono"}')),
  tags$style(HTML('#cpfe td {vertical-align: middle;}')),
  tags$style(HTML('#cpfe th {vertical-align: middle;}')),
  tags$style(HTML('#cpfe td {text-align: middle;}')),
  tags$style(HTML('#cpfe th {text-align: middle;}')),
  #tags$style(HTML('#cpfe tr:first-child td {border-top: 0}')),
  #tags$style(HTML('#cpfe tr:nth-child(2) td {border-top: 0}')),
  #tags$style(HTML('#cpfe tr:last-child td {border-top: 0}')),
  tags$style(HTML('#cpfe td:nth-child(4) {border-left: 2px solid grey}')),
  tags$style(HTML('#cpfe th:nth-child(4) {border-left: 2px solid grey}')),
  htmlOutput("cpfe"),
  br(),
  htmlOutput("mtwocommente"),
  br()
  )
panel.download <- fluidPage(
  br(),
  br(),
  downloadButton("downloadxlsx", "Download results (.xlsx)"),
  
  downloadButton("downloadcsv", "Download results (.csv)"),
  #br(),
  #br(),
  #downloadButton("report", "PDF report"),
  br(),
  br(),
  h3(htmlOutput("nextstep"))
)
urlr <-
  a("www.R-project.org",
    href = "https://www.R-project.org/",
    target = "_blank")
urlshiny <-
  a("www.CRAN.R-project.org/package=shiny",
    href = "https://CRAN.R-project.org/package=shiny",
    target = "_blank")
urlshinyjs <-
  a("www.CRAN.R-project.org/package=shinyjs",
    href = "https://CRAN.R-project.org/package=shinyjs",
    target = "_blank")
urlbiostrings <-
  a("www.bioconductor.org/packages/release/bioc/html/Biostrings.html",
    href = "https://bioconductor.org/packages/release/bioc/html/Biostrings.html",
    target = "_blank")
panel.about <- fluidPage(
   h3(strong("References")),
  # br(),
   tags$ul(
     tags$li(h4("R Core Team (2018).", em("R: A language and environment for statistical computing. R Foundation for Statistical Computing"), "Vienna, Austria. ", urlr)),
     tags$li(h4("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie, Jonathan McPherson (2018).", em("shiny: Web Application Framework for R."), "R package version 1.1.0. ", urlshiny)),
    tags$li(h4("Dean Attali (2018).", em("shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds."), "R package version 1.0. ", urlshinyjs)),
  tags$li(h4("Pages H, Aboyoun P, Gentleman R, DebRoy S (2017).", em("Biostrings: Efficient manipulation of biological strings."), "R package version 2.46.0. ", urlbiostrings))
  ),
  br(),
  br(),
  h3(strong("Disclaimer")),
  br(),
  h4("Under no circumstance shall the authors be liable for any claim, damage or loss arising from the use of the application ", em("'Online oligo design tool for PCR-targeting in mammalian cells'.")),
  h4("The use of the tool and the reliance on any information on the site is solely at the user's own risk.")
)
# panel.pcrr <- fluidPage(
#   
#   h4("Choose a template and a reverse oligo for the PCR:"),
#   br(),
#   actionButton("runpcr", label = "Run PCR"))
# pcrmix <- data.frame(
#   c("10x HiFi buffer",
#               "dNTPs (10 mM)",
#               "M1 (10 ÃÂÃÂ¼M)",
#               "M2 (10 ÃÂÃÂ¼M)",
#               "Template (~ 100 ng)",
#     "Betaine (5M)",
#     "MgCl2 (50 mM)",
#     "HiFi-Polymerase",
#     "H2O"),
#   c("5", "5", "2.5", "2.5", "1", "5", "1", "1", "Ad 50"),
# 
#   stringsAsFactors = F
# )
# names(pcrmix) <- c("", "ÃÂÃÂ¼L")
# pcr <- data.frame(
#   c("1",
#     "2",
#     "3",
#     "4",
#     "5",
#     "6"),
#   c("95",
#     "95",
#     "60",
#     "72",
#     "72",
#     "4"),
#   c("2:00",
#     "0:20",
#     "0:30",
#     "1:45",
#     "5:00",
#     "hold"),
#   c("",
#     "",
#     "",
#     "Go to 2. step x 29",
#     "",
#     ""),
#   stringsAsFactors = F
# )
# names(pcr) <- c("Step", "ÃÂÃÂ°C", "min", "")
# panel.pcr <-   fluidPage(
#   fluidRow(h5(em("Template:"), "pMaM519 / pMaM518", br(), br(), em("Primer:"), "M1, M2", br(), br(), em("Expected size:"), "~ 1.3kbp / ~ 2.2kb:")),
#   br(),
#   column(width = 2, h5(em("HiFi-Polymerase standard mix")),
#   tableOutput("pcrmixtable")),
#   column(width = 2, h5(em("HiFi-Polymerase standard program")),
#                          tableOutput("pcrtable")))

# tabPanel(
#   title = "FASTA input",
#   
#   fluidPage(
#     titlePanel(
#       strong("Primer Design Tool for PCR-based CRISPR-Cpf1-assisted C-terminal Tagging in Mammalian Cells [TEST]")
#     ),
#     h4(
#       "This webpage provides an oligonucleotide design tool...etc Publication:"
#     ),
#     br(),
#     h5(em("Under construction"), style = "color:red"),
#     panel.ftarget,
#     br(),
#     panel.fmoduls,
#     br(),
#     panel.fdownload
#   )
# ),

#       tabPanel(title = "Tagging PCR",
#                
#                fluidPage(
#                  titlePanel(
#                    strong("Protocol for Tagging PCR")
#                  ),
#                  h4(
#                    "Here the Tagging PCR Protocol is described...etc Publication:"
#                  ),
#                  br(),
#                  h5(em("Under construction"), style = "color:red"),
#                  panel.pcr
#                )
#                
# ),
