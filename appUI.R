# In this file the Shiny UI elements of the Mammalian PCR targetin online tool are defined


# What is PCR targeting?  --------------------------------------------------------------------


urlplasmid <-
  a("here",
    href = "https://www.addgene.org/69988/",
    target = "_blank")
urlplasmids <-
  a("here",
    href = "https://www.addgene.org/browse/article/28189750/",
    target = "_blank")
mailknop <-
  a("m.knop (at) zmbh.uni-heidelberg.de",
    href = "mailto:m.knop@zmbh.uni-heidelberg.de",
    target = "_blank")
knoplab <-
  a("www.knoplab.de",
    href = "http://www.knoplab.de",
    target = "_blank")
panel.needed <-
  fluidPage(
    h4("What do I need for PCR targeting?", style = "color:Crimson; font-weight:bold"),
    
    tags$ul(
      tags$li("the sequence of the target locus (e.g. your favorite gene)"),
      tags$li(
        "two oligos (M1 and M2)",
        " \u2192 ",
        "design them with ",
        actionLink("whatlinktotool", "our oligo design tool")
      ),
      tags$li("two helper plasmids"),
      tags$ol(tags$li(
        tagList(
          "a PCR template with the desired tag, list of cassettes available",
          actionLink("linktotemplates", "here")
        )
      ),
      tags$li(
        tagList(
          "a Cas12a (Cpf1) expression plasmid for co-transfection, available",
          urlplasmids,
          "and",
          urlplasmid
        )
      )),
      tags$li(
        tagList(
          "a suitable cell line - so far PCR targeting has been shown to work with these cell lines: HEK293T, HeLa, U2OS, RPE-1, mESCs, C2C12-1"
        )
      )
    ),
    br(),
    h4("How to perform PCR-targeting?", style = "color:Crimson; font-weight:bold"),
    div(
      tags$ol(
        style = "list-style-type: upper-roman;",
        tags$li("Design M1 and M2 oligos"),
        tags$li("Order M1 and M2 oligos"),
        tags$li(
          "Run tagging PCR, protocol available",
          actionLink("linktopcr", "here")
        ),
        tags$li("Transfect cells"),
        tags$li("Select/enrich positive cells")
      )
    ),
    br()
  )

panel.whatintro <- fluidPage(
  h3(strong("See reference: F\u00fcller", em("et al."))),
  br(),
  br(),
  fluidPage(
    h4("What is PCR targeting?", style = "color:Crimson; font-weight:bold"),
    tags$ul(
      tags$li(
        "PCR targeting is a one-step procedure for chromosomal gene tagging in mammals"
      ),
      tags$li(
        "PCR targeting enables the rapid creation of cell lines with targeted large chromosomal insertions, such as GFP"
      )
    )
  ),
  br(),
  br(),
  fluidPage(
    h4("How does PCR targeting work?", style = "color:Crimson; font-weight:bold"),
    tags$ul(
      tags$li(
        "a gene specific PCR cassette is transfected into the target cells together with a helper plasmid containing a Cas12a endonuclease"
      ),
      tags$li(
        "the insertion of the PCR cassette into the chromosome yields a fusion of the tag (e.g. GFP) with the target gene"
      )
    )
  ),
  br(),
  br(),
  fluidPage(
    h4("How is the PCR cassette generated?", style = "color:Crimson; font-weight:bold"),
    tags$ul(
      tags$li(
        "two gene specific oligos (M1 and M2) provide the homology arms (50 to 90 nts in length) for targeted integration by HDR"
      ),
      tags$ul(
        tags$li(
          "the M2 oligo additionally provides a protospacer sequence for the Cas12a endonuclease"
        )
      ),
      tags$li(
        "the generic template cassette provides the tag (e.g. a fluorescent protein) and additional features (e.g. a selection marker)"
      ),
      tags$ul(
        tags$li(
          "it also contains the backbone of a Cas12a specific crRNA gene, consisting of promoter and crRNA direct repeat"
        )
      )
    )
  ),
  br(),
  br(),
  fluidPage(
    h4("What is the tagging principle?", style = "color:Crimson; font-weight:bold"),
    tags$ul(
      tags$li(
        "the PCR cassette contains a crRNA gene that is expressed inside the cell"
      ),
      tags$li(
        "the crRNA directs Cas12a (expressed from the helper plasmid) to the target locus close to the insertion site"
      ),
      tags$li(
        "stimulated by the double strand break the linear tagging cassette is inserted into the genome by HDR"
      ),
      tags$li(
        "the homology arm of the M1-oligo directs in frame fusion of the tag with the target ORF, leading to the expression of a tagged protein from the target locus"
      ),
      tags$li(
        "integration leads to destruction of the crRNA target site, thus preventing re-cleavage of the locus"
      )
    )
  )
)
panel.intro <- fluidPage(
  h3("See reference: F\u00fcller", em("et al."),
     br(),
     br()),
  fluidPage(
    h4("What is PCR targeting?", style = "color:Crimson; font-weight:bold"),
    tags$ul(
      tags$li(
        "PCR targeting is a one-step procedure for chromosomal gene tagging in mammals"
      ),
      tags$li(
        "PCR targeting enables the rapid creation of cell lines with targeted large chromosomal insertions, such as GFP"
      )
    )
  ),
  br(),
  panel.needed
)

# Online oligo design tool ------------------------------------------------


panel.target <- fluidPage(splitLayout(
  cellWidths = c(1000, 810),
  fluidPage(
    h3(
      strong("STEP 1: Enter your target sequence", style = "color:Crimson; font-weight:bold")
    ),
    br(),
    em(
      h4("Please provide your target sequence in the following way:"),
      h4(
        strong(
          "200 nt before and 200 nt after the target site (stop codon), 403 nt altogether"
        )
      ),
      style = "color:MediumBlue"
    ),
    
    h4(uiOutput("inp_target_text")),
    textAreaInput(
      "inp_target",
      label = NULL,
      width = 900,
      rows = 4,
      resize = "both",
      placeholder = paste0(paste(rep("N", 200), collapse = ""), "TAA", paste(rep("N", 200), collapse = ""))
      
    ),
    tags$head(tags$style("#inp_target{font-family: Roboto Mono}")),
    actionButton("example", label = "Click here for an example"),
    br(),
    br(),
    h5(textOutput("inputlength"), style = "color:red"),
    h5(textOutput("inputnuc"), style = "color:red"),
    h4(htmlOutput("inp_target_seq")),
    br()
  ),
  fluidPage(
    br(),
    br(),
    br(),
    br(),
    img(src = 'oligos_part.svg', width = 800, align = "right")
  )
),
fluidPage(fluidPage(
  div(
    style = "width:1750px;",
    fluidRow(verbatimTextOutput("targetsequence")),
    tags$head(
      tags$style("#targetsequence{background: white; font-family: Roboto Mono}")
    )
  )
)))
panel.searchspace <- fluidPage(splitLayout(
  cellWidths = c(990, 820),
  fluidPage(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css?family=Roboto+Mono"),
    tags$style(HTML(
      '#nucleotides {font-family: "Roboto Mono"}'
    )),
    tags$style(HTML(
      '#nucleotidesr {font-family: "Roboto Mono"}'
    )),
    h4(htmlOutput("csearchspacetext")),
    htmlOutput("nucleotides", style = "font-size: 20px"),
    htmlOutput("nucleotidesr", style = "font-size: 20px"),
    h5(textOutput("stopwarning"), style = "color:red")
  ),
  fluidPage(img(
    src = 'searchspace_c.png', width = 800, align = "right"
  ))
))
panel.genename <-
  fluidPage(
    h3(
      strong("STEP 2: Provide a name", style = "color:Crimson; font-weight:bold")
    ),
    br(),
    h4(
      em(
        "This input will be used to generate meaningful names for the oligos."
      ),
      style = "color:MediumBlue"
    ),
    br(),
    splitLayout(
      cellWidths = c(400, 200),
      h4("Please provide a name (e.g. gene name):"),
      textInput(
        "genename",
        label = NULL,
        width = 100,
        value = "GENE"
      )
    )
  )
panel.moduls <-
  splitLayout(
    cellWidths = c(1000, 810),
    fluidPage(
      h3(
        strong("STEP 3: Select Cas12a (Cpf1) variant(s)", style = "color:Crimson; font-weight:bold")
      ),
      br(),
      h4(
        em(
          "PCR targeting makes use of published Cas12a (Cpf1) plasmids available form Addgene."
        ),
        style = "color:MediumBlue"
      ),
      h4(
        em(
          "In case you do not have access to all of them, you can unselect the plasmid."
        ),
        style = "color:MediumBlue"
      ),
      br(),
      
      checkboxGroupInput(
        "inp_cpf",
        label = NULL,
        width = "100%",
        choiceNames = list(
          HTML(
            "LbCpf1(TTTV) - <a href = 'https://www.addgene.org/69988/' target = '_blank'> Addgene: pY016</a>, direct repeat: UAAUUUCUACUAAGUGUAGAU"
          ),
          HTML(
            "LbCpf1(TYCV) - <a href = 'https://www.addgene.org/89355/' target = '_blank'> Addgene: pY230</a>, direct repeat: UAAUUUCUACUAAGUGUAGAU"
          ),
          HTML(
            "AsCpf1(TATV) - <a href = 'https://www.addgene.org/89353/' target = '_blank'> Addgene: pY220</a>, also recognizes RATR and TTTV PAMs, direct repeat: UAAUUUCUACUCUUGUAGAU"
          ),
          HTML(
            "AsCpf1(TYCV) - <a href = 'https://www.addgene.org/89351/' target = '_blank'> Addgene: pY210</a>, also recognizes MCCC PAMs, direct repeat: UAAUUUCUACUCUUGUAGAU"
          ),
          "In case new Cpf1 variants with different PAM site specificities and different direct repeat sequences become available, you can enter these here:"
        ),
        choiceValues = c(
          "LbCpf1_TTTV",
          "LbCpf1_TYCV",
          "AsCpf1_TATV",
          "AsCpf1_TYCV",
          "Other"
        ),
        selected = c("LbCpf1_TTTV",
                     "LbCpf1_TYCV",
                     "AsCpf1_TATV",
                     "AsCpf1_TYCV")
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
            inputId = "inp_dr",
            label = NULL,
            placeholder = "DIRECT REPEAT",
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
      fluidRow(#column(width = 1, h5(uiOutput("pamdr"), style = "color:red")),
        column(width = 2, uiOutput("pamdrlink"))),
      h5(textOutput("drcontrol"), style = "color:red")
    ),
    img(src = 'crrna.svg', width = 800, align = "right")
  )
panel.region <-
  splitLayout(
    cellWidths = c(1000, 810),
    fluidPage(
      # tags$style(HTML(
      #   '#forwardoligo {font-family: "Roboto Mono"}'
      # )),
      #htmlOutput("forwardoligo"),
      #tags$style(HTML('#forwardoligo th {font-family: "Arial"}')),
      h3(
        strong("STEP 4: Define length of homology arms", style = "color:Crimson; font-weight:bold")
      ),
      br(),
      h4(
        em(
          "Please feel free to choose the desired lengths of the homology arms."
        ),
        style = "color:MediumBlue"
      ),
      h4(
        em("We recommend to use 90 nt for the M1 oligo and 55 nt for the M2 oligo."),
        style = "color:MediumBlue"
      ),
      h4(
        em(
          "The current setting limits the oligo length to 120 nt which allows affordable oligo synthesis."
        ),
        style = "color:MediumBlue"
      ),
      br(),
      splitLayout(
        cellWidths = c(300, 300),
        h4("5'-homology arm for the M1 oligo:", style = "color:green"),
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
        h4("3'-homology arm for the M2 oligos:", style = "color:CornflowerBlue "),
        numericInput(
          "threeha",
          label = NULL,
          value = 55,
          min = 1,
          max = 200,
          width = 100
        )
      ),
      # sliderInput("slider1", label = NULL, min = 0,
      #             max = 100, value = 55, width = '500px'),
      hr(),
      h3(
        strong("STEP 5: Compute oligo sequences", style = "color:Crimson; font-weight:bold")
      ),
      br(),
      h4(
        em(
          "The algorithm will search for possible PAM sites in the search space"
        ),
        style = "color:MediumBlue"
      ),
      h4(
        em(
          "and generate the M1 and M2 oligos as outlined in the image on the right."
        ),
        style = "color:MediumBlue"
      ),
      br(),
      actionButton("compute", label = "Find PAM sites and obtain your oligos", icon("search")),
      br()
    ),
    fluidPage(img(
      src = 'oligos.svg', width = 800, align = "right"
    ))
  )
panel.output <-
  fluidPage(
    h3("RESULTS: Retrieve sequences of M1 and M2 oligos", style = "font-weight:bold; color:Crimson; font-weight:bold"),
    htmlOutput("monetitle"),
    htmlOutput("monedescr"),
    tags$style(HTML('#cpff {font-family: "Roboto Mono"}')),
    tags$style(HTML('#cpff th {font-family: "Arial"}')),
    #tags$style(HTML('#cpff th:nth-child(2) {width: 1028px}')),
    tags$style(HTML('#cpff td:nth-child(2) {width: 1028px}')),
    #tags$style(HTML('#cpff th:first-child {width: 200px}')),
    tags$style(HTML('#cpff td:first-child {width: 200px}')),
    htmlOutput("cpff"),
    htmlOutput("mtwotitle"),
    htmlOutput("mtwodescr"),
    tags$style(HTML('#cpf {table-layout: fixed}')),
    tags$style(HTML('#cpf {font-family: "Roboto Mono"}')),
    tags$style(HTML('#cpf td {vertical-align: middle;}')),
    tags$style(HTML('#cpf th {vertical-align: middle;}')),
    tags$style(HTML('#cpf td {text-align: center;}')),
    tags$style(HTML('#cpf th {text-align: center;}')),
    tags$style(HTML('#cpf th {font-family: "Arial"}')),
    tags$style(HTML(
      '#cpf td:nth-child(5) {border-left: 2px solid grey}'
    )),
    tags$style(HTML(
      '#cpf th:nth-child(5) {border-left: 2px solid grey}'
    )),
    tags$style(HTML('#cpf th:first-child {width: 200px}')),
    tags$style(HTML('#cpf td:first-child {width: 200px}')),
    tags$style(HTML('#cpf td:nth-child(2) {width: 1028px}')),
    #tags$style(HTML('#cpf th:nth-child(2) {width: 1028px}')),
    tags$style(HTML('#cpf tr:first-child td {border-top: 0}')),
    htmlOutput("cpf"),
    htmlOutput("mtwocomment")
  )
panel.extended <-
  
  fluidPage(
    htmlOutput("mtwoext"),
    splitLayout(
      cellWidths = c(1000, 810),
      
      fluidPage(
        br(),
        br(),
        splitLayout(
          cellWidths = c(500, 150, 275),
          h4(
            checkboxInput(
              "extended",
              "Extended search space, number of nucleotides (max. 100):",
              width = 500
            )
          ),
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
    tags$style(HTML(
      '#nucleotidese {font-family: "Roboto Mono"}'
    )),
    tags$style(HTML(
      '#nucleotidesre {font-family: "Roboto Mono"}'
    )),
    h4(htmlOutput("esearchspacetext")),
    htmlOutput("nucleotidese", style = "font-size: 20px"),
    htmlOutput("nucleotidesre", style = "font-size: 20px"),
    h4(htmlOutput("esearchspaceoligos")),
    h5(textOutput("stopwarninge"), style = "color:red"),
    tags$style(HTML('#cpfe th {font-family: "Arial"}')),
    tags$style(HTML('#cpfe th:first-child {width: 200px}')),
    tags$style(HTML('#cpfe td:first-child {width: 200px}')),
    tags$style(HTML('#cpfe th:nth-child(2) {width: 1000px}')),
    tags$style(HTML('#cpfe td:nth-child(2) {width: 1000px}')),
    tags$style(HTML('#cpfe {font-family: "Roboto Mono"}')),
    tags$style(HTML('#cpfe td {vertical-align: middle;}')),
    tags$style(HTML('#cpfe th {vertical-align: middle;}')),
    tags$style(HTML('#cpfe td {text-align: center;}')),
    tags$style(HTML('#cpfe th {text-align: center;}')),
    tags$style(HTML(
      '#cpfe td:nth-child(5) {border-left: 2px solid grey}'
    )),
    tags$style(HTML(
      '#cpfe th:nth-child(5) {border-left: 2px solid grey}'
    )),
    htmlOutput("cpfe"),
    htmlOutput("mtwocommente"),
    br()
  )
panel.download <- fluidPage(
  downloadButton("downloadxlsx", "Download results (.xlsx)"),
  downloadButton("downloadcsv", "Download results (.csv)"),
  #downloadButton("report", "PDF report"),
  h3(htmlOutput("nextstep")),
  htmlOutput("offtarget")
)

# About -------------------------------------------------------------------


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
  a(
    "www.bioconductor.org/packages/release/bioc/html/Biostrings.html",
    href = "https://bioconductor.org/packages/release/bioc/html/Biostrings.html",
    target = "_blank"
  )
panel.about <- fluidPage(
  h3(strong("Publication about the tagging method")),
  br(),
  tags$ul(tags$li(h4(
    "F\u00fcller", em("et al.")
  ))),
  br(),
  br(),
  h3(strong(
    "References for the online oligo design tool"
  )),
  br(),
  tags$ul(tags$li(
    h4(
      "R Core Team (2018).",
      em(
        "R: A language and environment for statistical computing. R Foundation for Statistical Computing"
      ),
      "Vienna, Austria. ",
      urlr
    )
  ),
  tags$li(
    h4(
      "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie, Jonathan McPherson (2018).",
      em("shiny: Web Application Framework for R."),
      "R package version 1.1.0. ",
      urlshiny
    )
  ),
  tags$li(
    h4(
      "Dean Attali (2018).",
      em(
        "shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds."
      ),
      "R package version 1.0. ",
      urlshinyjs
    )
  ),
  tags$li(
    h4(
      "Pages H, Aboyoun P, Gentleman R, DebRoy S (2017).",
      em("Biostrings: Efficient manipulation of biological strings."),
      "R package version 2.46.0. ",
      urlbiostrings
    )
  )),
  br(),
  br(),
  h3(strong("Disclaimer")),
  br(),
  h4(
    "The information contained in this website correct to the best of our knowledge."
  ),
  h4(
    "Under no circumstance shall the authors be liable for any potential mistakes, claim, damage or loss arising from the use of the application ",
    em("'Online oligo design tool for PCR-targeting in mammalian cells'.")
  ),
  h4(
    "The use of the tool and the reliance on any information on the site is solely at the user's own risk."
  ),
  h4("Only for non-commercial, academic or research purposes.")
)

# Impressum ---------------------------------------------------------------


panel.impressum <- fluidPage(br(),
                             br(),
                             br(),
                             fluidPage(img(
                               src = 'impressum.png', width = 800, align = "left"
                             )),
                             br(),
                             br(),
                             fluidPage(h3(br(), mailknop, br(), br(), knoplab)))


# Template cassettes ------------------------------------------------------


templatedata <- readRDS("./data/templatedata.rds")
for (i in (2:nrow(templatedata))) {
  templatedata[i, 5] <-
    as.character(a(
      templatedata[i, 5],
      href = paste0("http://doi.org/", gsub("doi: ", "", templatedata[i, 5])),
      target = "_blank"
    ))
}
panel.templates <- fluidPage(
  h3(strong("List of template cassettes for PCR tagging")),
  br(),
  br(),
  downloadButton("downloadseq", "Download sequence files (.zip)"),
  br(),
  br(),
  br(),
  br(),
  tags$style(
    HTML('#template td {vertical-align: middle;
         text-align: center;}')
    ),
  tags$style(
    HTML('#template th {vertical-align: middle;
         text-align: center;
         border-bottom: 0;}')
    ),
  tags$style(
    HTML(
      '#template tr:first-child td {font-weight: bold;
      border-bottom: 2px solid grey;
      border-top: 0;}'
    )
    ),
  htmlOutput("template")
    )

# Cassette PCR ------------------------------------------------------------


pcrmixhifi <- data.frame(
  c(
    "10x PCR buffer",
    "dNTPs (10 mM)",
    "M1 oligo (10 \u00b5M)",
    "M2 oligo (10 \u00b5M)",
    "Template (~ 100 ng)",
    "Betaine (5 M)",
    "MgCl<sub>2</sub> (50 mM)",
    "HiFi Polymerase (1 U/\u00b5l)",
    "H<sub>2</sub>O"
  ),
  c("5", "5", "2.5", "2.5", "1", "5", "1", "1", "Ad 50"),
  stringsAsFactors = F
)
names(pcrmixhifi) <- c("", "\u00b5L")
pcrmixphu <- data.frame(
  c(
    "10x PCR buffer",
    "dNTPs (10 mM)",
    "M1 oligo (10 \u00b5M)",
    "M2 oligo (10 \u00b5M)",
    "Template (~ 100 ng)",
    "Betaine (5 M)",
    "MgCl<sub>2</sub> (50 mM)",
    "Phusion Polymerase (ThermoFisher)",
    "H<sub>2</sub>O"
  ),
  c("5", "5", "2.5", "2.5", "1", "5", "1", "0.5", "Ad 50"),
  stringsAsFactors = F
)
names(pcrmixphu) <- c("", "\u00b5L")
#   actionButton("runpcr", label = "Run PCR"))
pcrmixvel <- data.frame(
  c(
    "10x PCR buffer",
    "dNTPs (10 mM)",
    "M1 oligo (10 \u00b5M)",
    "M2 oligo (10 \u00b5M)",
    "Template (~ 100 ng)",
    "Betaine (5 M)",
    "MgCl<sub>2</sub> (50 mM)",
    "Velocity Polymerase (Bioline)",
    "H<sub>2</sub>O"
  ),
  c("5", "5", "2.5", "2.5", "1", "5", "1", "0.25", "Ad 50"),
  stringsAsFactors = F
)
names(pcrmixvel) <- c("", "\u00b5L")
pcr <- data.frame(
  c("1",
    "2",
    "3",
    "4",
    "5",
    "6"),
  c("95",
    "95",
    "64",
    "72",
    "72",
    "4"),
  c("3:00",
    "0:20",
    "0:30",
    "2:20",
    "5:00",
    "hold"),
  c("",
    "",
    "",
    "Go to 2. step x 29",
    "",
    ""),
  stringsAsFactors = F
)
names(pcr) <- c("Step", "\u00b0C", "min", "")
panel.pcr <-  fluidPage(
  h3(
    strong(
      "Example protocol for the amplification of the gene tagging PCR cassette"
    )
  ),
  br(),
  br(),
  h4(
    strong("Template:"),
    "pMaCTag-Z21 ",
    br(),
    br(),
    strong("Primer:"),
    "M1 and M2 oligos, designed with ",
    actionLink("linktotool", "our oligo design tool"),
    br(),
    br(),
    strong("Expected size:"),
    "~ 3.4 kbp"
  ),
  br(),
  br(),
  tags$style(HTML(
    '#pcrmixhifitable td:nth-child(1) {width: 300px}'
  )),
  tags$style(HTML(
    '#pcrmixhifitable th:nth-child(1) {width: 300px}'
  )),
  tags$style(
    HTML(
      '#pcrmixhifitable td {vertical-align: middle;
      text-align: center; font-size: 16px;}'
    )
    ),
  tags$style(
    HTML(
      '#pcrmixhifitable th {vertical-align: middle;
      text-align: center; font-size: 16px;}'
    )
    ),
  tags$style(HTML(
    '#pcrmixhifitable tr:nth-child(9) td {border-top:0;}'
  )),
  tags$style(HTML(
    '#pcrmixhifitable tr:nth-child(10) td {border-top:0;}'
  )),
  tags$style(HTML(
    '#pcrmixphutable td:nth-child(1) {width: 300px}'
  )),
  tags$style(HTML(
    '#pcrmixphutable th:nth-child(1) {width: 300px}'
  )),
  tags$style(
    HTML(
      '#pcrmixphutable td {vertical-align: middle;
      text-align: center; font-size: 16px;}'
    )
    ),
  tags$style(
    HTML(
      '#pcrmixphutable th {vertical-align: middle;
      text-align: center; font-size: 16px;}'
    )
    ),
  tags$style(HTML(
    '#pcrmixphutable tr:nth-child(9) td {border-top:0;}'
  )),
  tags$style(HTML(
    '#pcrmixphutable tr:nth-child(10) td {border-top:0;}'
  )),
  tags$style(HTML(
    '#pcrmixveltable td:nth-child(1) {width: 300px}'
  )),
  tags$style(HTML(
    '#pcrmixveltable th:nth-child(1) {width: 300px}'
  )),
  tags$style(
    HTML(
      '#pcrmixveltable td {vertical-align: middle;
      text-align: center; font-size: 16px;}'
    )
    ),
  tags$style(
    HTML(
      '#pcrmixveltable th {vertical-align: middle;
      text-align: center; font-size: 16px;}'
    )
    ),
  tags$style(HTML(
    '#pcrmixveltable tr:nth-child(9) td {border-top:0;}'
  )),
  tags$style(HTML(
    '#pcrmixveltable tr:nth-child(10) td {border-top:0;}'
  )),
  tags$style(
    HTML(
      '#pcrtable td {vertical-align: middle;
      text-align: center; font-size: 16px;}'
    )
    ),
  tags$style(
    HTML(
      '#pcrtable th {vertical-align: middle;
      text-align: center; font-size: 16px;}'
    )
    ),
  tags$style(HTML(
    '#pcrtable tr td:nth-child(4) {border-top: 0;}'
  )),
  tags$style(HTML(
    '#pcrtable th:nth-child(4) {border-bottom: 0;}'
  )),
  
  splitLayout(
    cellWidths = c(400, 400, 400, 400),
    h4(em("PCR mixture - HiFi polymerase")),
    h4(em("PCR mixture - Phusion polymerase")),
    h4(em("PCR mixture - Velocity polymerase")),
    h4(em("PCR standard program"))
  ),
  splitLayout(
    cellWidths = c(400, 400, 400, 400),
    htmlOutput("pcrmixhifitable"),
    htmlOutput("pcrmixphutable"),
    htmlOutput("pcrmixveltable"),
    htmlOutput("pcrtable")
  ),
  br(),
  br(),
  fluidPage(
    h4(
      em(
        "*HiFi-Polymerase is our self-purified DNA polymerase",
        br(),
        br(),
        "10x HiFi buffer - to be used with any polymerase"
      )
    ),
    tags$ul(
      tags$li("200 mM Tris-HCl, pH 8.8"),
      tags$li("100 mM (NH\u2084)\u2082SO\u2084"),
      tags$li("500 mM KCl"),
      tags$li("1% (v/v) Triton X-100"),
      tags$li("1 mg/ml BSA"),
      tags$li("20 mM MgCl\u2082")
    ),
    br(),
    h4(
      em(
        "We note that the Phusion polymerase using the manufacturer supplied buffer does not work for cassette amplification with M1 and M2 oligos, whereas for Velocity polymerase using the buffer provided by the manufacturer, good amounts of the product are obtained. We suggest pipetting on ice and pre-heating the PCR machine or hot start. We found that all polymerases work well using the buffer conditions and amplification above.",
        style = "color:MediumBlue "
      )
    ),
    br()
    
  )
  )