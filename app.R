library(shiny)
library(shinyjs)
library(writexl)
library(Biostrings)
# Single target sequence input UI
# Input of target sequence
panel.target <- fluidPage(
  # h4(em("Single DNA sequence input")),
  # br(),

splitLayout(
  cellWidths = c(1000, 600),
  #cellArgs = list(style = "padding: 2px"),
  fluidPage(
    h4(
      "Please insert your target sequence the following way:"),
    br(),
    h4(strong("200 nts before and 200 nts after the insertion site (stop codon), 403 nts altogether")
    ),
    
    h4(uiOutput("inp_target_text")),
    br(),
    textAreaInput(
          "inp_target",
          label = NULL,
          width = 800,
          rows = 5,
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
      h5(htmlOutput("inp_target_seq")),
      div(
        style = "width:800px;",
        fluidRow(verbatimTextOutput("targetsequence")),
        tags$head(
          tags$style(
            "#targetsequence{background: ghostwhite; font-family: Roboto Mono}"
          )
      )
      )
),
      fluidPage(
        img(src='inputoutput.svg', width = 300, align = "center")
        )
),
br()

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
panel.apply <-
  fluidPage(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css?family=Roboto+Mono"),
    #h5(em("Extended search space*"), style = "color:peru"),
    tags$style(HTML(
      '#nucleotides {font-family: "Roboto Mono"}'
    )),
    tags$style(HTML(
      '#nucleotidesr {font-family: "Roboto Mono"}'
    )),
    htmlOutput("regiontext"),
    br(),
    htmlOutput("nucleotides"),
    htmlOutput("nucleotidesr")
    #textOutput("fasta"),
    #actionButton("apply", label = "Show sequence")
  )
# Select CASTLING moduls
panel.moduls <- 
  splitLayout(cellWidths = c(850, 800),
  fluidPage(
    h4("Which gene would you like to tag?"),
    fluidPage(
      column(
        width = 6,
        h5(em("This input will be used to name the oligos."), style = "color:grey")
      ),
      column(
        width = 3,
        textInput(
          "genename",
          label = NULL,
          width = 100,
          value = "GENE"
        )
      )),
    fluidRow(
  # em("Select CASTLING moduls:"),
  # radioButtons(
  #   "inp_moduls",
  #   label = NULL,
  #   choices = c("All from publication", "Subset from publication")
  # ),
  # em("Select target species:"),
  # radioButtons(
  #   "inp_species",
  #   label = NULL,
  #   choices = c("human", "mouse")
  # ),
  # Select Cpf1 variant
  column(
    width = 3,
    h4("Select Cpf1 variant(s) and PAM-site(s):"),
    checkboxGroupInput(
      "inp_cpf",
      label = NULL,
      width = "100%",
      choiceNames = list(
        HTML("LbCpf1 - TTTV - <a href = 'https://www.addgene.org/69988/' target = '_blank'> Addgene: pY016</a>"),
        HTML("LbCpf1 - TYCV - <a href = 'https://www.addgene.org/89355/' target = '_blank'> Addgene: pY230</a>"),
        HTML("AsCpf1 - TATV - <a href = 'https://www.addgene.org/89353/' target = '_blank'> Addgene: pY220</a>"),
        HTML("AsCpf1 - MCCC - <a href = 'https://www.addgene.org/89353/' target = '_blank'> Addgene: pY220</a>"),
        HTML("AsCpf1 - RATR - <a href = 'https://www.addgene.org/89351/' target = '_blank'> Addgene: pY210</a>"),
        "Other, please specify PAM and gRNA-handle:"
      ),
      choiceValues = c(
        "LbCpf1 - TTTV",
        "LbCpf1 - TYCV",
        "AsCpf1 - TATV",
        "AsCpf1 - MCCC",
        "AsCpf1 - RATR",
        "Other, please specify PAM and gRNA-handle:"
      ),
      selected = c(
        "LbCpf1 - TTTV",
        "LbCpf1 - TYCV",
        "AsCpf1 - TATV",
        "AsCpf1 - MCCC",
        "AsCpf1 - RATR"
      )
    )
    #,    fluidRow(column(12, verbatimTextOutput("value")))
    
    #column(3, verbatimTextOutput("cpfchosen"))
  )
  # column(
  #   width = 3,
  #   fluidPage(
  #     #tags$style(HTML('#endo {font-family: "Roboto Mono"}')),
  #     #tags$style(HTML('#endo th {font-family: "Arial"}')),
  #     #tableOutput("endo"),
  #     h4("Plasmids for co-transfection:"),
  #     uiOutput("lbwlink"),
  #     #uiOutput("lblink"),
  #     uiOutput("aslink2"),
  #     uiOutput("aslink"),
  #     br(),
  #     uiOutput("handlelink"),
  #     br(),
  #     uiOutput("iupaclink")
  #     
  #   )
  # )
),
bootstrapPage(
  div(
    style = "display:inline-block",
    textInput(
      inputId = "inp_pam",
      label = NULL,
      placeholder = "PAM",
      width = "50%"
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
  )
),
fluidRow(#column(width = 1, h5(uiOutput("pamhandle"), style = "color:red")),
  column(width = 2, uiOutput("pamhandlelink")))),
fluidPage(
  img(src='oligos.svg', width = 600, align = "center")
)
)
#h5(textOutput("handlecontrol"), style = "color:red"))
panel.output <-            fluidPage(
  # tags$style(HTML(
  #   '#forwardoligo {font-family: "Roboto Mono"}'
  # )),
  #htmlOutput("forwardoligo"),
  #tags$style(HTML('#forwardoligo th {font-family: "Arial"}')),
  actionButton("compute", label = "Find PAM-sites and obtain your oligos"),
  br(),
  br(),
  h4(textOutput("resultstitle"), style = "font-weight:bold"),
  br(),
  #h5(textOutput("reversetitle"), style = "font-weight:bold"),
  tags$style(HTML('#cpf {font-family: "Roboto Mono"}')),
  tags$style(HTML('#cpf td {vertical-align: middle}')),
  tags$style(HTML('#cpf th {font-family: "Arial"}')),
  #tags$style(HTML('#cpf tr:last-child td {border-top: 0}')),
  tags$style(HTML('#cpf td:nth-child(6) {border-left: 2px solid grey}')),
  tags$style(HTML('#cpf th:nth-child(6) {border-left: 2px solid grey}')),
  tags$style(HTML('#cpf tr:nth-child(2) td {border-top: 0}')),
  #tags$style(HTML('#cpf tr:nth-child(1) td {border-top: 2px solid lightgrey}')),
  tags$style(HTML('#cpf tr:nth-child(3) {border-top: 2px solid lightgrey}')),
  htmlOutput("cpf")
)
panel.extended <- 
  fluidPage(
    checkboxInput("extended", "Extended search space - *PAM-sites found here may lead to small deletions after the tag
", width = 1000),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css?family=Roboto+Mono"),
    #h5(em("Extended search space*"), style = "color:peru"),
    tags$style(HTML(
      '#nucleotidese {font-family: "Roboto Mono"}'
    )),
    tags$style(HTML(
      '#nucleotidesre {font-family: "Roboto Mono"}'
    )),
    htmlOutput("regiontexte"),
    br(),
    htmlOutput("nucleotidese"),
    htmlOutput("nucleotidesre"),
    h5(textOutput("stopwarning"), style = "color:red"),
    br(),
    tags$style(HTML('#cpfe {font-family: "Roboto Mono"}')),
    tags$style(HTML('#cpfe td {vertical-align: middle}')),
    tags$style(HTML('#cpfe th {font-family: "Arial"}')),
    tags$style(HTML('#cpfe tr:last-child td {border-top: 0}')),
    tags$style(HTML('#cpfe td:nth-child(6) {border-left: 2px solid grey}')),
    tags$style(HTML('#cpfe th:nth-child(6) {border-left: 2px solid grey}')),
    htmlOutput("cpfe")
  )
panel.download <- fluidPage(
  downloadButton("downloadxlsx", "Download results (.xlsx)"),
  
  downloadButton("downloadcsv", "Download results (.csv)"),
  #br(),
  #br(),
  #downloadButton("report", "PDF report"),
  br(),
  br()
)
panel.pcrr <- fluidPage(
  h4("Choose a template and a reverse oligo for the PCR:"),
  br(),
  actionButton("runpcr", label = "Run PCR"))


# FASTA UI

# panel.ftarget <- fluidPage(
# 
#   h4(em(
#     "Upload of multiple DNA sequences in FASTA format"
#   )),
#   
#   fluidPage(
#     h4(
#       "Please provide each target sequence the following way: 200 nts before and 200 nts after the insertion site/stop codon (403 nts per target)."
#     ),
#     br(),
#     tags$div(title = "FASTA format",
#              fileInput(
#                "inp_fasta",
#                label = NULL,
#                width = 600,
#                accept =
#              )),
#     h5(textOutput("fastasequence"), style = "color:red"),
#     h5(textOutput("finputlength"), style = "color:red"),
#     h5(textOutput("finputnuc"), style = "color:red"),
#     br(),
#     # fluidRow(column(
#     #   width = 4,
#     #   h4("Gene name:"),
#     #   h5(em("Will be used to name the oligos."), style = "color:grey")
#     # ),
#     # column(
#     #   width = 1,
#     #   textInput(
#     #     "fgenename",
#     #     label = NULL,
#     #     width = 100,
#     #     value = "GENE"
#     #   )
#     # )),
#     fluidRow(
#       column(
#         width = 5,
#         h4("Search space for PAM sites before the insertion site:"),
#         h5(
#           em(
#             "Number of nucleotides upstream of the insertion sites to search for PAM sites on the direct strand."
#           ),
#           style = "color:grey"
#         )
#       ),
#       column(
#         width = 1,
#         numericInput(
#           "fpamregionu",
#           label = NULL,
#           value = 17,
#           min = 3,
#           max = 20,
#           width = 100
#         )
#       ),
#       tags$head(tags$style(".container-fluid {width: 2000px;}"))
#     ),
#     fluidRow(column(
#       width = 5,
#       h4("Search space for PAM sites after the insertion site:"),
#       h5(
#         em(
#           "Number of nucleotides downstream of the insertion site (stop codon) to look for PAM sites on the reverse strand."
#         ),
#         style = "color:grey"
#       )
#     ),
#     column(
#       width = 1,
#       numericInput(
#         "fpamregiond",
#         label = NULL,
#         value = 17,
#         min = 3,
#         max = 50,
#         width = 100
#       )
#     ))
#   )
#   )
# panel.fmoduls <- fluidPage(fluidRow(
#   column(
#     width = 3,
#     h4("Select Cpf1 variant(s) and PAM-site(s):"),
#     checkboxGroupInput(
#       "finp_cpf",
#       label = NULL,
#       width = "100%",
#       choiceNames = list(
#         HTML("LbCpf1 - TTTV - <a href = 'https://www.addgene.org/69988/' target = '_blank'> Addgene: pY016</a>"),
#         HTML("LbCpf1 - TYCV - <a href = 'https://www.addgene.org/89355/' target = '_blank'> Addgene: pY230</a>"),
#         HTML("AsCpf1 - TATV - <a href = 'https://www.addgene.org/89353/' target = '_blank'> Addgene: pY220</a>"),
#         HTML("AsCpf1 - MCCC - <a href = 'https://www.addgene.org/89353/' target = '_blank'> Addgene: pY220</a>"),
#         HTML("AsCpf1 - RATR - <a href = 'https://www.addgene.org/89351/' target = '_blank'> Addgene: pY210</a>"),
#         "Other, please specify PAM and gRNA-handle:"
#       ),
#       choiceValues = c(
#         "LbCpf1 - TTTV",
#         "LbCpf1 - TYCV",
#         "AsCpf1 - TATV",
#         "AsCpf1 - MCCC",
#         "AsCpf1 - RATR",
#         "Other, please specify PAM and gRNA-handle:"
#       ),
#       selected = c(
#         "LbCpf1 - TTTV",
#         "LbCpf1 - TYCV",
#         "AsCpf1 - TATV",
#         "AsCpf1 - MCCC",
#         "AsCpf1 - RATR"
#       )
#     )  )
#   # column(
#   #   width = 3,
#   #   fluidPage(
#   #     h4("Plasmids for co-transfection:"),
#   #     uiOutput("flbwlink"),
#   #     uiOutput("flblink"),
#   #     uiOutput("faslink2"),
#   #     uiOutput("faslink"),
#   #     br(),
#   #     uiOutput("fhandlelink"),
#   #     br(),
#   #     uiOutput("fiupaclink")
#   #     
#   #   )
#   # )
# ),
# bootstrapPage(
#   div(
#     style = "display:inline-block",
#     textInput(
#       inputId = "inp_fpam",
#       label = NULL,
#       placeholder = "PAM",
#       width = "50%"
#     )
#   ),
#   div(
#     style = "display:inline-block",
#     textInput(
#       inputId = "inp_fhandle",
#       label = NULL,
#       placeholder = "HANDLE",
#       width = "100%"
#     )
#   )
# ),
# fluidRow(column(width = 2, uiOutput("fpamhandlelink"))))
# panel.foutput <-            fluidPage(
#   tags$style(HTML(
#     '#fforwardoligo {font-family: "Roboto Mono"}'
#   )),
#   htmlOutput("fforwardoligo"),
#   tags$style(HTML('#fforwardoligo th {font-family: "Arial"}')),
#   htmlOutput("freversetitle"),
#   br(),
#   tags$style(HTML('#fcpf {font-family: "Roboto Mono"}')),
#   tags$style(HTML('#fcpf th {font-family: "Arial"}')),
#   htmlOutput("fcpf"),
#   br()
# )
# panel.fdownload <- fluidPage(
#   actionButton("fcompute", label = "Get oligos"),
#   br(),
#   br(),
#   h4(textOutput("fdownload")),
#   br(),
#   downloadButton("fdownloadxlsx", "Download results (.xlsx)"),
#   
#   downloadButton("fdownloadcsv", "Download results (.csv)"),
#   #br(),
#   #br(),
#   #downloadButton("report", "PDF report"),
#   br(),
#   br()
# )
pcrmix <- data.frame(
  c("10x HiFi buffer",
              "dNTPs (10 mM)",
              "M1 (10 μM)",
              "M2 (10 μM)",
              "Template (~ 100 ng)",
    "Betaine (5M)",
    "MgCl2 (50 mM)",
    "HiFi-Polymerase",
    "H2O"),
  c("5", "5", "2.5", "2.5", "1", "5", "1", "1", "Ad 50"),

  stringsAsFactors = F
)
names(pcrmix) <- c("", "μL")
pcr <- data.frame(
  c("1",
    "2",
    "3",
    "4",
    "5",
    "6"),
  c("95",
    "95",
    "60",
    "72",
    "72",
    "4"),
  c("2:00",
    "0:20",
    "0:30",
    "1:45",
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
names(pcr) <- c("Step", "°C", "min", "")
# panel.pcr <-   fluidPage(
#   fluidRow(h5(em("Template:"), "pMaM519 / pMaM518", br(), br(), em("Primer:"), "M1, M2", br(), br(), em("Expected size:"), "~ 1.3kbp / ~ 2.2kb:")),
#   br(),
#   column(width = 2, h5(em("HiFi-Polymerase standard mix")),
#   tableOutput("pcrmixtable")),
#   column(width = 2, h5(em("HiFi-Polymerase standard program")),
#                          tableOutput("pcrtable")))
# Define UI ----
ui <-
  fluidPage(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css?family=Roboto+Mono"),
    #tags$style(HTML('* {font-family: "Arial"')),
    useShinyjs(),
    tags$head(tags$script(
      HTML(
        "
              Shiny.addCustomMessageHandler('jsCode',
              function(message) {
              console.log(message)
              eval(message.code);
              }
              );
              "
      )
    )),
    title = "Mammalian PCR-targeting",
    navbarPage(
      title = "Mammalian PCR-targeting",
      tabPanel(
        title = "Primer design tool for single DNA sequence",
        
        fluidPage(

          titlePanel(
            strong("Primer Design Tool for PCR-based CRISPR-Cpf1-assisted C-terminal Tagging in Mammalian Cells [TEST]")
          ),
          h4(
            "This webpage provides an oligonucleotide design tool...etc Publication:"
          ),
          br(),
          panel.target,
          br(),
          panel.moduls,
          panel.apply,
          br(),
          hr(),
          panel.output,
          panel.extended,
          panel.download,
          hr(),
          panel.pcrr,
          br()
          )

        
        
        ),
      
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
      
      tabPanel(title = "About",
               
               h5(em(
                 "Under construction"
               ), style = "color:red"),
               h4("Disclaimer"))
      )

      )
# Define server logic ----
server <- function(input, output, session)
{
  # General server elements
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
  output$pcrmixtable <- renderTable({pcrmix})
  output$pcrtable <- renderTable({pcr})
  urlhandle <-
      a("Direct repeat sequences of Cpf1 orthologs",
        href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4638220/",
        target = "_blank")
    
    output$handlelink <- renderUI({
      tagList("", urlhandle)
    })
    output$fhandlelink <- renderUI({
      tagList("", urlhandle)
    })
    urliupac <-
      a("Please use IUPAC Codes for Nucleotides.",
        href = "https://www.bioinformatics.org/sms/iupac.html",
        target = "_blank")
    # output$iupaclink <- renderUI({
    #   tagList("", urliupac)
    # })
    # urlas <-
    #   a("pcDNA3.1-hAsCpf1(TYCV) (pY210) - This variant also recognizes MCCC PAMs (M = A or C)",
    #     href = "https://www.addgene.org/89351/",
    #     target = "_blank")
    # output$aslink <- renderUI({
    #   tagList("", urlas)
    # })
    # output$faslink <- renderUI({
    #   tagList("", urlas)
    # })
    # urlas2 <-
    #   a("pcDNA3.1-hAsCpf1(TATV) (pY220) - This variant also recognizes TTTV and RATR PAMs (R = A or G).",
    #     href = "https://www.addgene.org/89353/",
    #     target = "_blank")
    # output$aslink2 <- renderUI({
    #   tagList("", urlas2)
    # })
    # output$faslink2 <- renderUI({
    #   tagList("", urlas2)
    # })
    # urllb <-
    #   a("pcDNA3.1-hLbCpf1(TYCV) (pY230)",
    #     href = "https://www.addgene.org/89355/",
    #     target = "_blank")
    # output$lblink <- renderUI({
    #   tagList("", urllb)
    # })
    # output$flblink <- renderUI({
    #   tagList("", urllb)
    # })
    # urllbw <-
    #   a("pcDNA3.1-hLbCpf1(TTTV) (pY016)",
    #     href = "https://www.addgene.org/69988/",
    #     target = "_blank")
    # output$lbwlink <- renderUI({
    #   tagList("", urllbw)
    # })
    # output$flbwlink <- renderUI({
    #   tagList("", urllbw)
    # })
    
    # find possible PAM-sequences around the stop codon depending on the Cpf1 chosen
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
    # Single DNA sequence input
    disableActionButton("compute", session)
    disableActionButton("runpcr", session)
    hideElement("extended")
    #disableActionButton("apply", session)
    disable("inp_pam")
    disable("inp_handle")
    #output$value <- renderPrint({ input$inp_cpf })
    observe({
      if ("Other, please specify PAM and gRNA-handle:" %in% input$inp_cpf) {
        enable("inp_pam")
        enable("inp_handle")
        
      } else {
        disable("inp_pam")
        disable("inp_handle")
      }
    })
    disable("downloadcsv")
    disable("downloadxlsx")
    
    # Target sequence
    
    # output$inp_target_text <- reactive({ paste0('Only ', max_char-nchar(input$inp_target), ' characters remaining.' ) })
    observeEvent(input$example, {
      updateTextInput(
        session,
        "inp_target",
        value = paste0(
          paste(rep("A", 103), collapse = ""),
          "ATCGGAAATGGAGATGGCCCATCTGTATTCACTTTGCGATGCCGCCCATGCCCAGACAGAAGTTGCAAAGAAATACGGATTAAAACCACCAACATTATAAAACAGGGGGAAAGCAGACTGACCCTCTTTTTAAAAGTTTACCCCCTCTTCAACTGAACCCTAAAGACACTGTCATGAACTGTGTTGAATGGTGGAAATCA",
          paste(rep("A", 100), collapse = "")
        )
      )
      updateTextInput(session, "genename", value = paste("TOMM70"))
      #updateTextInput(session, "beforestop", value = 97)
      #updateTextInput(session, "afterstop", value = 100)
      updateTextInput(session, "pamregionu", value = 17)
      updateTextInput(session, "pamregiond", value = 17)
    })
    targetseq <- eventReactive(input$inp_target, {
      targetseq.tmp <- toupper(gsub("\\s", "", input$inp_target))
      return(targetseq.tmp)
    })
    output$inp_target_seq <- renderText({
      if (nchar(targetseq()) != 0) {
        paste0("The recognized sequence consists of ", nchar(targetseq()), " nts:")
      }
    })
    output$targetsequence <- renderText({
      targetseq()
    })
    observeEvent(input$inp_target, {
      output$stopwarning <- renderText({
        ""
      })
      if ((nchar(targetseq()) != 200 + 200 + 3) &
          (nchar(targetseq()) > 0) &
          (all(strsplit(targetseq(), "")[[1]] %in% DNA_ALPHABET))) {
        disableActionButton("compute", session)
        #disableActionButton("apply", session)
        output$inputlength <-
          renderText({
            "Please provide a sequence of proper length."
          })
        output$inputnuc <-
          renderText({
            ""
          })
        output$nucleotides <- renderText({
          ""
        })
        output$nucleotidesr <- renderText({
          ""
        })
      }
      else if ((any(strsplit(targetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
               (nchar(targetseq()) != 200 + 200 + 3)) {
        disableActionButton("compute", session)
        #disableActionButton("apply", session)
        # output$inp_target_text <- renderUI({
        #   em(
        #     "Please insert your target sequence,",
        #     200,
        #     " nucleotides before and " ,
        #     200,
        #     " after the stop codon:",
        #     200 + 3 + 200,
        #     " nts."
        #   )
        # })
        output$inputlength <-
          renderText({
            "Please provide a sequence of proper length."
          })
        output$inputnuc <-
          renderText({
            "Please provide a DNA sequence."
          })
        output$nucleotides <- renderText({
          ""
        })
        output$nucleotidesr <- renderText({
          ""
        })
      }
      else if ((any(strsplit(targetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
               (nchar(targetseq()) == 200 + 200 + 3)) {
        disableActionButton("compute", session)
        #disableActionButton("apply", session)
        output$inputlength <-
          renderText({
            ""
          })
        output$inputnuc <-
          renderText({
            "Please provide a DNA sequence."
          })
        output$nucleotides <- renderText({
          ""
        })
        output$nucleotidesr <- renderText({
          ""
        })
      }
      else if ((all(strsplit(targetseq(), "")[[1]] %in% DNA_ALPHABET)) &
               (nchar(targetseq()) == 200 + 200 + 3)) {
        enableActionButton("compute", session)
        #enableActionButton("apply", session)
        
        output$inp_target_text <- renderUI({
          ""
        })
        output$inputlength <- renderText({
          ""
        })
        output$inputnuc <- renderText({
          ""
        })
        ntspam <- eventReactive({
          input$compute
          17
          17
        }, {
          subsetnts <- aroundpam()(targetseq())
          return(subsetnts)
        })
        ntsstop <- eventReactive({
          input$compute
          17
          17
        }, {
          subsetnts <- aroundstop()(targetseq())
          return(subsetnts)
        })
        forwardoligo <- eventReactive(input$compute, {
          cone <-
            as.character(xscat(
              DNAString(targetseq())[(200 - 89):200],
              DNAString("TCAGGTGGAGGAGGTAGTG")
            ))
          return(cone)
        })
        output$regiontext <- renderText({
          paste(h4(em("Search space for PAM-sites around the insertion site (stop codon):")),
        h5(em(
          "17 nucleotides upstream on the direct strand and 17 nucleotides downstream on the reverse strand"
        ), style = "color:coral"), sep = '\n')
        })
        regiond <- renderText({
          # if (input$apply == 0)
          #   return("")
          # isolate({
            paste(
              as.character(ntsstop()[1:10]),
              '<span style = "color:coral">',
              as.character(ntsstop()[11:(17 + 10)]),
              '</span>',
              strong(substr(targetseq(), 201, 203)),
              substr(targetseq(), 204, 230),
              " - direct strand",
              sep = ""
            )
        })
        output$nucleotides <- regiond
        regionr <- renderText({
          # if (input$apply == 0)
          #   return("")
          # isolate({
            paste(
              as.character(complement(ntsstop()[1:10])),
              as.character(complement(ntsstop()[11:(17 + 10)])),
              strong(as.character(complement(
                ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]
              ))),
              '<span style = "color:coral">',
              as.character(complement(ntsstop()[(17 + 3 + 10 + 1):(17 + 3 + 10 + 17)])),
              '</span>',
              as.character(complement(DNAString(
                targetseq()
              )[(203 + 17 + 1):230])),
              " - reverse strand",
              sep = ""
            )
        })
        output$nucleotidesr <- regionr

        #observeEvent(input$apply, {
          
          if ((as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TAA") |
              (as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TAG") |
              (as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TGA")) {
            output$stopwarning <- renderText({
              ""
            })
            
          } else {
            output$stopwarning <-
              renderText({
                "Warning! The insertion site (marked in bold) is not a stop codon."
              })
          }
        #})
        tttvd <- NULL
        tttvr <- NULL
        tycvd <- NULL
        tycvr <- NULL
        mcccd <- NULL
        mcccr <- NULL
        ratrd <- NULL
        ratrr <- NULL
        tatvd <- NULL
        tatvr <- NULL
        otherpamsd <- NULL
        otherpamsr <- NULL
        tttvdl <- NULL
        tttvrl <- NULL
        tycvdl <- NULL
        tycvrl <- NULL
        mcccdl <- NULL
        mcccrl <- NULL
        ratrdl <- NULL
        ratrrl <- NULL
        tatvdl <- NULL
        tatvrl <- NULL
        otherpamsdl <- NULL
        otherpamsrl <- NULL
        rvc <- reactiveValues(data = NULL)
        rv <- reactiveValues(data = NULL)
        rve <- reactiveValues(data = NULL)
        rvp <- reactiveValues(data = NULL)
        rvo <- reactiveValues(data = NULL)
        rvg <- reactiveValues(data = NULL)
        rvg$data <- input$genename
        observe({
          if ("Other, please specify PAM and gRNA-handle:" %in% input$inp_cpf) {
            observe({
              if ((any(strsplit(input$inp_pam, "")[[1]] %ni% DNA_ALPHABET)) |
                  (any(strsplit(input$inp_handle, "")[[1]] %ni% RNA_ALPHABET)))  {
                disableActionButton("compute", session)
                output$pamhandlelink <-
                  renderUI({
                    tagList("", urliupac)
                  })
              }
              else if ((all(strsplit(input$inp_pam, "")[[1]] %in% DNA_ALPHABET)) &
                       (all(strsplit(input$inp_handle, "")[[1]] %in% RNA_ALPHABET)) &
                       (nchar(input$inp_pam)) > 2 &
                       (nchar(input$inp_handle) > 10)) {
                enableActionButton("compute", session)
                output$pamhandlelink <-               renderText({
                  ""
                })
              } else {
                #disableActionButton("compute", session)
                output$pamhandlelink <-
                  renderUI({
                    tagList("", urliupac)
                  })
              }
            })
            output$cpf <- renderTable({
              validate(need(
                input$inp_target != "",
                'Please provide a DNA sequence.'
              ))
              validate(need(
                nchar(input$inp_pam) > 2,
                'Please specify the PAM-site'
              ))
              validate(need(
                all(strsplit(input$inp_pam, "")[[1]] %in% DNA_ALPHABET),
                'Please provide a nucleotide sequence for the PAM-site.'
              ))
              validate(need(
                nchar(input$inp_handle) > 10,
                'Please specify the handle.'
              ))
              validate(need(
                all(strsplit(input$inp_handle, "")[[1]] %in% RNA_ALPHABET),
                'Please provide a nucleotide sequence for the handle.'
              ))
              rv$data
            })
            # output$forwardoligo <- renderTable({
            #   validate(need(input$inp_target != "", ''))
            #   validate(need(nchar(input$inp_pam) > 2, ''))
            #   validate(need(all(
            #     strsplit(input$inp_pam, "")[[1]] %in% DNA_ALPHABET
            #   ), ''))
            #   validate(need(nchar(input$inp_handle) > 10, ''))
            #   validate(need(all(
            #     strsplit(input$inp_handle, "")[[1]] %in% RNA_ALPHABET
            #   ), ''))
            #   rvp$data
            # }, sanitize.text.function = function(x)
            #   x)
          } else if ("Other, please specify PAM and gRNA-handle:" %ni% input$inp_cpf) {
            output$cpf <- renderTable({
              validate(need(
                input$inp_target != "",
                'Please provide a DNA sequence.'
              ))
              validate(need(
                input$inp_cpf != "",
                'Select at least one Cpf1 variant!'
              ))
              rv$data
            })
            # output$forwardoligo <- renderTable({
            #   validate(need(input$inp_target != "", ''))
            #   validate(need(input$inp_cpf != "", ''))
            #   rvp$data
            # }, sanitize.text.function = function(x)
            #   x)
          }
        })
        observeEvent(input$inp_cpf, {
          rvc$data <- input$inp_cpf
        })
        observeEvent(input$compute, {
          if ("LbCpf1 - TTTV"  %in% rvc$data) {
            tttvd <-
              matchPattern("TTTV", ntspam()[1:(17 + 3)], fixed = F)
            tttvr <-
              matchPattern("TTTV", reverseComplement(ntspam()[(17 + 1):(17 + 3 + 17)]), fixed = F)
            tttvdl <-
              matchPattern("TTTV", DNAString(targetseq())[204:253], fixed = F)
            tttvrl <-
              matchPattern("TTTV", reverseComplement(DNAString(targetseq())[(200 + 17 + 1):(203 + 50)]), fixed = F)
          }
          if ("LbCpf1 - TYCV"  %in% rvc$data) {
            tycvd <-
              matchPattern("TYCV", ntspam()[1:(17 + 3)], fixed = F)
            tycvr <-
              matchPattern("TYCV", reverseComplement(ntspam()[(17 + 1):(17 + 3 + 17)]), fixed = F)
            tycvdl <-
              matchPattern("TYCV", DNAString(targetseq())[204:253], fixed = F)
            tycvrl <-
              matchPattern("TYCV", reverseComplement(DNAString(targetseq())[(200 + 17 + 1):(203 + 50)]), fixed = F)
          }
          if ("LbCpf1 - MCCC"  %in% rvc$data) {
            mcccd <-
              matchPattern("MCCC", ntspam()[1:(17 + 3)], fixed = F)
            mcccr <-
              matchPattern("MCCC", reverseComplement(ntspam()[(17 + 1):(17 + 3 + 17)]), fixed = F)
            mcccdl <-
              matchPattern("MCCC", DNAString(targetseq())[204:253], fixed = F)
            mcccrl <-
              matchPattern("MCCC", reverseComplement(DNAString(targetseq())[(200 + 17 + 1):(203 + 50)]), fixed = F)
          }
          if ("AsCpf1 - RATR"  %in% rvc$data) {
            ratrd <-
              matchPattern("RATR", ntspam()[1:(17 + 3)], fixed = F)
            ratrr <-
              matchPattern("RATR", reverseComplement(ntspam()[(17 + 1):(17 + 3 + 17)]), fixed = F)
            ratrdl <-
              matchPattern("RATR", DNAString(targetseq())[204:253], fixed = F)
            ratrrl <-
              matchPattern("RATR", reverseComplement(DNAString(targetseq())[(200 + 17 + 1):(203 + 50)]), fixed = F)
          }
          if ("AsCpf1 - TATV"  %in% rvc$data) {
            tatvd <-
              matchPattern("TATV", ntspam()[1:(17 + 3)], fixed = F)
            tatvr <-
              matchPattern("TATV", reverseComplement(ntspam()[(17 + 1):(17 + 3 + 17)]), fixed = F)
            tatvdl <-
              matchPattern("TATV", DNAString(targetseq())[204:253], fixed = F)
            tatvrl <-
              matchPattern("TATV", reverseComplement(DNAString(targetseq())[(200 + 17 + 1):(203 + 50)]), fixed = F)
          }
          if ("Other, please specify PAM and gRNA-handle:"  %in% rvc$data) {
            otherpamsd <-
              matchPattern(input$inp_pam, ntspam()[1:(17 + 3)], fixed = F)
            otherpamsr <-
              matchPattern(input$inp_pam,
                           reverseComplement(ntspam()[(17 + 1):(17 + 3 + 17)]),
                           fixed = F)
            otherpamsdl <-
              matchPattern(input$inp_pam, DNAString(targetseq())[204:253], fixed = F)
            otherpamsrl <-
              matchPattern(input$inp_pam,
                           reverseComplement(DNAString(targetseq())[(200 + 17 + 1):(200 + 17 + 50)]),
                           fixed = F)
          }
          ## ----
          allpamd <-
            list(tttvd, tycvd, mcccd, ratrd, tatvd, otherpamsd)
          if (is.null(tttvd) |
              is.null(tycvd) | is.null(mcccd) | is.null(ratrd) |
              is.null(tatvd) | is.null(otherpamsd))
          {
            allpamd <- allpamd[-which(sapply(allpamd, is.null))]
          }
          allpammd <- allpamd[[1]]
          if (length(allpamd) > 1) {
            for (i in (1:(length(allpamd) - 1))) {
              allpammd <- c(allpammd, allpamd[[i + 1]])
            }
          }
          allpamr <-
            list(tttvr, tycvr, mcccr, ratrr, tatvr, otherpamsr)
          if (is.null(tttvr) |
              is.null(tycvr) | is.null(mcccr) | is.null(ratrr) |
              is.null(tatvr) | is.null(otherpamsr))
          {
            allpamr <- allpamr[-which(sapply(allpamr, is.null))]
          }
          allpammr <- allpamr[[1]]
          if (length(allpamr) > 1) {
            for (i in (1:(length(allpamr) - 1))) {
              allpammr <- c(allpammr, allpamr[[i + 1]])
            }
          }
          allpamdl <-
            list(tttvdl, tycvdl, mcccdl, ratrdl, tatvdl, otherpamsdl)
          if (is.null(tttvdl) |
              is.null(tycvdl) | is.null(mcccdl) | is.null(ratrdl) |
              is.null(tatvdl) | is.null(otherpamsdl))
          {
            allpamdl <- allpamdl[-which(sapply(allpamdl, is.null))]
          }
          allpammdl <- allpamdl[[1]]
          if (length(allpamdl) > 1) {
            for (i in (1:(length(allpamdl) - 1))) {
              allpammdl <- c(allpammdl, allpamdl[[i + 1]])
            }
          }
          allpamrl <-
            list(tttvrl, tycvrl, mcccrl, ratrrl, tatvrl, otherpamsrl)
          if (is.null(tttvrl) |
              is.null(tycvrl) | is.null(mcccrl) | is.null(ratrrl) |
              is.null(tatvrl) | is.null(otherpamsrl))
          {
            allpamrl <- allpamrl[-which(sapply(allpamrl, is.null))]
          }
          allpammrl <- allpamrl[[1]]
          if (length(allpamrl) > 1) {
            for (i in (1:(length(allpamrl) - 1))) {
              allpammrl <- c(allpammrl, allpamrl[[i + 1]])
            }
          }
          
          # merge the two datasets in one data frame
          if (length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)  == 0) {
            output$cpf <- renderText("No PAM-site found.")
            output$forwardoligo <- renderText("")
            disable("downloadcsv")
            disable("downloadxlsx")
          } else {
            allpamm <-
              matrix(
                rep(0, len = (
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                ) * 6),
                length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
              )
            colnames(allpamm) <-
              c("start", "width", "PAM", "strand", "distance", "sort")
            allpamm = as.data.frame(allpamm)
            if (length(allpammd) > 0) {
              for (i in (1:(length(allpammd)))) {
                allpamm$start[i] <- allpammd@ranges@start[i]
                allpamm$sort[i] <- allpammd@ranges@start[i]
                allpamm$width[i] <- allpammd@ranges@width[i]
                allpamm$PAM[i] <- as.character(allpammd[[i]])
                allpamm$strand[i] <- "direct"
                allpamm$distance[i] <-
                  17 + 3 - allpammd@ranges@start[i]
              }
            }
            if (length(allpammr)  > 0) {
              for (i in (1:length(allpammr))) {
                allpamm$start[i + length(allpammd)] <- allpammr@ranges@start[i]
                allpamm$sort[i + length(allpammd)] <-
                  allpammr@ranges@start[i]
                allpamm$width[i + length(allpammd)] <-
                  allpammr@ranges@width[i]
                allpamm$PAM[i + length(allpammd)] <-
                  as.character(allpammr[[i]])
                allpamm$strand[i + length(allpammd)] <-
                  "reverse"
                allpamm$distance[i + length(allpammd)] <-
                  17 + 3 - allpammr@ranges@start[i]
                
              }
            }
            if (length(allpammdl) > 0) {
              for (i in (1:(length(allpammdl)))) {
                allpamm$start[i + length(allpammd) + length(allpammr)] <-
                  allpammdl@ranges@start[i]
                allpamm$sort[i + length(allpammd) + length(allpammr)] <-
                  50 - allpammdl@ranges@start[i]
                allpamm$width[i + length(allpammd) + length(allpammr)] <-
                  allpammdl@ranges@width[i]
                allpamm$PAM[i + length(allpammd) + length(allpammr)] <-
                  as.character(allpammdl[[i]])
                allpamm$strand[i + length(allpammd) + length(allpammr)] <-
                  "search space extended - direct*"
                allpamm$distance[i + length(allpammd) + length(allpammr)] <-
                  203 - allpammdl@ranges@start[i]
                
              }
            }
            if (length(allpammrl)  > 0) {
              for (i in (1:length(allpammrl))) {
                allpamm$start[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
                  allpammrl@ranges@start[i]
                allpamm$sort[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
                  allpammrl@ranges@start[i]
                allpamm$width[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
                  allpammrl@ranges@width[i]
                allpamm$PAM[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
                  as.character(allpammrl[[i]])
                allpamm$strand[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
                  "search space extended - reverse*"
                allpamm$distance[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
                  203 - allpammrl@ranges@start[i]
              }
            }
            # remove sites occuring twice
            single <- which(duplicated(allpamm) == F)
            allpam_single <- allpamm[single, ]
            # rank sites according to proximity to stop
            sortedallpam <-
              allpam_single[order(-allpam_single$sort), ]
            sortedallpam <-
              sortedallpam[order(sortedallpam$strand), ]
            # compute gRNAs for the 5 best PAM-sites
            #if (nrow(sortedallpam) < 5) {
            pamnumber <- nrow(sortedallpam)
            # } else {
            #   pamnumber <- 5
            # }
            handles <-
              unique(subset(endonucleases, select = -c(PAM)))
            handles <- handles[order(handles$Name), ]
            handle <- rep("handle", times = pamnumber)
            grnas <- rep("grna", times = pamnumber)
            cpfname <- rep("cpfname", times = pamnumber)
            rvcn <- reactiveValues(data = NULL)
            rvcn$data <- cpfname
            grnahandle <- rep("grnahandle", times = pamnumber)
            dim(grnas) <- c(pamnumber, 1)
            colnames(grnas) <- "gRNAs"
            for (i in (1:pamnumber)) {
              if (sortedallpam$strand[i] == "direct") {
                grnastart <-
                  sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 - 17
                grnas[i] <-
                  as.character(reverseComplement(DNAString(targetseq())[(grnastart):(grnastart + 19)]))
              } else if (sortedallpam$strand[i] == "reverse") {
                grnastart <-
                  sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 * (17 + 3 - sortedallpam$start[i]) + 200 - 17 - 2
                grnas[i] <-
                  as.character(DNAString(targetseq())[(grnastart - 19):(grnastart)])
              } else if (sortedallpam$strand[i] == "search space extended - direct*") {
                grnastart <-
                  sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203
                grnas[i] <-
                  as.character(reverseComplement(DNAString(targetseq())[(grnastart):(grnastart + 19)]))
              } else if (sortedallpam$strand[i] == "search space extended - reverse*") {
                grnastart <-
                  50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 + 203
                grnas[i] <-
                  as.character(DNAString(targetseq())[(grnastart - 19):(grnastart)])
              }
            }
            
            summary <-
              matrix(rep(0, len = (pamnumber + 3) * 9), nrow = (pamnumber + 3))
            colnames(summary) <-
              c(
                "Rank",
                #"Distance",
                "Target",
                "Suggested name",
                "Sequence",
                "Length",
                "Cpf1",
                "PAM",
                "Strand",
                "gRNA (handle + spacer)"
              )
            summary = as.data.frame(summary)
            for (i in (1:pamnumber)) {
              if (nchar("TTTV") == sortedallpam$width[i] &
                  countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("LbCpf1 - TTTV"  %in% rvc$data)) {
                cpfname[i] <- "LbCpf1_TTTV"
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "LbCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
            }
            for (i in (1:pamnumber)) {
              if (nchar("TYCV") == sortedallpam$width[i] &
                  countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TYCV"),
                               fixed = F) > 0 &
                  ("LbCpf1 - TYCV"  %in% rvc$data)) {
                cpfname[i] <- "LbCpf1_TYCV"
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "LbCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
            }
            for (i in (1:pamnumber)) {
              if (nchar("MCCC") == sortedallpam$width[i] &
                  countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("MCCC"),
                               fixed = F) > 0 &
                  ("LbCpf1 - MCCC"  %in% rvc$data)) {
                cpfname[i] <- "LbCpf1_MCCC"
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "LbCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
            }
            for (i in (1:pamnumber)) {
              if (nchar("TATV") == sortedallpam$width[i] &
                  countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TATV"),
                               fixed = F) > 0 &
                  ("AsCpf1 - TATV"  %in% rvc$data)) {
                cpfname[i] <- "AsCpf1_TATV"
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "AsCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
            }
            for (i in (1:pamnumber)) {
              if (nchar("RATR") == sortedallpam$width[i] &
                  countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("RATR"),
                               fixed = F) > 0 &
                  ("AsCpf1 - RATR"  %in% rvc$data)) {
                cpfname[i] <- "AsCpf1_RATR"
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "AsCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
            }
            for (i in (1:pamnumber)) {
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString(input$inp_pam),
                               fixed = F > 0) &
                  "Other, please specify PAM and gRNA-handle:"  %in% rvc$data) {
                cpfname[i] <-
                  paste("User_defined_Cpf1_", sortedallpam$PAM[i], sep = "")
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(
                    RNAString(input$inp_handle)
                  )))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
            }
            csvoutput <- summary
            for (i in (1:pamnumber)) {
              #summary$Distance[i] <- format(round(sortedallpam$start[i], 0), nsmall = 0)
              summary$Cpf1[i + 3] <- cpfname[i]
              summary$PAM[i + 3] <- paste(sortedallpam$PAM[i])
              summary$`gRNA (handle + spacer)`[i + 3] <-
                paste(
                  '<span style = "color:red">',
                  as.character(grnahandle[i]),
                  '</span>',
                  '<span style = "color:blue">',
                  gsub("T", "U", reverseComplement(DNAString(grnas[i]))),
                  '</span>',
                  sep = ""
                )
              summary$`Suggested name`[i + 3] <-
                paste("M2_", rvg$data, "_", cpfname[i], sep = "")
              if ((sortedallpam$strand[i] == "direct") |
                  (sortedallpam$strand[i] == "reverse")) {
                summary$Sequence[i + 3] <-
                  paste(
                    '<span style = "color:green">',
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(200 + 4):(200 + 4 + 54)])),
                    '</span>',
                    '<span style = "color:orange">',
                    "AAAAAA",
                    '</span>',
                    '<span style = "color:blue">',
                    grnas[i],
                    '</span>',
                    '<span style = "color:red">',
                    handle[i],
                    '</span>',
                    "GCTAGCTGCATCGGTACC",
                    sep = ""
                  )
                summary$Strand[i + 3] <-
                  paste(as.character(sortedallpam$strand[i]))
              }
              else if (sortedallpam$strand[i] == "search space extended - direct*") {
                summary$Strand[i+3] <- "direct*"
                summary$Sequence[i + 3] <-
                  paste(
                    '<span style = "color:green">',
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(grnas[i])):(
                      sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(grnas[i]) + 54
                    )])),
                    '</span>',
                    '<span style = "color:orange">',
                    "AAAAAA",
                    '</span>',
                    '<span style = "color:blue">',
                    grnas[i],
                    '</span>',
                    '<span style = "color:red">',
                    handle[i],
                    '</span>',
                    "GCTAGCTGCATCGGTACC",
                    sep = ""
                  )
                #summary$Target[i] <-
              } else if (sortedallpam$strand[i] == "search space extended - reverse*") {
                summary$Strand[i + 3] <- "reverse*"
                summary$Sequence[i + 3] <-
                  paste(
                    '<span style = "color:green">',
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2):(203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + 54)])),
                    '</span>',
                    '<span style = "color:orange">',
                    "AAAAAA",
                    '</span>',
                    '<span style = "color:blue">',
                    grnas[i],
                    '</span>',
                    '<span style = "color:red">',
                    handle[i],
                    '</span>',
                    "GCTAGCTGCATCGGTACC",
                    sep = ""
                  )
              }
              
              summary$Length[i + 3] <-
                paste(format(round(nchar(
                  paste(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(200 + 4):(200 + 4 + 54)])),
                    "AAAAAA",
                    grnas[i],
                    handle[i],
                    "GCTAGCTGCATCGGTACC",
                    sep = ""
                  )
                ), 0), nsmall = 0), "nts", sep = "&nbsp")
              stopstart <- (17 + 10 + 1)
              stopend <- (17 + 10 + 3)
              if (as.character(sortedallpam$strand[i] == "direct")) {
                regionds <- paste0(
                  as.character(ntsstop()[1:(17 + 10)]),
                  '<span style = "font-weight:bold">',
                  (as.character(ntsstop()[(17 + 10 + 1):(17 + 3 + 10)])),
                  '</span>',
                  as.character(ntsstop()[(17 + 3 + 10 + 1):nchar(ntsstop())])
                )
                regiondr <- paste0(
                  as.character(complement(ntsstop()[1:(17 + 10)])),
                  strong(as.character(complement(
                    ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]
                  ))),
                  as.character(complement(ntsstop()[(17 + 3 + 10 + 1):nchar(ntsstop())]))
                )
                blackf <-
                  substr(ntsstop(), 1, (sortedallpam$start[i] + 10 - 1))
                blackr <-
                  paste0(
                    '<span style = "text-decoration: underline">',
                    substr(
                      ntsstop(),
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i])
                      ),
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i]) + 2
                      )
                    ),
                    '</span>',
                    '<span style = "color:white">',
                    "_",
                    '</span>',
                    substr(
                      ntsstop(),
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i]) + 3
                      ),
                      nchar(ntsstop())
                    )
                  )
                bluer <-
                  paste0(
                    '<span style = "color:blue">',
                    substr(
                      ntsstop(),
                      stopend + 1,
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i]) - 3
                      )
                    ),
                    '</span>'
                  )
                bluerr <-
                  paste0(
                    '<span style = "color:blue; text-decoration: underline">',
                    substr(
                      ntsstop(),
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i]) - 2
                      ),
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i]) - 1
                      )
                    ),
                    '</span>'
                  )
                pamstart <-
                  sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) - 1
                pamend <- 10 + 17
                if (pamstart < pamend) {
                  coral <-
                    paste0('<span style = "color:coral">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (
                               sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) - 1
                             )
                           ),
                           '</span>')
                  coralbold <- ""
                  bluef <-
                    paste0(
                      '<span style = "color:blue">',
                      substr(
                        ntsstop(),
                        (
                          sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i])
                        ),
                        stopstart - 1
                      ),
                      '</span>'
                    )
                  bluebold <-
                    paste0(
                      '<span style = "color:blue;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopend),
                      '</span>'
                    )
                } else if (pamstart == pamend) {
                  coral <-
                    paste0('<span style = "color:coral">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (
                               sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) - 1
                             )
                           ),
                           '</span>')
                  coralbold <- ""
                  bluef <- ""
                  bluebold <-
                    paste0(
                      '<span style = "color:blue;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopend),
                      '</span>'
                    )
                } else if (pamstart == pamend + 1) {
                  coral <-
                    paste0('<span style = "color:coral">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (stopstart - 1)
                           ),
                           '</span>')
                  coralbold <-
                    paste0(
                      '<span style = "color:coral;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopstart),
                      '</span>'
                    )
                  bluef <- ""
                  bluebold <-
                    paste0(
                      '<span style = "color:blue;font-weight:bold">',
                      substr(ntsstop(), stopstart + 1, stopend),
                      '</span>'
                    )
                } else if (pamstart == pamend + 2) {
                  coral <-
                    paste0('<span style = "color:coral">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (stopstart - 1)
                           ),
                           '</span>')
                  coralbold <-
                    paste0(
                      '<span style = "color:coral;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopstart + 1),
                      '</span>'
                    )
                  bluef <- ""
                  bluebold <-
                    paste0(
                      '<span style = "color:blue;font-weight:bold">',
                      substr(ntsstop(), stopend, stopend),
                      '</span>'
                    )
                } else if (pamstart == pamend + 3) {
                  coral <-
                    paste0('<span style = "color:coral">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (stopstart - 1)
                           ),
                           '</span>')
                  coralbold <-
                    paste0(
                      '<span style = "color:coral;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopend),
                      '</span>'
                    )
                  bluef <- ""
                  bluebold <- ""
                }
                summary$Target[i + 3] <-
                  paste(
                    paste0
                    (
                      blackf,
                      coral,
                      coralbold,
                      bluef,
                      bluebold,
                      bluer,
                      '<sup>&#9660;</sup>',
                      bluerr,
                      blackr
                    ),
                    paste0(
                      as.character(complement(ntsstop()[1:(stopstart - 1)])),
                      strong(as.character(complement(
                        ntsstop()[(stopstart):(stopend)]
                      ))),
                      as.character(complement(ntsstop()[(stopend + 1):(sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i]) - 3)])),
                      '<span style = "color:white">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      as.character(complement(ntsstop()[(sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i]) - 2):(sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + 22)])),
                      '</span>',
                      '<sub>&#9650;</sub>',
                      as.character(complement(ntsstop()[(sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + 23):nchar(ntsstop())]))
                    ),
                    sep = "\n"
                  )
              } else if (as.character(sortedallpam$strand[i] == "reverse")) {
                blackr <-
                  paste0(
                    as.character(complement(ntsstop()[1:(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]) - 3
                    )])),
                    '<span style = "color:white">',
                    "_",
                    '</span>',
                    '<span style = "text-decoration: underline">',
                    as.character(complement(ntsstop()[(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]) - 2
                    ):(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i])
                    )])),
                    '</span>'
                  )
                bluerr <-
                  paste0(
                    '<span style = "color:blue; text-decoration: underline">',
                    as.character(complement(ntsstop()[(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]) + 1
                    ):(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i])  - nchar(grnas[i]) + 2
                    )])),
                    '</span>'
                  )
                bluer <-
                  paste0('<span style = "color:blue">',
                         as.character(complement(ntsstop()[(
                           nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]) + 2 + 1
                         ):(stopstart - 1)])),
                         '</span>')
                blackf <-
                  as.character(complement(ntsstop()[(nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 2):nchar(ntsstop())]))
                pamstart <-
                  (
                    nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) + 1
                  )
                if (stopstart == pamstart) {
                  bluebold <- ""
                  bluef <- ""
                  coralbold <-
                    paste0(
                      '<span style = "color:coral;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:stopend])),
                      '</span>'
                    )
                  coral <-
                    paste0(
                      '<span style = "color:coral">',
                      as.character(complement(ntsstop()[(stopend + 1):(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                } else if (stopstart + 1 == pamstart) {
                  bluebold <-
                    paste0(
                      '<span style = "color:blue;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:stopstart])),
                      '</span>'
                    )
                  bluef <- ""
                  coralbold <-
                    paste0(
                      '<span style = "color:coral;font-weight:bold">',
                      as.character(complement(ntsstop()[(stopstart + 1):stopend])),
                      '</span>'
                    )
                  coral <-
                    paste0(
                      '<span style = "color:coral">',
                      as.character(complement(ntsstop()[(stopend + 1):(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                } else if (stopstart + 2 == pamstart) {
                  bluebold <-
                    paste0(
                      '<span style = "color:blue;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:(stopstart + 1)])),
                      '</span>'
                    )
                  bluef <- ""
                  coralbold <-
                    paste0(
                      '<span style = "color:coral;font-weight:bold">',
                      as.character(complement(ntsstop()[stopend:stopend])),
                      '</span>'
                    )
                  coral <-
                    paste0(
                      '<span style = "color:coral">',
                      as.character(complement(ntsstop()[(stopend + 1):(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                } else if (stopstart + 3 == pamstart) {
                  bluebold <-
                    paste0(
                      '<span style = "color:blue;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:stopend])),
                      '</span>'
                    )
                  bluef <- ""
                  coralbold <- ""
                  coral <-
                    paste0(
                      '<span style = "color:coral">',
                      as.character(complement(ntsstop()[pamstart:(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                } else if (stopstart + 3 < pamstart) {
                  bluebold <-
                    paste0(
                      '<span style = "color:blue;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:stopend])),
                      '</span>'
                    )
                  bluef <-
                    paste0('<span style = "color:blue">',
                           as.character(complement(ntsstop()[(stopend + 1):(pamstart - 1)])),
                           '</span>')
                  coralbold <- ""
                  coral <-
                    paste0(
                      '<span style = "color:coral">',
                      as.character(complement(ntsstop()[pamstart:(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                }
                summary$Target[i + 3] <-
                  paste(
                    paste0(
                      as.character(ntsstop()[1:(
                        nchar(ntsstop()) - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 10 - 23 + 1
                      )]),
                      '<sup>&#9660;</sup>',
                      '<span style = "text-decoration: underline">',
                      as.character(ntsstop()[(
                        nchar(ntsstop()) - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 10 - 22 + 1
                      ):(
                        nchar(ntsstop()) - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 10 - 22 + 1 + 4
                      )]),
                      '</span>',
                      '<span style = "color:white">',
                      "_",
                      '</span>',
                      as.character(ntsstop()[(
                        nchar(ntsstop()) - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 10 - 22 + 1 + 5
                      ):(stopstart - 1)]),
                      strong(as.character(ntsstop()[(stopstart):(stopend)])),
                      as.character(ntsstop()[(stopend + 1):nchar(ntsstop())])
                    ),
                    paste0(
                      blackr,
                      bluerr,
                      '<sub>&#9650;</sub>',
                      bluer,
                      bluebold,
                      bluef,
                      coralbold,
                      coral,
                      blackf
                    ),
                    sep = "\n"
                  )
              }
              else if (sortedallpam$strand[i] == "search space extended - direct*") {
                summary$Target[i + 3] <- paste(
                  paste0(
                    substr(targetseq(), 195, 200),
                    strong(substr(targetseq(), 201, 203)),
                    substr(
                      targetseq(),
                      (203 + 1),
                      (203 + sortedallpam$start[i] - 1)
                    ),
                    '<span style = "color:peru">',
                    substr(
                      targetseq(),
                      (203 + sortedallpam$start[i]),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) - 1
                      )
                    ),
                    '</span>',
                    '<span style = "color:blue">',
                    substr(
                      targetseq(),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i])
                      ),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 17
                      )
                    ),
                    '</span>',
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    '<span style = "color:blue">',
                    substr(
                      targetseq(),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 18
                      ),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 19
                      )
                    ),
                    '</span>',
                    substr(
                      targetseq(),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 20
                      ),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 22
                      )
                    ),
                    '</span>',
                    '<span style = "color:white">',
                    "_",
                    '</span>',
                    substr(
                      targetseq(),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 23
                      ),
                      (203 + 17 + 75)
                    )
                  ),
                  paste0(
                    complement(DNAString(
                      substr(targetseq(), 195, 200)
                    )),
                    strong(complement(DNAString(
                      substr(targetseq(), 201, 203)
                    ))),
                    complement(DNAString(substr(
                      targetseq(),
                      (203 + 1),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 17
                      )
                    ))),
                    '<span style = "color:white">',
                    "_",
                    '</span>',
                    '<span style = "text-decoration: underline">',
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 18
                      ),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 22
                      )
                    ))),
                    '</span>',
                    '<sub>&#9650;</sub>',
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 23
                      ),
                      (203 + 17 + 75)
                    )))
                  ),
                  sep = "\n"
                )
              }
              else if (sortedallpam$strand[i] == "search space extended - reverse*") {
                blackr <- complement(DNAString(substr(targetseq(), 195, 200)))
                peru <-
                  complement(DNAString(substr(
                    targetseq(),
                    (
                      203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2
                    ),
                    (203 + 50 - sortedallpam$start[i] + 1)
                  )))
                blackf <-
                  complement(DNAString(substr(
                    targetseq(),
                    (203 + 50 - sortedallpam$start[i] + 2),
                    (203 + 17 + 75)
                  )))
                # more than 3 nts distance between stop and grna end
                if ((50 - sortedallpam$start[i] - 2) > (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    strong(complement(DNAString(
                      substr(targetseq(), 201, 203)
                    )))
                  blackrr <-
                    paste0(
                      complement(DNAString(
                        substr(
                          targetseq(),
                          204,
                          (
                            203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 23
                          )
                        )
                      )),
                      '<span style = "color:white">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(substr(
                        targetseq(),
                        (
                          203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 22
                        ),
                        (
                          203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 20
                        )
                      ))),
                      '</span>'
                    )
                  bluebold <- ""
                  bluer <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 195, 200),
                    strong(substr(targetseq(), 201, 203)),
                    substr(
                      targetseq(),
                      204,
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 23
                      )
                    ),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 22
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    ),
                    '</span>',
                    '<span style = "color:white">',
                    "_",
                    '</span>',
                    substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (203 + 17 + 75)
                    )
                  )
                  # 3 nts distance between stop and grna end
                } else if ((50 - sortedallpam$start[i] - 2) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    paste0(strong(complement(DNAString(
                      substr(targetseq(), 201, 203)
                    ))),
                    '<span style = "color:white">',
                    "_",
                    '</span>')
                  blackrr <-
                    paste0(
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(
                        substr(targetseq(), 204, 206)
                      )),
                      '</span>'
                    )
                  bluebold <- ""
                  bluer <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 195, 200),
                    strong(substr(targetseq(), 201, 203)),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(targetseq(), 204, 208),
                    '</span>',
                    '<span style = "color:white">',
                    "_",
                    '</span>',
                    substr(targetseq(), 209, (
                      203 + 17 + 75
                    ))
                  )
                  # 2 nts distance between stop and grna end
                } else if ((50 - sortedallpam$start[i] - 1) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    paste0(
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 202)
                      ))),
                      '<span style = "color:white">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      strong(complement(DNAString(
                        substr(targetseq(), 203, 203)
                      ))),
                      '</span>'
                    )
                  blackrr <-
                    paste0(
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(
                        substr(targetseq(), 204, 205)
                      )),
                      '</span>'
                    )
                  bluebold <- ""
                  bluer <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 195, 200),
                    strong(substr(targetseq(), 201, 202)),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    strong(substr(targetseq(), 203, 203)),
                    substr(targetseq(), 204, 207),
                    '</span>',
                    '<span style = "color:white">',
                    "_",
                    '</span>',
                    substr(targetseq(), 208, (
                      203 + 17 + 75
                    ))
                  )
                  # 1 nt distance between stop and grna end
                } else if ((50 - sortedallpam$start[i]) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    paste0(
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 201)
                      ))),
                      '<span style = "color:white">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      strong(complement(DNAString(
                        substr(targetseq(), 202, 203)
                      ))),
                      '</span>'
                    )
                  blackrr <-
                    paste0(
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(
                        substr(targetseq(), 204, 204)
                      )),
                      '</span>'
                    )
                  bluebold <- ""
                  bluer <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 195, 200),
                    strong(substr(targetseq(), 201, 201)),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    strong(substr(targetseq(), 202, 203)),
                    substr(targetseq(), 204, 206),
                    '</span>',
                    '<span style = "color:white">',
                    "_",
                    '</span>',
                    substr(targetseq(), 207, (
                      203 + 17 + 75
                    ))
                  )
                  # no distance between stop and grna end
                } else if ((50 - sortedallpam$start[i] + 1) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    paste0(
                      '<span style = "color:white">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 203)
                      ))),
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- ""
                  bluer <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 195, 200),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    strong(substr(targetseq(), 201, 203)),
                    substr(targetseq(), 204, 205),
                    '</span>',
                    '<span style = "color:white">',
                    "_",
                    '</span>',
                    substr(targetseq(), 206, (
                      203 + 17 + 75
                    ))
                  )
                }
                summary$Target[i + 3] <- paste(
                  directstrand,
                  paste0(
                    blackr,
                    blackbold,
                    '<span style = "color:blue;font-weight:bold">',
                    bluebold,
                    '</span>',
                    blackrr,
                    '<span style = "color:blue; text-decoration: underline">',
                    bluer,
                    '</span>',
                    '<sub>&#9650;</sub>',
                    '<span style = "color:blue">',
                    blue,
                    '</span>',
                    '<span style = "color:peru">',
                    peru,
                    '</span>',
                    blackf
                  ),
                  sep = "\n"
                )
              }
              if ((sortedallpam$strand[i] == "search space extended - reverse*") |
                  (sortedallpam$strand[i] == "search space extended - direct*")) {
                summary$Rank[i + 3] <- paste0(as.character(i), "*")
              } else {
                summary$Rank[i + 3] <- paste0(as.character(i))
              }
              summary$Rank[3] <- paste0('<span style = "font-weight: bold; font-family: Arial">', "Rank", '</span>')
                summary$Target[3] <- paste0(
                  '<span style = "font-weight:bold; font-family: Arial">',
                                    "Target direct and reverse strand with ",
                  '<span style = "color:coral; font-weight:bold; font-family: Arial">',
                  "PAM",
                  '</span>',
                  " and ",
                  '<span style = "color:blue">',
                  "target",
                  '</span>',
                  ", ",
                  '<sup>&#9660;</sup>',
                  '<sub>&#9650;</sub>',
                  " Cpf1 cleavage sites, ",
                  '<span style = "text-decoration: underline">',
                  "overhangs",
                  '</span>',
                  '</span>')
                summary$`Suggested name`[3] <- paste0('<span style = "font-weight: bold; font-family: Arial">', "Suggested name", '</span>')
                summary$Sequence[3] <- paste0(
                  '<span style = "font-weight:bold; font-family: Arial">',
                  "Sequence: ",
                  '<span style = "color:green">',
                  "3'-homology (55 bases after the insertion site, reverse orientation)",
                  '</span>',
                  " + ",
                  '<span style = "color:orange">',
                  "TTTTTT-complement",
                  '</span>',
                  " + ",
                  '<br>',
                  '<span style = "color:blue">',
                  "gRNA spacer encoding sequence",
                  '</span>',
                  " + ",
                  '<span style = "color:red">',
                  "gRNA handle encoding sequence",
                  '</span>',
                  " + primer bindig reverse oligo",
                  '</span>'
                )
                summary$Length[3] <- paste0('<span style = "font-weight: bold; font-family: Arial">', "Length", '</span>')
                summary$Cpf1[3] <- paste0('<span style = "font-weight: bold; font-family: Arial">', "Cpf1", '</span>')
                summary$PAM[3] <- paste0('<span style = "font-weight: bold; font-family: Arial">', "PAM", '</span>')
                summary$Strand[3] <- paste0('<span style = "font-weight: bold; font-family: Arial">', "Strand", '</span>')
                summary$`gRNA (handle + spacer)`[3] <- paste0(
                  '<span style = "font-weight: bold; font-family: Arial">',
                  "gRNA (",
                  '<span style = "color:red">',
                  "Cpf1-variant-specific handle",
                  '</span>',
                  " + ",
                  '<span style = "color:blue">',
                  "spacer",
                  '</span>',
                  ")",
                  '</span>')
                summary$Rank[1] <- ""
                summary$Target[1] <- ""
                summary$`Suggested name`[1] <- paste0("M1_", rvg$data)
                summary$Sequence[1] <- paste0(
                  '<span style = "color:brown">',
                  as.character(DNAString(targetseq())[(200 - 89):200]),
                  '</span>',
                  "TCAGGTGGAGGAGGTAGTG",
                  sep = ""
                )
                summary$Length[1] <- paste0(nchar(as.character(
                  forwardoligo()
                )), "nts")
                summary$Cpf1[1] <- ""
                summary$PAM[1] <- ""
                summary$Strand[1] <- ""
                summary$`gRNA (handle + spacer)`[1] <- paste0('<span style = "font-weight: bold; font-family: Arial">', "Additional information", '</span>')
                summary$Rank[2] <- ""
                summary$Target[2] <- paste0('<span style = "font-weight: bold; font-family: Arial">', "Possible reverse oligos", '</span>')
                summary$`Suggested name`[2] <- ""
                summary$Sequence[2] <- ""
                summary$Length[2] <- ""
                summary$Cpf1[2] <- ""
                summary$PAM[2] <- ""
                summary$Strand[2] <- ""
                summary$`gRNA (handle + spacer)`[2] <- ""
                
                csvoutput$Cpf1[i] <- cpfname[i]
              csvoutput$PAM[i] <- sortedallpam$PAM[i]
              csvoutput$Strand[i] <-
                as.character(sortedallpam$strand[i])
              csvoutput$`gRNA (handle + spacer)`[i] <-
                paste0(as.character(grnahandle[i]),
                       gsub("T", "U", reverseComplement(DNAString(grnas[i]))))
              csvoutput$`Suggested name`[i] <-
                paste0("M2_",
                       rvg$data,
                       "_",
                       cpfname[i])
              if ((sortedallpam$strand[i] == "direct") |
                  (sortedallpam$strand[i] == "reverse")) {
                csvoutput$Sequence[i] <-
                  paste0(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(200 + 4):(200 + 4 + 54)])),
                    "AAAAAA",
                    grnas[i],
                    handle[i],
                    "GCTAGCTGCATCGGTACC"
                  )
              }
              else if (sortedallpam$strand[i] == "search space extended - direct*") {
                csvoutput$Sequence[i] <-
                  paste0(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(grnas[i])):(
                      sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(grnas[i]) + 54
                    )])),
                    "AAAAAA",
                    grnas[i],
                    handle[i],
                    "GCTAGCTGCATCGGTACC"
                  )
                #summary$Target[i] <-
              } else if (sortedallpam$strand[i] == "search space extended - reverse*") {
                csvoutput$Sequence[i] <-
                  paste0(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2):(203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + 54)])),
                    "AAAAAA",
                    grnas[i],
                    handle[i],
                    "GCTAGCTGCATCGGTACC",
                    sep = ""
                  )
              }
              csvoutput$Length[i] <-
                paste(format(round(nchar(
                  paste(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(200 + 4):(200 + 4 + 54)])),
                    "AAAAAA",
                    grnas[i],
                    handle[i],
                    "GCTAGCTGCATCGGTACC",
                    sep = ""
                  )
                ), 0), nsmall = 0), " nts")
                # paste(as.character(ntspam()),
                #       as.character(complement(ntspam())),
                #       sep = "\n")
              csvoutput$Rank[i] <- as.character(i)
              csvoutput <- csvoutput[-c(pamnumber + 1, pamnumber + 2, pamnumber + 3), ]
              
            }
            colnames(summary)[colnames(summary) == "Rank"] <- ""
            colnames(summary)[colnames(summary) == "gRNA (handle + spacer)"] <- ""
              # paste(
              #   "gRNA (",
              #   '<span style = "color:red">',
              #   "Cpf1-variant-specific handle",
              #   '</span>',
              #   " + ",
              #   '<span style = "color:blue">',
              #   "spacer",
              #   '</span>',
              #   ")",
              #   sep = ""
              # )
            colnames(summary)[colnames(summary) == "Target"] <-
              paste0('<span style = "font-weight: bold; font-family: Arial; text-align:right">', "Forward oligo", '</span>')
            colnames(summary)[colnames(summary) == "Sequence"] <-
              paste0(
                '<span style = "font-weight:bold; font-family: Arial">', "Sequence: ",
                '<span style = "color:brown">',
                "5'-homology (90 bases before the insertion site, direct orientation)",
                '</span>',
                " + primer binding forward oligo", '</span>'
              )
            colnames(summary)[colnames(summary) == "Cpf1"] <- ""
            colnames(summary)[colnames(summary) == "PAM"] <- ""
            colnames(summary)[colnames(summary) == "Strand"] <- ""
            rv$data <- summary [-c((4 + length(allpammd) + length(allpammr)):(4 + length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl) - 1)), ]
            rve$data <- summary [-c(4:(4 + length(allpammd) + length(allpammr) - 1)), ]
        
            csvoutput[nrow(csvoutput) + 1,] <-
              c("", "", "", "", "", "", "", "", "")
            csvoutput[nrow(csvoutput) + 1,] <-
              c(
                "Forward oligo",
                paste0("M1_", rvg$data),
                as.character(forwardoligo()),
                paste(nchar(as.character(
                  forwardoligo()
                )), "nts"),
                "",
                "",
                "",
                "",
                ""
              )
            csvoutput$Target <- NULL
            
            rvo$data <- csvoutput
            enable("downloadcsv")
            enable("downloadxlsx")
            summaryf <-
              matrix(rep(0, len = 1 * 4), nrow = 1)
            colnames(summaryf) <-
              c("Forward oligo:",
                "Suggested name",
                "Sequence",
                "Length")
            summaryf = as.data.frame(summaryf)
            summaryf$`Forward oligo:`[1] <- ""
            summaryf$`Suggested name`[1] <-
              paste("M1_", rvg$data, '</span>', sep = "")
            summaryf$`Sequence`[1] <-
              paste(
                '<span style = "color:brown">',
                as.character(DNAString(targetseq())[(200 - 89):200]),
                '</span>',
                "TCAGGTGGAGGAGGTAGTG",
                sep = ""
              )
            summaryf$`Length`[1] <-
              paste(nchar(as.character(forwardoligo())), "nts", sep = "&nbsp")
            colnames(summaryf)[colnames(summaryf) == "Sequence"] <-
              paste(
                strong("Sequence:"),
                '<span style = "color:brown">',
                "5'-homology (90 bases before the insertion site, direct orientation)",
                '</span>',
                "+ primer binding forward oligo"
              )
            rvp$data <- summaryf
            output$resultstitle <- renderText({"RESULTS - Forward oligo (M1) and possible reverse oligos (M2)"})
            # output$reversetitle <-
            #   renderText({
            #     "Possible reverse oligos:"
            #   })
            showElement("extended")
            # paste(strong("Forward oligo:"), "5'-homology (90 bases before the insertion site, direct orientation)", as.character(forwardoligo()), sep = "\n")
          }
        })
        observe({
          if ("Other, please specify PAM and gRNA-handle:" %in% input$inp_cpf) {
            output$cpf <- renderTable({
              validate(need(
                input$inp_target != "",
                'Please provide a nucleotide sequence.'
              ))
              validate(need(
                nchar(input$inp_pam) > 2,
                'Please specify the PAM-site.'
              ))
              validate(need(
                nchar(input$inp_handle) > 10,
                'Please specify the handle.'
              ))
              rv$data
            }, sanitize.text.function = function(x)
              x)
            output$forwardoligo <- renderTable({
              validate(need(input$inp_target != "", ''))
              validate(need(nchar(input$inp_pam) > 2, ''))
              validate(need(nchar(input$inp_handle) > 5, ''))
              rvp$data
            }, sanitize.text.function = function(x)
              x)
          } else if ("Other, please specify PAM and gRNA-handle:" %ni% input$inp_cpf) {
            output$cpf <- renderTable({
              validate(need(
                input$inp_target != "",
                'Please provide a nucleotide sequence.'
              ))
              validate(need(
                input$inp_cpf != "",
                'Select at least one Cpf1 variant.'
              ))
              rv$data
            }, sanitize.text.function = function(x)
              x)
            # output$forwardoligo <- renderTable({
            #   validate(need(input$inp_target != "", ''))
            #   validate(need(input$inp_cpf != "", ''))
            #   rvp$data
            # }, sanitize.text.function = function(x)
            #   x)
          }
        })
        # observeEvent(input$compute, {
        #   enable("downloadcsv")
        # })
        output$downloadcsv <- downloadHandler(
          filename = paste0("M_", rvg$data),
          content = function(file) {
            write.csv(rvo$data, file, row.names = FALSE)
          }
        )
        output$downloadxlsx <- downloadHandler(
          filename = paste0("M_", rvg$data, ".xlsx"),
          content = function(file) {
            tempFile <- tempfile(fileext = ".xlsx")
            write_xlsx(rvo$data, tempFile)
            file.rename(tempFile, file)
          }
        )
        output$report <- downloadHandler(
          filename = function() {
            'Mammalian_PCR_tagging_report.pdf'
          },
          
          content = function(file) {
            src <- normalizePath('report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            library(rmarkdown)
            out <- render('report.Rmd')
            file.rename(out, file)
          }
        )
        
      }
      observe({
        if (input$extended == 1) {
          output$regiontexte <- renderText({
            paste(h4(em("Extended search space for PAM-sites:")),
                  h5(em(
                    "Up to 50 nucleotides downstream on the direct and reverse strand"
                  ), style = "color:peru"), sep = '\n')
          })
          ntsstop <- aroundstop()(targetseq())
          output$nucleotidese <- renderText({
            regionde <- paste(
              as.character(ntsstop()[1:(17 + 10)]),
              strong(substr(targetseq(), 201, 203)),
              '<span style = "color:peru">',
              substr(targetseq(), 204, 253),
              '</span>',
              substr(targetseq(), 254, 260),
              " - direct strand",
              sep = ""
            )
          })
          output$nucleotidesre <- renderText({
            paste(
              as.character(complement(ntsstop()[1:10])),
              as.character(complement(ntsstop()[11:(17 + 10)])),
              strong(as.character(complement(
                ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]
              ))),
              as.character(complement(ntsstop()[(17 + 3 + 10 + 1):(17 + 3 + 10 + 17)])),
              '<span style = "color:peru">',
              as.character(complement(DNAString(
                targetseq()
              )[(203 + 17 + 1):253])),
              '</span>',
              as.character(complement(DNAString(
                targetseq()
              )[254:260])),
              " - reverse strand",
              sep = ""
            )
          })
          output$cpfe <- renderTable({
            rve$data
          }, sanitize.text.function = function(x)
            x)
        } else {
          output$regiontexte <- renderText({""})
          output$nucleotidese <- renderText({""})
          output$nucleotidesre <- renderText({""})
          output$cpfe <- renderText({""})
        }
      })
      })

    ##### FASTA
    # disableActionButton("fcompute", session)
    # disable("inp_fastapam")
    # disable("inp_fastahandle")
    # observe({
    #   if ("Other, please specify PAM and gRNA-handle:" %in% input$finp_cpf) {
    #     enable("inp_fpam")
    #     enable("inp_fhandle")
    #     
    #   } else {
    #     disable("inp_fpam")
    #     disable("inp_fhandle")
    #   }
    # })
    # disable("fdownloadcsv")
    # disable("fdownloadxlsx")
    # observeEvent(input$inp_fasta, {
    #   fastaread <- reactive({
    #     if (is.null(input$inp_fasta))
    #       return(NULL)
    #     fastafile <- input$inp_fasta
    #     file.rename(fastafile$datapath,
    #                 file.path(dirname(fastafile$datapath),
    #                           fastafile$name))
    #     # returns a DNAStringSet object:
    #     readDNAStringSet(filepath = file.path(dirname(fastafile$datapath), fastafile$name))
    #   })
    #   fastaresultlist <- rep(list(), length(fastaread()))
    #   
    #   for (j in (1:length(fastaread()))) {
    #     ftargetseq <- reactive({
    #       ntst.tmp <- as.character(fastaread()[[j]])
    #       return(ntst.tmp)
    #     })
    #     # output$fastasequence <-
    #     #   renderText({
    #     #     paste0(nchar(ftargetseq()), ftargetseq(), ""  )
    #     #     })
    #     #   ftargetseq <- reactive({
    #     #     ntst.tmp <- as.character(fastaread()[j])
    #     #     return(ntst.tmp)
    #     #   })
    #     if ((nchar(ftargetseq()) != 200 + 200 + 3) &
    #         (all(strsplit(ftargetseq(), "")[[1]] %in% DNA_ALPHABET))) {
    #       disableActionButton("fcompute", session)
    #       #disableActionButton("fapply", session)
    #       output$finputlength <-
    #         renderText({
    #           paste0("Please provide a sequence of proper length (sequence ",
    #                  j,
    #                  ").")
    #         })
    #       output$finputnuc <-
    #         renderText({
    #           ""
    #         })
    #     }
    #     else if ((any(strsplit(ftargetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
    #              (nchar(ftargetseq()) != 200 + 200 + 3)) {
    #       disableActionButton("fcompute", session)
    #       #disableActionButton("fapply", session)
    #       output$finputlength <-
    #         renderText({
    #           paste0("Please provide a sequence of proper length (sequence ",
    #                  j,
    #                  ").")
    #         })
    #       output$finputnuc <-
    #         renderText({
    #           paste0("Please provide a DNA sequence (sequence ", j, ").")
    #         })
    #     }
    #     else if ((any(strsplit(ftargetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
    #              (nchar(ftargetseq()) == 200 + 200 + 3)) {
    #       disableActionButton("fcompute", session)
    #       #disableActionButton("fapply", session)
    #       output$finputlength <-
    #         renderText({
    #           ""
    #         })
    #       output$finputnuc <-
    #         renderText({
    #           paste0("Please provide a DNA sequence (sequence ", j, ").")
    #         })
    #       
    #     }
    #     else if ((nchar(ftargetseq()) == 200 + 200 + 3) &
    #                (all(strsplit(ftargetseq(), "")[[1]] %in% DNA_ALPHABET))) {
    #       enableActionButton("fcompute", session)
    #       #disableActionButton("fapply", session)
    #       output$finputlength <-
    #         renderText({
    #           ""
    #         })
    #       output$finputnuc <-
    #         renderText({
    #           ftargetseq()
    #         })
    #     
    #   
    # 
    #       
    #         fntspam <- eventReactive({
    #           input$fpamregionu
    #           input$fpamregiond
    #         }, {
    #           subsetnts <- aroundpam()(ftargetseq())
    #           return(subsetnts)
    #         })
    #         fntsstop <- eventReactive({
    #           input$fpamregionu
    #           input$fpamregiond
    #         }, {
    #           subsetnts <- aroundstop()(ftargetseq())
    #           return(subsetnts)
    #         })
    #         fforwardoligo <- eventReactive(input$fcompute, {
    #           cone <-
    #             as.character(xscat(
    #               DNAString(ftargetseq())[(200 - 89):200],
    #               DNAString("TCAGGTGGAGGAGGTAGTG")
    #             ))
    #           return(cone)
    #         })
    #         tttvd <- NULL
    #         tttvr <- NULL
    #         tycvd <- NULL
    #         tycvr <- NULL
    #         mcccd <- NULL
    #         mcccr <- NULL
    #         ratrd <- NULL
    #         ratrr <- NULL
    #         tatvd <- NULL
    #         tatvr <- NULL
    #         otherpamsd <- NULL
    #         otherpamsr <- NULL
    #         tttvdl <- NULL
    #         tttvrl <- NULL
    #         tycvdl <- NULL
    #         tycvrl <- NULL
    #         mcccdl <- NULL
    #         mcccrl <- NULL
    #         ratrdl <- NULL
    #         ratrrl <- NULL
    #         tatvdl <- NULL
    #         tatvrl <- NULL
    #         otherpamsdl <- NULL
    #         otherpamsrl <- NULL
    #         rvc <- reactiveValues(data = NULL)
    #         rv <- reactiveValues(data = NULL)
    #         rvp <- reactiveValues(data = NULL)
    #         rvo <- reactiveValues(data = NULL)
    #         #rvg <- reactiveValues(data = NULL)
    #         observe({
    #           if ("Other, please specify PAM and gRNA-handle:" %in% input$inp_fcpf) {
    #             observe({
    #               if ((any(strsplit(input$inp_fpam, "")[[1]] %ni% DNA_ALPHABET)) |
    #                   (any(strsplit(input$inp_fhandle, "")[[1]] %ni% RNA_ALPHABET)))  {
    #                 disableActionButton("fcompute", session)
    #                 output$fiupaclink <-
    #                   renderUI({
    #                     tagList("", urliupac)
    #                   })
    #               }
    #               else if ((all(strsplit(input$inp_fpam, "")[[1]] %in% DNA_ALPHABET)) &
    #                        (all(strsplit(input$inp_fhandle, "")[[1]] %in% RNA_ALPHABET)) &
    #                        (nchar(input$inp_fpam)) > 2 &
    #                        (nchar(input$inp_fhandle) > 10)) {
    #                 enableActionButton("fcompute", session)
    #                 output$fiupaclink <-               renderText({
    #                   ""
    #                 })
    #               } else {
    #                 disableActionButton("fcompute", session)
    #                 output$fiupaclink <-
    #                   renderUI({
    #                     tagList("", urliupac)
    #                   })
    #               }
    #             })
    # 
    #           } 
    #           
    #         })
    #         observeEvent(input$inp_fcpf, {
    #           rvc$data <- input$inp_fcpf
    #         })
    #         observeEvent(input$fcompute, {    
    #           
    #         if ("LbCpf1 - TTTV"  %in% rvc$data) {
    #             tttvd <-
    #               matchPattern("TTTV", fntspam()[1:(input$fpamregionu + 3)], fixed = F)
    #             tttvr <-
    #               matchPattern("TTTV", reverseComplement(fntspam()[(input$fpamregionu + 1):(input$fpamregionu + 3 + input$fpamregiond)]), fixed = F)
    #             tttvdl <-
    #               matchPattern("TTTV", DNAString(ftargetseq())[204:253], fixed = F)
    #             tttvrl <-
    #               matchPattern("TTTV", reverseComplement(DNAString(ftargetseq())[(200 + input$fpamregiond + 1):(203 + 50)]), fixed = F)
    #           }
    #           if ("LbCpf1 - TYCV"  %in% rvc$data) {
    #             tycvd <-
    #               matchPattern("TYCV", fntspam()[1:(input$fpamregionu + 3)], fixed = F)
    #             tycvr <-
    #               matchPattern("TYCV", reverseComplement(fntspam()[(input$fpamregionu + 1):(input$fpamregionu + 3 + input$fpamregiond)]), fixed = F)
    #             tycvdl <-
    #               matchPattern("TYCV", DNAString(ftargetseq())[204:253], fixed = F)
    #             tycvrl <-
    #               matchPattern("TYCV", reverseComplement(DNAString(ftargetseq())[(200 + input$fpamregiond + 1):(203 + 50)]), fixed = F)
    #           }
    #           if ("LbCpf1 - MCCC"  %in% rvc$data) {
    #             mcccd <-
    #               matchPattern("MCCC", fntspam()[1:(input$fpamregionu + 3)], fixed = F)
    #             mcccr <-
    #               matchPattern("MCCC", reverseComplement(fntspam()[(input$fpamregionu + 1):(input$fpamregionu + 3 + input$fpamregiond)]), fixed = F)
    #             mcccdl <-
    #               matchPattern("MCCC", DNAString(ftargetseq())[204:253], fixed = F)
    #             mcccrl <-
    #               matchPattern("MCCC", reverseComplement(DNAString(ftargetseq())[(200 + input$fpamregiond + 1):(203 + 50)]), fixed = F)
    #           }
    #           if ("AsCpf1 - RATR"  %in% rvc$data) {
    #             ratrd <-
    #               matchPattern("RATR", fntspam()[1:(input$fpamregionu + 3)], fixed = F)
    #             ratrr <-
    #               matchPattern("RATR", reverseComplement(fntspam()[(input$fpamregionu + 1):(input$fpamregionu + 3 + input$fpamregiond)]), fixed = F)
    #             ratrdl <-
    #               matchPattern("RATR", DNAString(ftargetseq())[204:253], fixed = F)
    #             ratrrl <-
    #               matchPattern("RATR", reverseComplement(DNAString(ftargetseq())[(200 + input$fpamregiond + 1):(203 + 50)]), fixed = F)
    #           }
    #           if ("AsCpf1 - TATV"  %in% rvc$data) {
    #             tatvd <-
    #               matchPattern("TATV", fntspam()[1:(input$fpamregionu + 3)], fixed = F)
    #             tatvr <-
    #               matchPattern("TATV", reverseComplement(fntspam()[(input$fpamregionu + 1):(input$fpamregionu + 3 + input$fpamregiond)]), fixed = F)
    #             tatvdl <-
    #               matchPattern("TATV", DNAString(ftargetseq())[204:253], fixed = F)
    #             tatvrl <-
    #               matchPattern("TATV", reverseComplement(DNAString(ftargetseq())[(200 + input$fpamregiond + 1):(203 + 50)]), fixed = F)
    #           }
    #           if ("Other, please specify PAM and gRNA-handle:"  %in% rvc$data) {
    #             otherpamsd <-
    #               matchPattern(input$inp_fpam, fntspam()[1:(input$fpamregionu + 3)], fixed = F)
    #             otherpamsr <-
    #               matchPattern(input$inp_fpam,
    #                            reverseComplement(fntspam()[(input$fpamregionu + 1):(input$fpamregionu + 3 + input$fpamregiond)]),
    #                            fixed = F)
    #             otherpamsdl <-
    #               matchPattern(input$inp_fpam, DNAString(ftargetseq())[204:253], fixed = F)
    #             otherpamsrl <-
    #               matchPattern(input$inp_fpam,
    #                            reverseComplement(DNAString(ftargetseq())[(200 + input$fpamregiond + 1):(200 + input$fpamregiond + 50)]),
    #                            fixed = F)
    #           }
    #           ## ----
    #           allpamd <-
    #             list(tttvd, tycvd, mcccd, ratrd, tatvd, otherpamsd)
    #           if (is.null(tttvd) |
    #               is.null(tycvd) | is.null(mcccd) | is.null(ratrd) |
    #               is.null(tatvd) | is.null(otherpamsd))
    #           {
    #             allpamd <- allpamd[-which(sapply(allpamd, is.null))]
    #           }
    #           if (length(allpamd) == 0) {
    #             allpammd <- list()
    #           } else {
    #             allpammd <- allpamd[[1]]
    #                         if (length(allpamd) > 1) {
    #             for (i in (1:(length(allpamd) - 1))) {
    #               allpammd <- c(allpammd, allpamd[[i + 1]])
    #             }
    #                         }
    #           }
    #           allpamr <-
    #             list(tttvr, tycvr, mcccr, ratrr, tatvr, otherpamsr)
    #           if (is.null(tttvr) |
    #               is.null(tycvr) | is.null(mcccr) | is.null(ratrr) |
    #               is.null(tatvr) | is.null(otherpamsr))
    #           {
    #             allpamr <- allpamr[-which(sapply(allpamr, is.null))]
    #           }
    #           if (length(allpamr) == 0) {
    #             allpammr <- list()
    #           } else {
    #             allpammr <- allpamr[[1]]
    #             if (length(allpamr) > 1) {
    #               for (i in (1:(length(allpamr) - 1))) {
    #                 allpammr <- c(allpammr, allpamr[[i + 1]])
    #               }
    #             }
    #           }
    #           allpamdl <-
    #             list(tttvdl, tycvdl, mcccdl, ratrdl, tatvdl, otherpamsdl)
    #           if (is.null(tttvdl) |
    #               is.null(tycvdl) | is.null(mcccdl) | is.null(ratrdl) |
    #               is.null(tatvdl) | is.null(otherpamsdl))
    #           {
    #             allpamdl <- allpamdl[-which(sapply(allpamdl, is.null))]
    #           }
    #           if (length(allpamdl) == 0) {
    #             allpammdl <- list()
    #           } else {
    #             allpammdl <- allpamdl[[1]]
    #             if (length(allpamdl) > 1) {
    #               for (i in (1:(length(allpamdl) - 1))) {
    #                 allpammdl <- c(allpammdl, allpamdl[[i + 1]])
    #               }
    #             }
    #           }
    #           allpamrl <-
    #             list(tttvrl, tycvrl, mcccrl, ratrrl, tatvrl, otherpamsrl)
    #           if (is.null(tttvrl) |
    #               is.null(tycvrl) | is.null(mcccrl) | is.null(ratrrl) |
    #               is.null(tatvrl) | is.null(otherpamsrl))
    #           {
    #             allpamrl <- allpamrl[-which(sapply(allpamrl, is.null))]
    #           }
    #           if (length(allpamrl) == 0) {
    #             allpammrl <- list()
    #           } else {
    #             allpammrl <- allpamrl[[1]]
    #             if (length(allpamrl) > 1) {
    #               for (i in (1:(length(allpamrl) - 1))) {
    #                 allpammrl <- c(allpammrl, allpamrl[[i + 1]])
    #               }
    #             }
    #           }
    #           
    #           # merge the two datasets in one data frame
    #           if (length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)  == 0) {
    #             fastaresultlist[[j]] <- "No PAM-site found with these settings."
    #             #output$fdownload <- renderText({"Now you can download your oligos."})
    #           } else {
    #             allpamm <-
    #               matrix(
    #                 rep(0, len = (
    #                   length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
    #                 ) * 6),
    #                 length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
    #               )
    #             colnames(allpamm) <-
    #               c("start", "width", "PAM", "strand", "distance", "sort")
    #             allpamm = as.data.frame(allpamm)
    #             if (length(allpammd) > 0) {
    #               for (i in (1:(length(allpammd)))) {
    #                 allpamm$start[i] <- allpammd@ranges@start[i]
    #                 allpamm$sort[i] <- allpammd@ranges@start[i]
    #                 allpamm$width[i] <- allpammd@ranges@width[i]
    #                 allpamm$PAM[i] <- as.character(allpammd[[i]])
    #                 allpamm$strand[i] <- "direct"
    #                 allpamm$distance[i] <-
    #                   input$fpamregionu + 3 - allpammd@ranges@start[i]
    #               }
    #             }
    #             if (length(allpammr)  > 0) {
    #               for (i in (1:length(allpammr))) {
    #                 allpamm$start[i + length(allpammd)] <- allpammr@ranges@start[i]
    #                 allpamm$sort[i + length(allpammd)] <-
    #                   allpammr@ranges@start[i]
    #                 allpamm$width[i + length(allpammd)] <-
    #                   allpammr@ranges@width[i]
    #                 allpamm$PAM[i + length(allpammd)] <-
    #                   as.character(allpammr[[i]])
    #                 allpamm$strand[i + length(allpammd)] <-
    #                   "reverse"
    #                 allpamm$distance[i + length(allpammd)] <-
    #                   input$fpamregiond + 3 - allpammr@ranges@start[i]
    #                 
    #               }
    #             }
    #             if (length(allpammdl) > 0) {
    #               for (i in (1:(length(allpammdl)))) {
    #                 allpamm$start[i + length(allpammd) + length(allpammr)] <-
    #                   allpammdl@ranges@start[i]
    #                 allpamm$sort[i + length(allpammd) + length(allpammr)] <-
    #                   50 - allpammdl@ranges@start[i]
    #                 allpamm$width[i + length(allpammd) + length(allpammr)] <-
    #                   allpammdl@ranges@width[i]
    #                 allpamm$PAM[i + length(allpammd) + length(allpammr)] <-
    #                   as.character(allpammdl[[i]])
    #                 allpamm$strand[i + length(allpammd) + length(allpammr)] <-
    #                   "search space extended - direct*"
    #                 allpamm$distance[i + length(allpammd) + length(allpammr)] <-
    #                   203 - allpammdl@ranges@start[i]
    #                 
    #               }
    #             }
    #             if (length(allpammrl)  > 0) {
    #               for (i in (1:length(allpammrl))) {
    #                 allpamm$start[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
    #                   allpammrl@ranges@start[i]
    #                 allpamm$sort[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
    #                   allpammrl@ranges@start[i]
    #                 allpamm$width[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
    #                   allpammrl@ranges@width[i]
    #                 allpamm$PAM[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
    #                   as.character(allpammrl[[i]])
    #                 allpamm$strand[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
    #                   "search space extended - reverse*"
    #                 allpamm$distance[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
    #                   203 - allpammrl@ranges@start[i]
    #               }
    #             }
    #             # remove sites occuring twice
    #             single <- which(duplicated(allpamm) == F)
    #             allpam_single <- allpamm[single, ]
    #             # rank sites according to proximity to stop
    #             sortedallpam <-
    #               allpam_single[order(-allpam_single$sort), ]
    #             sortedallpam <-
    #               sortedallpam[order(sortedallpam$strand), ]
    #             # compute gRNAs for the 5 best PAM-sites
    #             #if (nrow(sortedallpam) < 5) {
    #             pamnumber <- nrow(sortedallpam)
    #             # } else {
    #             #   pamnumber <- 5
    #             # }
    #             handles <-
    #               unique(subset(endonucleases, select = -c(PAM)))
    #             handles <- handles[order(handles$Name), ]
    #             handle <- rep("handle", times = pamnumber)
    #             grnas <- rep("grna", times = pamnumber)
    #             cpfname <- rep("cpfname", times = pamnumber)
    #             rvcn <- reactiveValues(data = NULL)
    #             rvcn$data <- cpfname
    #             grnahandle <- rep("grnahandle", times = pamnumber)
    #             dim(grnas) <- c(pamnumber, 1)
    #             colnames(grnas) <- "gRNAs"
    #             for (i in (1:pamnumber)) {
    #               if (sortedallpam$strand[i] == "direct") {
    #                 grnastart <-
    #                   sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 - input$fpamregionu
    #                 grnas[i] <-
    #                   as.character(reverseComplement(DNAString(ftargetseq())[(grnastart):(grnastart + 19)]))
    #               } else if (sortedallpam$strand[i] == "reverse") {
    #                 grnastart <-
    #                   sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 * (input$fpamregionu + 3 - sortedallpam$start[i]) + 200 - input$fpamregionu - 2
    #                 grnas[i] <-
    #                   as.character(DNAString(ftargetseq())[(grnastart - 19):(grnastart)])
    #               } else if (sortedallpam$strand[i] == "search space extended - direct*") {
    #                 grnastart <-
    #                   sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203
    #                 grnas[i] <-
    #                   as.character(reverseComplement(DNAString(ftargetseq())[(grnastart):(grnastart + 19)]))
    #               } else if (sortedallpam$strand[i] == "search space extended - reverse*") {
    #                 grnastart <-
    #                   50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 + 203
    #                 grnas[i] <-
    #                   as.character(DNAString(ftargetseq())[(grnastart - 19):(grnastart)])
    #               }
    #             }
    #             
    #             fsummary <-
    #               matrix(rep(0, len = pamnumber * 8), nrow = pamnumber)
    #             colnames(fsummary) <-
    #               c(
    #                 "Rank",
    #                 #"Distance",
    #                 #"Target",
    #                 "Suggested reverse oligo name",
    #                 "Sequence",
    #                 "Length",
    #                 "Cpf1",
    #                 "PAM",
    #                 "Strand",
    #                 "gRNA (handle + spacer)"
    #               )
    #             fsummary <- as.data.frame(fsummary)
    #             for (i in (1:pamnumber)) {
    #               if (nchar("TTTV") == sortedallpam$width[i] &
    #                   countPattern(DNAString(sortedallpam$PAM[i]),
    #                                DNAString("TTTV"),
    #                                fixed = F) > 0 &
    #                   ("LbCpf1 - TTTV"  %in% rvc$data)) {
    #                 cpfname[i] <- "LbCpf1_TTTV"
    #                 handle[i] <-
    #                   gsub("U", "T", as.character(reverseComplement(RNAString(
    #                     handles[handles$Name == "LbCpf1", 2]
    #                   ))))
    #                 grnahandle[i] <-
    #                   gsub("T", "U", reverseComplement(DNAString(handle[i])))
    #               }
    #             }
    #             for (i in (1:pamnumber)) {
    #               if (nchar("TYCV") == sortedallpam$width[i] &
    #                   countPattern(DNAString(sortedallpam$PAM[i]),
    #                                DNAString("TYCV"),
    #                                fixed = F) > 0 &
    #                   ("LbCpf1 - TYCV"  %in% rvc$data)) {
    #                 cpfname[i] <- "LbCpf1_TYCV"
    #                 handle[i] <-
    #                   gsub("U", "T", as.character(reverseComplement(RNAString(
    #                     handles[handles$Name == "LbCpf1", 2]
    #                   ))))
    #                 grnahandle[i] <-
    #                   gsub("T", "U", reverseComplement(DNAString(handle[i])))
    #               }
    #             }
    #             for (i in (1:pamnumber)) {
    #               if (nchar("MCCC") == sortedallpam$width[i] &
    #                   countPattern(DNAString(sortedallpam$PAM[i]),
    #                                DNAString("MCCC"),
    #                                fixed = F) > 0 &
    #                   ("LbCpf1 - MCCC"  %in% rvc$data)) {
    #                 cpfname[i] <- "LbCpf1_MCCC"
    #                 handle[i] <-
    #                   gsub("U", "T", as.character(reverseComplement(RNAString(
    #                     handles[handles$Name == "LbCpf1", 2]
    #                   ))))
    #                 grnahandle[i] <-
    #                   gsub("T", "U", reverseComplement(DNAString(handle[i])))
    #               }
    #             }
    #             for (i in (1:pamnumber)) {
    #               if (nchar("TATV") == sortedallpam$width[i] &
    #                   countPattern(DNAString(sortedallpam$PAM[i]),
    #                                DNAString("TATV"),
    #                                fixed = F) > 0 &
    #                   ("AsCpf1 - TATV"  %in% rvc$data)) {
    #                 cpfname[i] <- "AsCpf1_TATV"
    #                 handle[i] <-
    #                   gsub("U", "T", as.character(reverseComplement(RNAString(
    #                     handles[handles$Name == "AsCpf1", 2]
    #                   ))))
    #                 grnahandle[i] <-
    #                   gsub("T", "U", reverseComplement(DNAString(handle[i])))
    #               }
    #             }
    #             for (i in (1:pamnumber)) {
    #               if (nchar("RATR") == sortedallpam$width[i] &
    #                   countPattern(DNAString(sortedallpam$PAM[i]),
    #                                DNAString("RATR"),
    #                                fixed = F) > 0 &
    #                   ("AsCpf1 - RATR"  %in% rvc$data)) {
    #                 cpfname[i] <- "AsCpf1_RATR"
    #                 handle[i] <-
    #                   gsub("U", "T", as.character(reverseComplement(RNAString(
    #                     handles[handles$Name == "AsCpf1", 2]
    #                   ))))
    #                 grnahandle[i] <-
    #                   gsub("T", "U", reverseComplement(DNAString(handle[i])))
    #               }
    #             }
    #             for (i in (1:pamnumber)) {
    #               if (countPattern(DNAString(sortedallpam$PAM[i]),
    #                                DNAString(input$inp_fpam),
    #                                fixed = F > 0) &
    #                   "Other, please specify PAM and gRNA-handle:"  %in% rvc$data) {
    #                 cpfname[i] <-
    #                   paste("User_defined_Cpf1_", sortedallpam$PAM[i], sep = "")
    #                 handle[i] <-
    #                   gsub("U", "T", as.character(reverseComplement(
    #                     RNAString(input$inp_fhandle)
    #                   )))
    #                 grnahandle[i] <-
    #                   gsub("T", "U", reverseComplement(DNAString(handle[i])))
    #               }
    #             }
    #             for (i in (1:pamnumber)) {
    #               #fsummary$Distance[i] <- format(round(sortedallpam$start[i], 0), nsmall = 0)
    #               fsummary$Cpf1[i] <- cpfname[i]
    #               fsummary$PAM[i] <- paste(sortedallpam$PAM[i])
    #               fsummary$`gRNA (handle + spacer)`[i] <-
    #                 paste(
    #                   as.character(grnahandle[i]),
    #                   gsub("T", "U", reverseComplement(DNAString(grnas[i]))),
    #                   sep = ""
    #                 )
    #               fsummary$`Suggested reverse oligo name`[i] <-
    #                 paste("M2_", cpfname[i], sep = "")
    #               if ((sortedallpam$strand[i] == "direct") |
    #                   (sortedallpam$strand[i] == "reverse")) {
    #                 fsummary$Sequence[i] <-
    #                   paste(
    #                     as.character(reverseComplement(DNAString(
    #                       ftargetseq()
    #                     )[(200 + 4):(200 + 4 + 54)])),
    #                     "AAAAAA",
    #                     grnas[i],
    #                     handle[i],
    #                     "GCTAGCTGCATCGGTACC",
    #                     sep = ""
    #                   )
    #                 fsummary$Strand[i] <-
    #                   paste(as.character(sortedallpam$strand[i]))
    #               }
    #               if (sortedallpam$strand[i] == "search space extended - direct*") {
    #                 fsummary$Strand[i] <- "direct*"
    #                 fsummary$Sequence[i] <-
    #                   paste(
    #                     as.character(reverseComplement(DNAString(
    #                       ftargetseq()
    #                     )[(sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(grnas[i])):(
    #                       sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(grnas[i]) + 54
    #                     )])),
    #                     "AAAAAA",
    #                     grnas[i],
    #                     handle[i],
    #                     "GCTAGCTGCATCGGTACC",
    #                     sep = ""
    #                   )
    #               } else if (sortedallpam$strand[i] == "search space extended - reverse*") {
    #                 fsummary$Strand[i] <- "reverse*"
    #                 fsummary$Sequence[i] <-
    #                   paste(
    #                     as.character(reverseComplement(DNAString(
    #                       ftargetseq()
    #                     )[(203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2):(203 + 50 - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + 54)])),
    #                     "AAAAAA",
    #                     grnas[i],
    #                     handle[i],
    #                     "GCTAGCTGCATCGGTACC",
    #                     sep = ""
    #                   )
    #               }
    #               
    #               fsummary$Length[i] <-
    #                 paste(format(round(nchar(
    #                   paste(
    #                     as.character(reverseComplement(DNAString(
    #                       ftargetseq()
    #                     )[(200 + 4):(200 + 4 + 54)])),
    #                     "AAAAAA",
    #                     grnas[i],
    #                     handle[i],
    #                     "GCTAGCTGCATCGGTACC",
    #                     sep = ""
    #                   )
    #                 ), 0), nsmall = 0), " nts", sep = "")
    #               
    #               if ((sortedallpam$strand[i] == "search space extended - reverse*") |
    #                   (sortedallpam$strand[i] == "search space extended - direct*")) {
    #                 fsummary$Rank[i] <- paste0(as.character(i), "*")
    #               } else {
    #                 fsummary$Rank[i] <- paste0(as.character(i))
    #               }
    #               
    #             }
    #             fsummary[nrow(fsummary) + 1,] <-
    #               c("", "", "", "", "", "", "", "")
    #             fsummary[nrow(fsummary) + 1,] <-
    #               c(
    #                 "Forward oligo",
    #                 paste0("M1_sequence", j),
    #                 as.character(fforwardoligo()),
    #                 paste(nchar(as.character(
    #                   fforwardoligo()
    #                 )), "nts"),
    #                 "",
    #                 "",
    #                 #"",
    #                 "",
    #                 ""
    #               )
    #             fastaresultlist[[j]] <- fsummary
    #             
    #             }
    # 
    #           capture.output(fastaresultlist, file = "fastaresultlist.csv")
    #           
    #           
    #           #rvo$data <- as.data.frame(fastaresultlist)
    #           #rv$data <- fastaresultlist
    #       
    #           })
    #     }
    #   }
    #     #}  
    #     
    #   
    #   observeEvent(input$fcompute, {
    #   enable("fdownloadcsv")
    #   enable("fdownloadxlsx")
    #   output$fdownload <- renderText({"Now you can download your oligos."})
    #   
    #   })
    # 
    #   output$fdownloadcsv <- downloadHandler(
    #           filename = paste0("PCR_targeting_oligos"),
    #           content = function(file) {
    #             write.csv(rvo$data, file, row.names = FALSE)
    #           }
    #         )
    #         # output$fdownloadxlsx <- downloadHandler(
    #         #   filename = paste0("PCR_targeting_oligos.xlsx"),
    #         #                     content = function(file) {
    #         #                     tempFile <- tempfile(fileext = ".xlsx")
    #         #                     write_xlsx(rvo$data, tempFile)
    #         #                     file.rename(tempFile, file)
    #         #                     }
    #         #   )
    #         #                     output$freport <- downloadHandler(
    #         #                     filename = function() {
    #         #                     'Mammalian_PCR_tagging_report.pdf'
    #         #                     },
    #         #                     
    #         #                     content = function(file) {
    #         #                     src <- normalizePath('report.Rmd')
    #         #                     
    #         #                     # temporarily switch to the temp dir, in case you do not have write
    #         #                     # permission to the current working directory
    #         #                     owd <- setwd(tempdir())
    #         #                     on.exit(setwd(owd))
    #         #                     file.copy(src, 'report.Rmd', overwrite = TRUE)
    #         #                     library(rmarkdown)
    #         #                     out <- render('report.Rmd')
    #         #                     file.rename(out, file)
    #         #                     }
    #         #                     )
    #                             
    # 
    # })
    
    
}
# Run the app ----
shinyApp(ui = ui, server = server)