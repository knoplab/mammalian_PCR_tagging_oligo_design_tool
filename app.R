library(shiny)
library(shinyjs)
library(writexl)
library(Biostrings)
# Single target sequence input UI
# Input of target sequence
panel.target <- fluidPage(
  h5(
    "Please insert target sequence the following way: 200 nts before and 200 nts after the stop codon (403 nts per target)."
  ),
  
  h4(uiOutput("inp_target_text")),
  br(),
  fluidRow(
    textAreaInput(
      "inp_target",
      label = NULL,
      width = 800,
      rows = 5,
      resize = "both",
      placeholder = paste0(paste(rep("N", 200), collapse = ""), "TAA", paste(rep("N", 200), collapse = ""))
    ),
    tags$style("#inp_target{font-family: Roboto Mono}")
  ),
  h5(textOutput("inputlength"), style = "color:red"),
  h5(textOutput("inputnuc"), style = "color:red"),
  br(),
  actionButton("example", label = "Example"),
  br(),
  br(),
  h4(uiOutput("inp_target_seq")),
  div(
    style = "width:800px;",
    fluidRow(verbatimTextOutput("targetsequence")),
    tags$head(
      tags$style(
        "#targetsequence{background: ghostwhite; font-family: Roboto Mono}"
      )
    )
  ),
  br()
  ,
  fluidRow(column(
    width = 4,
    h4("Gene name:"),
    h5(em("Will be used to name the oligos."), style = "color:grey")
  ),
  column(
    width = 1,
    textInput(
      "genename",
      label = NULL,
      width = 100,
      value = "GENE"
    )
  )),
  fluidRow(
    column(
      width = 4,
      h4("Search space for PAM sites before the insertion site:"),
      h5(
        em(
          "Number of nucleotides upstream of the insertion sites to search for PAM sites on the direct strand."
        ),
        style = "color:grey"
      )
    ),
    column(
      width = 1,
      numericInput(
        "pamregionu",
        label = NULL,
        value = 17,
        min = 3,
        max = 20,
        width = 100
      )
    ),
    tags$head(tags$style(".container-fluid {width: 2000px;}"))
  ),
  # fluidPage(column(
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
  fluidRow(column(
    width = 4,
    h4("Search space for PAM sites after the insertion site:"),
    h5(
      em(
        "Number of nucleotides downstream of the stop codon to look for PAM sites on the reverse strand."
      ),
      style = "color:grey"
    )
  ),
  column(
    width = 1,
    numericInput(
      "pamregiond",
      label = NULL,
      value = 17,
      min = 3,
      max = 50,
      width = 100
    )
  ))
)
panel.apply <-
  fluidPage(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css?family=Roboto+Mono"),
    br(),
    h4(em("Region around the stop codon:")),
    h5(em("Search space for PAM-sites based on user inputs"), style = "color:coral"),
    h5(em("Extended search space*"), style = "color:peru"),
    tags$style(HTML(
      '#nucleotides {font-family: "Roboto Mono"}'
    )),
    tags$style(HTML(
      '#nucleotidesr {font-family: "Roboto Mono"}'
    )),
    htmlOutput("nucleotides"),
    htmlOutput("nucleotidesr"),
    br(),
    h5(textOutput("stopwarning"), style = "color:red"),
    #textOutput("fasta"),
    actionButton("apply", label = "Show sequence")
  )
# Select CASTLING moduls
panel.moduls <- fluidPage(fluidRow(
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
      choices = c(
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
    #column(3, verbatimTextOutput("cpfchosen"))
  ),
  column(
    width = 3,
    fluidPage(
      #tags$style(HTML('#endo {font-family: "Roboto Mono"}')),
      #tags$style(HTML('#endo th {font-family: "Arial"}')),
      #tableOutput("endo"),
      h4("Plasmids for co-transfection:"),
      uiOutput("lbwlink"),
      uiOutput("lblink"),
      uiOutput("aslink2"),
      uiOutput("aslink"),
      br(),
      uiOutput("handlelink"),
      br(),
      uiOutput("iupaclink")
      
    )
  )
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
  column(width = 2, uiOutput("pamhandlelink"))))
#h5(textOutput("handlecontrol"), style = "color:red"))
panel.output <-            fluidPage(
  tags$style(HTML(
    '#forwardoligo {font-family: "Roboto Mono"}'
  )),
  htmlOutput("forwardoligo"),
  tags$style(HTML('#forwardoligo th {font-family: "Arial"}')),
  htmlOutput("reversetitle"),
  br(),
  tags$style(HTML('#cpf {font-family: "Roboto Mono"}')),
  tags$style(HTML('#cpf th {font-family: "Arial"}')),
  htmlOutput("cpf"),
  br()
)
panel.download <- fixedPage(
  downloadButton("downloadxlsx", "Download results (.xlsx)"),
  
  downloadButton("downloadcsv", "Download results (.csv)"),
  #br(),
  #br(),
  #downloadButton("report", "PDF report"),
  br(),
  br()
)

# FASTA UI

panel.ftarget <- fixedPage(
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
  titlePanel(
    strong("Primer Design Tool for PCR-tagging in Mammalian Cells [TEST]")
  ),
  h4(
    "This webpage provides an oligonucleotide design tool. Publication:"
  ),
  br(),
  h4(em(
    "Upload of multiple DNA sequences in FASTA format"
  )),
  
  fluidPage(
    h5(
      "Please provide each target sequence the following way: 200 nts before and 200 nts after the stop codon (403 nts per target)."
    ),
    br(),
    tags$div(title = "FASTA format",
             fileInput(
               "inp_fasta",
               label = NULL,
               width = 600,
               accept =
             )),
    h5(textOutput("finputlength"), style = "color:red"),
    h5(textOutput("finputnuc"), style = "color:red"),
    br(),
    fluidRow(column(
      width = 4,
      h4("Gene name:"),
      h5(em("Will be used to name the oligos."), style = "color:grey")
    ),
    column(
      width = 1,
      textInput(
        "fgenename",
        label = NULL,
        width = 100,
        value = "GENE"
      )
    )),
    fluidRow(
      column(
        width = 4,
        h4("Search space for PAM sites before the insertion site:"),
        h5(
          em(
            "Number of nucleotides upstream of the insertion sites to search for PAM sites on the direct strand."
          ),
          style = "color:grey"
        )
      ),
      column(
        width = 1,
        numericInput(
          "fpamregionu",
          label = NULL,
          value = 17,
          min = 3,
          max = 20,
          width = 100
        )
      ),
      tags$head(tags$style(".container-fluid {width: 2000px;}"))
    ),
    fluidRow(column(
      width = 4,
      h4("Search space for PAM sites after the insertion site:"),
      h5(
        em(
          "Number of nucleotides downstream of the stop codon to look for PAM sites on the reverse strand."
        ),
        style = "color:grey"
      )
    ),
    column(
      width = 1,
      numericInput(
        "fpamregiond",
        label = NULL,
        value = 17,
        min = 3,
        max = 50,
        width = 100
      )
    ))
  )
  )
panel.fmoduls <- fluidPage(fluidRow(
  column(
    width = 3,
    h4("Select Cpf1 variant(s) and PAM-site(s):"),
    checkboxGroupInput(
      "inp_fcpf",
      label = NULL,
      width = "100%",
      choices = c(
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
  ),
  column(
    width = 3,
    fluidPage(
      h4("Plasmids for co-transfection:"),
      uiOutput("flbwlink"),
      uiOutput("flblink"),
      uiOutput("faslink2"),
      uiOutput("faslink"),
      br(),
      uiOutput("fhandlelink"),
      br(),
      uiOutput("fiupaclink")
      
    )
  )
),
bootstrapPage(
  div(
    style = "display:inline-block",
    textInput(
      inputId = "inp_fpam",
      label = NULL,
      placeholder = "PAM",
      width = "50%"
    )
  ),
  div(
    style = "display:inline-block",
    textInput(
      inputId = "inp_fhandle",
      label = NULL,
      placeholder = "HANDLE",
      width = "100%"
    )
  )
),
fluidRow(column(width = 2, uiOutput("fpamhandlelink"))))
panel.fdownload <- fixedPage(
  downloadButton("fdownloadxlsx", "Download results (.xlsx)"),
  
  downloadButton("fdownloadcsv", "Download results (.csv)"),
  #br(),
  #br(),
  #downloadButton("report", "PDF report"),
  br(),
  br()
)
# Define UI ----
ui <-
  fluidPage(
    title = "Mammalian PCR-targeting",
    navbarPage(
      title = "Mammalian PCR-targeting",
      tabPanel(
        title = "Single DNA sequence",
        
        fixedPage(
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
          titlePanel(
            strong("Primer Design Tool for PCR-tagging in Mammalian Cells [TEST]")
          ),
          h4(
            "This webpage provides an oligonucleotide design tool. Publication:"
          ),
          br(),
          h4(em("Single DNA sequence input")),
          panel.target,
          panel.apply,
          hr(),
          panel.moduls,
          br(),
          actionButton("compute", label = "Get oligos"),
          br(),
          br()
          ),
        panel.output,
        panel.download,
        br()
        
        
        ),
      
      tabPanel(
        title = "FASTA input",
        
        fixedPage(
          panel.ftarget,
          br(),
          actionButton("fcompute", label = "Get oligos"),
          br(),
          panel.fmoduls,
          br(),
          panel.fdownload
        )
      ),
      
      tabPanel(title = "PCR",
               
               h4("Protocol")),
      
      tabPanel(title = "About",
               
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
      "UAAUUUCUACUCUUGUAGAU"  # AsCpf1
    ),
    stringsAsFactors = F
  )
  handletable <- data.frame(
    Name    = c("LbCpf1",
                "AsCpf1"),
    `gRNA_handle`  = c("UAAUUUCUACUAAGUGUAGAU",
                       # LbCpf1
                       "UAAUUUCUACUCUUGUAGAU"  # AsCpf1
                       ),
                       stringsAsFactors = F
    )
    # output$endo <- renderTable({
    #   handletable
    # })
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
    urlas <-
      a("pcDNA3.1-hAsCpf1(TYCV) (pY210)",
        href = "https://www.addgene.org/89351/",
        target = "_blank")
    output$aslink <- renderUI({
      tagList("", urlas)
    })
    output$faslink <- renderUI({
      tagList("", urlas)
    })
    urlas2 <-
      a("pcDNA3.1-hAsCpf1(TATV) (pY220)",
        href = "https://www.addgene.org/89353/",
        target = "_blank")
    output$aslink2 <- renderUI({
      tagList("", urlas2)
    })
    output$faslink2 <- renderUI({
      tagList("", urlas2)
    })
    urllb <-
      a("pcDNA3.1-hLbCpf1(TYCV) (pY230)",
        href = "https://www.addgene.org/89355/",
        target = "_blank")
    output$lblink <- renderUI({
      tagList("", urllb)
    })
    output$flblink <- renderUI({
      tagList("", urllb)
    })
    urllbw <-
      a("pcDNA3.1-hLbCpf1(TTTV) (pY016)",
        href = "https://www.addgene.org/69988/",
        target = "_blank")
    output$lbwlink <- renderUI({
      tagList("", urllbw)
    })
    output$flbwlink <- renderUI({
      tagList("", urllbw)
    })
    
    # find possible PAM-sequences around the stop codon depending on the Cpf1 chosen
    aroundpam <- reactive({
      function(targetinput) {
        (DNAString(targetinput))[(200 - input$pamregionu + 1):(200 + 3 + input$pamregiond)]
      }
    })
    aroundstop <- reactive({
      function(targetinput) {
        (DNAString(targetinput))[(200 - input$pamregionu - 10 + 1):(200 + 3 + input$pamregiond + 10)]
      }
    })
    # Single DNA sequence input
    disableActionButton("compute", session)
    disableActionButton("apply", session)
    disable("inp_pam")
    disable("inp_handle")
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
    output$inp_target_seq <- renderUI({
      if (nchar(targetseq()) != 0) {
        em(paste0("Target sequence (", nchar(targetseq()), " nts):"))
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
        disableActionButton("apply", session)
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
        disableActionButton("apply", session)
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
        disableActionButton("apply", session)
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
        enableActionButton("apply", session)
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
          input$apply
          input$pamregionu
          input$pamregiond
        }, {
          subsetnts <- aroundpam()(targetseq())
          return(subsetnts)
        })
        ntsstop <- eventReactive({
          input$apply
          input$pamregionu
          input$pamregiond
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
        regiond <- renderText({
          if (input$apply == 0)
            return("")
          isolate({
            paste(
              as.character(ntsstop()[1:10]),
              '<span style = "color:coral">',
              as.character(ntsstop()[11:(input$pamregionu + 10)]),
              '</span>',
              strong(substr(targetseq(), 201, 203)),
              '<span style = "color:peru">',
              substr(targetseq(), 204, 253),
              '</span>',
              substr(targetseq(), 254, 260),
              " - direct strand",
              sep = ""
            )
          })
        })
        output$nucleotides <- regiond
        regionr <- renderText({
          if (input$apply == 0)
            return("")
          isolate({
            paste(
              as.character(complement(ntsstop()[1:10])),
              as.character(complement(ntsstop()[11:(input$pamregionu + 10)])),
              strong(as.character(complement(
                ntsstop()[(input$pamregionu + 10 + 1):(input$pamregionu + 10 + 3)]
              ))),
              '<span style = "color:coral">',
              as.character(complement(ntsstop()[(input$pamregionu + 3 + 10 + 1):(input$pamregionu + 3 + 10 + input$pamregiond)])),
              '</span>',
              '<span style = "color:peru">',
              as.character(complement(DNAString(targetseq())[(203 + input$pamregiond + 1):253])),
              '</span>',
              as.character(complement(DNAString(targetseq())[254:260])),
              " - reverse strand",
              sep = ""
            )
          })
        })
        output$nucleotidesr <- regionr
        observeEvent(input$apply, {
          if ((as.character(ntsstop()[(input$pamregionu + 10 + 1):(input$pamregionu + 10 + 3)]) == "TAA") |
              (as.character(ntsstop()[(input$pamregionu + 10 + 1):(input$pamregionu + 10 + 3)]) == "TAG") |
              (as.character(ntsstop()[(input$pamregionu + 10 + 1):(input$pamregionu + 10 + 3)]) == "TGA")) {
            output$stopwarning <- renderText({
              ""
            })
            
          } else {
            output$stopwarning <-
              renderText({
                "Warning! The insertion site (marked in bold) is not a stop codon."
              })
          }
        })
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
                       (nchar(input$inp_pam)) > 2 & (nchar(input$inp_handle) > 10)) {
                enableActionButton("compute", session)
                output$pamhandlelink <-               renderText({
                  ""
                })
              } else {
                disableActionButton("compute", session)
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
            output$forwardoligo <- renderTable({
              validate(need(input$inp_target != "", ''))
              validate(need(nchar(input$inp_pam) > 2, ''))
              validate(need(all(
                strsplit(input$inp_pam, "")[[1]] %in% DNA_ALPHABET
              ), ''))
              validate(need(nchar(input$inp_handle) > 10, ''))
              validate(need(all(
                strsplit(input$inp_handle, "")[[1]] %in% RNA_ALPHABET
              ), ''))
              rvp$data
            }, sanitize.text.function = function(x)
              x)
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
            output$forwardoligo <- renderTable({
              validate(need(input$inp_target != "", ''))
              validate(need(input$inp_cpf != "", ''))
              rvp$data
            }, sanitize.text.function = function(x)
              x)
          }
        })
        observeEvent(input$inp_cpf, {
          rvc$data <- input$inp_cpf
        })
        observeEvent(input$compute, {
          if ("LbCpf1 - TTTV"  %in% rvc$data) {
            tttvd <-
              matchPattern("TTTV", ntspam()[1:(input$pamregionu + 3)], fixed = F)
            tttvr <-
              matchPattern("TTTV", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F)
            tttvdl <-
              matchPattern("TTTV", DNAString(targetseq())[204:253], fixed = F)
            tttvrl <-
              matchPattern("TTTV", reverseComplement(DNAString(targetseq())[(200 + input$pamregiond + 1):(203 + 50)]), fixed = F)
          }
          if ("LbCpf1 - TYCV"  %in% rvc$data) {
            tycvd <-
              matchPattern("TYCV", ntspam()[1:(input$pamregionu + 3)], fixed = F)
            tycvr <-
              matchPattern("TYCV", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F)
            tycvdl <-
              matchPattern("TYCV", DNAString(targetseq())[204:253], fixed = F)
            tycvrl <-
              matchPattern("TYCV", reverseComplement(DNAString(targetseq())[(200 + input$pamregiond + 1):(203 + 50)]), fixed = F)
          }
          if ("LbCpf1 - MCCC"  %in% rvc$data) {
            mcccd <-
              matchPattern("MCCC", ntspam()[1:(input$pamregionu + 3)], fixed = F)
            mcccr <-
              matchPattern("MCCC", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F)
            mcccdl <-
              matchPattern("MCCC", DNAString(targetseq())[204:253], fixed = F)
            mcccrl <-
              matchPattern("MCCC", reverseComplement(DNAString(targetseq())[(200 + input$pamregiond + 1):(203 + 50)]), fixed = F)
          }
          if ("AsCpf1 - RATR"  %in% rvc$data) {
            ratrd <-
              matchPattern("RATR", ntspam()[1:(input$pamregionu + 3)], fixed = F)
            ratrr <-
              matchPattern("RATR", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F)
            ratrdl <-
              matchPattern("RATR", DNAString(targetseq())[204:253], fixed = F)
            ratrrl <-
              matchPattern("RATR", reverseComplement(DNAString(targetseq())[(200 + input$pamregiond + 1):(203 + 50)]), fixed = F)
          }
          if ("AsCpf1 - TATV"  %in% rvc$data) {
            tatvd <-
              matchPattern("TATV", ntspam()[1:(input$pamregionu + 3)], fixed = F)
            tatvr <-
              matchPattern("TATV", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F)
            tatvdl <-
              matchPattern("TATV", DNAString(targetseq())[204:253], fixed = F)
            tatvrl <-
              matchPattern("TATV", reverseComplement(DNAString(targetseq())[(200 + input$pamregiond + 1):(203 + 50)]), fixed = F)
          }
          if ("Other, please specify PAM and gRNA-handle:"  %in% rvc$data) {
            otherpamsd <-
              matchPattern(input$inp_pam, ntspam()[1:(input$pamregionu + 3)], fixed = F)
            otherpamsr <-
              matchPattern(input$inp_pam,
                           reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]),
                           fixed = F)
            otherpamsdl <-
              matchPattern(input$inp_pam, DNAString(targetseq())[204:253], fixed = F)
            otherpamsrl <-
              matchPattern(input$inp_pam,
                           reverseComplement(DNAString(targetseq())[(200 + input$pamregiond + 1):(200 + input$pamregiond + 50)]),
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
                ) * 5),
                length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
              )
            colnames(allpamm) <-
              c("start", "width", "PAM", "strand", "distance")
            allpamm = as.data.frame(allpamm)
            if (length(allpammd) > 0) {
              for (i in (1:(length(allpammd)))) {
                allpamm$start[i] <- allpammd@ranges@start[i]
                allpamm$width[i] <- allpammd@ranges@width[i]
                allpamm$PAM[i] <- as.character(allpammd[[i]])
                allpamm$strand[i] <- "direct"
                allpamm$distance[i] <-
                  input$pamregionu + 3 - allpammd@ranges@start[i]
              }
            }
            if (length(allpammr)  > 0) {
              for (i in (1:length(allpammr))) {
                allpamm$start[i + length(allpammd)] <- allpammr@ranges@start[i]
                allpamm$width[i + length(allpammd)] <-
                  allpammr@ranges@width[i]
                allpamm$PAM[i + length(allpammd)] <-
                  as.character(allpammr[[i]])
                allpamm$strand[i + length(allpammd)] <-
                  "reverse"
                allpamm$distance[i + length(allpammd)] <-
                  input$pamregiond + 3 - allpammr@ranges@start[i]
                
              }
            }
            if (length(allpammdl) > 0) {
              for (i in (1:(length(allpammdl)))) {
                allpamm$start[i + length(allpammd) + length(allpammr)] <-
                  allpammdl@ranges@start[i]
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
            allpam_single <- allpamm[single,]
            # rank sites according to proximity to stop
            sortedallpam <-
              allpam_single[order(-allpam_single$start),]
            sortedallpam <-
              sortedallpam[order(sortedallpam$strand),]
            # compute gRNAs for the 5 best PAM-sites
            #if (nrow(sortedallpam) < 5) {
            pamnumber <- nrow(sortedallpam)
            # } else {
            #   pamnumber <- 5
            # }
            handles <-
              unique(subset(endonucleases, select = -c(PAM)))
            handles <- handles[order(handles$Name),]
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
                  sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 - input$pamregionu
                grnas[i] <-
                  as.character(reverseComplement(DNAString(targetseq())[(grnastart):(grnastart + 19)]))
              } else if (sortedallpam$strand[i] == "reverse") {
                grnastart <-
                  sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 * (input$pamregionu + 3 - sortedallpam$start[i]) + 200 - input$pamregionu - 2
                grnas[i] <-
                  as.character(DNAString(targetseq())[(grnastart - 19):(grnastart)])
              } else if (sortedallpam$strand[i] == "search space extended - direct*") {
                grnastart <-
                  sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203
                grnas[i] <-
                  as.character(reverseComplement(DNAString(targetseq())[(grnastart):(grnastart + 19)]))
              } else if (sortedallpam$strand[i] == "search space extended - reverse*") {
                grnastart <-
                  sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 203
                grnas[i] <-
                  as.character(DNAString(targetseq())[(grnastart - 19):(grnastart)])
              }
            }
            
            summary <-
              matrix(rep(0, len = pamnumber * 10), nrow = pamnumber)
            colnames(summary) <-
              c(
                "Rank",
                "Distance",
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
              summary$Distance[i] <- format(round(sortedallpam$start[i], 0), nsmall = 0)
              summary$Cpf1[i] <- cpfname[i]
              summary$PAM[i] <- paste(sortedallpam$PAM[i])
              summary$`gRNA (handle + spacer)`[i] <-
                paste(
                  '<span style = "color:red">',
                  as.character(grnahandle[i]),
                  '</span>',
                  '<span style = "color:blue">',
                  gsub("T", "U", reverseComplement(DNAString(grnas[i]))),
                  '</span>',
                  sep = ""
                )
              summary$`Suggested name`[i] <-
                paste("M2_", rvg$data, "_", cpfname[i], sep = "")
              if ((sortedallpam$strand[i] == "direct") |
                  (sortedallpam$strand[i] == "reverse")) {
                summary$Sequence[i] <-
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
                summary$Strand[i] <-
                  paste(as.character(sortedallpam$strand[i]))
              }
              if (sortedallpam$strand[i] == "search space extended - direct*") {
                summary$Strand[i] <- "direct*"
                summary$Sequence[i] <-
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
                summary$Strand[i] <- "reverse*"
                summary$Sequence[i] <-
                  paste(
                    '<span style = "color:green">',
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203):(sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + 54)])),
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
              
              summary$Length[i] <-
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
              stopstart <- (input$pamregionu + 10 + 1)
              stopend <- (input$pamregionu + 10 + 3)
              if (as.character(sortedallpam$strand[i] == "direct")) {
                regionds <- paste0(
                  as.character(ntsstop()[1:(input$pamregionu + 10)]),
                  '<span style = "font-weight:bold">',
                  (as.character(ntsstop()[(input$pamregionu + 10 + 1):(input$pamregionu + 3 + 10)])),
                  '</span>',
                  as.character(ntsstop()[(input$pamregionu + 3 + 10 + 1):nchar(ntsstop())])
                )
                regiondr <- paste0(
                  as.character(complement(ntsstop()[1:(input$pamregionu + 10)])),
                  strong(as.character(complement(
                    ntsstop()[(input$pamregionu + 10 + 1):(input$pamregionu + 10 + 3)]
                  ))),
                  as.character(complement(ntsstop()[(input$pamregionu + 3 + 10 + 1):nchar(ntsstop())]))
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
                    '<span style = "color:white">',"_", '</span>',
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
                pamend <- 10 + input$pamregionu
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
                summary$Target[i] <-
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
                      '<span style = "color:white">',"_", '</span>',
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
                    as.character(complement(ntsstop()[1:(nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]) - 3)])),
                    '<span style = "color:white">',"_", '</span>',
                    '<span style = "text-decoration: underline">',
                    as.character(complement(ntsstop()[(nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]) - 2):(nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]))])),
                    '</span>'
                  )
                bluerr <-
                  paste0('<span style = "color:blue; text-decoration: underline">',
                         as.character(complement(ntsstop()[(
                           nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]) + 1
                         ):(
                           nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i])  - nchar(grnas[i]) + 2
                         )])),
                         '</span>')
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
                summary$Target[i] <-
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
                      '<span style = "color:white">',"_", '</span>',
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
                summary$Target[i] <- paste(paste0(
                  substr(targetseq(), 195, 201),
                  strong(substr(targetseq(), 201, 203)),
                  substr(targetseq(), (203 + 1), (203 + sortedallpam$start[i] - 1)),
                  '<span style = "color:peru">',
                  substr(targetseq(), (203 + sortedallpam$start[i]), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) - 1) ),
                  '</span>',
                  '<span style = "color:blue">',
                  substr(targetseq(), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i])), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 17) ),
                  '</span>',
                  '<sup>&#9660;</sup>',
                  '<span style = "text-decoration: underline">',
                  '<span style = "color:blue">',
                  substr(targetseq(), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 18), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 19) ),
                  '</span>',
                  substr(targetseq(), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 20), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 22) ),
                  '</span>',
                  '<span style = "color:white">',"_", '</span>',
                  substr(targetseq(), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 23), (203 + input$pamregiond + 75) )
                ),
                  paste0(
                    complement(DNAString(substr(targetseq(), 195, 201))),
                    strong(complement(DNAString(substr(targetseq(), 201, 203)))),
                    complement(DNAString(substr(targetseq(), (203 + 1), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 17))) ),
                    '<span style = "color:white">',"_", '</span>',
                    '<span style = "text-decoration: underline">',
                    complement(DNAString(substr(targetseq(), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 18), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 22))) ),
                    '</span>',
                    '<sub>&#9650;</sub>',
                    complement(DNAString(substr(targetseq(), (203 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 23), (203 + input$pamregiond + 75))) )
                  ),
                  sep = "\n")
              }
              else if (sortedallpam$strand[i] == "search space extended - reverse*") {
                blackr <- complement(DNAString(substr(targetseq(), 195, 201)))
                peru <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1), (203 + input$pamregiond + sortedallpam$start[i]))))
                blackf <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] + 1), (203 + input$pamregiond + 75))))
                if ((sortedallpam$start[i] + input$pamregiond ) > (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <- strong(complement(DNAString(substr(targetseq(), 201, 203))))
                  blackrr <- paste0(complement(DNAString(substr(targetseq(), 204, (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 23)))), '<span style = "color:white">',"_", '</span>',
                                    '<span style = "text-decoration: underline">', complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 22), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 20)))), '</span>')
                  bluebold <- ""
                  bluer <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 19), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 18))))
                  blue <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 17), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i])))))
                }
                # if (sortedallpam$start[i] == 7) {
                #   blackbold <- strong(complement(DNAString(substr(targetseq(), 201, 203))))
                #   bluebold <- ""
                #   blackrr <- ""
                #   bluer <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 19), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 18))))
                #   blue <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 17), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i])))))
                # }
                # if (sortedallpam$start[i] == 6) {
                #   blackbold <- strong(complement(DNAString(substr(targetseq(), 201, 202))))
                #   bluebold <- complement(DNAString(substr(targetseq(), 203, 203)))
                #   blackrr <- ""
                #   bluer <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 18), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 18))))
                #   blue <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 17), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i])))))
                # }
                # if (sortedallpam$start[i] == 5) {
                #   blackbold <- strong(complement(DNAString(substr(targetseq(), 201, 201))))
                #   bluebold <- complement(DNAString(substr(targetseq(), 202, 203)))
                #   blackrr <- ""
                #   bluer <- ""
                #   blue <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 17), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i])))))
                # }
                # if (sortedallpam$start[i] == 4) {
                #   blackbold <- ""
                #   bluebold <- complement(DNAString(substr(targetseq(), 202, 203)))
                #   blackrr <- ""
                #   bluer <- paste0('<span style = "font-weight:bold">', complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 18), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 18)))), '</span>')
                #   blue <- complement(DNAString(substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 17), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i])))))
                # }
                
                summary$Target[i] <- paste(paste0(
                  substr(targetseq(), 195, 201),
                  strong(substr(targetseq(), 201, 203)),
                  substr(targetseq(), 204, (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 23)),
                  '<sup>&#9660;</sup>',
                  '<span style = "text-decoration: underline">',
                  substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 22), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 18)),
                  '</span>',
                  '<span style = "color:white">',"_", '</span>',
                  substr(targetseq(), (203 + input$pamregiond + sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 17), (203 + input$pamregiond + 75))
                ),
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
                sep = "\n")
              }
                            summary$Rank[i] <- paste(as.character(i))
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
                       cpfname[i],
                       "_",
                       sortedallpam$PAM[i])
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
              csvoutput$Target[i] <-
                paste(as.character(ntspam()),
                      as.character(complement(ntspam())),
                      sep = "\n")
              csvoutput$Rank[i] <- as.character(i)
            }
            colnames(summary)[colnames(summary) == "gRNA (handle + spacer)"] <-
              paste(
                "gRNA (",
                '<span style = "color:red">',
                "Cpf1-variant-specific handle",
                '</span>',
                " + ",
                '<span style = "color:blue">',
                "spacer",
                '</span>',
                ")",
                sep = ""
              )
            colnames(summary)[colnames(summary) == "Target"] <-
              paste(
                "Target direct and reverse strand with ",
                '<span style = "color:coral">',
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
                sep = ""
              )
            colnames(summary)[colnames(summary) == "Sequence"] <-
              paste(
                "Sequence: ",
                '<span style = "color:green">',
                "3'-homology (55 bases after the stop-codon, reverse orientation)",
                '</span>',
                " + ",
                '<span style = "color:orange">',
                "TTTTTT-complement",
                '</span>',
                " + ",
                '<span style = "color:blue">',
                "gRNA spacer coding sequence",
                '</span>',
                " + ",
                '<span style = "color:red">',
                "gRNA handle coding sequence",
                '</span>',
                " + primer bindig reverse oligo",
                sep = ""
              )
            rv$data <- summary
            csvoutput[nrow(csvoutput) + 1, ] <-
              c("", "", "", "", "", "", "", "", "", "")
            csvoutput[nrow(csvoutput) + 1, ] <-
              c(
                "Forward oligo",
                "",
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
                "5'-homology (90 bases before the stop-codon, direct orientation)",
                '</span>',
                "+ primer binding forward oligo"
              )
            rvp$data <- summaryf
            output$reversetitle <-
              renderText({
                paste(strong("Possible reverse oligos:"))
              })
            # paste(strong("Forward oligo:"), "5'-homology (90 bases before the stop-codon, direct orientation)", as.character(forwardoligo()), sep = "\n")
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
            output$forwardoligo <- renderTable({
              validate(need(input$inp_target != "", ''))
              validate(need(input$inp_cpf != "", ''))
              rvp$data
            }, sanitize.text.function = function(x)
              x)
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
    })
    ##### FASTA
    disableActionButton("fcompute", session)
    disable("inp_fastapam")
    disable("inp_fastahandle")
    observe({
      if ("Other, please specify PAM and gRNA-handle:" %in% input$finp_cpf) {
        enable("inp_fpam")
        enable("inp_fhandle")
        
      } else {
        disable("inp_fpam")
        disable("inp_fhandle")
      }
    })
    disable("fdownloadcsv")
    disable("fdownloadxlsx")
    
    observeEvent(input$inp_fasta, {
      fastaread <- reactive({
        if (is.null(input$inp_fasta))
          return(NULL)
        fastafile <- input$inp_fasta
        file.rename(fastafile$datapath,
                    file.path(dirname(fastafile$datapath),
                              fastafile$name))
        # returns a DNAStringSet object:
        readDNAStringSet(filepath = file.path(dirname(fastafile$datapath), fastafile$name))
      })
      ftargetseq <- reactive({
        ntst.tmp <- as.character(fastaread())
        return(ntst.tmp)
      })
      if ((nchar(ftargetseq()) != 200 + 200 + 3) &
          (nchar(ftargetseq()) > 0) &
          (all(strsplit(ftargetseq(), "")[[1]] %in% DNA_ALPHABET))) {
        disableActionButton("fcompute", session)
        #disableActionButton("fapply", session)
        output$finputlength <-
          renderText({
            "Please provide a sequence of proper length."
          })
        output$finputnuc <-
          renderText({
            ""
          })
      }
      else if ((any(strsplit(ftargetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
               (nchar(ftargetseq()) != 200 + 200 + 3)) {
        disableActionButton("fcompute", session)
        #disableActionButton("fapply", session)
        output$finputlength <-
          renderText({
            "Please provide a sequence of proper length."
          })
        output$finputnuc <-
          renderText({
            "Please provide a DNA sequence."
          })
      }
      else if ((any(strsplit(ftargetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
               (nchar(ftargetseq()) == 200 + 200 + 3)) {
        disableActionButton("fcompute", session)
        #disableActionButton("fapply", session)
        output$finputlength <-
          renderText({
            ""
          })
        output$finputnuc <-
          renderText({
            "Please provide a DNA sequence."
          })
        
      }
    })
    
}
# Run the app ----
shinyApp(ui = ui, server = server)