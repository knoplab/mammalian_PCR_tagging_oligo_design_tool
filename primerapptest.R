library(shiny)
library(shinyjs)
require(Biostrings)
# Input of target sequence
panel.target <- fluidRow(fluidPage(
  column(
    width = 12,
    h4(uiOutput("inp_target_text")),
    br(),
    #helpText("100-100 nucleotides before and after the stop codon."),
    
    # em("Input choice"),
    # tags$style(HTML(".radio-inline {margin-right: 200px;}")),
    # radioButtons(
    #   "inp_choice",
    #   label = NULL,
    #   choices = c(
    #     "Single nucleotid sequence" = 1,
    #     "Multiple nucleotide sequences in FASTA format" = 2
    #   ),
    #   inline = T,
    #   width = "100%"
    # ),
    fluidPage(column(
      width = 3,
      textAreaInput(
        "inp_target",
        label = NULL,
        width = "100%",
        rows = 8,
        resize = "vertical",
        placeholder = "NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNTAANNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN"
      )
    )
    # column(width = 4,
    #        
    #        fluidPage(
    #          em("Or upload in FASTA format:"),
    #          
    #          tags$div(
    #            title = "FASTA format",
    #            fileInput(
    #              "inp_fasta",
    #              label = NULL,
    #              width = "50%",
    #              accept =
    #            )
    #          )
    #        ))
    ),
    fluidPage(column(
      width = 6,
      h4("Number of nucleotides before the stop codon:")
    ),
    column(
      width = 1,
      numericInput(
        "beforestop",
        label = NULL,
        value = 100,
        min = 90,
        width = 65
      )
    )),
    fluidPage(column(
      width = 6,
      h4("Number of nucleotides after the stop codon:")
    ),
    column(
      width = 1,
      numericInput(
        "afterstop",
        label = NULL,
        value = 100,
        min = 60,
        width = 65
      )
    )),
    fluidPage(column(
      width = 6,
      h4("Number of nucleotides upstream of the stop codon to look for PAM sites on the direct strang:")
    ),
    column(
      width = 1,
      numericInput(
        "pamregionu",
        label = NULL,
        value = 17,
        min = 3,
        max = 20,
        width = 65
      )
    )),
    fluidPage(column(
      width = 6,
      h4("Number of nucleotides downstream of the stop codon to look for PAM sites on the reverse strang:")
    ),
    column(
      width = 1,
      numericInput(
        "pamregiond",
        label = NULL,
        value = 17,
        min = 3,
        max = 50,
        width = 65
      )
    ))
  )
))
panel.apply <-
  fluidPage(
    h4(em("Region around the stop codon:")),
    h5(textOutput("inputlength"), style = "color:grey"),
    htmlOutput("nucleotides"),
    #textOutput("fasta"),
    br(),
    actionButton("apply", label = "Show sequence")
  )
# Select CASTLING moduls
panel.moduls <- fluidRow(column(width = 3,
                                fluidPage(
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
                                  em("Select Cpf1 variant(s):"),
                                  checkboxGroupInput(
                                    "inp_cpf",
                                    label = NULL,
                                    width = "100%",
                                    choices = c("AsCpf1", "LbCpf1"),
                                    selected = c("AsCpf1", "LbCpf1")
                                  )
                                  #column(3, verbatimTextOutput("cpfchosen"))
                                )
                                # bootstrapPage(
                                #   div(style="display:inline-block",textInput(inputId = "inp_pam", label = NULL, placeholder = "PAM", width = "50%")),
                                #   div(style="display:inline-block",textInput(inputId = "inp_handle", label = NULL, placeholder = "HANDLE", width = "100%"))
                                # )
                                ),
                         (column(width = 5,
                                 fluidPage(tableOutput("endo")))))
panel.compute <- (actionButton("compute", label = "Get oligos"))
# Define UI ----
ui <-
  fluidPage(
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
    titlePanel("Primer Design Tool for PCR-tagging in Mammalian Cells"),
    br(),
    panel.target,
    panel.apply,
    hr(),
    panel.moduls,
    br(),
    panel.compute,
    br(),
    br(),
    textOutput("forwardoligo"),
    br(),
    htmlOutput("cpf"),
    br(),
    downloadButton("downloadData", "Download results (.csv)"),
    br(),
    br()
    )
# Define server logic ----
server <- function(input, output, session)
{
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
  disable("inp_pam")
  disable("inp_handle")
  # observe({
  #   if (input$inp_choice == 1) {
  #     disable("inp_fasta")
  #     enable("inp_target")
  #   } else if (input$inp_choice == 2) {
  #     enable("inp_fasta")
  #     disable("inp_target")
  #   }
  # })
  # observe({
  #   if ("Other, please specify PAM and gRNA-handle:" %in% input$inp_cpf) {
  #     enable("inp_pam")
  #     enable("inp_handle")
  #   } else {
  #     disable("inp_pam")
  #     disable("inp_handle")
  #   }
  # })
  disable("downloadData")
  endonucleases <- data.frame(
    Name    = c(
      "LbCpf1",
      "LbCpf1",
      "LbCpf1",
      "AsCpf1",
      "AsCpf1"
    ),
    Pattern = c("TTTV", "TYCV", "MCCC", "TATV", "RATR"),
    Handle  = c(
      "ATCTACACTTAGTAGAAATTA",
      # LbCpf1
      "ATCTACACTTAGTAGAAATTA",
      # LbCpf1
      "ATCTACACTTAGTAGAAATTA",
      # LbCpf1
      "ATCTACAAGAGTAGAAATTA",
      # AsCpf1
      "ATCTACAAGAGTAGAAATTA"  # AsCpf1
    ),
    stringsAsFactors = F
  )
  output$endo <- renderTable({endonucleases})
  # find possible PAM-sequences around the stop codon depending on the Cpf1 chosen
  aroundpam <- reactive({
    function(targetinput) {
      (DNAString(targetinput))[(input$beforestop - input$pamregionu + 1):(input$beforestop + 3 + input$pamregiond)]
    }
  })
  # Target sequence
  # output$inp_target_text <- reactive({ paste0('Only ', max_char-nchar(input$inp_target), ' characters remaining.' ) })
  output$inp_target_text <- renderUI({
    if (nchar(input$inp_target) == 0) {
      em(
        "Please provide the target sequence:",
        input$beforestop,
        " nucleotides before and " ,
        input$afterstop,
        " after the stop codon (" ,
        input$beforestop + 3 + input$afterstop,
        " nts)."
      )
    } else {
      em(paste0("Target sequence (", nchar(input$inp_target), " nt):"))
    }
  })
observeEvent(input$inp_target, {
        if (nchar(input$inp_target) > input$beforestop + input$afterstop + 3)
        {
          updateTextInput(
            session,
            'inp_target',
            value = substr(
              input$inp_target,
              1,
              input$beforestop + input$afterstop + 3
            )
          )
          showModal(modalDialog(
            title = "Error!",
            "Character limit exceeded!",
            easyClose = TRUE
          ))
        }
        observe({
          if (nchar(input$inp_target) != input$beforestop + input$afterstop + 3) {
            disableActionButton("compute", session)
            disableActionButton("apply", session)
            output$inputlength <-
              renderText({
                "Please provide a sequence of proper length!"
              })
            output$nucleotides <- renderText({
              ""
            })
            #rv$data <- print("Please provide a nucleotid sequence!")
          } else if (all(strsplit(input$inp_target, "")[[1]] %in% DNA_ALPHABET)) {
            enableActionButton("compute", session)
            enableActionButton("apply", session)
            output$inputlength <- renderText({
              ""
            })
            ntspam <- eventReactive({
              input$apply
              input$beforestop
              input$afterstop
              input$pamregionu
              input$pamregiond
            }, {
              subsetnts <- aroundpam()(input$inp_target)
              return(subsetnts)
            })
            forwardoligo <- eventReactive(input$compute, {
              cone <-
                as.character(xscat(
                  DNAString(input$inp_target)[(input$beforestop - 89):input$beforestop],
                  DNAString("TCAGGTGGAGGAGGTAGTG")
                ))
              return(cone)
            })
            output$nucleotides <- renderText({
              if (input$apply == 0)
                return("")
              isolate({
                paste(
                  as.character(ntspam()[1:input$pamregionu]),
                  strong(as.character(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3)])),
                  as.character(ntspam()[(input$pamregionu + 3 + 1):(input$pamregionu + 3 + input$pamregiond)]),
                  sep = ""
                )
              })
            })
            lbpamsd <- NULL
            aspamsd <- NULL
            lbpamsr <- NULL
            aspamsr <- NULL
            rvc <- reactiveValues(data = NULL)
            rv <- reactiveValues(data = NULL)
            rvp <- reactiveValues(data = NULL)
            rvo <- reactiveValues(data = NULL)
            observeEvent(input$inp_cpf, {
              rvc$data <- input$inp_cpf
            })
            observeEvent(input$compute, {
              if ("LbCpf1"  %in% rvc$data) {
                lbpamsd <-
                  list(
                    matchPattern("TTTV", ntspam()[1:(input$pamregionu + 3)], fixed = F),
                    matchPattern("TYCV", ntspam()[1:(input$pamregionu + 3)], fixed = F),
                    matchPattern("MCCC", ntspam()[1:(input$pamregionu + 3)], fixed = F)
                  )
                lbpamsr <-
                  list(
                    matchPattern("TTTV", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F),
                    matchPattern("TYCV", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F),
                    matchPattern("MCCC", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F)
                  )
              }
              if (length(lbpamsd) == 2) {
                lbpamsd <- c(lbpamsd[[1]], lbpamsd[[2]])
              }
              if (length(lbpamsr) == 2) {
                lbpamsr <- c(lbpamsr[[1]], lbpamsr[[2]])
              }
              if (length(lbpamsd) == 3) {
                lbpamsd <- c(lbpamsd[[1]], lbpamsd[[2]], lbpamsd[[3]])
              }
              if (length(lbpamsr) == 3) {
                lbpamsr <- c(lbpamsr[[1]], lbpamsr[[2]], lbpamsr[[3]])
              }
              if ("AsCpf1"  %in% rvc$data) {
                aspamsd <-
                  list(
                    matchPattern("TATV", ntspam()[1:(input$pamregionu + 3)], fixed = F),
                    matchPattern("RATR", ntspam()[1:(input$pamregionu + 3)], fixed = F)
                  )
                aspamsr <-
                  list(
                    matchPattern("TATV", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F),
                    matchPattern("RATR", reverseComplement(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3 + input$pamregiond)]), fixed = F)
                  )
              }
              if (length(aspamsd) > 1) {
                aspamsd <- c(aspamsd[[1]], aspamsd[[2]])
              }
              if (length(aspamsr) > 1) {
                aspamsr <- c(aspamsr[[1]], aspamsr[[2]])
              }
              ## ----
              allpamd <- list(lbpamsd, aspamsd)
              if (is.null(aspamsd) |
                  is.null(lbpamsd))
              {
                allpamd <- allpamd[-which(sapply(allpamd, is.null))]
              }
              allpammd <- allpamd[[1]]
              if (length(allpamd) > 1) {
                for (i in (1:(length(allpamd) - 1))) {
                  allpammd <- c(allpammd, allpamd[[i + 1]])
                }
              }
              allpamr <- list(lbpamsr, aspamsr)
              if (is.null(aspamsr) |
                  is.null(lbpamsr))
              {
                allpamr <- allpamr[-which(sapply(allpamr, is.null))]
              }
              allpammr <- allpamr[[1]]
              if (length(allpamr) > 1) {
                for (i in (1:(length(allpamr) - 1))) {
                  allpammr <- c(allpammr, allpamr[[i + 1]])
                }
              }
              
              # merge the two datasets in one data frame
              if (length(allpammd) + length(allpammr)  == 0) {
                rv$data <- print("No PAM-site found.")
              } else {
                allpamm <-
                  matrix(rep(0, len = (
                    length(allpammd) + length(allpammr)
                  ) * 4),
                  length(allpammd) + length(allpammr))
                colnames(allpamm) <-
                  c("start", "width", "PAM", "strand")
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
                    allpamm$width[i + length(allpammd)] <-
                      allpammr@ranges@width[i]
                    allpamm$PAM[i + length(allpammd)] <-
                      as.character(allpammr[[i]])
                    allpamm$strand[i + length(allpammd)] <-
                      "reverse"
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
                if (nrow(sortedallpam) < 5) {
                  pamnumber <- nrow(sortedallpam)
                } else {
                  pamnumber <- 5
                }
                handles <-
                  unique(subset(endonucleases, select = -c(Pattern)))
                handles <- handles[order(handles$Name),]
                handle <- rep("handle", times = pamnumber)
                grnas <- rep("grna", times = pamnumber)
                dim(grnas) <- c(pamnumber, 1)
                colnames(grnas) <- "gRNAs"
                for (i in (1:pamnumber)) {
                  if (sortedallpam$strand[i] == "direct") {
                    grnastart <-
                      sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + input$beforestop - input$pamregionu
                    grnas[i] <-
                      as.character(reverseComplement(DNAString(input$inp_target)[(grnastart):(grnastart + 19)]))
                  } else if (sortedallpam$strand[i] == "reverse") {
                    grnastart <-
                      sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 * (input$pamregionu + 3 - sortedallpam$start[i]) + input$beforestop - input$pamregionu - 2
                    grnas[i] <-
                      as.character(DNAString(input$inp_target)[(grnastart - 19):(grnastart)])
                  }
                }
                summary <-
                  matrix(rep(0, len = pamnumber * 6), nrow = pamnumber)
                colnames(summary) <-
                  c(
                    "Rank",
                    "Cpf1",
                    "PAM",
                    "Strand",
                    "gRNA-coding sequence",
                    "Reverse oligo"
                  )
                summary = as.data.frame(summary)
                for (i in (1:pamnumber)) {
                  if (nchar("TTTV") == sortedallpam$width[i] &
                      countPattern(DNAString(sortedallpam$PAM[i]),
                                   DNAString("TTTV"),
                                   fixed = F) > 0 &
                      ("LbCpf1"  %in% rvc$data)) {
                    summary$Cpf1[i] <- "LbCpf1"
                    handle[i] <-
                      handles[handles$Name == "LbCpf1", 2]
                  }
                }
                for (i in (1:pamnumber)) {
                  if (nchar("TYCV") == sortedallpam$width[i] &
                      countPattern(DNAString(sortedallpam$PAM[i]),
                                   DNAString("TYCV"),
                                   fixed = F) > 0 &
                      ("LbCpf1"  %in% rvc$data)) {
                    summary$Cpf1[i] <- "LbCpf1"
                    handle[i] <-
                      handles[handles$Name == "LbCpf1", 2]
                  }
                }
                for (i in (1:pamnumber)) {
                  if (nchar("MCCC") == sortedallpam$width[i] &
                      countPattern(DNAString(sortedallpam$PAM[i]),
                                   DNAString("MCCC"),
                                   fixed = F) > 0 &
                      ("LbCpf1"  %in% rvc$data)) {
                    summary$Cpf1[i] <- "LbCpf1"
                    handle[i] <-
                      handles[handles$Name == "LbCpf1", 2]
                  }
                }
                for (i in (1:pamnumber)) {
                  if (nchar("TATV") == sortedallpam$width[i] &
                      countPattern(DNAString(sortedallpam$PAM[i]),
                                   DNAString("TATV"),
                                   fixed = F) > 0 &
                      ("AsCpf1"  %in% rvc$data)) {
                    summary$Cpf1[i] <- "AsCpf1"
                    handle[i] <-
                      handles[handles$Name == "AsCpf1", 2]
                  }
                }
                for (i in (1:pamnumber)) {
                  if (nchar("RATR") == sortedallpam$width[i] &
                      countPattern(DNAString(sortedallpam$PAM[i]),
                                   DNAString("RATR"),
                                   fixed = F) > 0 &
                      ("AsCpf1"  %in% rvc$data)) {
                    summary$Cpf1[i] <- "AsCpf1"
                    handle[i] <-
                      handles[handles$Name == "AsCpf1", 2]
                  }
                }
                for (i in (1:pamnumber)) {
                  summary$PAM[i] <- sortedallpam$PAM[i]
                  summary$Strand[i] <-
                    as.character(sortedallpam$strand[i])
                  summary$`gRNA-coding sequence`[i] <- grnas[i]
                  summary$`Reverse oligo`[i] <-
                    paste(
                      as.character(reverseComplement(
                        DNAString(input$inp_target)[(input$beforestop + 4):(input$beforestop + 4 + 54)]
                      )),
                      "AAAAAA",
                      grnas[i],
                      DNAString(handle[i]),
                      "GCTAGCTGCATCGGTACC",
                      sep = ""
                    ) #!!!
                  summary$Rank[i] <- as.character(i)
                }
                rv$data <- summary
                csvoutput <- summary
                csvoutput[nrow(csvoutput) + 1, ] <-
                  c("", "", "", "", "", "")
                csvoutput[nrow(csvoutput) + 1, ] <-
                  c("",
                    "",
                    "",
                    "",
                    "Forward oligo:",
                    as.character(forwardoligo()))
                rvo$data <- csvoutput
              }
            })
            '%ni%' <- Negate('%in%')
            observe({
              if ("Other, please specify PAM and gRNA-handle:" %in% input$inp_cpf) {
                output$cpf <- renderTable({
                  validate(need(
                    input$inp_target != "",
                    'Please provide a nucleotid sequence!'
                  ))
                  validate(need(
                    nchar(input$inp_pam) > 2,
                    'Please specify the PAM-site!'
                  ))
                  validate(need(nchar(input$inp_handle) > 5, 'Please specify the handle!'))
                  rv$data
                })
                output$forwardoligo <- renderText({
                  validate(need(input$inp_target != "", ''))
                  validate(need(nchar(input$inp_pam) > 2, ''))
                  validate(need(nchar(input$inp_handle) > 5, ''))
                  rvp$data
                })
              } else if ("Other, please specify PAM and gRNA-handle:" %ni% input$inp_cpf) {
                output$cpf <- renderTable({
                  validate(need(
                    input$inp_target != "",
                    'Please provide a nucleotid sequence!'
                  ))
                  validate(need(
                    input$inp_cpf != "",
                    'Select at least one Cpf1 variant!'
                  ))
                  rv$data
                })
                output$forwardoligo <- renderText({
                  validate(need(input$inp_target != "", ''))
                  validate(need(input$inp_cpf != "", ''))
                  rvp$data
                })
              }
            })
            observeEvent(input$compute, {
              rvp$data <- paste("Forward oligo:", as.character(forwardoligo()))
            })
            observeEvent(input$compute, {
              enable("downloadData")
            })
            output$downloadData <- downloadHandler(
              filename = "oligos.csv",
              content = function(file) {
                write.csv(rvo$data, file, row.names = FALSE)
              }
            )
            
            # output$nucleotides <- renderText({
            #   if (input$apply == 0)
            #     return("")
            #   isolate({
            #     paste(as.character(ntspam()[1:input$pamregionu]), strong(as.character(ntspam()[(input$pamregionu + 1):(input$pamregionu + 3)])), as.character(ntspam()[(input$pamregionu + 3 + 1):(input$pamregionu + 3 + input$pamregiond)]), sep = "")
            #   })
            # })
          } else {
            disableActionButton("compute", session)
            disableActionButton("apply", session)
            output$inputlength <-
              renderText({
                "Please provide a nucleotid sequence!"
              })
            output$nucleotides <- renderText({
              ""
            })
          }
        })
        # ntst <- eventReactive(input$apply, {
        #    ntst.tmp <- input$inp_target
        #   return(ntst.tmp)
        # })
        #
        # nts <- eventReactive(input$apply, {
        #         targetnts <- DNAString(ntst())
        #   return(targetnts)
        # })
        
      })
}
# Run the app ----
shinyApp(ui = ui, server = server)