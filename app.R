library(shiny)
library(shinyjs)
require(Biostrings)
# Input of target sequence
panel.target <- fluidRow(
  fluidPage(
    column(
  width = 10,
  h4(uiOutput("inp_target_text")),
  br(),
  #helpText("100-100 nucleotides before and after the stop codon."),
  
  textAreaInput(
    "inp_target",
    label = NULL,
    width = "100%",
    rows = 8,
    resize = "vertical",
    placeholder = "NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNTAANNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN"
  )),
  fluidPage(
    column(
      width = 5,
      h4("Number of nucleotides before the stop codon:")),
    column(
      width = 1,
    numericInput("beforestop", label = NULL, value = 100, min = 90, width = 65))),
    fluidPage(
      column(
      width = 5,
      h4("Number of nucleotides after the stop codon:")),
    column(
      width = 1,
      numericInput("afterstop", label = NULL, value = 100, min = 60, width = 65))),
    fluidPage(  
    column(
      width = 5,
      h4("Number of nucleotides around the stop codon to look for PAM sites:")),
    column(
      width = 1,
      numericInput("pamregion", label = NULL, value = 17, min = 3, max = 20, width = 65))
)
)
# column(width = 8,
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
)

panel.apply <-
  fluidPage(
    h4(em("Region around the stop codon:")),
    h5(textOutput("inputlength"), style = "color:grey"),
    textOutput("nucleotides"),
    br(),
    actionButton("apply", label = "Show sequence")
  )
# Select CASTLING moduls
panel.moduls <- fluidRow(column(
  width = 5,
  fluidPage(
    # em("Select CASTLING moduls:"),
    # radioButtons(
    #   "inp_moduls",
    #   label = NULL,
    #   choices = c("All from publication", "Subset from publication")
    # ),
    em("Select target species:"),
    radioButtons(
      "inp_species",
      label = NULL,
      choices = c("human", "mouse")
    ),
    # Select Cpf1 variant
    em("Select Cpf1 variant(s):"),
    checkboxGroupInput(
      "inp_cpf",
      label = NULL,
      choices = c("AsCpf1", "FnCpf1", "LbCpf1", "MbCpf1"),
      selected = c("AsCpf1", "FnCpf1", "LbCpf1", "MbCpf1")
    )
    #column(3, verbatimTextOutput("cpfchosen"))
  )
))
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
    panel.compute,
    tableOutput("cpf"),
    textOutput("beforeforwardoligo"),
    textOutput("forwardoligo"),
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
  # find possible PAM-sequences 17 bp around the stop codon depending on the Cpf1 chosen
  #length_nts <- (max_char-3)/2
  #length_nts <- 100
  aroundpam <- reactive({function(targetinput) {
    (DNAString(targetinput))[(input$beforestop - input$pamregion + 1):(input$beforestop + 3 + input$pamregion)]
  }})
  # Target sequence
  # output$inp_target_text <- reactive({ paste0('Only ', max_char-nchar(input$inp_target), ' characters remaining.' ) })
  output$inp_target_text <- renderUI({
    if (nchar(input$inp_target) == 0) {
      em("Enter target sequence:")
    em("Please provide the target sequence:",input$beforestop, " nucleotides before and " ,input$afterstop, " after the stop codon.")
    } else {
      em(paste0("Target sequence (", nchar(input$inp_target), " nt):"))
    }
  })
  observeEvent(input$inp_target, {
    if (nchar(input$inp_target) > input$beforestop + input$afterstop + 3)
    {
      updateTextInput(session,
                      'inp_target',
                      value = substr(input$inp_target, 1, input$beforestop + input$afterstop + 3))
      showModal(modalDialog(
        title = "Error!",
        "Character limit exceeded!",
        easyClose = TRUE
      ))
    }
  })
  disable("downloadData")
  observe({
    if (nchar(input$inp_target) < input$beforestop + input$afterstop + 3) {
      disableActionButton("compute", session)
      disableActionButton("apply", session)
      output$inputlength <-
        renderText({
          "Please provide a sequence of the desired length!"
        })
      #rv$data <- print("Please provide a nucleotid sequence!")
    } else if (all(strsplit(input$inp_target, "")[[1]] %in% DNA_ALPHABET)) {
      enableActionButton("compute", session)
      enableActionButton("apply", session)
      output$inputlength <- renderText({
        ""
      })
    } else {
      disableActionButton("compute", session)
      disableActionButton("apply", session)
      output$inputlength <-
        renderText({
          "Please provide a nucleotid sequence!"
        })
    }
  })
  nts <- eventReactive(input$inp_target, {
    targetnts <- DNAString(input$inp_target)
    return(targetnts)
  })
  ntspam <- eventReactive(input$inp_target, {
    subsetnts <- aroundpam()(input$inp_target)
    return(subsetnts)
  })
  output$nucleotides <- renderText({
    if (input$apply == 0)
      return("")
    isolate({
      as.character(ntspam())
    })
  })
  forwardoligo <- eventReactive(input$inp_target, {
    cone <-
      as.character(xscat(nts()[(input$beforestop - 89):input$beforestop], DNAString("TCAGGTGGAGGAGGTAGTG")))
    return(cone)
  })
  fnpamsd <- NULL
  lbpamsd <- NULL
  aspamsd <- NULL
  mbpamsd <- NULL
  fnpamsr <- NULL
  lbpamsr <- NULL
  aspamsr <- NULL
  mbpamsr <- NULL
  rvc <- reactiveValues(data = NULL)
  rv <- reactiveValues(data = NULL)
  rvp <- reactiveValues(data = NULL)
  rvo <- reactiveValues(data = NULL)
  observeEvent(input$inp_cpf, {
    rvc$data <- input$inp_cpf
  })
  observeEvent(input$compute, {
    if ("FnCpf1" %in% rvc$data) {
      fnpamsd <- list(
        matchPattern("TTV", ntspam()[1:(input$pamregion + 3)], fixed = F),
        matchPattern("TYN", ntspam()[1:(input$pamregion + 3)], fixed = F)
      )
      fnpamsr <-
        list(
          matchPattern("TTV", reverseComplement(ntspam()[(input$pamregion + 1):(input$pamregion + 3 + input$pamregion)]), fixed = F),
          matchPattern("TYN", reverseComplement(ntspam()[(input$pamregion + 1):(input$pamregion + 3 + input$pamregion)]), fixed = F)
        )
    }
    if (length(fnpamsd) > 1) {
      fnpamsd <- c(fnpamsd[[1]], fnpamsd[[2]])
    }
    if (length(fnpamsr) > 1) {
      fnpamsr <- c(fnpamsr[[1]], fnpamsr[[2]])
    }
    if ("LbCpf1"  %in% rvc$data) {
      lbpamsd <- matchPattern("HYBV", ntspam()[1:(input$pamregion + 3)], fixed = F)
      lbpamsr <-
        matchPattern("HYBV", reverseComplement(ntspam()[(input$pamregion + 1):(input$pamregion + 3 + input$pamregion)]), fixed = F)
    }
    if ("AsCpf1"  %in% rvc$data) {
      aspamsd <- matchPattern("TTTV", ntspam()[1:(input$pamregion + 3)], fixed = F)
      aspamsr <-
        matchPattern("TTTV", reverseComplement(ntspam()[(input$pamregion + 1):(input$pamregion + 3 + input$pamregion)]), fixed = F)
    }
    if ("MbCpf1"  %in% rvc$data) {
      mbpamsd <- matchPattern("TYYV", ntspam()[1:(input$pamregion + 3)], fixed = F)
      mbpamsr <-
        matchPattern("TYYV", reverseComplement(ntspam()[(input$pamregion + 1):(input$pamregion + 3 + input$pamregion)]), fixed = F)
    }
    ## ----
    allpamd <- list(fnpamsd, lbpamsd, aspamsd, mbpamsd)
    if (is.null(aspamsd) |
        is.null(fnpamsd) | is.null(lbpamsd) | is.null(mbpamsd))
    {
      allpamd <- allpamd[-which(sapply(allpamd, is.null))]
    }
    allpammd <- allpamd[[1]]
    if (length(allpamd) > 1) {
      for (i in (1:(length(allpamd) - 1))) {
        allpammd <- c(allpammd, allpamd[[i + 1]])
      }
    }
    allpamr <- list(fnpamsr, lbpamsr, aspamsr, mbpamsr)
    if (is.null(aspamsr) |
        is.null(fnpamsr) | is.null(lbpamsr) | is.null(mbpamsr))
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
        ) * 4), length(allpammd) + length(allpammr))
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
          allpamm$width[i + length(allpammd)] <-
            allpammr@ranges@width[i]
          allpamm$PAM[i + length(allpammd)] <-
            as.character(allpammr[[i]])
          allpamm$strand[i + length(allpammd)] <- "reverse"
        }
      }
      
      # remove sites occuring twice
      single <- which(duplicated(allpamm) == F)
      allpam_single <- allpamm[single,]
      # rank sites according to proximity to stop
      sortedallpam <- allpam_single[order(-allpam_single$start),]
      sortedallpam <- sortedallpam[order(sortedallpam$strand),]
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
          "GTTGTAGAT",
          # FnCpf1
          "GTTGTAGAT",
          # - " -
          "AAGTGTAGAT",
          # LbCpf1
          "CTTGTAGAT",
          # AsCpf1
          "TTTGTAGAT"  # MbCpf1
        ),
        stringsAsFactors = F
      )
      handles <- unique(subset(endonucleases, select = -c(pattern)))
      handles <- handles[order(handles$name),]
      handle <- rep("handle", times = pamnumber)
      grnas <- rep("grna", times = pamnumber)
      dim(grnas) <- c(pamnumber, 1)
      colnames(grnas) <- "gRNAs"
      for (i in (1:pamnumber)) {
        if (sortedallpam$strand[i] == "direct") {
          grnastart <- sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + input$beforestop - input$pamregion
          grnas[i] <-
            as.character(reverseComplement(nts()[(grnastart):(grnastart + 19)]))
        } else if (sortedallpam$strand[i] == "reverse") {
          grnastart <-
            sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 * (input$pamregion + 3 - sortedallpam$start[i]) + input$beforestop - input$pamregion - 2
          grnas[i] <-
            as.character(nts()[(grnastart - 19):(grnastart)])
        }
      }
      summary <- matrix(rep(0, len = pamnumber * 6), nrow = pamnumber)
      colnames(summary) <-
        c("Rank",
          "Cpf1",
          "PAM",
          "Strand",
          "gRNA-coding sequence",
          "Reverse oligo")
      summary = as.data.frame(summary)
      for (i in (1:pamnumber)) {
        if (nchar("TTV") == sortedallpam$width[i] &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TTV"),
                         fixed = F) > 0 & ("FnCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "FnCpf1"
          handle[i] <- handles[handles$name == "FnCpf1", 2]
        }
      }
      for (i in (1:pamnumber)) {
        if (nchar("TYN") == sortedallpam$width[i] &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TYN"),
                         fixed = F) > 0 & ("FnCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "FnCpf1"
          handle[i] <- handles[handles$name == "FnCpf1", 2]
        }
      }
      for (i in (1:pamnumber)) {
        if (nchar("HYBV") == sortedallpam$width[i] &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("HYBV"),
                         fixed = F) > 0 & ("LbCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "LbCpf1"
          handle[i] <- handles[handles$name == "LbCpf1", 2]
        }
      }
      for (i in (1:pamnumber)) {
        if (nchar("TTTV") == sortedallpam$width[i] &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TTTV"),
                         fixed = F) > 0 & ("AsCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "AsCpf1"
          handle[i] <- handles[handles$name == "AsCpf1", 2]
        }
      }
      for (i in (1:pamnumber)) {
        if (nchar("TYYV") == sortedallpam$width[i] &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TYYV"),
                         fixed = F) > 0 & ("MbCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "MbCpf1"
          handle[i] <- handles[handles$name == "MbCpf1", 2]
        }
      }
      for (i in (1:pamnumber)) {
        if (nchar("TYYV") == sortedallpam$width[i] &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TYYV"),
                         fixed = F) > 0 &
            ("MbCpf1"  %in% rvc$data) &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TTTV"),
                         fixed = F) > 0 & ("AsCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "AsCpf1, MbCpf1"
          handle[i] <- handles[handles$name == "AsCpf1", 2]
        }
      }
      for (i in (1:pamnumber)) {
        if (nchar("TYYV") == sortedallpam$width[i] &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("HYBV"),
                         fixed = F) > 0 &
            ("LbCpf1"  %in% rvc$data) &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TTTV"),
                         fixed = F) > 0 & ("AsCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "AsCpf1, LbCpf1"
          handle[i] <- handles[handles$name == "AsCpf1", 2]
        }
      }
      for (i in (1:pamnumber)) {
        if (nchar("TYYV") == sortedallpam$width[i] &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("HYBV"),
                         fixed = F) > 0 &
            ("LbCpf1"  %in% rvc$data) &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TYYV"),
                         fixed = F) > 0 & ("MbCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "LbCpf1, MbCpf1"
          handle[i] <- handles[handles$name == "LbCpf1", 2]
        }
      }
      for (i in (1:pamnumber)) {
        if (nchar("TYYV") == sortedallpam$width[i] &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TTTV"),
                         fixed = F) > 0 &
            ("AsCpf1"  %in% rvc$data) &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("HYBV"),
                         fixed = F) > 0
            &
            ("LbCpf1"  %in% rvc$data) &
            countPattern(DNAString(sortedallpam$PAM[i]),
                         DNAString("TYYV"),
                         fixed = F) > 0
            & ("MbCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "AsCpf1, LbCpf1, MbCpf1"
          handle[i] <- handles[handles$name == "AsCpf1", 2]
        }
      }
      for (i in (1:pamnumber)) {
        summary$PAM[i] <- sortedallpam$PAM[i]
        summary$Strand[i] <- as.character(sortedallpam$strand[i])
        summary$`gRNA-coding sequence`[i] <- grnas[i]
        summary$`Reverse oligo`[i] <-
          paste(
            as.character(reverseComplement(nts()[(input$beforestop + 4):(input$beforestop + 4 + 59)])),
            "AAAAAA",
            grnas[i],
            reverseComplement(DNAString(handle[i])),
            "AGTAGAAATTACGGTGTTTCGTC",
            sep = ""
          ) #!!!
        summary$Rank[i] <- as.character(i)
      }
      rv$data <- summary
      csvoutput <- summary
      csvoutput[nrow(csvoutput) + 1, ] <- c("", "", "", "", "", "")
      csvoutput[nrow(csvoutput) + 1, ] <- c("", "", "", "", "Forward oligo:", as.character(forwardoligo()))
      rvo$data <- csvoutput       
      }
  })
  output$cpf <- renderTable({
    validate(need(
      input$inp_target != "",
      'Please provide a nucleotid sequence!'
    ))
    validate(need(input$inp_cpf != "", 'Select at least one Cpf1 variant!'))
    rv$data
  })
  output$nucleotides <- renderText({
    if (input$apply == 0)
      return("")
    isolate({
      as.character(ntspam())
    })
  })
  observeEvent(input$compute, {
    rvp$data <- paste("Forward oligo:", as.character(forwardoligo()))
  })
  observeEvent(input$compute, {
    enable("downloadData")
  })
  output$forwardoligo <- renderText({
    validate(need(input$inp_target != "", ''))
    validate(need(input$inp_cpf != "", ''))
    rvp$data
  })
  output$downloadData <- downloadHandler(
    filename = "oligos.csv",
    content = function(file) {
      write.csv(rvo$data, file, row.names = FALSE)
    }
  )
}
# Run the app ----
shinyApp(ui = ui, server = server)