library(shiny)
library(shinyjs)
require(Biostrings)
# Input of target sequence
panel.target <- fluidRow(column(
  width = 5,
  uiOutput("inp_target_text"),
  helpText("100-100 nucleotides before and after the stop codon (TAA)."),
  
  textAreaInput(
    "inp_target",
    label = NULL,
    width = "100%",
    rows = 8,
    resize = "vertical",
    placeholder = "NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNTAANNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN"
  )
),
column(width = 5,
       
       fluidPage(
         em("Or upload in FASTA format:"),
         
         tags$div(
           title = "FASTA format",
           fileInput(
             "inp_fasta",
             label = NULL,
             width = "50%",
             accept =
           )
         )
       )))
panel.apply <-
  fluidPage(actionButton("apply", label = "Show +/- 17 bp"),
            em(textOutput("inputlength"), style = "color:grey"))
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
panel.compute <- (actionButton("compute", label = "Compute"))
max_char = 203
# Define UI ----
ui <-
  fixedPage(
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
    strong("Primer Design Tool for CASTLING in mammalian cells"),
    panel.target,
    panel.apply,
    textOutput("nucleotides"),
    hr(),
    panel.moduls,
    panel.compute,
    tableOutput("cpf"),
    textOutput("beforeconeprimer"),
    textOutput("coneprimer")
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
  length_nts <- 100
  aroundpam <-
    function(targetinput) {
      (DNAString(targetinput))[(length_nts - 16):(length_nts + 3 + 17)]
    }
  # Target sequence
  # output$inp_target_text <- reactive({ paste0('Only ', max_char-nchar(input$inp_target), ' characters remaining.' ) })
  output$inp_target_text <- renderUI({
    if (nchar(input$inp_target) == 0)
      em("Enter target sequence:")
    else
      em(paste0("Target sequence (", nchar(input$inp_target), " nt):"))
    
  })
  observeEvent(input$inp_target, {
    if (nchar(input$inp_target) > max_char)
    {
      updateTextInput(session,
                      'inp_target',
                      value = substr(input$inp_target, 1, max_char))
      showModal(modalDialog(
        title = "Error!",
        "Character limit exceeded!",
        easyClose = TRUE
      ))
    }
  })
  observe({
    if (nchar(input$inp_target) < 203) {
      disableActionButton("compute", session)
      disableActionButton("apply", session)
      output$inputlength <-
        renderText({
          "Please provide a sequence of 203 nts!"
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
    subsetnts <- aroundpam(input$inp_target)
    return(subsetnts)
  })
  output$nucleotides <- renderText({
    if (input$apply == 0)
      return("")
    isolate({
      as.character(ntspam())
    })
  })
  fnpams <- NULL
  lbpams <- NULL
  aspams <- NULL
  mbpams <- NULL
  rvc <- reactiveValues(data = NULL)
  rv <- reactiveValues(data = NULL)
  rvp <- reactiveValues(data = NULL)
  observeEvent(input$inp_cpf, {
    rvc$data <- input$inp_cpf
  })
  observeEvent(input$compute, {
    if ("FnCpf1" %in% rvc$data) {
      fnpams <- list(matchPattern("TTV", ntspam(), fixed = F),
                     matchPattern("TYN", ntspam(), fixed = F))
    }
    if (length(fnpams) > 1) {
      fnpams <- c(fnpams[[1]], fnpams[[2]])
    }
    if ("LbCpf1"  %in% rvc$data) {
      lbpams <- matchPattern("HYBV", ntspam(), fixed = F)
    }
    if ("AsCpf1"  %in% rvc$data) {
      aspams <- matchPattern("TTTV", ntspam(), fixed = F)
    }
    if ("MbCpf1"  %in% rvc$data) {
      mbpams <- matchPattern("TYYV", ntspam(), fixed = F)
    }
    allpam <- list(fnpams, lbpams, aspams, mbpams)
    if (is.null(aspams) |
        is.null(fnpams) | is.null(lbpams) | is.null(mbpams))
    {
      allpam <- allpam[-which(sapply(allpam, is.null))]
    }
    allpamm <- allpam[[1]]
    if (length(allpam) > 1) {
      for (i in (1:(length(allpam) - 1))) {
        allpamm <- c(allpamm, allpam[[i + 1]])
      }
    }
    # remove sites occuring twice
    if (length(allpamm) == 0) {
      rv$data <- print("No PAM-site found.")
    } else {
      single <-
        which(duplicated(cbind(start(allpamm), end(allpamm))) == F)
      allpam_single <- allpamm[single]
      # rank sites according to proximity to stop
      sortedallpam <-
        allpam_single[order(abs((length(ntspam(
        )) - 3) / 2 - allpam_single@ranges@start)),]
      # compute gRNAs for the 5 best PAM-sites
      if (length(sortedallpam) < 5) {
        pamnumber <- length(sortedallpam)
      } else {
        pamnumber <- 5
      }
      grnas <- rep("grna", times = pamnumber)
      dim(grnas) <- c(pamnumber, 1)
      colnames(grnas) <- "gRNAs"
      for (i in (1:pamnumber)) {
        targetstart <-
          sortedallpam@ranges@start[i] + length(sortedallpam[[i]])
        grnas[i] <-
          as.character(complement(nts()[(targetstart + 100 - 17):(targetstart + 19 +
                                                                    100 - 17)]))
      }
      summary <-
        matrix(rep(0, len = pamnumber * 5), nrow = pamnumber)
      colnames(summary) <-
        c("Rank",
          "Cpf1",
          "PAM",
          "gRNA-coding sequence",
          "C2-primer")
      summary = as.data.frame(summary)
      for (i in (1:pamnumber)) {
        summary$PAM[i] <- as.character(sortedallpam[[i]])
        summary$`C2-primer`[i] <-
          paste("grnahandle", grnas[i], "TTTTTT", as.character(nts()[104:163]), sep = "")
      }
      for (i in (1:pamnumber)) {
        if (length(DNAString("TTV")) == sortedallpam@ranges@width[i] &
            (countPattern(sortedallpam[[i]], DNAString("TTV"), fixed = F) > 0) &
            ("FnCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "FnCpf1"
        }
      }
      for (i in (1:pamnumber)) {
        if (length(DNAString("TYN")) == sortedallpam@ranges@width[i] &
            (countPattern(sortedallpam[[i]], DNAString("TYN"), fixed = F) > 0) &
            ("FnCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "FnCpf1"
        }
      }
      for (i in (1:pamnumber)) {
        if (length(DNAString("HYBV")) == sortedallpam@ranges@width[i] &
            (countPattern(sortedallpam[[i]], DNAString("HYBV"), fixed = F) > 0) &
            ("LbCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "LbCpf1"
        }
      }
      for (i in (1:pamnumber)) {
        if (length(DNAString("TTTV")) == sortedallpam@ranges@width[i] &
            (countPattern(sortedallpam[[i]], DNAString("TTTV"), fixed = F) > 0) &
            ("AsCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "AsCpf1"
        }
      }
      for (i in (1:pamnumber)) {
        if (length(DNAString("TYYV")) == sortedallpam@ranges@width[i] &
            (countPattern(sortedallpam[[i]], DNAString("TYYV"), fixed = F) > 0) &
            ("MbCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "MbCpf1"
        }
      }
      for (i in (1:pamnumber)) {
        if (length(DNAString("TYYV")) == sortedallpam@ranges@width[i] &
            (countPattern(sortedallpam[[i]], DNAString("TYYV"), fixed = F) > 0) &
            ("MbCpf1"  %in% rvc$data) &
            (countPattern(sortedallpam[[i]], DNAString("TTTV"), fixed = F) > 0) &
            ("AsCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "AsCpf1, MbCpf1"
        }
      }
      for (i in (1:pamnumber)) {
        if (length(DNAString("TYYV")) == sortedallpam@ranges@width[i] &
            (countPattern(sortedallpam[[i]], DNAString("HYBV"), fixed = F) > 0) &
            ("LbCpf1"  %in% rvc$data) &
            (countPattern(sortedallpam[[i]], DNAString("TTTV"), fixed = F) > 0) &
            ("AsCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "AsCpf1, LbCpf1"
        }
      }
      for (i in (1:pamnumber)) {
        if (length(DNAString("TYYV")) == sortedallpam@ranges@width[i] &
            (countPattern(sortedallpam[[i]], DNAString("HYBV"), fixed = F) > 0) &
            ("LbCpf1"  %in% rvc$data) &
            (countPattern(sortedallpam[[i]], DNAString("TYYV"), fixed = F) > 0) &
            ("MbCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "LbCpf1, MbCpf1"
        }
      }
      for (i in (1:pamnumber)) {
        if ((length(DNAString("TYYV")) == sortedallpam@ranges@width[i]) &
            (countPattern(sortedallpam[[i]], DNAString("TTTV"), fixed = F) > 0) &
            ("AsCpf1"  %in% rvc$data) &
            (countPattern(sortedallpam[[i]], DNAString("HYBV"), fixed = F) > 0)
            &
            ("LbCpf1"  %in% rvc$data) &
            (countPattern(sortedallpam[[i]], DNAString("TYYV"), fixed = F) > 0)
            & ("MbCpf1"  %in% rvc$data)) {
          summary$Cpf1[i] <- "AsCpf1, LbCpf1, MbCpf1"
        }
      }
      for (i in (1:pamnumber)) {
        summary$`gRNA-coding sequence`[i] <- grnas[i]
      }
      for (i in (1:pamnumber)) {
        summary$Rank[i] <- as.character(i)
      }
      rv$data <- summary
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
  coneprimer <- eventReactive(input$inp_target, {
    cone <-
      as.character(xscat(complement(nts()[11:100]), complement(DNAString("SGGGGSGGGGS"))))
    return(cone)
  })
  observeEvent(input$compute, {
    rvp$data <- paste("C1-primer:", as.character(coneprimer()))
  })
  output$coneprimer <- renderText({
    validate(need(input$inp_target != "", ''))
    validate(need(input$inp_cpf != "", ''))
    rvp$data
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)