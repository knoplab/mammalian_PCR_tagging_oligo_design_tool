library(shiny)
#showReactLog()
library(shinyjs)
library(writexl)
library(Biostrings)
source("appUI.R")
source("appelements.R")
source("appfunctions.R")
#source("appcomments.R")
# Define UI ----
ui <-
  fluidPage(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css?family=Roboto+Mono"),
    useShinyjs(),
    tags$head(includeHTML("google-analytics.html")),
    tags$head(
      tags$style(
        
HTML("
                 body {
                   background-color: GhostWhite;
                 }
                    li {
                    font-size: 18px;
                      
                    }

                      body{
                      width:100%;
                      height:100%;
                      }
                 ")),
      tags$script(
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
        title = "Oligo design tool",
        mainPanel(
        fluidPage(
          splitLayout(cellWidths = c(1600, 180),
                     fluidPage(
                       titlePanel(
                        strong("Online oligo design tool for PCR-targeting in mammalian cells [TEST]")
                      ),
                      br(),
                      tags$head(
                        tags$style(HTML("hr {border-top: 1px solid #A9A9A9;}"))
                      ),
                      tags$head(
                        tags$style(HTML(".navbar-default {background-color: LightSteelBlue; font-size: 28px;}")),
                        tags$style(HTML(".navbar-default:hover {background-color: LightSteelBlue; color: DodgerBlue;}")),
                        tags$style(HTML(".navbar-default .navbar-brand {color: DimGrey; font-weight: bold; font-size: 28px;}")),
                        tags$style(HTML(".navbar-default:hover .navbar-brand {color: DimGrey; font-weight: bold;}"))
                        ),
                      h4(
                        "See reference: F\u00fcller", em("et al."),
                        br(),
                        br()
                      ),
                      panel.intro),
          img(src='cells.png', width = 150, align = "right")
          ),
          hr(),
          panel.target,
          hr(),
          panel.genename,
          hr(),
          panel.moduls,
          hr(),
          panel.region,
          hr(),
          panel.output,
          panel.extended,
          panel.download,
          #hr(),
          #panel.pcrr,
          br()
          ), width = 12
        )
        ),
      # tabPanel(title = "Discussion forum",
      #          
      #          panel.comment
      # ),
      tabPanel(title = "About",
                              
                              panel.about
      )
      )
      )
# Define server logic ----
server <- function(input, output, session)
{
  output$handlelink <- renderUI({
    tagList("", urlhandle)
  })
  output$fhandlelink <- renderUI({
    tagList("", urlhandle)
  })
  output$comments <- renderTable(loadData("responses")[order(-loadData("responses")[[4]]), ][1:3], colnames = F)
  # Single DNA sequence input
    output$nextstep <- renderText({""})
    output$nopam <- renderText({""})
    disableActionButton("compute", session)
    disableActionButton("runpcr", session)
    disableActionButton("submit", session)
    hideElement("extended")
    hideElement("inp_ext")
    hideElement("inp_apply")
    hideElement("downloadcsv")
    hideElement("downloadxlsx")
    hideElement("runpcr")
    # observe({
    #   if ((nchar(input$commenttext) > 0) & (nchar(input$commentname) > 0))  {
    #     enableActionButton("submit", session)
    #   } else {
    #     disableActionButton("submit", session)
    #   }
    # })
    #Discussion forum
    # observeEvent(input$submit, {
    # outputDir <- "responses"
    # saveData(as.data.frame(t(c(humanTime(), input$commentname, input$commenttext, as.integer(Sys.time())))), "responses")
    # write.csv(
    #   x = loadData("responses"),
    #   file = file.path("all", "allcomments.csv"),
    #   row.names = FALSE, quote = TRUE
    # )
    # reset("commentname")
    # reset("commenttext")
    # responsedata <- loadData("responses")
    # responsedata <- responsedata[order(-responsedata[[4]]), ]
    # output$comments <- renderTable(responsedata[1:3], colnames = F)
    # })
    observe({
      if ("Other" %in% input$inp_cpf) {
        enable("inp_pam")
        enable("inp_handle")
        enable("inp_name")
      } else {
        disable("inp_pam")
        disable("inp_handle")
        disable("inp_name")
      }
    })
    # Target sequence
    observeEvent(input$example, {
      updateTextInput(
        session,
        "inp_target",
        value = paste0(
          "ATTGTGAAGTGCTTGTTGAATCTGAGACTTAAAAATTTTGTTCTTTTAGAGGAAACATGGAGAAAGCCATTGACATGTTCAACAAAGCTATTAACCTGGCCAAATCGGAAATGGAGATGGCCCATCTGTATTCACTTTGCGATGCCGCCCATGCCCAGACAGAAGTTGCAAAGAAATACGGATTAAAACCACCAACATTATAAAACAGGGGGAAAGCAGACTGACCCTCTTTTTAAAAGTTTACCCCCTCTTCAACTGAACCCTAAAGACACTGTCATGAACTGTGTTGAATGGTGGAAATCAGTATTTCTGTTTGTGGTGTTGTTATTTGTTACATCTGTTTCATGTCTAGGTGTTGTGGGTGTGGCTGTTGAAGGAAGTTTGCAGTCTTGCAGCTTTTATT"
        )
      )
      updateTextInput(session, "genename", value = paste("TOMM70"))
      #updateTextInput(session, "beforestop", value = 97)
      #updateTextInput(session, "afterstop", value = 100)
      #updateTextInput(session, "pamregionu", value = 17)
      #updateTextInput(session, "pamregiond", value = 17)
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
      output$stopwarning <- renderText({""})
      output$cpf <- renderText({""})
      output$cpfe <- renderText({""})
      output$cpff <- renderText({""})
      output$monetitle <- renderText({""})
      output$monedescr <- renderText({""})
      output$mtwotitle <- renderText({""})
      output$mtwodescr <- renderText({""})
      output$mtwocomment <- renderText({""})
      output$mtwocommente <- renderText({""})
      output$nucleotidese <- renderText({""})
      output$nucleotidesre <- renderText({""})
      output$mtwoext <- renderText({""})
      output$nextstep <- renderText({""})
      hideElement("extended")
      hideElement("inp_ext")
      hideElement("inp_apply")
      hideElement("downloadcsv")
      hideElement("downloadxlsx")
      if ((nchar(targetseq()) != 200 + 200 + 3) &
          (nchar(targetseq()) > 0) &
          (all(strsplit(targetseq(), "")[[1]] %in% DNA_ALPHABET))) {
        disableActionButton("compute", session)
        output$inputlength <- renderText({"Please provide a sequence of 403 nts."})
        output$inputnuc <- renderText({""})
        output$nucleotides <- renderText({""})
        output$nucleotidesr <- renderText({""})
        output$searchspacee <- NULL
        output$nextstep <- renderText({""})
        disableActionButton("compute", session)
        disableActionButton("runpcr", session)
        hideElement("extended")
        hideElement("inp_ext")
        hideElement("inp_apply")
        hideElement("downloadcsv")
        hideElement("downloadxlsx")
        hideElement("runpcr")
        disable("inp_pam")
        disable("inp_handle")
        disable("inp_name")
      } else if (nchar(targetseq()) == 0) {
        disableActionButton("compute", session)
        output$inputnuc <- renderText({""})
        output$nucleotides <- renderText({""})
        output$nucleotidesr <- renderText({""})
        output$nextstep <- renderText({""})
        output$searchspacee <- NULL
        disableActionButton("compute", session)
        disableActionButton("runpcr", session)
        hideElement("extended")
        hideElement("inp_ext")
        hideElement("inp_apply")
        hideElement("downloadcsv")
        hideElement("downloadxlsx")
        hideElement("runpcr")
        disable("inp_pam")
        disable("inp_handle")
        disable("inp_name")
      }
      else if ((any(strsplit(targetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
               (nchar(targetseq()) != 200 + 200 + 3)) {
        disableActionButton("compute", session)
        #disableActionButton(, session)
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
        output$inputlength <- renderText({"Please provide a sequence of 403 nts."})
        output$inputnuc <- renderText({ "Please provide a DNA sequence."})
        output$nucleotides <- renderText({""})
        output$nucleotidesr <- renderText({""})
        disableActionButton("compute", session)
        disableActionButton("runpcr", session)
        hideElement("extended")
        hideElement("inp_ext")
        hideElement("inp_apply")
        hideElement("downloadcsv")
        hideElement("downloadxlsx")
        hideElement("runpcr")
        disable("inp_pam")
        disable("inp_handle")
        disable("inp_name")
      }
      else if ((any(strsplit(targetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
               (nchar(targetseq()) == 200 + 200 + 3)) {
        disableActionButton("compute", session)
        output$inputlength <- renderText({""})
        output$inputnuc <- renderText({"Please provide a DNA sequence."})
        output$nucleotides <- renderText({""})
        output$nucleotidesr <- renderText({""})
        disableActionButton("compute", session)
        disableActionButton("runpcr", session)
        hideElement("extended")
        hideElement("inp_ext")
        hideElement("inp_apply")
        hideElement("downloadcsv")
        hideElement("downloadxlsx")
        hideElement("runpcr")
        disable("inp_pam")
        disable("inp_handle")
        disable("inp_name")
      }
      else if ((all(strsplit(targetseq(), "")[[1]] %in% DNA_ALPHABET)) &
               (nchar(targetseq()) == 200 + 200 + 3)) {
        enableActionButton("compute", session)
        output$inp_target_text <- renderUI({""})
        output$inputlength <- renderText({""})
        output$inputnuc <- renderText({""})
        ntspam <- eventReactive({
          input$compute
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
              DNAString(targetseq())[(200 - rvfive$data + 1):200],
              DNAString("TCAGGTGGAGGAGGTAGTG")
            ))
          return(cone)
        })
        output$regiontext <- renderText({
          paste(h4(em("Search space for PAM sites around the insertion site (stop codon):")),
        h5(em(
          "17 nucleotides upstream on the direct strand and 17 nucleotides downstream on the reverse strand"
        ), style = "color:MediumAquaMarine"), sep = '\n')
        })
        regiond <- renderText({
            paste(
              as.character(ntsstop()[1:10]),
              '<span style = "color:MediumAquaMarine ">',
              as.character(ntsstop()[11:(17 + 10)]),
              '</span>',
              '<span style = "color:red ">',
              strong(substr(targetseq(), 201, 203)),
              '</span>',
              substr(targetseq(), 204, 230),
              " - direct strand",
              sep = ""
            )
        })
        output$nucleotides <- regiond
        regionr <- renderText({
            paste(
              as.character(complement(ntsstop()[1:10])),
              as.character(complement(ntsstop()[11:(17 + 10)])),
              '<span style = "color:red">',
              strong(as.character(complement(
                ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]
              ))),
              '</span>',
              '<span style = "color:MediumAquaMarine">',
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
          if ((as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TAA") |
              (as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TAG") |
              (as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TGA")) {
            output$stopwarning <- renderText({""})
          } else {
            output$stopwarning <-
              renderText({
                "Warning! The insertion site (marked in bold) is not a stop codon."
              })
          }
        rvc <- reactiveValues(data = NULL)
        rvcp <- reactiveValues(data = NULL)
        rvname <- reactiveValues(data = NULL)
        rvhandle <- reactiveValues(data = NULL)
        rvpam <- reactiveValues(data = NULL)
        rvf <- reactiveValues(data = NULL)
        rv <- reactiveValues(data = NULL)
        rve <- reactiveValues(data = NULL)
        rvp <- reactiveValues(data = NULL)
        rvo <- reactiveValues(data = NULL)
        rvoc <- reactiveValues(data = NULL)
        rvg <- reactiveValues(data = NULL)
        rvei <- reactiveValues(data = NULL)
        rvthree <- reactiveValues(data = NULL)
        rvfive <- reactiveValues(data = NULL)
        observeEvent(input$threeha, {
          rvthree$data <- input$threeha
        })
        observeEvent(input$fiveha, {
          rvfive$data <- input$fiveha
        })
        observeEvent(input$genename, {
          rvg$data <- input$genename
        })
        observeEvent(input$inp_name, {
          rvname$data <- input$inp_name
        })
        observeEvent(input$inp_handle, {
          rvhandle$data <- input$inp_handle
        })
        observeEvent(input$inp_pam, {
          rvpam$data <- input$inp_pam
        })
# save user inputs
        # observeEvent(input$compute, {
        # 
        #   saveData(c(rvg$data, targetseq()), "userinputs")
        # loadData("userinputs")
        # write.csv(
        #   x = loadData("userinputs"),
        #   file = file.path("all", "alluserinputs.csv"),
        #   row.names = FALSE, quote = TRUE
        # )
        # })
        observe({
          if ("Other" %in% input$inp_cpf) {
            observe({
               if ((all(strsplit(rvpam$data, "")[[1]] %in% DNA_ALPHABET)) &
                       (all(strsplit(rvhandle$data, "")[[1]] %in% RNA_ALPHABET)) &
                       (nchar(rvpam$data)) > 2 &
                       (nchar(rvhandle$data) > 10)) {
                enableActionButton("compute", session)
                output$pamhandlelink <-               renderText({
                  ""
                })
                output$handlecontrol <- renderText({
                  ""
                })
              } else {
                output$pamhandlelink <-
                  renderUI({
                    tagList("", urliupac)
                  })
                output$handlecontrol <- renderText({
                  ""
                })
                disableActionButton("compute", session)
              }
            })
          } else if (is.null(input$inp_cpf)) {
            output$pamhandlelink <- renderUI({
              ""
            })
            output$handlecontrol <- renderText({
              "Please select at least one Cpf1 variant!"
            })
            disableActionButton("compute", session)
          } else {
            output$pamhandlelink <- renderText({
              ""
            })
            output$handlecontrol <- renderText({
              ""
            })
            enableActionButton("compute", session)
          }
        })
        observeEvent(input$inp_cpf, {
          rvc$data <- input$inp_cpf
          rvcp$data <- c(
            "LbCpf1_TTTV",
            "LbCpf1_TYCV",
            "AsCpf1_TYCV",
            "AsCpf1_TATV"
          )
        })
        output$cpf <- renderText({""})
        output$cpfe <- renderText({""})
        output$cpff <- renderText({""})
        rvei$data <- 50
        observeEvent( input$inp_apply, {
            rvei$data <- input$inp_ext 
        })
        observeEvent( {input$compute
          input$inp_apply
          input$genename
          input$threeha
          input$fiveha}
          , {
            tttv <<- lookforpam("LbCpf1_TTTV", "TTTV", rvc$data, targetseq(), rvei$data)
            tycv <<- lookforpam("LbCpf1_TYCV", "TYCV", rvc$data, targetseq(), rvei$data)
            astttv <<- lookforpam("AsCpf1_TATV", "TTTV", rvc$data, targetseq(), rvei$data)
            astycv <<- lookforpam("AsCpf1_TYCV", "TYCV", rvc$data, targetseq(), rvei$data)
            tatv <<- lookforpam("AsCpf1_TATV", "TATV", rvc$data, targetseq(), rvei$data)
            mccc <<- lookforpam("AsCpf1_TYCV", "MCCC", rvc$data, targetseq(), rvei$data)
            ratr <<- lookforpam("AsCpf1_TATV", "RATR", rvc$data, targetseq(), rvei$data)
            otherpams <<- lookforpam("Other", rvpam$data, rvc$data, targetseq(), rvei$data)
           cpfs <<-  c(rep("Lb", times = length(tttv[[1]])), rep("Lb", times = length(tycv[[1]])), rep("As", times = length(astttv[[1]])), rep("As", times = length(astycv[[1]])), rep("As", times = length(tatv[[1]])), rep("As", times = length(mccc[[1]])), rep("As", times = length(ratr[[1]])), rep("other", times = length(otherpams[[1]])),
                            rep("Lb", times = length(tttv[[2]])), rep("Lb", times = length(tycv[[2]])), rep("As", times = length(astttv[[2]])), rep("As", times = length(astycv[[2]])), rep("As", times = length(tatv[[2]])), rep("As", times = length(mccc[[2]])), rep("As", times = length(ratr[[2]])), rep("other", times = length(otherpams[[2]])),
                            rep("Lb", times = length(tttv[[3]])), rep("Lb", times = length(tycv[[3]])), rep("As", times = length(astttv[[3]])), rep("As", times = length(astycv[[3]])), rep("As", times = length(tatv[[3]])), rep("As", times = length(mccc[[3]])), rep("As", times = length(ratr[[3]])), rep("other", times = length(otherpams[[3]])), 
                            rep("Lb", times = length(tttv[[4]])), rep("Lb", times = length(tycv[[4]])), rep("As", times = length(astttv[[4]])), rep("As", times = length(astycv[[4]])), rep("As", times = length(tatv[[4]])), rep("As", times = length(mccc[[4]])), rep("As", times = length(ratr[[4]])), rep("other", times = length(otherpams[[4]])))
            ## ----
           allpammd <- mergepamlists(1)
           allpammr <- mergepamlists(2)
           allpammdl <- mergepamlists(3)
           allpammrl <- mergepamlists(4)
          # merge the two datasets in one data frame
            if (length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl) > 0) {
            allpamm <-
              matrix(
                rep(0, len = (
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                ) * 7),
                length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
              )
            colnames(allpamm) <-
              c("start", "width", "PAM", "strand", "distance", "sort", "cpf")
            allpamm = as.data.frame(allpamm)
            if (length(allpammd) > 0) {
              for (i in (1:(length(allpammd)))) {
                allpamm$start[i] <- allpammd@ranges@start[i]
                allpamm$sort[i] <- allpammd@ranges@start[i]
                allpamm$width[i] <- allpammd@ranges@width[i]
                allpamm$PAM[i] <- as.character(allpammd[[i]])
                allpamm$strand[i] <- "direct"
                allpamm$distance[i] <-
                  200 - 17 + allpammd@ranges@start[i] + 22 - 203 - 1
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
                  abs(203 + 17 - allpammr@ranges@start[i] + 1 - 22 - 200)
                
              }
            }
            if (length(allpammdl) > 0) {
              for (i in (1:(length(allpammdl)))) {
                allpamm$start[i + length(allpammd) + length(allpammr)] <-
                  allpammdl@ranges@start[i]
                allpamm$sort[i + length(allpammd) + length(allpammr)] <-
                  rvei$data - allpammdl@ranges@start[i]
                allpamm$width[i + length(allpammd) + length(allpammr)] <-
                  allpammdl@ranges@width[i]
                allpamm$PAM[i + length(allpammd) + length(allpammr)] <-
                  as.character(allpammdl[[i]])
                allpamm$strand[i + length(allpammd) + length(allpammr)] <-
                  "direct*"
                allpamm$distance[i + length(allpammd) + length(allpammr)] <-
                  allpammdl@ranges@start[i] + 22 - 1
                
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
                  "reverse*"
                allpamm$distance[i + length(allpammd) + length(allpammr) + length(allpammdl)] <-
                  abs(rvei$data - allpammrl@ranges@start[i] + 1 - 22)
              }
            }
            for (i in (1:length(cpfs))) {
            allpamm$cpf[i] <- cpfs[i]
            }
            sortedallpam <<- allpamm
            pamnumber <- nrow(sortedallpam)
            handles <- unique(subset(endonucleases, select = -c(PAM)))
            handles <- handles[order(handles$Name), ]
            handle <- rep("handle", times = pamnumber)
            grnas <- rep("grna", times = pamnumber)
            cpfname <- rep("cpfname", times = pamnumber)
            rvcn <- reactiveValues(data = NULL)
            rvcn$data <- cpfname
            grnahandle <- rep("grnahandle", times = pamnumber)
            dim(grnas) <- c(pamnumber, 1)
            colnames(grnas) <- "gRNAs"
            oligorank <- rep(0, times = pamnumber)
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
              } else if (sortedallpam$strand[i] == "direct*") {
                grnastart <-
                  sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200
                grnas[i] <-
                  as.character(reverseComplement(DNAString(targetseq())[(grnastart):(grnastart + 19)]))
              } else if (sortedallpam$strand[i] == "reverse*") {
                grnastart <-
                  rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 + 203
                grnas[i] <-
                  as.character(DNAString(targetseq())[(grnastart - 19):(grnastart)])
              }
              if (grepl("AAAA", grnas[i])) {
                                 oligorank[i] <- 3
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("MCCC"),
                               fixed = F) > 0 |
                  countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("RATR"),
                               fixed = F) > 0) {
                oligorank[i] <- oligorank[i] + 1
              }
              }
            summary <-
              matrix(rep(0, len = (pamnumber) * 9), nrow = (pamnumber))
            colnames(summary) <- colsum
            summary = as.data.frame(summary)
            for (i in (1:pamnumber)) {
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("LbCpf1_TTTV"  %in% rvc$data) & sortedallpam$cpf[i] == "Lb") {
                cpfname[i] <- rvcp$data[1]
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "LbCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %in% rvc$data)  & sortedallpam$cpf[i] == "As") {
                cpfname[i] <- rvcp$data[4]
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "AsCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TYCV"),
                               fixed = F) > 0 &
                  ("LbCpf1_TYCV"  %in% rvc$data)  & sortedallpam$cpf[i] == "Lb") {
                cpfname[i] <- rvcp$data[2]
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "LbCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TYCV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TYCV"  %in% rvc$data)  & sortedallpam$cpf[i] == "As") {
                cpfname[i] <- rvcp$data[3]
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "AsCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("MCCC"),
                               fixed = F) > 0 &
                  ("AsCpf1_TYCV"  %in% rvc$data)  & sortedallpam$cpf[i] == "As") {
                cpfname[i] <- rvcp$data[3]
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "AsCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TATV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %in% rvc$data)  & sortedallpam$cpf[i] == "As") {
                cpfname[i] <- rvcp$data[4]
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "AsCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("RATR"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %in% rvc$data)  & sortedallpam$cpf[i] == "As") {
                cpfname[i] <- rvcp$data[4]
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    handles[handles$Name == "AsCpf1", 2]
                  ))))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString(rvpam$data),
                               fixed = F > 0) &
                  "Other"  %in% rvc$data) {
                cpfname[i] <-
                  paste(rvname$data, sep = "_")
                handle[i] <-
                  gsub("U", "T", as.character(reverseComplement(
                    RNAString(rvhandle$data)
                  )))
                grnahandle[i] <-
                  gsub("T", "U", reverseComplement(DNAString(handle[i])))
              }
            }
            csvoutput <- summary
            for (i in (1:pamnumber)) {
              #summary$Distance[i] <- format(round(sortedallpam$start[i], 0), nsmall = 0)
              summary$Cpf1[i] <- cpfname[i]
              summary$PAM[i] <- paste(sortedallpam$PAM[i])
              summary$`gRNA (handle + spacer)`[i] <-
                paste(
                  '<span style = "color:orange">',
                  as.character(grnahandle[i]),
                  '</span>',
                  '<span style = "color:orange">',
                  gsub("T", "U", reverseComplement(DNAString(grnas[i]))),
                  '</span>',
                  sep = ""
                )
              summary$`Suggested name`[i] <-
                paste("M2_", rvg$data, "_", cpfname[i], sep = "")
              if ((sortedallpam$strand[i] == "direct") |
                  (sortedallpam$strand[i] == "reverse")) {
                summary$Sequence[i] <-
                  paste0(
                    '<span style = "color:CornflowerBlue ">',
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(200 + 4):(200 + 4 + rvthree$data - 1)])),
                    '</span>',
                    '<span style = "color:brown">',
                    "AAAAAA",
                    '</span>',
                    '<span style = "color:orange">',
                    grnas[i],
                    '</span>',
                    '<span style = "color:orange">',
                    handle[i],
                    '</span>',
                    '<span style = "color:grey">',
                    "GCTAGCTGCATCGGTACC",
                    '</span>')
                summary$Strand[i] <-
                  paste(as.character(sortedallpam$strand[i]))
              }
              else if (sortedallpam$strand[i] == "direct*") {
                summary$Strand[i] <- "direct"
                summary$Sequence[i] <-
                  paste0(
                    '<span style = "color:CornflowerBlue ">',
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(grnas[i])):(
                      sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(grnas[i]) + rvthree$data - 1
                    )])),
                    '</span>',
                    '<span style = "color:brown">',
                    "AAAAAA",
                    '</span>',
                    '<span style = "color:orange">',
                    grnas[i],
                    '</span>',
                    '<span style = "color:orange">',
                    handle[i],
                    '</span>',
                    '<span style = "color:grey">',
                    "GCTAGCTGCATCGGTACC",
                    '</span>')
                #summary$Target[i] <-
              } else if (sortedallpam$strand[i] == "reverse*") {
                summary$Strand[i] <- "reverse"
                summary$Sequence[i] <-
                  paste0(
                    '<span style = "color:CornflowerBlue ">',
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2):(203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + rvthree$data - 1)])),
                    '</span>',
                    '<span style = "color:brown">',
                    "AAAAAA",
                    '</span>',
                    '<span style = "color:orange">',
                    grnas[i],
                    '</span>',
                    '<span style = "color:orange">',
                    handle[i],
                    '</span>',
                    '<span style = "color:grey">',
                    "GCTAGCTGCATCGGTACC",
                    '</span>')
              }
              
              summary$Length[i] <-
                paste(format(round(nchar(
                  paste(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(200 + 4):(200 + 4 + rvthree$data - 1)])),
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
                    '<span style = "color:GhostWhite">',
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
                    '<span style = "color:orange">',
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
                    '<span style = "color:orange; text-decoration: underline">',
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
                  MediumAquaMarine <-
                    paste0('<span style = "color:MediumAquaMarine">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (
                               sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) - 1
                             )
                           ),
                           '</span>')
                  MediumAquaMarinebold <- ""
                  bluef <-
                    paste0(
                      '<span style = "color:orange">',
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
                      '<span style = "color:orange;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopend),
                      '</span>'
                    )
                } else if (pamstart == pamend) {
                  MediumAquaMarine <-
                    paste0('<span style = "color:MediumAquaMarine">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (
                               sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) - 1
                             )
                           ),
                           '</span>')
                  MediumAquaMarinebold <- ""
                  bluef <- ""
                  bluebold <-
                    paste0(
                      '<span style = "color:orange;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopend),
                      '</span>'
                    )
                } else if (pamstart == pamend + 1) {
                  MediumAquaMarine <-
                    paste0('<span style = "color:MediumAquaMarine">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (stopstart - 1)
                           ),
                           '</span>')
                  MediumAquaMarinebold <-
                    paste0(
                      '<span style = "color:MediumAquaMarine;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopstart),
                      '</span>'
                    )
                  bluef <- ""
                  bluebold <-
                    paste0(
                      '<span style = "color:orange;font-weight:bold">',
                      substr(ntsstop(), stopstart + 1, stopend),
                      '</span>'
                    )
                } else if (pamstart == pamend + 2) {
                  MediumAquaMarine <-
                    paste0('<span style = "color:MediumAquaMarine">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (stopstart - 1)
                           ),
                           '</span>')
                  MediumAquaMarinebold <-
                    paste0(
                      '<span style = "color:MediumAquaMarine;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopstart + 1),
                      '</span>'
                    )
                  bluef <- ""
                  bluebold <-
                    paste0(
                      '<span style = "color:orange;font-weight:bold">',
                      substr(ntsstop(), stopend, stopend),
                      '</span>'
                    )
                } else if (pamstart == pamend + 3) {
                  MediumAquaMarine <-
                    paste0('<span style = "color:MediumAquaMarine">',
                           substr(
                             ntsstop(),
                             (sortedallpam$start[i] + 10),
                             (stopstart - 1)
                           ),
                           '</span>')
                  MediumAquaMarinebold <-
                    paste0(
                      '<span style = "color:MediumAquaMarine;font-weight:bold">',
                      substr(ntsstop(), stopstart, stopend),
                      '</span>'
                    )
                  bluef <- ""
                  bluebold <- ""
                }
                summary$Target[i] <-
                  paste0(
                    paste0
                    (
                      blackf,
                      MediumAquaMarine,
                      MediumAquaMarinebold,
                      bluef,
                      bluebold,
                      bluer,
                      '<sup>&#9660;</sup>',
                      bluerr,
                      blackr
                    ),
                    '<br>',
                    paste0(
                      as.character(complement(ntsstop()[1:(stopstart - 1)])),
                      strong(as.character(complement(
                        ntsstop()[(stopstart):(stopend)]
                      ))),
                      as.character(complement(ntsstop()[(stopend + 1):(sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i]) - 3)])),
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      as.character(complement(ntsstop()[(sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(grnas[i]) - 2):(sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + 22)])),
                      '</span>',
                      '<sub>&#9650;</sub>',
                      as.character(complement(ntsstop()[(sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + 23):nchar(ntsstop())]))
                    )
                  )
              }
              else if (as.character(sortedallpam$strand[i] == "reverse")) {
                blackr <-
                  paste0(
                    as.character(complement(ntsstop()[1:(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]) - 3
                    )])),
                    '<span style = "color:GhostWhite">',
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
                    '<span style = "color:orange; text-decoration: underline">',
                    as.character(complement(ntsstop()[(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(grnas[i]) + 1
                    ):(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i])  - nchar(grnas[i]) + 2
                    )])),
                    '</span>'
                  )
                bluer <-
                  paste0('<span style = "color:orange">',
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
                  MediumAquaMarinebold <-
                    paste0(
                      '<span style = "color:MediumAquaMarine;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:stopend])),
                      '</span>'
                    )
                  MediumAquaMarine <-
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      as.character(complement(ntsstop()[(stopend + 1):(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                }
                else if (stopstart + 1 == pamstart) {
                  bluebold <-
                    paste0(
                      '<span style = "color:orange;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:stopstart])),
                      '</span>'
                    )
                  bluef <- ""
                  MediumAquaMarinebold <-
                    paste0(
                      '<span style = "color:MediumAquaMarine;font-weight:bold">',
                      as.character(complement(ntsstop()[(stopstart + 1):stopend])),
                      '</span>'
                    )
                  MediumAquaMarine <-
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      as.character(complement(ntsstop()[(stopend + 1):(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                } else if (stopstart + 2 == pamstart) {
                  bluebold <-
                    paste0(
                      '<span style = "color:orange;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:(stopstart + 1)])),
                      '</span>'
                    )
                  bluef <- ""
                  MediumAquaMarinebold <-
                    paste0(
                      '<span style = "color:MediumAquaMarine;font-weight:bold">',
                      as.character(complement(ntsstop()[stopend:stopend])),
                      '</span>'
                    )
                  MediumAquaMarine <-
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      as.character(complement(ntsstop()[(stopend + 1):(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                }
                else if (stopstart + 3 == pamstart) {
                  bluebold <-
                    paste0(
                      '<span style = "color:orange;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:stopend])),
                      '</span>'
                    )
                  bluef <- ""
                  MediumAquaMarinebold <- ""
                  MediumAquaMarine <-
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      as.character(complement(ntsstop()[pamstart:(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                } else if (stopstart + 3 < pamstart) {
                  bluebold <-
                    paste0(
                      '<span style = "color:orange;font-weight:bold">',
                      as.character(complement(ntsstop()[stopstart:stopend])),
                      '</span>'
                    )
                  bluef <-
                    paste0('<span style = "color:orange">',
                           as.character(complement(ntsstop()[(stopend + 1):(pamstart - 1)])),
                           '</span>')
                  MediumAquaMarinebold <- ""
                  MediumAquaMarine <-
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      as.character(complement(ntsstop()[pamstart:(pamstart + nchar(sortedallpam$PAM[i]) - 1)])),
                      '</span>'
                    )
                }
                summary$Target[i] <-
                  paste0(
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
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      as.character(ntsstop()[(
                        nchar(ntsstop()) - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) - 10 - 22 + 1 + 5
                      ):(stopstart - 1)]),
                      strong(as.character(ntsstop()[(stopstart):(stopend)])),
                      as.character(ntsstop()[(stopend + 1):nchar(ntsstop())])
                    ),
                    '<br>',
                    paste0(
                      blackr,
                      bluerr,
                      '<sub>&#9650;</sub>',
                      bluer,
                      bluebold,
                      bluef,
                      MediumAquaMarinebold,
                      MediumAquaMarine,
                      blackf
                    )
                  )
              }
              else if (sortedallpam$strand[i] == "direct*") {
                summary$Target[i] <- paste0(
                  paste0(
                    substr(targetseq(), 194, 200),
                    strong(substr(targetseq(), 201, 203)),
                    substr(
                      targetseq(),
                      (203 + 1),
                      (200 + sortedallpam$start[i] - 1)
                    ),
                    '<span style = "color:MediumVioletRed">',
                    substr(
                      targetseq(),
                      (200 + sortedallpam$start[i]),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) - 1
                      )
                    ),
                    '</span>',
                    '<span style = "color:orange">',
                    substr(
                      targetseq(),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i])
                      ),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 17
                      )
                    ),
                    '</span>',
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    '<span style = "color:orange">',
                    substr(
                      targetseq(),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 18
                      ),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 19
                      )
                    ),
                    '</span>',
                    substr(
                      targetseq(),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 20
                      ),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 22
                      )
                    ),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(
                      targetseq(),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 23
                      ),
                      (204 + 17 + 125)
                    )
                  ),
                  '<br>',
                  paste0(
                    complement(DNAString(
                      substr(targetseq(), 194, 200)
                    )),
                    strong(complement(DNAString(
                      substr(targetseq(), 201, 203)
                    ))),
                    complement(DNAString(substr(
                      targetseq(),
                      (203 + 1),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 17
                      )
                    ))),
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    '<span style = "text-decoration: underline">',
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 18
                      ),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 22
                      )
                    ))),
                    '</span>',
                    '<sub>&#9650;</sub>',
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        200 + sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 23
                      ),
                      (204 + 17 + 125)
                    )))
                  )
                  )
              }
              else if (sortedallpam$strand[i] == "reverse*") {
                blackr <- complement(DNAString(substr(targetseq(), 194, 200)))
                MediumVioletRed <-
                  complement(DNAString(substr(
                    targetseq(),
                    (
                      203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2
                    ),
                    (203 + rvei$data - sortedallpam$start[i] + 1)
                  )))
                blackf <-
                  complement(DNAString(substr(
                    targetseq(),
                    (203 + rvei$data - sortedallpam$start[i] + 2),
                    (204 + 17 + 125)
                  )))
                # more than 3 nts distance between stop and grna end
                if ((rvei$data - sortedallpam$start[i] - 2) > (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
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
                            203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 23
                          )
                        )
                      )),
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(substr(
                        targetseq(),
                        (
                          203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 22
                        ),
                        (
                          203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 20
                        )
                      ))),
                      '</span>'
                    )
                  bluebold <- ""
                  bluer <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 200),
                    strong(substr(targetseq(), 201, 203)),
                    substr(
                      targetseq(),
                      204,
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 23
                      )
                    ),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(
                      targetseq(),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 22
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    ),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(
                      targetseq(),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (204 + 17 + 125)
                    )
                  )
                  # 3 nts distance between stop and grna end
                }
                else if ((rvei$data - sortedallpam$start[i] - 2) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    paste0(strong(complement(DNAString(
                      substr(targetseq(), 201, 203)
                    ))),
                    '<span style = "color:GhostWhite">',
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
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 200),
                    strong(substr(targetseq(), 201, 203)),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(targetseq(), 204, 208),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(targetseq(), 209, (
                      204 + 17 + 125
                    ))
                  )
                  # 2 nts distance between stop and grna end
                } else if ((rvei$data - sortedallpam$start[i] - 1) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    paste0(
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 202)
                      ))),
                      '<span style = "color:GhostWhite">',
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
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 200),
                    strong(substr(targetseq(), 201, 202)),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    strong(substr(targetseq(), 203, 203)),
                    substr(targetseq(), 204, 207),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(targetseq(), 208, (
                      204 + 17 + 125
                    ))
                  )
                  # 1 nt distance between stop and grna end
                }
                else if ((rvei$data - sortedallpam$start[i]) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    paste0(
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 201)
                      ))),
                      '<span style = "color:GhostWhite">',
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
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 200),
                    strong(substr(targetseq(), 201, 201)),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    strong(substr(targetseq(), 202, 203)),
                    substr(targetseq(), 204, 206),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(targetseq(), 207, (
                      204 + 17 + 125
                    ))
                  )
                  # no distance between stop and grna end
                } else if ((rvei$data - sortedallpam$start[i] + 1) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
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
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 19
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 18
                      )
                    )))
                  blue <-
                    complement(DNAString(substr(
                      targetseq(),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 17
                      ),
                      (
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1
                      )
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 200),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    strong(substr(targetseq(), 201, 203)),
                    substr(targetseq(), 204, 205),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(targetseq(), 206, (
                      204 + 17 + 125
                    ))
                  )
                }
                # gRNA end overlaps with the STOP codon - 1 nt
                else if ((rvei$data - sortedallpam$start[i] + 2) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackr <- complement(DNAString(substr(targetseq(), 194, 199)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(substr(targetseq(), 200, 200))),
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 202)
                        ))),
                      '<span style = "color:orange">',
                      strong(complement(DNAString(
                        substr(targetseq(), 203, 203)
                      ))),
                      '</span>',
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- ""
                  bluer <- ""
                  blue <-
                    complement(DNAString(substr(
                      targetseq(), 205, 205 + 18 - 1
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 199),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(targetseq(), 200, 200),
                    strong(substr(targetseq(), 201, 203)),
                    substr(targetseq(), 204, 204),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(targetseq(), 205, (
                      204 + 17 + 125
                    ))
                  )
                }
                # gRNA end overlaps with the STOP codon - 2 nts
                else if ((rvei$data - sortedallpam$start[i] + 3) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackr <- complement(DNAString(substr(targetseq(), 194, 198)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(substr(targetseq(), 199, 200))),
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 201)
                      ))),
                      '<span style = "color:orange">',
                      strong(complement(DNAString(
                        substr(targetseq(), 202, 203)
                      ))),
                      '</span>',
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- ""
                  bluer <- ""
                  blue <-
                    complement(DNAString(substr(
                      targetseq(), 204, 204 + 18 - 1
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 198),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(targetseq(), 199, 200),
                    strong(substr(targetseq(), 201, 203)),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(targetseq(), 204, (
                      204 + 17 + 125
                    ))
                  )
                }
                # gRNA end overlaps with the STOP codon - 3 nts
                else if ((rvei$data - sortedallpam$start[i] + 4) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackr <- complement(DNAString(substr(targetseq(), 194, 197)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(substr(targetseq(), 198, 200))),
                      '<span style = "color:orange">',
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 202)
                      ))),
                      '</span>',
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- paste0(                      strong(complement(DNAString(
                    substr(targetseq(), 203, 203)
                  ))))
                  bluer <- ""
                  blue <-
                    complement(DNAString(substr(
                      targetseq(), 203, 203 + 17 - 1
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 197),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(targetseq(), 198, 200),
                    strong(substr(targetseq(), 201, 202)),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    strong(substr(targetseq(), 203, 203)),
                    substr(targetseq(), 204, (
                      204 + 17 + 125
                    ))
                  )
                }
                # gRNA end overlaps with the STOP codon - 3 nts + 1
                else if ((rvei$data - sortedallpam$start[i] + 5) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackr <- complement(DNAString(substr(targetseq(), 194, 196)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(substr(targetseq(), 197, 199))),
                      '<span style = "color:orange">',
                      complement(DNAString(substr(targetseq(), 200, 200))),
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 201)
                      ))),
                      '</span>',
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- paste0(                      strong(complement(DNAString(
                    substr(targetseq(), 202, 203)
                  ))))
                  bluer <- ""
                  blue <-
                    complement(DNAString(substr(
                      targetseq(), 203, 203 + 16 - 1
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 196),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(targetseq(), 197, 200),
                    strong(substr(targetseq(), 201, 201)),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    strong(substr(targetseq(), 202, 203)),
                    substr(targetseq(), 204, (
                      204 + 17 + 125
                    ))
                  )
                }
                # gRNA end overlaps with the STOP codon - 3 nts + 2
                else if ((rvei$data - sortedallpam$start[i] + 6) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackr <- complement(DNAString(substr(targetseq(), 194, 195)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(substr(targetseq(), 196, 198))),
                      '<span style = "color:orange">',
                      complement(DNAString(substr(targetseq(), 199, 200))),
                      '</span>',
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- paste0(                      strong(complement(DNAString(
                    substr(targetseq(), 201, 203)
                  ))))
                  bluer <- ""
                  blue <-
                    complement(DNAString(substr(
                      targetseq(), 203, 203 + 15 - 1
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 195),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(targetseq(), 196, 200),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    strong(substr(targetseq(), 201, 203)),
                    substr(targetseq(), 204, (
                      204 + 17 + 125
                    ))
                  )
                }
                # gRNA end overlaps with the STOP codon - 3 nts + 3
                else if ((rvei$data - sortedallpam$start[i] + 7) == (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackr <- complement(DNAString(substr(targetseq(), 194, 194)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(substr(targetseq(), 195, 197))),
                      '<span style = "color:orange">',
                      complement(DNAString(substr(targetseq(), 198, 199))),
                      '</span>',
                      '</span>',
                      '<span style = "color:orange">',
                      complement(DNAString(substr(targetseq(), 199, 200))),
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- paste0(                      strong(complement(DNAString(
                    substr(targetseq(), 201, 203)
                  ))))
                  bluer <- ""
                  blue <-
                    complement(DNAString(substr(
                      targetseq(), 203, 203 + 14 - 1
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 194),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(targetseq(), 195, 199),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(targetseq(), 199, 200),
                    strong(substr(targetseq(), 201, 203)),
                    substr(targetseq(), 204, (
                      204 + 17 + 125
                    ))
                  )
                }
                else if ((rvei$data - sortedallpam$start[i] + 7) < (nchar(grnas[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackr <- complement(DNAString(substr(targetseq(), 194, 194)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      complement(DNAString(substr(targetseq(), 195, 197))),
                      '<span style = "color:orange">',
                      complement(DNAString(substr(targetseq(), 198, 199))),
                      '</span>',
                      '</span>',
                      '<span style = "color:orange">',
                      complement(DNAString(substr(targetseq(), 199, 200))),
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- paste0(                      strong(complement(DNAString(
                    substr(targetseq(), 201, 203)
                  ))))
                  bluer <- ""
                  blue <-
                    complement(DNAString(substr(
                      targetseq(), 203, 203 + 14 - 1
                    )))
                  directstrand <- paste0(
                    substr(targetseq(), 194, 194),
                    '<sup>&#9660;</sup>',
                    '<span style = "text-decoration: underline">',
                    substr(targetseq(), 195, 199),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(targetseq(), 199, 200),
                    strong(substr(targetseq(), 201, 203)),
                    substr(targetseq(), 204, (
                      204 + 17 + 125
                    ))
                  )
                }
                summary$Target[i] <- paste0(
                  directstrand,
                  '<br>',
                  paste0(
                    blackr,
                    blackbold,
                    '<span style = "color:orange;font-weight:bold">',
                    bluebold,
                    '</span>',
                    blackrr,
                    '<span style = "color:orange; text-decoration: underline">',
                    bluer,
                    '</span>',
                    '<sub>&#9650;</sub>',
                    '<span style = "color:orange">',
                    blue,
                    '</span>',
                    '<span style = "color:MediumVioletRed">',
                    MediumVioletRed,
                    '</span>',
                    blackf
                  )
                )
              }
              csvoutput$Cpf1[i] <- cpfname[i]
              csvoutput$PAM[i] <- sortedallpam$PAM[i]
              csvoutput$Strand[i] <- as.character(sortedallpam$strand[i])
              csvoutput$`gRNA (handle + spacer)`[i] <- paste0(as.character(grnahandle[i]), gsub("T", "U", reverseComplement(DNAString(grnas[i]))))
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
                    )[(200 + 4):(200 + 4 + rvthree$data - 1)])),
                    "AAAAAA",
                    grnas[i],
                    handle[i],
                    "GCTAGCTGCATCGGTACC"
                  )
              }
              else if (sortedallpam$strand[i] == "direct*") {
                csvoutput$Strand[i] <- "direct"
                csvoutput$Sequence[i] <-
                  paste0(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(grnas[i])):(
                      sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(grnas[i]) + rvthree$data - 1
                    )])),
                    "AAAAAA",
                    grnas[i],
                    handle[i],
                    "GCTAGCTGCATCGGTACC"
                  )
              } else if (sortedallpam$strand[i] == "reverse*") {
                csvoutput$Strand[i] <- "reverse"
                csvoutput$Sequence[i] <-
                  paste0(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2):(203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + rvthree$data - 1)])),
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
                    )[(200 + 4):(200 + 4 + rvthree$data - 1)])),
                    "AAAAAA",
                    grnas[i],
                    handle[i],
                    "GCTAGCTGCATCGGTACC",
                    sep = ""
                  )
                ), 0), nsmall = 0), " nts")
              csvoutput <- csvoutput[-c(pamnumber + 1, pamnumber + 2, pamnumber + 3), ]
              csvoutput$Target <- NULL
            }
            # Reverse oligo - result table column names
            colnames(summary)[colnames(summary) == "gRNA (handle + spacer)"] <- sumgrna
            colnames(summary)[colnames(summary) == "Sequence"] <- paste0(
              '<span style = "font-weight:bold; font-family: Arial">',
              "Sequence: ",
              '<span style = "color:CornflowerBlue ">',
              "3'-homology: ", rvthree$data, " bases after the insertion site, reverse orientation",
              '</span>',
              " + ",
              '<span style = "color:brown">',
              "Pol III terminator",
              '</span>',
              " + ",
              '<br>',
              '<span style = "color:orange">',
              "gRNA spacer encoding sequence",
              '</span>',
              " + ",
              '<span style = "color:orange">',
              "gRNA handle encoding sequence",
              '</span>',
              " + ",
              '<span style = "color:grey">', 
              "primer binding reverse oligo",
              '</span>',
              '</span>')
            colnames(summary)[colnames(summary) == "PAM"] <- "PAM"
            # Forward oligo result
            csvforward <-  matrix(rep(0, len = (1) * 8), nrow = 1)
            csvforward[1, ] <- c(
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
            csvforward <- as.data.frame(csvforward)
            colnames(csvforward) <- colcsvf
            summaryf <-
              matrix(rep(0, len = 1 * 3), nrow = 1)
            colnames(summaryf) <- colsumf
            summaryf = as.data.frame(summaryf)
            summaryf$`Suggested name`[1] <-
              paste("M1_", rvg$data, '</span>', sep = "")
            summaryf$`Sequence`[1] <-
              paste0(
                '<span style = "color:green">',
                as.character(DNAString(targetseq())[(200 - rvfive$data + 1):200]),
                '</span>',
                '<span style = "color:DodgerBlue">',
                "TCAGGTGGAGGAGGTAGTG",
                '</span>')
            summaryf$`Length`[1] <-
              paste(nchar(as.character(forwardoligo())), "nts", sep = "&nbsp")
            colnames(summaryf)[colnames(summaryf) == "Sequence"] <-
              paste(
                strong("Sequence:"),
                '<span style = "color:green">',
                "5'-homology: ", rvfive$data, " bases before the insertion site, direct orientation",
                '</span>',
                " + ",
                '<span style = "color:DodgerBlue">',
                "primer binding forward oligo",
                '</span>'
              )
            rvp$data <- summaryf
            output$cpff <- renderTable({
              rvp$data
            }, sanitize.text.function = function(x)
              x)
            # Textoutputs to the results
            output$monetitle <- renderText({monetitle})
            output$monedescr <- renderText({monedescr})
            output$mtwotitle <- renderText({mtwotitle})
            output$mtwodescr <- renderText({mtwodescr})
            output$mtwoext <- renderText({mtwoextb})
              output$searchspacee <- renderUI({
                img(src='searchspace_e.svg', width = 800, align = "right")
              })
              showElement("extended")
              showElement("inp_ext")
              showElement("inp_apply")
              showElement("downloadcsv")
              showElement("downloadxlsx")
              observeEvent (input$compute, {
                if ("Other" %in% input$inp_cpf) {
                  output$cpf <- renderTable({
                    validate(need(
                      input$inp_target != "",
                      'Please provide a nucleotide sequence.'
                    ))
                    validate(need(
                      nchar(rvpam$data) > 2,
                      'Please specify the PAM site.'
                    ))
                    validate(need(
                      nchar(rvhandle$data) > 10,
                      'Please specify the handle.'
                    ))
                    rv$data
                  }, sanitize.text.function = function(x)
                    x)
                  output$forwardoligo <- renderTable({
                    validate(need(input$inp_target != "", ''))
                    validate(need(nchar(rvpam$data) > 2, ''))
                    validate(need(nchar(rvhandle$data) > 5, ''))
                    rvp$data
                  }, sanitize.text.function = function(x)
                    x)
                } else if ("Other" %ni% input$inp_cpf) {
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
                  output$cpff <- renderTable({
                    rvp$data
                  }, sanitize.text.function = function(x)
                    x)
                }
              })
              csvoutputr <- NULL
              csvoutpute <- NULL
              if (length(allpammd) + length(allpammr) > 0) {
              enable("downloadcsv")
              enable("downloadxlsx")
              summaryr <- summary [c(1:(length(allpammd) + length(allpammr))), ]
              oligorankr <- oligorank[(1:(length(allpammd) + length(allpammr)))]
              summaryr <- summaryr[order(sortedallpam$distance[1:(length(allpammd) + length(allpammr))]), ]
              oligorankr <- oligorankr[order(sortedallpam$distance[1:(length(allpammd) + length(allpammr))])]
              summaryr <- summaryr[order(oligorankr), ]
              oligorankr <- sort(oligorankr)
              colnames(summaryr)[colnames(summaryr) == "Target"] <- sumrtarget
              for (i in (1:(length(allpammd) + length(allpammr)))) {
                if (oligorankr[i] > 2)
                {
                  summaryr$Rank[i] <- paste0(as.character(i), "*")
                } else {
                  summaryr$Rank[i] <- paste0(as.character(i))
                }
              }
              rv$data <- summaryr
              output$nextstep <- renderText({"Next step: Order oligos"})
              csvoutputr <- csvoutput [c(1:(length(allpammd) + length(allpammr))), ]
              oligorankr <- oligorank[((1:(length(allpammd) + length(allpammr))))]
              csvoutputr <- csvoutputr[order(sortedallpam$distance[1:(length(allpammd) + length(allpammr))]), ]
              oligorankr <- oligorankr[order(sortedallpam$distance[1:(length(allpammd) + length(allpammr))])]
              csvoutputr <- csvoutputr[order(oligorankr), ]
              oligorankr <- sort(oligorankr)
              for (i in (1:(length(allpammd) + length(allpammr)))) {
                if (oligorankr[i] > 2)
                {
                  csvoutputr$Rank[i] <- paste0(as.character(i), "*")
                } else {
                  csvoutputr$Rank[i] <- paste0(as.character(i))
                }
              }
              csvoutputrc <- csvoutputr
              csvoutputrc[nrow(csvoutputrc) + 1,] <-
                c("", "", "", "", "", "", "", "")
              csvoutputrc[nrow(csvoutputrc) + 1,] <-
                c(
                  paste0("M1_", rvg$data),
                  as.character(forwardoligo()),
                  paste(nchar(as.character(
                    forwardoligo()
                  )), "nts"),
                  "Forward oligo",
                  "",
                  "",
                  "",
                  ""
                )
              confinedlist <- list('Confined search space'=csvoutputr, 'Forward oligo'=csvforward)
              if (length(allpammdl) + length(allpammrl) > 0) {
              summarye <- summary [c((length(allpammd) + length(allpammr) + 1):(length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl))), ]
              oligoranke <- oligorank[(length(allpammd) + length(allpammr) + 1):(length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl))]
              summarye <- summarye[order(sortedallpam$distance[(length(allpammd) + length(allpammr) + 1):(length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl))]), ]
              oligoranke <- oligoranke[order(sortedallpam$distance[(length(allpammd) + length(allpammr) + 1):(length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl))])]
              summarye <- summarye[order(oligoranke), ]
              oligoranke <- sort(oligoranke)
              for (i in (1:(nrow(summarye)))) {
                if (oligoranke[i] > 2)
                {
                  summarye$Rank[i] <- paste0(as.character(i + (length(allpammd) + length(allpammr))), "*")
                } else {
                  summarye$Rank[i] <- paste0(as.character(i + (length(allpammd) + length(allpammr))))
                }
              }
              colnames(summarye)[colnames(summarye) == "Target"] <- sumetarget
              rve$data <- summarye
              csvoutpute <- csvoutput [c(length(allpammd) + length(allpammr) + 1):(length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)), ]
              oligoranke <- oligorank[(length(allpammd) + length(allpammr) + 1):(length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl))]
              csvoutpute <- csvoutpute[order(sortedallpam$distance[(length(allpammd) + length(allpammr) + 1):(length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl))]), ]
              oligoranke <- oligoranke[order(sortedallpam$distance[(length(allpammd) + length(allpammr) + 1):(length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl))])]
              csvoutpute <-  csvoutpute[order(oligoranke), ]
              oligoranke <- sort(oligoranke)
              for (i in (1:(nrow(csvoutpute)))) {
                if (oligoranke[i] > 2)
                {
                  csvoutpute$Rank[i] <- paste0(as.character(i + (length(allpammd) + length(allpammr))), "*")
                } else {
                  csvoutpute$Rank[i] <- paste0(as.character(i + (length(allpammd) + length(allpammr))))
                }
              }
              csvoutputec <- csvoutpute
              csvoutputec[nrow(csvoutputec) + 1,] <-
                c("", "", "", "", "", "", "", "")
              csvoutputec[nrow(csvoutputec) + 1,] <-
                c(
                  paste0("M1_", rvg$data),
                  as.character(forwardoligo()),
                  paste(nchar(as.character(
                    forwardoligo()
                  )), "nts"),
                  "Forward oligo",
                  "",
                  "",
                  "",
                  ""
                )
              extendedlist <- list('Confined search space'= csvoutputr, 'Extended search space' = csvoutpute, 'Forward oligo' = csvforward)
              }
              }
              else if (length(allpammdl) + length(allpammrl) > 0) {
                csvoutpute <- NULL
                output$nextstep <- renderText({""})
                output$mtwocomment <- renderText({mtwocomment})
                output$mtwoext <- renderText({mtwoext})
                observe({
                  if (input$extended == 1) {
                  enable("downloadcsv")
                  enable("downloadxlsx")
                  } else {
                    disable("downloadcsv")
                    disable("downloadxlsx") 
                  }
                })
                  summarye <- summary
                  summarye <- summarye[order(sortedallpam$distance), ]
                  oligoranke <- oligorank[order(sortedallpam$distance)]
                  summarye <- summarye[order(oligoranke), ]
                  oligoranke <- sort(oligoranke)
                  for (i in (1:(nrow(summarye)))) {
                    if (oligoranke[i] > 2)
                    {
                      summarye$Rank[i] <- paste0(as.character(i + (length(allpammd) + length(allpammr))), "*")
                    } else {
                      summarye$Rank[i] <- paste0(as.character(i + (length(allpammd) + length(allpammr))))
                    }
                  }
                  colnames(summarye)[colnames(summarye) == "Target"] <- sumetarget
                  rve$data <- summarye 
                  csvoutpute <- csvoutput
                  csvoutpute <- csvoutpute[order(sortedallpam$distance), ]
                  oligoranke <- oligorank[order(sortedallpam$distance)]
                  csvoutpute <- csvoutpute[order(oligoranke), ]
                  oligoranke <- sort(oligoranke)
                  for (i in (1:(nrow(csvoutpute)))) {
                    if (oligoranke[i] > 2)
                    {
                      csvoutpute$Rank[i] <- paste0(as.character(i + (length(allpammd) + length(allpammr))), "*")
                    } else {
                      csvoutpute$Rank[i] <- paste0(as.character(i + (length(allpammd) + length(allpammr))))
                    }
                    
                  }
                  csvoutputec <- csvoutpute
                  csvoutputec[nrow(csvoutputec) + 1,] <-
                    c("", "", "", "", "", "", "", "")
                  csvoutputec[nrow(csvoutputec) + 1,] <-
                    c(
                      paste0("M1_", rvg$data),
                      as.character(forwardoligo()),
                      paste(nchar(as.character(
                        forwardoligo()
                      )), "nts"),
                      "Forward oligo",
                      "",
                      "",
                      "",
                      ""
                    )
                  confinedlist <- NULL
                  csvoutputr <- NULL
                  csvoutputrc <- NULL
                  extendedlist <- list('Extended search space'=csvoutpute, 'Forward oligo'=csvforward)
              } else {
                  output$monetitle <- renderText({monetitle})
                  output$monedescr <- renderText({monedescr})
                  output$mtwotitle <- renderText({mtwotitle})
                  output$mtwodescr <- renderText({mtwodescr})
                  output$mtwoext <- renderText({mtwoext})
                  output$searchspacee <- renderUI({img(src='searchspace_e.svg', width = 800, align = "right") })
                  output$mtwocomment <- renderText({mtwocomment})
                  showElement("extended")
                  showElement("inp_ext")
                  enable("inp_ext")
                  showElement("inp_apply")
                  enableActionButton("inp_apply", session)
                  output$cpfe <- renderText({""})
                  output$mtwocommente <- renderText({paste0('<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumVioletRed">', "No PAM sites found within the ", rvei$data, " nts extended search space.", '</span>' )})
                  output$nextstep <- renderText({nextstep})
                  hideElement("downloadcsv")
                  hideElement("downloadxlsx")
              }
            observe({
              if ((input$extended == 1) & (nchar(targetseq()) == 403)) {
                enable("inp_ext")
                enableActionButton("inp_apply", session)

                  ntsstop <- aroundstop()(targetseq())
                 isolate({
                   output$nucleotidese <- renderText({
                    regionde <- paste(
                      as.character(ntsstop()[1:(17 + 10)]),
                      strong(substr(targetseq(), 201, 203)),
                      '<span style = "color:MediumVioletRed">',
                      substr(targetseq(), 204, 204 + rvei$data - 1),
                      '</span>',
                      substr(targetseq(), (204 + rvei$data), 305),
                      "&nbsp-&nbspdirect&nbspstrand",
                      sep = ""
                    )
                 
                })
                 })
                isolate({  
                
                  if (rvei$data > 17) {
     
                  output$nucleotidesre <- renderText({
                    paste(
                      as.character(complement(ntsstop()[1:10])),
                      as.character(complement(ntsstop()[11:(17 + 10)])),
                      strong(as.character(complement(
                        ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]
                      ))),
                      as.character(complement(ntsstop()[(17 + 3 + 10 + 1):(17 + 3 + 10 + 17)])),
                      '<span style = "color:MediumVioletRed">',
                      as.character(complement(DNAString(
                        targetseq()
                      )[(203 + 17 + 1):(204 + rvei$data - 1)])),
                      '</span>',
                      as.character(complement(DNAString(
                        targetseq()
                      )[(204 + rvei$data):305])),
                      "&nbsp-&nbspreverse&nbspstrand",
                      sep = ""
                    )
                  })
  
                  } else {
   
                    output$nucleotidesre <- renderText({
                      paste(
                        as.character(complement(ntsstop()[1:10])),
                        as.character(complement(ntsstop()[11:(17 + 10)])),
                        strong(as.character(complement(
                          ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]
                        ))),
                        as.character(complement(DNAString(
                          targetseq()
                        )[204:305])),
                        " - reverse strand",
                        sep = ""
                      )
  
                    })
                    }
                })
                    output$mtwocomment <- renderText({""})
                        if (length(allpammdl) + length(allpammrl) > 0) {
                          isolate({ output$cpfe <- renderTable({
                            rve$data
                          }, sanitize.text.function = function(x)
                            x)
                          rvo$data <- extendedlist
                          csvoutputec <- csvoutpute
                          csvoutputec[nrow(csvoutputec) + 1,] <-
                            c("", "", "", "", "", "", "", "")
                          csvoutputec[nrow(csvoutputec) + 1,] <-
                            c(
                              paste0("M1_", rvg$data),
                              as.character(forwardoligo()),
                              paste(nchar(as.character(
                                forwardoligo()
                              )), "nts"),
                              "Forward oligo",
                              "",
                              "",
                              "",
                              ""
                            )
                          rvoc$data <- rbind('Confined search space'=csvoutputr, 'Extended search space' = csvoutputec)
                          output$nextstep <- renderText({"Next step: Order oligos"})
                          output$mtwocommente <- renderText({mtwocommenterank})
                          })
                        } else {
                          showElement("extended")
                          showElement("inp_ext")
                          enable("inp_ext")
                          showElement("inp_apply")
                          enableActionButton("inp_apply", session)
                          isolate({
                            output$cpfe <- renderText({""})
                          output$mtwocommente <- renderText({paste0('<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumVioletRed">', "No PAM sites found within the ", rvei$data, " nts extended search space.", '</span>' )})
                          output$nextstep <- renderText({nextstep})
                          })
                          if (length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl) > 0) {
                            showElement("downloadcsv")
                            showElement("downloadxlsx")
                            confinedlist <- list('Confined search space' = csvoutputr, 'Forward oligo' = csvforward)
                            isolate({
                              rvo$data <- confinedlist
                            rvoc$data <- csvoutputrc
                            output$nextstep <- renderText({"Next step: Order oligos"})
                            output$mtwoext <- renderText({mtwoextb})
                            })
                                  } else {
                            hideElement("downloadcsv")
                            hideElement("downloadxlsx")
                          } 
                        } 
              } else {
                isolate({
                  output$nucleotidese <- renderText({""})
                output$nucleotidesre <- renderText({""})
                output$cpfe <- renderText({""})
                disable("inp_ext")
                disableActionButton("inp_apply", session)
                output$mtwocommente <- renderText({""})
                })
                  if (length(allpammd) + length(allpammr) > 0) {
                isolate({    output$mtwocomment <- renderText({mtwocommenterank})
                confinedlist <- list('Confined search space' = csvoutputr, 'Forward oligo' = csvforward)
                rvo$data <- confinedlist
                rvoc$data <- csvoutputrc
                output$nextstep <- renderText({"Next step: Order oligos"})
                })
                  } else {
                    isolate({
                      output$nextstep <- renderText({""})
                    })
                  }
                }
            })
            } else {
              isolate({
                output$monetitle <- renderText({monetitle})
              output$monedescr <- renderText({monedescr})
              output$mtwotitle <- renderText({mtwotitle})
              output$mtwodescr <- renderText({mtwodescr})
              output$mtwoext <- renderText({mtwoext})
              output$searchspacee <- renderUI({img(src='searchspace_e.svg', width = 800, align = "right")})
              output$mtwocomment <- renderText({mtwocomment})
              showElement("extended")
              showElement("inp_ext")
              enable("inp_ext")
              showElement("inp_apply")
              enableActionButton("inp_apply", session)
              output$cpfe <- renderText({""})
              output$mtwocommente <- renderText({paste0('<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumVioletRed">', "No PAM sites found within the ", rvei$data, " nts extended search space.", '</span>' )})
              output$nextstep <- renderText({nextstep})
              })
              hideElement("downloadcsv")
              hideElement("downloadxlsx")
              if ((input$extended == 1) & (nchar(targetseq()) == 403)) {
                
                ntsstop <- aroundstop()(targetseq())
                isolate({
                  output$nucleotidese <- renderText({
                    regionde <- paste(
                      as.character(ntsstop()[1:(17 + 10)]),
                      strong(substr(targetseq(), 201, 203)),
                      '<span style = "color:MediumVioletRed">',
                      substr(targetseq(), 204, 204 + rvei$data - 1),
                      '</span>',
                      substr(targetseq(), (204 + rvei$data), 305),
                      "&nbsp-&nbspdirect&nbspstrand",
                      sep = ""
                    )
                    
                  })
                })
                isolate({  
                  
                  if (rvei$data > 17) {
                    
                    output$nucleotidesre <- renderText({
                      paste(
                        as.character(complement(ntsstop()[1:10])),
                        as.character(complement(ntsstop()[11:(17 + 10)])),
                        strong(as.character(complement(
                          ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]
                        ))),
                        as.character(complement(ntsstop()[(17 + 3 + 10 + 1):(17 + 3 + 10 + 17)])),
                        '<span style = "color:MediumVioletRed">',
                        as.character(complement(DNAString(
                          targetseq()
                        )[(203 + 17 + 1):(204 + rvei$data - 1)])),
                        '</span>',
                        as.character(complement(DNAString(
                          targetseq()
                        )[(204 + rvei$data):305])),
                        "&nbsp-&nbspreverse&nbspstrand",
                        sep = ""
                      )
                    })
                    
                  } else {
                    
                    output$nucleotidesre <- renderText({
                      paste(
                        as.character(complement(ntsstop()[1:10])),
                        as.character(complement(ntsstop()[11:(17 + 10)])),
                        strong(as.character(complement(
                          ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]
                        ))),
                        as.character(complement(DNAString(
                          targetseq()
                        )[204:305])),
                        " - reverse strand",
                        sep = ""
                      )
                      
                    })
                  }
                })
              }
            }
        }, ignoreInit=TRUE)
        output$downloadcsv <- downloadHandler(
          filename = paste0("M_", rvg$data),
          content = function(file) {
            write.csv(rvoc$data, file, row.names = FALSE)
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
        if (input$compute == 0) {
          isolate({
          output$nucleotidese <- renderText({""})
          output$nucleotidesre <- renderText({""})
          output$cpfe <- renderText({""})
          })
        }
      })
      }, ignoreInit=TRUE)
}
# Run the app ----
shinyApp(ui = ui, server = server)