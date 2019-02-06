library(shiny)
library(shinyjs)
library(writexl)
library(Biostrings)
#library(kableExtra)
library(tinytex)
#options(knitr.table.format = "latex")
source("appUI.R")
source("appelements.R")
source("appfunctions.R")
source("appcomments.R")
source("20181212-retrieve_by_ENS.R")
# Define UI - panels defined in appUI.R ----
ui <-
  fluidPage(
    tags$head(includeScript("google-analytics.js")),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css?family=Roboto+Mono"),
    useShinyjs(),
    list(tags$head(
      HTML('<link rel="icon", href="s62.png",
           type="image/png" />')
      )),
    # div(style="padding: 1px 0px; width: '100%'",
    #               titlePanel(
    #                 title="", windowTitle="Mammalian PCR tagging"
    #               )
    #           ),
    tags$head(tags$style(
      HTML(
        "
        body {
        background-color: GhostWhite;
        }
        li {
        font-size: 18px;
        }
        body{
        width:100%;
        height:100%;
        white-space: nowrap;
        }
        "
      )
      ),
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
    # Action button style
    tags$style(
      ".btn {
      color: #fff !important;
      background: #1f49c7;
      padding: 10px;
      border-radius: 2px;
      font-size: 16px;
      font-weight: bold;
      display: inline-block;
      border: none;
      }"
),
tags$style(".btn:hover {
           color: #fff;
           background: #7b97ea;
           }"),
tags$style(".btn:focus {
           color: #fff;
           background: #1f49c7;
}"),
tags$style(".btn:active {
           color: #fff;
           background: #7b97ea;
}"),
tags$style(
  ".btn:disabled {
  opacity: 0.4;
  cursor: not-allowed;
  pointer-events: none;
  }"
),
tags$head(tags$style(
  HTML(
    "hr {border-top: 1px solid #A9A9A9;}
    h1 {
    color: #1f49c7;
    font-weight: bold;
    line-height: 1.1;
    }
    "
  )
  ),
  tags$style(
    HTML(
      ".shiny-notification {
      height: 60px;
      width: 300px;
      position:fixed;
      bottom: 0px;
      right: 0px;
      }
      "
    )
    )),
# Modal dialog style
tags$head(tags$style(".modal-dialog{ width:1000px}")),
tags$head(tags$style(".modal-body{ min-height:150px}")),
# tags$style(".checkbox, .radio-inline {
#     text-align: middle;
#            margin-left: 0px;
#            margin-right: 0px;
#            padding: 0px;
#            width: 20%;} "),
# Navigation bar style
tags$head(
  tags$style(HTML(
    ".navbar-default {background-color: #1f49c7;}"
  )),
  tags$style(HTML(
    ".navbar-default:hover {background-color: #1f49c7;}"
  )),
  tags$style(
    HTML(
      ".navbar-default .navbar-nav > li > a {color: white;  font-size: 24px; font-weight: bold; background-color: #1f49c7; text-shadow: 1px 1px black}"
    )
  ),
  tags$style(
    HTML(
      ".navbar-default .navbar-nav > li > a:hover {color: white; background-color: #7b97ea;  font-weight: bold; font-size: 24px; text-shadow: 1px 1px black}"
    )
  ),
  tags$style(
    HTML(
      ".navbar-default .navbar-nav > .active > a {color: #1f49c7; background-color: GhostWhite; font-size: 24px; text-shadow: none  }"
    )
  ),
  tags$style(
    HTML(
      ".navbar-default .navbar-nav > .active > a:hover {color: #1f49c7; background-color: GhostWhite;  font-size: 24px; text-shadow: none }"
    )
  ),
  tags$style(
    HTML(
      ".navbar-default .navbar-nav > .active > a:focus {color: #1f49c7; background-color: GhostWhite; font-size: 24px; text-shadow: none }"
    )
  ),
  tags$style(
    HTML(
      ".navbar-default .navbar-brand {background-color: #1f49c7; color: white; font-weight: bold; font-size: 24px; text-shadow: 1px 1px black; height: 0px; padding: 0px 0px}"
    )
  ),
  tags$style(
    HTML(
      ".navbar-default .navbar-brand:hover {background-color: #1f49c7; color: white; font-weight: bold; font-size: 28px; text-shadow: 1px 1px black}"
    )
  ),
  tags$style(
    HTML(
      ".navbar-default:hover .navbar-brand {color: white; font-weight: bold; text-shadow: 1px 1px black;}"
    )
  ),
  tags$style(HTML(".navbar-static-top {border-width: 0px}"))
),
navbarPage(
  title = NULL,
  id = "home",
  tabPanel(
    title = "What is PCR tagging?",
    headerPanel("Mammalian PCR tagging"),
    br(),
    br(),
    br(),
    mainPanel(fluidPage(
      splitLayout(
        cellWidths = c(1450, 360),
        panel.whatintro,
        img(src = 'pcrtagging.svg', width = 350, align = "right")
      ),
      hr(),
      splitLayout(
        cellWidths = c(1340, 610),
        panel.needed,
        img(src = 's6.png', width = 600, align = "left")
      )
    ), width = 12)
  ),
  tabPanel(
    title = "Online oligo design tool",
    headerPanel("Mammalian PCR tagging"),
    br(),
    br(),
    br(),
    mainPanel(
      fluidPage(
        panel.target,
        panel.searchspace,
        hr(),
        panel.genename,
        hr(),
        panel.moduls,
        hr(),
        panel.region,
        hr(),
        panel.output,
        panel.extended,
        panel.download
      ),
      width = 12
    )
  ),
  tabPanel(
    title = "Template cassettes",
    headerPanel("Mammalian PCR tagging"),
    br(),
    br(),
    br(),
    br(),
    mainPanel(
      splitLayout(
        cellWidths = c(600, 510),
        template,
        img(src = 'tags.svg', width = 500, align = "left")
      ),
      br(),
      h4(
        em(
          "Most of the pMaCTag plasmids are now available at",
          urlownplasmids,
          br(),
          "Please feel free to contact the corresponding author,",
          mailknopauthor,
          "for further plasmid requests."
        )
      ),
      panel.templates
    )
  ),
  tabPanel(
    title = "Cassette PCR",
    headerPanel("Mammalian PCR tagging"),
    mainPanel(br(),
              h3(
                strong(
                  "Example protocol for the amplification of the gene tagging PCR cassette"
                )
              ),
              panel.pcr)
  ),
  tabPanel(
    title = "Discussion forum",
    headerPanel("Mammalian PCR tagging"),
    br(),
    br(),
    br(),
    mainPanel(panel.comment, width = 12)
  ),
  tabPanel(
    title = "About",
    headerPanel("Mammalian PCR tagging"),
    br(),
    br(),
    br(),
    br(),
    br(),
    panel.about
  ),
  tabPanel(
    title = "Impressum",
    headerPanel("Mammalian PCR tagging"),
    br(),
    br(),
    br(),
    panel.impressum
  )
)
    )

# Define server logic ----

server <- function(input, output, session)
{
  output$drlink <- renderUI({
    tagList("", urldr)
  })
  output$fdrlink <- renderUI({
    tagList("", urldr)
  })
  output$downloadseq <- downloadHandler(
    filename = function() {
      paste("pMaCTag-sequences", "zip", sep = ".")
    },
    content = function(file) {
      file.copy("data/pMaCTag-sequences.zip", file)
    },
    contentType = "application/zip"
  )
  output$nextstep <- renderText({
    ""
  })
  output$nopam <- renderText({
    ""
  })
  disableActionButton("compute", session)
  disableActionButton("runpcr", session)
  disableActionButton("submit", session)
  hideElement("extended")
  hideElement("inp_ext")
  hideElement("inp_apply")
  hideElement("downloadcsv")
  hideElement("downloadxlsx")
  hideElement("report")
  hideElement("runpcr")
  output$template <-
    renderTable({
      templatedata
    }, sanitize.text.function = function(x)
      x)
  output$pcrmixhifitable <-
    renderTable({
      pcrmixhifi
    }, sanitize.text.function = function(x)
      x)
  output$pcrmixphutable <-
    renderTable({
      pcrmixphu
    }, sanitize.text.function = function(x)
      x)
  output$pcrmixveltable <-
    renderTable({
      pcrmixvel
    }, sanitize.text.function = function(x)
      x)
  output$pcrtable <-
    renderTable({
      pcr
    })
  rvens <-
    reactiveValues(data = NULL)
  observe({
    if (input$inputmethod == "Ensembl") {
      enable("inp_ensembl")
      output$ensinputdescr <- renderText({
        ensinputact
      })
      output$targinputdescr <- renderText({
        targinputpass
      })
      disable("inp_target")
      prefixes <- readRDS("data/prefixes.rds")
      observeEvent(input$inp_ensembl, {
        output$ensfeedback <- renderText({
          ""
        })
        if (nchar(input$inp_ensembl) > 0) {
          if (grepl(
            "*T[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]",
            toupper(gsub(
              "\t", "", gsub(" ", "", input$inp_ensembl)
            ))
          ) &
          toupper(strsplit((gsub(
            "\t", "", gsub(" ", "", input$inp_ensembl)
          )), "T[0-9]")[[1]][1]) %in% prefixes$Prefix) {
            output$enswarn <- renderText({
              ""
            })
            enableActionButton("ens", session)
          } else if (grepl(
            "*G[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]",
            toupper(gsub(
              "\t", "", gsub(" ", "", input$inp_ensembl)
            ))
          ) &
          toupper(strsplit(input$inp_ensembl, "G[0-9]")[[1]][1]) %in% prefixes$Prefix) {
            rvens$data <-
              toupper(strsplit((gsub(
                "\t", "", gsub(" ", "", input$inp_ensembl)
              )) , "\\.")[[1]][1])
            options(stringsAsFactors = FALSE)
            prefixes <- readRDS("data/prefixes.rds")
            species <- readRDS("data/species.rds")
            gene_datasets <- readRDS("data/gene_datasets.rds")
            findspecname <-
              sapply(prefixes$Prefix, function(.)
                grep(., rvens$data))
            indspecname <- which(sapply(findspecname, length) == 1)
            indspecname <- which(sapply(findspecname, length) == 1)
            if (length(indspecname) == 2 & indspecname[1] == 87) {
              indspecname <- indspecname [2]
            }
            if (length(indspecname) == 3 &
                "ENSCGR" %in% prefixes$Prefix[indspecname]) {
              indspecname <- indspecname [3]
            }
            speciesname <-
              gsub(")",
                   "",
                   strsplit(prefixes$Species.name[indspecname], "\\(")[[1]][2])
            specieslaturl <-
              paste(gsub(")", "", strsplit(
                prefixes$Species.name[indspecname], "\\ "
              )[[1]][1]),
              gsub(")", "", strsplit(
                prefixes$Species.name[indspecname], "\\ "
              )[[1]][2]),
              sep = "_")
            if ("ENSCGR" %in% prefixes$Prefix[indspecname]) {
              specieslaturl <- paste0(specieslaturl, "_crigri")
            }
            if ("ENSORL" %in% prefixes$Prefix[indspecname]) {
              specieslaturl <- paste0(specieslaturl, "_hni")
            }
            output$enswarn <-   renderUI(tagList(
              HTML(ensgwarning),
              a(
                href = paste0(
                  'https://www.ensembl.org/',
                  specieslaturl,
                  '/Gene/Summary?g=',
                  (gsub(
                    "\t", "", gsub(" ", "", input$inp_ensembl)
                  ))
                ),
                "here.",
                target = "_blank"
              )
            ))
            disableActionButton("ens", session)
          } else {
            output$enswarn <- renderText({
              enstwarning
            })
            disableActionButton("ens", session)
          }
        } else {
          disableActionButton("ens", session)
          output$enswarn <- renderText({
            ""
          })
        }
      })
    } else if (input$inputmethod == "Sequence") {
      disableActionButton("ens", session)
      disable("inp_ensembl")
      output$ensinputdescr <- renderText({
        ensinputpass
      })
      output$targinputdescr <- renderText({
        targinputact
      })
      enable("inp_target")
      output$enswarn <- renderText({
        ""
      })
    }
  })
  observeEvent(input$ens,
               {
                 withProgress(message = 'Retrieving Ensembl data...', value = 0.25, {
                   rvens$data <-
                     toupper(strsplit((gsub(
                       "\t", "", gsub(" ", "", input$inp_ensembl)
                     )) , "\\.")[[1]][1])
                   options(stringsAsFactors = FALSE)
                   prefixes <- readRDS("data/prefixes.rds")
                   species <- readRDS("data/species.rds")
                   gene_datasets <- readRDS("data/gene_datasets.rds")
                   findspecname <-
                     sapply(prefixes$Prefix, function(.)
                       grep(., rvens$data))
                   ## dog
                   # findspecname <- sapply(prefixes$Prefix, function(.) grep(., "ENSCAFT00000049095"))
                   ## koala ENSPCIT00000033457
                   # findspecname <- sapply(prefixes$Prefix, function(.) grep(., "ENSPCIT00000033457"))
                   ## rabbit
                   # findspecname <- sapply(prefixes$Prefix, function(.) grep(., "ENSOCUT00000015301"))
                   ## squirrel
                   # findspecname <- sapply(prefixes$Prefix, function(.) grep(., "ENSSTOT00000026678"))
                   ## elephant
                   # findspecname <- sapply(prefixes$Prefix, function(.) grep(., "ENSLAFT00000029390"))
                   ## sheep
                   # findspecname <- sapply(prefixes$Prefix, function(.) grep(., "ENSOART00000020885"))
                   ## rat
                   # findspecname <- sapply(prefixes$Prefix, function(.) grep(., "ENSRNOT00000022702"))
                   ## mouse
                   # findspecname <- sapply(prefixes$Prefix, function(.) grep(., "ENSMUST00000052915.13"))
                   
                   indspecname <-
                     which(sapply(findspecname, length) == 1)
                   if (length(indspecname) == 2 &
                       indspecname[1] == 87) {
                     indspecname <- indspecname [2]
                   }
                   if (length(indspecname) == 3 &
                       "ENSCGR" %in% prefixes$Prefix[indspecname]) {
                     indspecname <- indspecname [3]
                   }
                   speciesname <-
                     gsub(")", "", strsplit(prefixes$Species.name[indspecname], "\\(")[[1]][2])
                   specieslaturl <-
                     paste(gsub(")", "", strsplit(prefixes$Species.name[indspecname], "\\ ")[[1]][1]),
                           gsub(")", "", strsplit(prefixes$Species.name[indspecname], "\\ ")[[1]][2]),
                           sep = "_")
                   #specieslatpartone <- tolower(strsplit(prefixes$Species.name[indspecname], "\\ ")[[1]][1])
                   #findassembly <- sapply(species$name, function(.) grep(specieslat, .))
                   
                   speciesnameurl <- gsub(" ", "%20", speciesname)
                   findassembly <-
                     sapply(species$display_name, function(.)
                       grep(., speciesname))
                   findassemblya <-
                     sapply(species$display_name, function(.)
                       grep(speciesname, .))
                   assembly <-
                     tolower(species$assembly[which(sapply(findassembly, length) == 1)])
                   assemblya <-
                     tolower(species$assembly[which(sapply(findassemblya, length) == 1)])
                   finddataset <-
                     sapply(tolower(gene_datasets$version), function(.)
                       grep(intersect(assembly, assemblya), .))
                   speciesdata <-
                     gene_datasets$dataset[which(sapply(finddataset, length) == 1)]
                   sequence <- tryCatch(
                     get_flanking_genomic(
                       rvens$data,
                       useMart("ensembl", dataset = speciesdata),
                       "https://rest.ensembl.org",
                       speciesnameurl,
                       flankSize = 200
                     ),
                     error = function(e)
                       e
                   )
                   incProgress(0.25)
                   if ("message" %in% names(sequence)) {
                     if (grepl("Provided transcript_id not found", sequence[1]$message)) {
                       ## substitute with shiny dialog
                       message("Provided transcript id not found.")
                       showModal(modalDialog(
                         title = "Transcript ID not found.",
                         HTML("Please provide a valid Ensembl transcript ID.")
                       ))
                     } else if ("Bad Request (HTTP 400)." %in% sequence[1]$message) {
                       sequenceb <- tryCatch(
                         get_flanking_genomic(
                           rvens$data,
                           useMart("ensembl", dataset = speciesdata),
                           "https://rest.ensembl.org",
                           specieslaturl,
                           flankSize = 200
                         ),
                         error = function(e)
                           e
                       )
                       incProgress(0.25)
                       if ("message" %in% names(sequenceb)) {
                         showModal(modalDialog(
                           title = "Error with automatic fetching occured",
                           HTML(
                             "Retrieving a target sequence using the Ensembl transcript ID is currently not possible. <br> <br> You can still use the direct sequence input to define your target. <br> <br> For further questions regarding the functionality of the online tool you can contact us at k.gubicza (at) zmbh.uni-heidelberg.de."
                           )
                         ))
                         message("Error with automatic fetching occured.")
                       } else {
                         message("Valid Ensembl ID")
                         updateTextInput(session,
                                         "inp_target",
                                         value = sequenceb[1])
                         updateTextInput(session,
                                         "genename",
                                         value = get_transcripts(
                                           rvens$data,
                                           useMart("ensembl", dataset = speciesdata)
                                         )[[1, 5]])
                         incProgress(0.25)
                         output$ensfeedback <- renderText({
                           paste(speciesname,
                                 get_transcripts(
                                   rvens$data,
                                   useMart("ensembl", dataset = speciesdata)
                                 )[[1, 5]],
                                 sep = ", ")
                         })
                       }
                     } else {
                       ## substitute with shiny dialog
                       showModal(modalDialog(
                         title = "Error with automatic fetching occured",
                         HTML(
                           "Retrieving a target sequence using the Ensembl transcript ID is currently not possible. <br> <br> You can still use the direct sequence input to define your target. <br> <br> For further questions regarding the functionality of the online tool you can contact us at k.gubicza (at) zmbh.uni-heidelberg.de."
                         )
                       ))
                       message("Error with automatic fetching occured")
                     }
                   } else {
                     ## ... code for primer identification...
                     message("Valid Ensembl ID")
                     
                     updateTextInput(session,
                                     "inp_target",
                                     value = sequence[1])
                     updateTextInput(session,
                                     "genename",
                                     value = get_transcripts(rvens$data,
                                                             useMart("ensembl", dataset = speciesdata))[[1, 5]])
                     incProgress(0.25)
                     output$ensfeedback <- renderText({
                       paste(speciesname,
                             get_transcripts(
                               rvens$data,
                               useMart("ensembl", dataset = speciesdata)
                             )[[1, 5]],
                             sep = ", ")
                     })
                   }
                   # Increment the progress bar, and update the detail text.
                   incProgress(1, detail = paste("Completed"))
                 })
               })
  observeEvent(input$linktotemplates, {
    updateNavbarPage(session, "home", "Template cassettes")
  })
  observeEvent(input$linktopcr, {
    updateNavbarPage(session, "home", "Cassette PCR")
  })
  observeEvent(input$linktotool, {
    updateNavbarPage(session, "home", "Online oligo design tool")
  })
  observeEvent(input$whatlinktotool, {
    updateNavbarPage(session, "home", "Online oligo design tool")
  })
  observe({
    if ("Other" %in% input$inp_cpf) {
      enable("inp_pam")
      enable("inp_dr")
      enable("inp_name")
    } else {
      disable("inp_pam")
      disable("inp_dr")
      disable("inp_name")
    }
  })
  rvpn <- reactiveValues(data = NULL) # Cas12a-choice of the user
  observe({
    if (input$phospho == 1) {
      enable("phosphonum")
      rvpn$data <- input$phosphonum
    } else {
      disable("phosphonum")
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
    updateTextInput(session, "inp_ensembl", value = paste("ENST00000284320.5"))
    updateTextInput(session, "genename", value = paste("TOMM70"))
    updateNumericInput(session, "threeha", value = 55)
    updateNumericInput(session, "fiveha", value = 90)
  })
  targetseq <- eventReactive(input$inp_target, {
    targetseq.tmp <- toupper(gsub("\\s", "", input$inp_target))
    return(targetseq.tmp)
  })
  output$inp_target_seq <- renderText({
    if (nchar(targetseq()) != 0) {
      paste0(
        '<span style = "font-style: italic; color:MediumBlue">',
        "The recognized sequence consists of ",
        nchar(targetseq()),
        " nt:",
        '<br>',
        '</span>'
      )
    }
  })
  output$targetsequence <- renderText({
    targetseq()
  })
  observeEvent(input$inp_target, {
    output$stopwarning <- renderText({
      ""
    })
    output$stopwarninge <- renderText({
      ""
    })
    output$cpf <- renderText({
      ""
    })
    output$cpfe <- renderText({
      ""
    })
    output$cpff <- renderText({
      ""
    })
    output$monetitle <- renderText({
      ""
    })
    output$monedescr <- renderText({
      ""
    })
    output$mtwotitle <- renderText({
      ""
    })
    output$mtwodescr <- renderText({
      ""
    })
    output$mtwocomment <- renderText({
      ""
    })
    output$mtwocommente <- renderText({
      ""
    })
    output$nucleotidese <- renderText({
      ""
    })
    output$nucleotidesre <- renderText({
      ""
    })
    output$mtwoext <- renderText({
      ""
    })
    output$nextstep <- renderText({
      ""
    })
    output$esearchspacetext <- renderText({
      ""
    })
    hideElement("extended")
    hideElement("inp_ext")
    hideElement("inp_apply")
    hideElement("downloadcsv")
    hideElement("downloadxlsx")
    #hideElement("searchspacee")
    hideElement("mtwocommente")
    hideElement("mtwocomment")
    hideElement("nextstep")
    hideElement("esearchspacetext")
    hideElement("esearchspaceoligos")
    # Input control - length and DNA alphabet
    if ((nchar(targetseq()) != 200 + 200 + 3) &
        (nchar(targetseq()) > 0) &
        (all(strsplit(targetseq(), "")[[1]] %in% DNA_ALPHABET))) {
      disableActionButton("compute", session)
      output$inputlength <-
        renderText({
          "Please provide a sequence of 403 nt."
        })
      output$inputnuc <- renderText({
        ""
      })
      output$nucleotides <- renderText({
        ""
      })
      output$nucleotidesr <- renderText({
        ""
      })
      output$searchspacee <- NULL
      output$nextstep <- renderText({
        ""
      })
      disableActionButton("compute", session)
      disableActionButton("runpcr", session)
      hideElement("extended")
      hideElement("inp_ext")
      hideElement("inp_apply")
      hideElement("downloadcsv")
      hideElement("downloadxlsx")
      hideElement("runpcr")
      disable("inp_pam")
      disable("inp_dr")
      disable("inp_name")
    } else if (nchar(targetseq()) == 0) {
      disableActionButton("compute", session)
      output$inputnuc <- renderText({
        ""
      })
      output$nucleotides <- renderText({
        ""
      })
      output$nucleotidesr <- renderText({
        ""
      })
      output$nextstep <- renderText({
        ""
      })
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
      disable("inp_dr")
      disable("inp_name")
    }
    else if ((any(strsplit(targetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
             (nchar(targetseq()) != 200 + 200 + 3)) {
      disableActionButton("compute", session)
      output$inputlength <-
        renderText({
          "Please provide a sequence of 403 nt."
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
      disableActionButton("compute", session)
      disableActionButton("runpcr", session)
      hideElement("extended")
      hideElement("inp_ext")
      hideElement("inp_apply")
      hideElement("downloadcsv")
      hideElement("downloadxlsx")
      hideElement("runpcr")
      disable("inp_pam")
      disable("inp_dr")
      disable("inp_name")
    }
    else if ((any(strsplit(targetseq(), "")[[1]] %ni% DNA_ALPHABET)) &
             (nchar(targetseq()) == 200 + 200 + 3)) {
      disableActionButton("compute", session)
      output$inputlength <- renderText({
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
      disableActionButton("compute", session)
      disableActionButton("runpcr", session)
      hideElement("extended")
      hideElement("inp_ext")
      hideElement("inp_apply")
      hideElement("downloadcsv")
      hideElement("downloadxlsx")
      hideElement("runpcr")
      disable("inp_pam")
      disable("inp_dr")
      disable("inp_name")
    }
    # Wen the input is correct, the further functions of the app become available
    else if ((all(strsplit(targetseq(), "")[[1]] %in% DNA_ALPHABET)) &
             (nchar(targetseq()) == 200 + 200 + 3)) {
      enableActionButton("compute", session)
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
      forwardoligo <- eventReactive({
        input$compute
        input$phospho
        input$phosphonum
      }, {
        fwmodha <- ""
        if (input$phospho == 1) {
          for (j in (1:rvpn$data)) {
            fwmodelement <-
              paste0(as.character(DNAString(targetseq())[(200 - rvfive$data + 1 + j - 1)]), "*")
            fwmodha <- paste0(fwmodha, fwmodelement)
          }
          fwmodharest <-
            as.character(DNAString(targetseq())[(200 - rvfive$data + 1 + rvpn$data):200])
          cone <-
            paste0(fwmodha,
                   fwmodharest,
                   DNAString("TCAGGTGGAGGAGGTAGTG"))
        } else {
          cone <-
            as.character(xscat(
              DNAString(targetseq())[(200 - rvfive$data + 1):200],
              DNAString("TCAGGTGGAGGAGGTAGTG")
            ))
        }
        return(cone)
      })
      output$regiontext <- renderText({
        paste(h4(em("Search space for PAM sites:")),
              h5(
                em(
                  "17 nucleotides upstream on the direct strand and 17 nucleotides downstream on the reverse strand"
                ),
                style = "color:MediumAquaMarine"
              ),
              sep = '\n')
      })
      # Target sequence output - direct strand
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
      output$csearchspacetext <- renderText({
        paste0(
          '<br>',
          '<span style = "font-style: italic; color:MediumBlue">',
          "The +/- 17 nt search space for PAM sites is the following:",
          '<br>',
          '<br>',
          '</span>'
        )
      })
      output$nucleotides <- regiond
      # Target sequence output - reverse strand
      regionr <- renderText({
        paste(
          as.character(complement(ntsstop()[1:10])),
          as.character(complement(ntsstop()[11:(17 + 10)])),
          '<span style = "color:red">',
          strong(as.character(complement(ntsstop(
            
          )[(17 + 10 + 1):(17 + 10 + 3)]))),
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
        output$stopwarning <- renderText({
          ""
        })
      } else {
        output$stopwarning <-
          renderText({
            stopwarning
          })
      }
      rvc <- reactiveValues(data = NULL) # Cas12a-choice of the user
      rvcp <-
        reactiveValues(data = NULL) # Cas12a plasmid names based on user choice
      rvname <-
        reactiveValues(data = NULL) # User defined plasmid name
      rvdr <-
        reactiveValues(data = NULL) # Direct repeat defined by the user
      rvpam <-
        reactiveValues(data = NULL) # PAM-site defined by the user
      rv <-
        reactiveValues(data = NULL) # Reverse oligo table output - 17 nt search space
      rve <-
        reactiveValues(data = NULL) # Reverse oligo table output - extended search space
      rvp <-
        reactiveValues(data = NULL) # Forward origo table output
      rvo <-
        reactiveValues(data = NULL) # .xlsx downloadable file output
      rvoc <-
        reactiveValues(data = NULL) # .csv downloadable file output
      rvg <-
        reactiveValues(data = NULL) # Gene name defined by the user
      rvei <-
        reactiveValues(data = NULL) # Length of extended search space defined by the user
      rvthree <-
        reactiveValues(data = NULL) # Length of 3'-homology arm defined by the user
      rvfive <-
        reactiveValues(data = NULL) # Length of 5'-homology arm defined by the user
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
      observeEvent(input$inp_dr, {
        rvdr$data <- input$inp_dr
      })
      observeEvent(input$inp_pam, {
        rvpam$data <- input$inp_pam
      })
      observe({
        # Input control for PAM-site and direct repeat if the user-defined option is selected
        if ("Other" %in% input$inp_cpf) {
          observe({
            if ((all(strsplit(rvpam$data, "")[[1]] %in% DNA_ALPHABET)) &
                (all(strsplit(rvdr$data, "")[[1]] %in% RNA_ALPHABET)) &
                (nchar(rvpam$data)) > 2 &
                (nchar(rvdr$data) > 10)) {
              enableActionButton("compute", session)
              output$pamdrlink <-               renderText({
                ""
              })
              output$drcontrol <- renderText({
                ""
              })
            } else {
              output$pamdrlink <-
                renderUI({
                  tagList("", urliupac)
                })
              output$drcontrol <- renderText({
                ""
              })
              disableActionButton("compute", session)
            }
          })
        } else if (is.null(input$inp_cpf)) {
          output$pamdrlink <- renderUI({
            ""
          })
          output$drcontrol <- renderText({
            "Please select at least one Cas12a (Cpf1) variant!"
          })
          disableActionButton("compute", session)
        } else {
          output$pamdrlink <- renderText({
            ""
          })
          output$drcontrol <- renderText({
            ""
          })
          enableActionButton("compute", session)
        }
      })
      observeEvent(input$inp_cpf, {
        rvc$data <- input$inp_cpf
      })
      output$cpf <- renderText({
        ""
      })
      output$cpfe <- renderText({
        ""
      })
      output$cpff <- renderText({
        ""
      })
      rvei$data <- 50
      # Length of extended search space gets updated, if button is clicked
      observeEvent(input$inp_apply, {
        rvei$data <- input$inp_ext
      })
      observeEvent({
        input$compute
        input$inp_apply
        input$genename
        input$threeha
        input$fiveha
        #input$phospho
        #input$phosphonum
      }
      , {
        # Looking for PAM-sites
        # tttv <<-
        #   lookforpam("LbCpf1_TTTV",
        #              "TTTV",
        #              rvc$data,
        #              targetseq(),
        #              rvei$data)
        if ("LbCpf1_TYCV" %in% rvc$data) {
          tycv <<-
            lookforpam(c("LbCpf1_TYCV"),
                       "TYYV",
                       rvc$data,
                       targetseq(),
                       rvei$data)
        } else {
          tycv <<-
            lookforpam(c("LbCpf1_TTTV"),
                       "TTTV",
                       rvc$data,
                       targetseq(),
                       rvei$data)
        }
        if ("AsCpf1_TATV" %in% rvc$data) {
          astttv <<-
            lookforpam("AsCpf1_TATV",
                       "TTTV",
                       rvc$data,
                       targetseq(),
                       rvei$data)
        } else if ("AsCpf1_TYCV" %in% rvc$data) {
          astttv <<-
            lookforpam("AsCpf1_TYCV",
                       "TTTV",
                       rvc$data,
                       targetseq(),
                       rvei$data)
        } else {
          astttv <<-
            lookforpam("AsCpf1_TTTV",
                       "TTTV",
                       rvc$data,
                       targetseq(),
                       rvei$data)
        }
        astycv <<-
          lookforpam("AsCpf1_TYCV",
                     "TYCV",
                     rvc$data,
                     targetseq(),
                     rvei$data)
        tatv <<-
          lookforpam("AsCpf1_TATV",
                     "TATV",
                     rvc$data,
                     targetseq(),
                     rvei$data)
        mccc <<-
          lookforpam("AsCpf1_TYCV",
                     "MCCC",
                     rvc$data,
                     targetseq(),
                     rvei$data)
        # asrrtttv <<-
        #   lookforpam("AsCpf1_TYCV",
        #              "TTTV",
        #              rvc$data,
        #              targetseq(),
        #              rvei$data)
        ratr <<-
          lookforpam("AsCpf1_TATV",
                     "RATR",
                     rvc$data,
                     targetseq(),
                     rvei$data)
        otherpams <<-
          lookforpam("Other", rvpam$data, rvc$data, targetseq(), rvei$data)
        cpfs <<- # The results get a "plasmid index"
          c(
            #rep("Lb_TTTV", times = length(tttv[[1]])),
            rep("Lb_TYCV", times = length(tycv[[1]])),
            rep("As_TTTV", times = length(astttv[[1]])),
            rep("As_TYCV", times = length(astycv[[1]])),
            rep("As_TATV", times = length(tatv[[1]])),
            rep("As_MCCC", times = length(mccc[[1]])),
            #rep("As_rrTTTV", times = length(asrrtttv[[1]])),
            rep("As_RATR", times = length(ratr[[1]])),
            rep("other", times = length(otherpams[[1]])),
            #rep("Lb_TTTV", times = length(tttv[[2]])),
            rep("Lb_TYCV", times = length(tycv[[2]])),
            rep("As_TTTV", times = length(astttv[[2]])),
            rep("As_TYCV", times = length(astycv[[2]])),
            rep("As_TATV", times = length(tatv[[2]])),
            rep("As_MCCC", times = length(mccc[[2]])),
            #rep("As_rrTTTV", times = length(asrrtttv[[2]])),
            rep("As_RATR", times = length(ratr[[2]])),
            rep("other", times = length(otherpams[[2]])),
            #rep("Lb_TTTV", times = length(tttv[[3]])),
            rep("Lb_TYCV", times = length(tycv[[3]])),
            rep("As_TTTV", times = length(astttv[[3]])),
            rep("As_TYCV", times = length(astycv[[3]])),
            rep("As_TATV", times = length(tatv[[3]])),
            rep("As_MCCC", times = length(mccc[[3]])),
            #rep("As_rrTTTV", times = length(asrrtttv[[3]])),
            rep("As_RATR", times = length(ratr[[3]])),
            rep("other", times = length(otherpams[[3]])),
            #rep("Lb_TTTV", times = length(tttv[[4]])),
            rep("Lb_TYCV", times = length(tycv[[4]])),
            rep("As_TTTV", times = length(astttv[[4]])),
            rep("As_TYCV", times = length(astycv[[4]])),
            rep("As_TATV", times = length(tatv[[4]])),
            rep("As_MCCC", times = length(mccc[[4]])),
            #rep("As_rrTTTV", times = length(asrrtttv[[4]])),
            rep("As_RATR", times = length(ratr[[4]])),
            rep("other", times = length(otherpams[[4]]))
          )
        # List of PAm sites grouped by strand and search space
        allpammd <- mergepamlists(1)
        allpammr <- mergepamlists(2)
        allpammdl <- mergepamlists(3)
        allpammrl <- mergepamlists(4)
        # Merge the four datasets in one data frame
        if (length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl) > 0) {
          allpamm <-
            matrix(
              rep(0, len = (
                length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
              ) * 7),
              length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
            )
          colnames(allpamm) <-
            c("start",
              "width",
              "PAM",
              "strand",
              "distance",
              "sort",
              "cpf")
          allpamm <- as.data.frame(allpamm)
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
          #sortedallpam <<- sortedallpam[sortedallpam$cpf != "As_rrTTTV" & sortedallpam$cpf != "Lb_TYCV", ]
          pamnumber <- nrow(sortedallpam)
          drs <- unique(subset(endonucleases, select = -c(PAM)))
          drs <- drs[order(drs$Name), ]
          dr <- rep("dr", times = pamnumber)
          crRNAs <- rep("crRNA", times = pamnumber)
          cpfname <- rep("cpfname", times = pamnumber)
          sugname <- rep("sugname", times = pamnumber)
          crRNAdr <- rep("crRNAdr", times = pamnumber)
          dim(crRNAs) <- c(pamnumber, 1)
          colnames(crRNAs) <- "crRNAs"
          oligorank <- rep(0, times = pamnumber)
          for (i in (1:pamnumber)) {
            if (sortedallpam$strand[i] == "direct") {
              crRNAstart <-
                sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 - 17
              crRNAs[i] <-
                as.character(reverseComplement(DNAString(targetseq())[(crRNAstart):(crRNAstart + 19)]))
            } else if (sortedallpam$strand[i] == "reverse") {
              crRNAstart <-
                sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 * (17 + 3 - sortedallpam$start[i]) + 200 - 17 - 2
              crRNAs[i] <-
                as.character(DNAString(targetseq())[(crRNAstart - 19):(crRNAstart)])
            } else if (sortedallpam$strand[i] == "direct*") {
              crRNAstart <-
                sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200
              crRNAs[i] <-
                as.character(reverseComplement(DNAString(targetseq())[(crRNAstart):(crRNAstart + 19)]))
            } else if (sortedallpam$strand[i] == "reverse*") {
              crRNAstart <-
                rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 + 203
              crRNAs[i] <-
                as.character(DNAString(targetseq())[(crRNAstart - 19):(crRNAstart)])
            }
            if (sortedallpam$cpf[i] == "Lb") {
              dr[i] <-
                gsub("U", "T", as.character(reverseComplement(RNAString(drs[drs$Name == "LbCas12a", 2]))))
            } else if (sortedallpam$cpf[i] == "As") {
              dr[i] <-
                gsub("U", "T", as.character(reverseComplement(RNAString(drs[drs$Name == "AsCas12a", 2]))))
            }
            # Oligos with lower rank numbers are considered to be more suitable for gene tagging.            if (grepl("AAAA", paste0(crRNAs[i], dr[i]))) {
            if (grepl("AAAA", paste0(crRNAs[i], dr[i]))) {
              oligorank[i] <- 6
            }
            if (countPattern(DNAString(sortedallpam$PAM[i]),
                             # Unconventional PAM sites
                             DNAString("MCCC"),
                             fixed = F) > 0 |
                countPattern(DNAString(sortedallpam$PAM[i]),
                             DNAString("RATR"),
                             fixed = F) > 0) {
              oligorank[i] <- oligorank[i] + 3
            }
            if (sortedallpam$strand[i] == "reverse") {
              # The cleavage site is always before the stop codon in this case
              oligorank[i] <- oligorank[i] + 1
            }
          }
          
          # Reverse oligo result ----------------------------------------------------
          
          summary <-
            # This will be the output table containing the reverse oligos and information about them
            matrix(rep(0, len = (pamnumber) * 9), nrow = (pamnumber))
          colnames(summary) <- colsum
          summary = as.data.frame(summary)
          csvoutput <-
            summary # Output table for .xlsx and .csv download
          withProgress(message = 'Computing...', value = 0, {
            # Number of times we'll go through the loop
            for (i in (1:pamnumber)) {
              # Each row contains one reverse oligo and the information about it
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TYYV"),
                               fixed = F) > 0 &
                  ("LbCpf1_TYCV"  %in% rvc$data)) {
                cpfname[i] <- "LbCpf1_TYCV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "LbCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("LbCpf1_TTTV"  %in% rvc$data) &
                  ("LbCpf1_TYCV"  %in% rvc$data)) {
                cpfname[i] <- "LbCpf1_TTTV/LbCpf1_TYCV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "LbCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("LbCpf1_TTTV"  %in% rvc$data) &
                  ("LbCpf1_TYCV"  %ni% rvc$data)) {
                cpfname[i] <- "LbCpf1_TTTV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "LbCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TTTV"  %in% rvc$data)  &
                  sortedallpam$cpf[i] == "As_TTTV") {
                cpfname[i] <- "AsCpf1_TTTV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %in% rvc$data)  &
                  ("AsCpf1_TYCV"  %ni% rvc$data) &
                  sortedallpam$cpf[i] == "As_TTTV") {
                cpfname[i] <- "AsCpf1_TATV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %ni% rvc$data)  &
                  ("AsCpf1_TYCV"  %in% rvc$data) &
                  sortedallpam$cpf[i] == "As_TTTV") {
                cpfname[i] <- "AsCpf1_TYCV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %in% rvc$data)  &
                  ("AsCpf1_TYCV"  %in% rvc$data) &
                  ("AsCpf1_TTTV"  %ni% rvc$data) &
                  sortedallpam$cpf[i] == "As_TTTV") {
                cpfname[i] <- "AsCpf1_TATV/AsCpf1_TYCV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %in% rvc$data)  &
                  ("AsCpf1_TYCV"  %in% rvc$data) &
                  ("AsCpf1_TTTV"  %in% rvc$data) &
                  sortedallpam$cpf[i] == "As_TTTV") {
                cpfname[i] <- "AsCpf1_TTTV/AsCpf1_TATV/AsCpf1_TYCV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %ni% rvc$data)  &
                  ("AsCpf1_TYCV"  %in% rvc$data) &
                  ("AsCpf1_TTTV"  %in% rvc$data) &
                  sortedallpam$cpf[i] == "As_TTTV") {
                cpfname[i] <- "AsCpf1_TTTV/AsCpf1_TYCV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TTTV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %in% rvc$data)  &
                  ("AsCpf1_TYCV"  %ni% rvc$data) &
                  ("AsCpf1_TTTV"  %in% rvc$data) &
                  sortedallpam$cpf[i] == "As_TTTV") {
                cpfname[i] <- "AsCpf1_TTTV/AsCpf1_TATV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TYCV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TYCV"  %in% rvc$data)  &
                  sortedallpam$cpf[i] == "As_TYCV") {
                cpfname[i] <- "AsCpf1_TYCV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("MCCC"),
                               fixed = F) > 0 &
                  ("AsCpf1_TYCV"  %in% rvc$data)  &
                  sortedallpam$cpf[i] == "As_MCCC") {
                cpfname[i] <- "AsCpf1_TYCV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("TATV"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %in% rvc$data)  &
                  sortedallpam$cpf[i] == "As_TATV") {
                cpfname[i] <- "AsCpf1_TATV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString("RATR"),
                               fixed = F) > 0 &
                  ("AsCpf1_TATV"  %in% rvc$data)  &
                  sortedallpam$cpf[i] == "As_RATR") {
                cpfname[i] <- "AsCpf1_TATV"
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    drs[drs$Name == "AsCas12a", 2]
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (countPattern(DNAString(sortedallpam$PAM[i]),
                               DNAString(rvpam$data),
                               fixed = F > 0) &
                  "Other"  %in% rvc$data &
                  sortedallpam$cpf[i] == "other") {
                cpfname[i] <-
                  paste(rvname$data, sep = "_")
                sugname[i] <-
                  paste0("M2_", rvg$data, "_", cpfname[i])
                dr[i] <-
                  gsub("U", "T", as.character(reverseComplement(RNAString(
                    rvdr$data
                  ))))
                crRNAdr[i] <-
                  gsub("T", "U", reverseComplement(DNAString(dr[i])))
              }
              if (i == 1) {
                allpamcrrna <-
                  DNAStringSet(paste0(
                    sortedallpam$PAM[i],
                    as.character(reverseComplement(DNAString(
                      crRNAs[i]
                    )))
                  ))
              } else {
                allpamcrrna <-
                  c(allpamcrrna, DNAStringSet(paste0(
                    sortedallpam$PAM[i],
                    as.character(reverseComplement(DNAString(
                      crRNAs[i]
                    )))
                  )))
              }
              summary$`Cas12a (Cpf1)`[i] <-
                cpfname[i] # Cas 12a plasmid name
              summary$PAM[i] <- paste(sortedallpam$PAM[i])
              summary$`crRNA (direct repeat + spacer)`[i] <-
                paste(
                  '<span style = "text-decoration: underline; color:orange; text-decoration-color: black;">',
                  as.character(crRNAdr[i]),
                  '</span>',
                  '<span style = "color:orange">',
                  gsub("T", "U", reverseComplement(DNAString(
                    crRNAs[i]
                  ))),
                  '</span>',
                  sep = ""
                )
              summary$`Suggested name`[i] <- sugname[i]
              if ((sortedallpam$strand[i] == "direct") |
                  (sortedallpam$strand[i] == "reverse")) {
                if (input$phospho == 1) {
                  modha <- ""
                  for (j in (1:rvpn$data)) {
                    modelement <- paste0(as.character(reverseComplement(
                      DNAString(targetseq())[(200 + 4 + rvthree$data - 1 - j + 1)]
                    )), "*")
                    modha <- paste0(modha, modelement)
                  }
                  modharest <-
                    as.character(reverseComplement(DNAString(targetseq())[(200 + 4):(200 + 4 + rvthree$data - 1 - rvpn$data)]))
                  summary$Sequence[i] <-
                    paste0(
                      '<span style = "color:CornflowerBlue ">',
                      modha,
                      modharest,
                      '</span>',
                      '<span style = "color:brown">',
                      "AAAAAA",
                      '</span>',
                      '<span style = "color:orange">',
                      crRNAs[i],
                      '</span>',
                      '<span style = "text-decoration: underline; color:orange; text-decoration-color: black;">',
                      dr[i],
                      '</span>',
                      '<span style = "color:grey">',
                      "GCTAGCTGCATCGGTACC",
                      '</span>'
                    )
                } else {
                  summary$Sequence[i] <-  # Reverse oligo sequence
                    paste0(
                      '<span style = "color:CornflowerBlue ">',
                      as.character(reverseComplement(
                        DNAString(targetseq())[(200 + 4):(200 + 4 + rvthree$data - 1)]
                      )),
                      '</span>',
                      '<span style = "color:brown">',
                      "AAAAAA",
                      '</span>',
                      '<span style = "color:orange">',
                      crRNAs[i],
                      '</span>',
                      '<span style = "text-decoration: underline; color:orange; text-decoration-color: black;">',
                      dr[i],
                      '</span>',
                      '<span style = "color:grey">',
                      "GCTAGCTGCATCGGTACC",
                      '</span>'
                    )
                }
                summary$Strand[i] <-
                  # PAM site found on the direct or reverse strand
                  paste(as.character(sortedallpam$strand[i]))
              }
              else if (sortedallpam$strand[i] == "direct*") {
                summary$Strand[i] <- "direct"
                if (input$phospho == 1) {
                  modha <- ""
                  for (j in (1:rvpn$data)) {
                    modelement <- paste0(as.character(reverseComplement(
                      DNAString(targetseq())[(
                        sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(crRNAs[i]) + rvthree$data - 1 - j + 1
                      )]
                    )), "*")
                    modha <- paste0(modha, modelement)
                  }
                  modharest <-
                    as.character(reverseComplement(DNAString(targetseq())[(
                      sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(crRNAs[i])
                    ):(
                      sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(crRNAs[i]) + rvthree$data - 1 - rvpn$data
                    )]))
                  summary$Sequence[i] <-
                    paste0(
                      '<span style = "color:CornflowerBlue ">',
                      modha,
                      modharest,
                      '</span>',
                      '<span style = "color:brown">',
                      "AAAAAA",
                      '</span>',
                      '<span style = "color:orange">',
                      crRNAs[i],
                      '</span>',
                      '<span style = "text-decoration: underline; color:orange; text-decoration-color: black;">',
                      dr[i],
                      '</span>',
                      '<span style = "color:grey">',
                      "GCTAGCTGCATCGGTACC",
                      '</span>'
                    )
                } else {
                  summary$Sequence[i] <-
                    paste0(
                      '<span style = "color:CornflowerBlue ">',
                      as.character(reverseComplement(
                        DNAString(targetseq())[(
                          sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(crRNAs[i])
                        ):(
                          sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(crRNAs[i]) + rvthree$data - 1
                        )]
                      )),
                      '</span>',
                      '<span style = "color:brown">',
                      "AAAAAA",
                      '</span>',
                      '<span style = "color:orange">',
                      crRNAs[i],
                      '</span>',
                      '<span style = "text-decoration: underline; color:orange; text-decoration-color: black;">',
                      dr[i],
                      '</span>',
                      '<span style = "color:grey">',
                      "GCTAGCTGCATCGGTACC",
                      '</span>'
                    )
                }
              } else if (sortedallpam$strand[i] == "reverse*") {
                summary$Strand[i] <- "reverse"
                if (input$phospho == 1) {
                  modha <- ""
                  for (j in (1:rvpn$data)) {
                    modelement <- paste0(as.character(reverseComplement(
                      DNAString(targetseq())[(
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + rvthree$data - 1 - j + 1
                      )]
                    )), "*")
                    modha <- paste0(modha, modelement)
                  }
                  modharest <-
                    as.character(reverseComplement(DNAString(targetseq())[(203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2):(
                      203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + rvthree$data - 1 - rvpn$data
                    )]))
                  summary$Sequence[i] <-
                    paste0(
                      '<span style = "color:CornflowerBlue ">',
                      modha,
                      modharest,
                      '</span>',
                      '<span style = "color:brown">',
                      "AAAAAA",
                      '</span>',
                      '<span style = "color:orange">',
                      crRNAs[i],
                      '</span>',
                      '<span style = "text-decoration: underline; color:orange; text-decoration-color: black;">',
                      dr[i],
                      '</span>',
                      '<span style = "color:grey">',
                      "GCTAGCTGCATCGGTACC",
                      '</span>'
                    )
                } else {
                  summary$Sequence[i] <-
                    paste0(
                      '<span style = "color:CornflowerBlue ">',
                      as.character(reverseComplement(
                        DNAString(targetseq())[(203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2):(
                          203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + rvthree$data - 1
                        )]
                      )),
                      '</span>',
                      '<span style = "color:brown">',
                      "AAAAAA",
                      '</span>',
                      '<span style = "color:orange">',
                      crRNAs[i],
                      '</span>',
                      '<span style = "text-decoration: underline; color:orange; text-decoration-color: black;">',
                      dr[i],
                      '</span>',
                      '<span style = "color:grey">',
                      "GCTAGCTGCATCGGTACC",
                      '</span>'
                    )
                }
              }
              summary$Length[i] <- # Length of the revers oligo (nt)
                paste(format(round(nchar(
                  paste(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(200 + 4):(200 + 4 + rvthree$data - 1)])),
                    "AAAAAA",
                    crRNAs[i],
                    dr[i],
                    "GCTAGCTGCATCGGTACC",
                    sep = ""
                  )
                ), 0), nsmall = 0), "nt", sep = "&nbsp")
              stopstart <- (17 + 10 + 1)
              stopend <- (17 + 10 + 3)
              
              # Target sequence formatting ----------------------------------------------------------------------
              
              # For the formatted visualization of the target sequence with the PAM site, the cleavage site and the overhangs, the sequence was divided into
              # several sections, which were defined according to the cleavage site location in each case
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
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(crRNAs[i])
                      ),
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(crRNAs[i]) + 2
                      )
                    ),
                    '</span>',
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    substr(
                      ntsstop(),
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(crRNAs[i]) + 3
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
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(crRNAs[i]) - 3
                      )
                    ),
                    '</span>'
                  )
                bluerr <-
                  paste0(
                    '<span style = "color:orange; text-decoration: underline; text-decoration-color: black;">',
                    substr(
                      ntsstop(),
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(crRNAs[i]) - 2
                      ),
                      (
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(crRNAs[i]) - 1
                      )
                    ),
                    '</span>'
                  )
                pamstart <-
                  sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) - 1
                pamend <- 10 + 17
                if (pamstart < pamend) {
                  MediumAquaMarine <-
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      substr(
                        ntsstop(),
                        (sortedallpam$start[i] + 10),
                        (
                          sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) - 1
                        )
                      ),
                      '</span>'
                    )
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
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      substr(
                        ntsstop(),
                        (sortedallpam$start[i] + 10),
                        (
                          sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) - 1
                        )
                      ),
                      '</span>'
                    )
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
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      substr(
                        ntsstop(),
                        (sortedallpam$start[i] + 10),
                        (stopstart - 1)
                      ),
                      '</span>'
                    )
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
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      substr(
                        ntsstop(),
                        (sortedallpam$start[i] + 10),
                        (stopstart - 1)
                      ),
                      '</span>'
                    )
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
                    paste0(
                      '<span style = "color:MediumAquaMarine">',
                      substr(
                        ntsstop(),
                        (sortedallpam$start[i] + 10),
                        (stopstart - 1)
                      ),
                      '</span>'
                    )
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
                      as.character(complement(ntsstop()[(stopend + 1):(
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(crRNAs[i]) - 3
                      )])),
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline">',
                      as.character(complement(ntsstop()[(
                        sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + nchar(crRNAs[i]) - 2
                      ):(sortedallpam$start[i] + 10 + nchar(sortedallpam$PAM[i]) + 22)])),
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
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(crRNAs[i]) - 3
                    )])),
                    '<span style = "color:GhostWhite">',
                    "_",
                    '</span>',
                    '<span style = "text-decoration: underline">',
                    as.character(complement(ntsstop()[(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(crRNAs[i]) - 2
                    ):(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(crRNAs[i])
                    )])),
                    '</span>'
                  )
                bluerr <-
                  paste0(
                    '<span style = "color:orange; text-decoration: underline; text-decoration-color: black;">',
                    as.character(complement(ntsstop()[(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(crRNAs[i]) + 1
                    ):(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i])  - nchar(crRNAs[i]) + 2
                    )])),
                    '</span>'
                  )
                bluer <-
                  paste0(
                    '<span style = "color:orange">',
                    as.character(complement(ntsstop()[(
                      nchar(as.character(ntsstop())) - sortedallpam$start[i] - 10 + 1 - nchar(sortedallpam$PAM[i]) - nchar(crRNAs[i]) + 2 + 1
                    ):(stopstart - 1)])),
                    '</span>'
                  )
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
                    paste0(
                      '<span style = "color:orange">',
                      as.character(complement(ntsstop()[(stopend + 1):(pamstart - 1)])),
                      '</span>'
                    )
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
                reversestrand <- paste0(
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
                if (sortedallpam$start[i] == 1) {
                  directstrand <- paste0(
                    substr(targetseq(), 194, 200),
                    '<span style = "color:MediumVioletRed">',
                    strong(substr(targetseq(), 201, 203)),
                    substr(targetseq(),
                           204,
                           204),
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
                    '<span style = "text-decoration: underline; text-decoration-color: black;">',
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
                  )
                } else if (sortedallpam$start[i] == 2) {
                  directstrand <- paste0(
                    substr(targetseq(), 194, 200),
                    strong(substr(targetseq(), 201, 201)),
                    '<span style = "color:MediumVioletRed">',
                    strong(substr(targetseq(), 202, 203)),
                    substr(targetseq(),
                           203, 204),
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
                    '<span style = "text-decoration: underline; text-decoration-color: black;">',
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
                  )
                } else if (sortedallpam$start[i] == 3) {
                  directstrand <- paste0(
                    substr(targetseq(), 194, 200),
                    strong(substr(targetseq(), 201, 202)),
                    '<span style = "color:MediumVioletRed">',
                    strong(substr(targetseq(), 203, 203)),
                    substr(targetseq(),
                           204, 204),
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
                    '<span style = "text-decoration: underline; text-decoration-color: black;">',
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
                  )
                } else {
                  directstrand <- paste0(
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
                    '<span style = "text-decoration: underline; text-decoration-color: black;">',
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
                  )
                }
                summary$Target[i] <- paste0(directstrand,
                                            '<br>',
                                            reversestrand)
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
                # more than 3 nt distance between stop and crRNA end
                if ((rvei$data - sortedallpam$start[i] - 2) > (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
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
                      complement(DNAString(
                        substr(
                          targetseq(),
                          (
                            203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 22
                          ),
                          (
                            203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 1 - 20
                          )
                        )
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
                  # 3 nt distance between stop and crRNA end
                }
                else if ((rvei$data - sortedallpam$start[i] - 2) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  blackbold <-
                    paste0(
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 203)
                      ))),
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>'
                    )
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
                    substr(targetseq(), 209, (204 + 17 + 125))
                  )
                  # 2 nt distance between stop and crRNA end
                } else if ((rvei$data - sortedallpam$start[i] - 1) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
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
                    substr(targetseq(), 208, (204 + 17 + 125))
                  )
                  # 1 nt distance between stop and crRNA end
                }
                else if ((rvei$data - sortedallpam$start[i]) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
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
                    substr(targetseq(), 207, (204 + 17 + 125))
                  )
                  # no distance between stop and crRNA end
                } else if ((rvei$data - sortedallpam$start[i] + 1) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
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
                    substr(targetseq(), 206, (204 + 17 + 125))
                  )
                }
                # crRNA end overlaps with the STOP codon - 1 nt
                else if ((rvei$data - sortedallpam$start[i] + 2) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
                  blackr <-
                    complement(DNAString(substr(targetseq(), 194, 199)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline; text-decoration-color: black;">',
                      complement(DNAString(
                        substr(targetseq(), 200, 200)
                      )),
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 202)
                      ))),
                      '<span style = "color:orange">',
                      strong(complement(DNAString(
                        substr(targetseq(), 203, 203)
                      ))),
                      complement(DNAString(
                        substr(targetseq(), 204, 204)
                      )),
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
                    substr(targetseq(), 205, (204 + 17 + 125))
                  )
                }
                # crRNA end overlaps with the STOP codon - 2 nt
                else if ((rvei$data - sortedallpam$start[i] + 3) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
                  blackr <-
                    complement(DNAString(substr(targetseq(), 194, 198)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline; text-decoration-color: black;">',
                      complement(DNAString(
                        substr(targetseq(), 199, 200)
                      )),
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
                    substr(targetseq(), 204, (204 + 17 + 125))
                  )
                }
                # crRNA end overlaps with the STOP codon - 3 nt
                else if ((rvei$data - sortedallpam$start[i] + 4) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
                  blackr <-
                    complement(DNAString(substr(targetseq(), 194, 197)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline; text-decoration-color: black;">',
                      complement(DNAString(
                        substr(targetseq(), 198, 200)
                      )),
                      '<span style = "color:orange">',
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 202)
                      ))),
                      '</span>',
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- ""
                  bluer <- ""
                  blue <-
                    paste0(strong(complement(DNAString(
                      substr(targetseq(), 203, 203)
                    ))),
                    complement(DNAString(
                      substr(targetseq(), 204, 204 + 17 - 1)
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
                    substr(targetseq(), 204, (204 + 17 + 125))
                  )
                }
                # crRNA end overlaps with the STOP codon - 3 nt + 1
                else if ((rvei$data - sortedallpam$start[i] + 5) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
                  blackr <-
                    complement(DNAString(substr(targetseq(), 194, 196)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline; text-decoration-color: black;">',
                      complement(DNAString(
                        substr(targetseq(), 197, 199)
                      )),
                      '<span style = "color:orange">',
                      complement(DNAString(
                        substr(targetseq(), 200, 200)
                      )),
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 201)
                      ))),
                      '</span>',
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- ""
                  bluer <- ""
                  blue <-
                    paste0(strong(complement(DNAString(
                      substr(targetseq(), 202, 203)
                    ))),
                    complement(DNAString(
                      substr(targetseq(), 203, 203 + 16 - 1)
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
                    substr(targetseq(), 204, (204 + 17 + 125))
                  )
                }
                # crRNA end overlaps with the STOP codon - 3 nt + 2
                else if ((rvei$data - sortedallpam$start[i] + 6) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
                  blackr <-
                    complement(DNAString(substr(targetseq(), 194, 195)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline; text-decoration-color: black;">',
                      complement(DNAString(
                        substr(targetseq(), 196, 198)
                      )),
                      '<span style = "color:orange">',
                      complement(DNAString(
                        substr(targetseq(), 199, 200)
                      )),
                      '</span>',
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- ""
                  bluer <- ""
                  blue <-
                    paste0(strong(complement(DNAString(
                      substr(targetseq(), 201, 203)
                    ))),
                    complement(DNAString(
                      substr(targetseq(), 203, 203 + 15 - 1)
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
                    substr(targetseq(), 204, (204 + 17 + 125))
                  )
                }
                # crRNA end overlaps with the STOP codon - 3 nt + 3
                else if ((rvei$data - sortedallpam$start[i] + 7) == (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
                  blackr <-
                    complement(DNAString(substr(targetseq(), 194, 194)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline; text-decoration-color: black;">',
                      complement(DNAString(
                        substr(targetseq(), 195, 197)
                      )),
                      '<span style = "color:orange">',
                      complement(DNAString(
                        substr(targetseq(), 198, 199)
                      )),
                      '</span>',
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <- ""
                  bluer <- ""
                  blue <-
                    paste0(
                      complement(DNAString(
                        substr(targetseq(), 199, 200)
                      )),
                      strong(complement(DNAString(
                        substr(targetseq(), 201, 203)
                      ))),
                      complement(DNAString(
                        substr(targetseq(), 204, 204 + 14 - 1)
                      ))
                    )
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
                    substr(targetseq(), 204, (204 + 17 + 125))
                  )
                }
                else if ((rvei$data - sortedallpam$start[i] + 7) < (nchar(crRNAs[i]) + nchar(sortedallpam$PAM[i]))) {
                  oligorank[i] <- oligorank[i] + 1
                  blackr <-
                    complement(DNAString(substr(targetseq(), 194, 194)))
                  blackbold <-
                    paste0(
                      '<span style = "color:GhostWhite">',
                      "_",
                      '</span>',
                      '<span style = "text-decoration: underline; text-decoration-color: black;">',
                      complement(DNAString(
                        substr(targetseq(), 195, 197)
                      )),
                      '<span style = "color:orange">',
                      complement(DNAString(
                        substr(targetseq(), 198, 199)
                      )),
                      '</span>',
                      '</span>',
                      '<span style = "color:orange">',
                      complement(DNAString(
                        substr(targetseq(), 199, 200)
                      )),
                      '</span>'
                    )
                  blackrr <- ""
                  bluebold <-
                    paste0(strong(complement(DNAString(
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
                    substr(targetseq(), 204, (204 + 17 + 125))
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
                    '<span style = "color:orange; text-decoration: underline; text-decoration-color: black;">',
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
              
              # End of target sequence formatting ---------------------------------------
              
              # Output table for download, without html-formatting
              csvoutput$`Cas12a (Cpf1)`[i] <- cpfname[i]
              csvoutput$PAM[i] <- sortedallpam$PAM[i]
              csvoutput$Strand[i] <-
                as.character(sortedallpam$strand[i])
              csvoutput$`crRNA (direct repeat + spacer)`[i] <-
                paste0(as.character(crRNAdr[i]),
                       gsub("T", "U", reverseComplement(DNAString(
                         crRNAs[i]
                       ))))
              csvoutput$`Suggested name`[i] <- sugname[i]
              if ((sortedallpam$strand[i] == "direct") |
                  (sortedallpam$strand[i] == "reverse")) {
                if (input$phospho == 1) {
                  modha <- ""
                  for (j in (1:rvpn$data)) {
                    modelement <- paste0(as.character(reverseComplement(
                      DNAString(targetseq())[(200 + 4 + rvthree$data - 1 - j + 1)]
                    )), "*")
                    modha <- paste0(modha, modelement)
                  }
                  modharest <-
                    as.character(reverseComplement(DNAString(targetseq())[(200 + 4):(200 + 4 + rvthree$data - 1 - rvpn$data)]))
                  csvoutput$Sequence[i] <-
                    paste0(modha,
                           modharest,
                           "AAAAAA",
                           crRNAs[i],
                           dr[i],
                           "GCTAGCTGCATCGGTACC")
                } else {
                  csvoutput$Sequence[i] <-
                    paste0(
                      as.character(reverseComplement(
                        DNAString(targetseq())[(200 + 4):(200 + 4 + rvthree$data - 1)]
                      )),
                      "AAAAAA",
                      crRNAs[i],
                      dr[i],
                      "GCTAGCTGCATCGGTACC"
                    )
                }
              }
              else if (sortedallpam$strand[i] == "direct*") {
                csvoutput$Strand[i] <- "direct"
                if (input$phospho == 1) {
                  modha <- ""
                  for (j in (1:rvpn$data)) {
                    modelement <- paste0(as.character(reverseComplement(
                      DNAString(targetseq())[(
                        sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(crRNAs[i]) + rvthree$data - 1 - j + 1
                      )]
                    )), "*")
                    modha <- paste0(modha, modelement)
                  }
                  modharest <-
                    as.character(reverseComplement(DNAString(targetseq())[(
                      sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(crRNAs[i])
                    ):(
                      sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 200 + nchar(crRNAs[i]) + rvthree$data - 1 - rvpn$data
                    )]))
                  csvoutput$Sequence[i] <-
                    paste0(modha,
                           modharest,
                           "AAAAAA",
                           crRNAs[i],
                           dr[i],
                           "GCTAGCTGCATCGGTACC")
                } else {
                  csvoutput$Sequence[i] <-
                    paste0(
                      as.character(reverseComplement(
                        DNAString(targetseq())[(
                          sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(crRNAs[i])
                        ):(
                          sortedallpam$start[i] + nchar(sortedallpam$PAM[i]) + 203 + nchar(crRNAs[i]) + rvthree$data - 1
                        )]
                      )),
                      "AAAAAA",
                      crRNAs[i],
                      dr[i],
                      "GCTAGCTGCATCGGTACC"
                    )
                }
              } else if (sortedallpam$strand[i] == "reverse*") {
                csvoutput$Strand[i] <- "reverse"
                if (input$phospho == 1) {
                  modha <- ""
                  for (j in (1:rvpn$data)) {
                    modelement <- paste0(as.character(reverseComplement(
                      DNAString(targetseq())[(
                        203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + rvthree$data - 1 - j + 1
                      )]
                    )), "*")
                    modha <- paste0(modha, modelement)
                  }
                  modharest <-
                    as.character(reverseComplement(DNAString(targetseq())[(203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2):(
                      203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + rvthree$data - 1 - rvpn$data
                    )]))
                  csvoutput$Sequence[i] <-
                    paste0(
                      modha,
                      modharest,
                      "AAAAAA",
                      crRNAs[i],
                      dr[i],
                      "GCTAGCTGCATCGGTACC",
                      sep = ""
                    )
                } else {
                  csvoutput$Sequence[i] <-
                    paste0(
                      as.character(reverseComplement(
                        DNAString(targetseq())[(203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2):(
                          203 + rvei$data - sortedallpam$start[i] - nchar(sortedallpam$PAM[i]) + 2 + rvthree$data - 1
                        )]
                      )),
                      "AAAAAA",
                      crRNAs[i],
                      dr[i],
                      "GCTAGCTGCATCGGTACC",
                      sep = ""
                    )
                }
              }
              csvoutput$Length[i] <-
                paste(format(round(nchar(
                  paste(
                    as.character(reverseComplement(DNAString(
                      targetseq()
                    )[(200 + 4):(200 + 4 + rvthree$data - 1)])),
                    "AAAAAA",
                    crRNAs[i],
                    dr[i],
                    "GCTAGCTGCATCGGTACC",
                    sep = ""
                  )
                ), 0), nsmall = 0), " nt")
              csvoutput <-
                csvoutput[-c(pamnumber + 1, pamnumber + 2, pamnumber + 3), ]
              csvoutput$Target <- NULL
              # Increment the progress bar, and update the detail text.
              incProgress(1 / pamnumber,
                          detail = paste("Reverse oligo", i, "/", pamnumber))
              
              # Pause for 0.1 seconds to simulate a long computation.
              #Sys.sleep(0.1)
            }
          })
          # Reverse oligo - result table column names
          colnames(summary)[colnames(summary) == "Sequence"] <-
            paste0(
              '<span style = "font-weight:bold; font-family: Arial">',
              "Sequence: ",
              '<span style = "color:CornflowerBlue ">',
              "3'-homology: ",
              rvthree$data,
              " bases after the insertion site, reverse orientation",
              '</span>',
              " + ",
              '<span style = "color:brown">',
              "Pol III terminator",
              '</span>',
              " + ",
              '<br>',
              '<span style = "color:orange">',
              "crRNA spacer encoding sequence + ",
              '</span>',
              '<span style = "text-decoration: underline; color:orange; text-decoration-color: black;">',
              "crRNA direct repeat encoding sequence",
              '</span>',
              " + ",
              '<span style = "color:grey">',
              "primer binding reverse oligo",
              '</span>',
              '</span>'
            )
          colnames(summary)[colnames(summary) == "crRNA (direct repeat + spacer)"] <-
            paste0(
              '<span style = "color:orange">',
              "crRNA (",
              '<span style = "text-decoration: underline; color:orange; text-decoration-color: black;">',
              "direct repeat",
              '</span>',
              " + spacer)",
              '</span>'
            )
          colnames(summary)[colnames(summary) == "PAM"] <- "PAM"
          
          # Forward oligo result ----------------------------------------------------
          
          csvforward <-  matrix(rep(0, len = (1) * 8), nrow = 1)
          csvforward[1, ] <- c(
            paste0("M1_", rvg$data),
            forwardoligo(),
            paste(nchar(forwardoligo()), "nt"),
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
          if (input$phospho == 1) {
            fwmodha <- ""
            for (j in (1:rvpn$data)) {
              fwmodelement <-
                paste0(as.character(DNAString(targetseq())[(200 - rvfive$data + 1 + j - 1)]), "*")
              fwmodha <- paste0(fwmodha, fwmodelement)
            }
            fwmodharest <-
              as.character(DNAString(targetseq())[(200 - rvfive$data + 1 + rvpn$data):200])
            summaryf$`Sequence`[1] <-
              paste0(
                '<span style = "color:green">',
                fwmodha,
                fwmodharest,
                '</span>',
                '<span style = "color:DodgerBlue">',
                "TCAGGTGGAGGAGGTAGTG",
                '</span>'
              )
          } else {
            summaryf$`Sequence`[1] <-
              paste0(
                '<span style = "color:green">',
                as.character(DNAString(targetseq())[(200 - rvfive$data + 1):200]),
                '</span>',
                '<span style = "color:DodgerBlue">',
                "TCAGGTGGAGGAGGTAGTG",
                '</span>'
              )
          }
          summaryf$`Length`[1] <-
            paste(nchar(as.character(forwardoligo())), "nt", sep = "&nbsp")
          colnames(summaryf)[colnames(summaryf) == "Sequence"] <-
            paste(
              strong("Sequence:"),
              '<span style = "color:green">',
              "5'-homology: ",
              rvfive$data,
              " bases before the insertion site, direct orientation",
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
          
          # Output values -----------------------------------------------------------
          
          # Textoutputs to the results
          output$monetitle <- renderText({
            monetitle
          })
          output$monedescr <- renderText({
            monedescr
          })
          output$mtwotitle <- renderText({
            mtwotitle
          })
          output$mtwodescr <- renderText({
            mtwodescr
          })
          output$mtwoext <- renderText({
            mtwoextb
          })
          output$searchspacee <- renderUI({
            img(src = 'searchspace_e.png',
                width = 800,
                align = "right")
          })
          showElement("extended")
          showElement("inp_ext")
          showElement("inp_apply")
          showElement("downloadcsv")
          showElement("downloadxlsx")
          showElement("report")
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
                  nchar(rvdr$data) > 10,
                  'Please specify the direct repeat sequence'
                ))
                rv$data
              }, sanitize.text.function = function(x)
                x)
              output$forwardoligo <- renderTable({
                validate(need(input$inp_target != "", ''))
                validate(need(nchar(rvpam$data) > 2, ''))
                validate(need(nchar(rvdr$data) > 5, ''))
                rvp$data
              }, sanitize.text.function = function(x)
                x)
            } else if ("Other" %ni% input$inp_cpf) {
              output$cpf <- renderTable({
                # validate(need(
                #   input$inp_target != "",
                #   'Please provide a nucleotide sequence.'
                # ))
                # validate(need(
                #   input$inp_cpf != "",
                #   'Select at least one Cas12a (Cpf1) variant.'
                # ))
                rv$data
              }, sanitize.text.function = function(x)
                x)
              output$cpff <- renderTable({
                rvp$data
              }, sanitize.text.function = function(x)
                x)
            }
          })
          csvoutputr <<- NULL
          csvoutpute <- NULL
          
          # PAM sites found in the 17 nt search space -------------------------------
          
          if (length(allpammd) + length(allpammr) > 0) {
            enable("downloadcsv")
            enable("downloadxlsx")
            enable("report")
            summaryr <-
              # First part of the reverse oligo output table with PAM sites found in the 17 nt search space
              summary [c(1:(length(allpammd) + length(allpammr))), ]
            oligorankr <-
              oligorank[(1:(length(allpammd) + length(allpammr)))]
            allpamcrrnar <-
              allpamcrrna[c(1:(length(allpammd) + length(allpammr))), ]
            summaryr <-
              summaryr[order(sortedallpam$distance[1:(length(allpammd) + length(allpammr))]), ]
            oligorankr <-
              oligorankr[order(sortedallpam$distance[1:(length(allpammd) + length(allpammr))])]
            allpamcrrnar <-
              allpamcrrnar[order(sortedallpam$distance[1:(length(allpammd) + length(allpammr))]), ]
            summaryr <- summaryr[order(oligorankr), ]
            allpamcrrnar <- allpamcrrnar[order(oligorankr), ]
            oligorankr <- sort(oligorankr)
            colnames(summaryr)[colnames(summaryr) == "Target"] <-
              sumrtarget
            for (i in (1:(length(allpammd) + length(allpammr)))) {
              if (oligorankr[i] > 5)
              {
                summaryr$Rank[i] <-
                  paste0(as.character(i), "*") # Oligos not sugeested for PCR targeting
              } else {
                summaryr$Rank[i] <- paste0(as.character(i))
              }
              
            }
            rv$data <- summaryr
            output$nextstep <-
              renderText({
                paste0(
                  '<br>',
                  '<br>',
                  "We suggest aligning the oligos to the target before ordering them.",
                  '<br>',
                  '<br>',
                  '<br>'
                )
              })
            showElement("nextstep")
            # Reverse oligos for the 17 nt search space for download
            csvoutputr <<-
              csvoutput [c(1:(length(allpammd) + length(allpammr))), ]
            oligorankr <-
              oligorank[((1:(
                length(allpammd) + length(allpammr)
              )))]
            csvoutputr <<-
              csvoutputr[order(sortedallpam$distance[1:(length(allpammd) + length(allpammr))]), ]
            oligorankr <-
              oligorankr[order(sortedallpam$distance[1:(length(allpammd) + length(allpammr))])]
            csvoutputr <<- csvoutputr[order(oligorankr), ]
            oligorankr <- sort(oligorankr)
            for (i in (1:(length(allpammd) + length(allpammr)))) {
              if (oligorankr[i] > 5)
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
                paste(nchar(
                  as.character(forwardoligo())
                ), "nt"),
                "Forward oligo",
                "",
                "",
                "",
                ""
              )
            confinedlist <- # For the .xlsx output
              list('17 nt search space' = csvoutputr,
                   'Forward oligo' = csvforward)
            revreport <<- csvoutputr
            row.names(revreport) <<- revreport[, 4]
            
            # PAM sites found in the extended search space ----------------------------
            
            if (length(allpammdl) + length(allpammrl) > 0) {
              summarye <-
                # Second part of the reverse oligo output table with PAM sites found in the 17 nt search space
                summary [c((length(allpammd) + length(allpammr) + 1):(
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                )), ]
              oligoranke <-
                oligorank[(length(allpammd) + length(allpammr) + 1):(
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                )]
              summarye <-
                summarye[order(sortedallpam$distance[(length(allpammd) + length(allpammr) + 1):(
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                )]), ]
              oligoranke <-
                oligoranke[order(sortedallpam$distance[(length(allpammd) + length(allpammr) + 1):(
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                )])]
              summarye <- summarye[order(oligoranke), ]
              oligoranke <- sort(oligoranke)
              for (i in (1:(nrow(summarye)))) {
                if (oligoranke[i] > 5)
                {
                  summarye$Rank[i] <-
                    paste0(as.character(i + (
                      length(allpammd) + length(allpammr)
                    )), "*")
                } else {
                  summarye$Rank[i] <-
                    paste0(as.character(i + (
                      length(allpammd) + length(allpammr)
                    )))
                }
              }
              colnames(summarye)[colnames(summarye) == "Target"] <-
                sumetarget
              rve$data <- summarye
              csvoutpute <-
                # Reverse oligos for the extended search space for download
                csvoutput [c(length(allpammd) + length(allpammr) + 1):(
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                ), ]
              oligoranke <-
                oligorank[(length(allpammd) + length(allpammr) + 1):(
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                )]
              csvoutpute <-
                csvoutpute[order(sortedallpam$distance[(length(allpammd) + length(allpammr) + 1):(
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                )]), ]
              oligoranke <-
                oligoranke[order(sortedallpam$distance[(length(allpammd) + length(allpammr) + 1):(
                  length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl)
                )])]
              csvoutpute <-  csvoutpute[order(oligoranke), ]
              oligoranke <- sort(oligoranke)
              for (i in (1:(nrow(csvoutpute)))) {
                if (oligoranke[i] > 5)
                {
                  csvoutpute$Rank[i] <-
                    paste0(as.character(i + (
                      length(allpammd) + length(allpammr)
                    )), "*")
                } else {
                  csvoutpute$Rank[i] <-
                    paste0(as.character(i + (
                      length(allpammd) + length(allpammr)
                    )))
                }
              }
              csvoutputec <- csvoutpute
              csvoutputec[nrow(csvoutputec) + 1,] <-
                c("", "", "", "", "", "", "", "")
              csvoutputec[nrow(csvoutputec) + 1,] <-
                c(
                  paste0("M1_", rvg$data),
                  as.character(forwardoligo()),
                  paste(nchar(
                    as.character(forwardoligo())
                  ), "nt"),
                  "Forward oligo",
                  "",
                  "",
                  "",
                  ""
                )
              extendedlist <- # for the .xlsx file
                list(
                  '17 nt search space' = csvoutputr,
                  'Extended search space' = csvoutpute,
                  'Forward oligo' = csvforward
                )
              revreport <<- rbind(csvoutputr, csvoutpute)
              row.names(revreport) <<- revreport[, 4]
            }
          }
          
          # PAM sites not found in the 17 nt search space but in the extended --------
          
          else if (length(allpammdl) + length(allpammrl) > 0) {
            csvoutpute <- NULL
            output$nextstep <- renderText({
              ""
            })
            output$mtwocomment <- renderText({
              mtwocomment
            })
            output$mtwoext <- renderText({
              mtwoext
            })
            showElement("mtwocomment")
            observe({
              if (input$extended == 1) {
                enable("downloadcsv")
                enable("downloadxlsx")
                enable("report")
              } else {
                disable("downloadcsv")
                disable("downloadxlsx")
                disable("report")
              }
            })
            summarye <- summary
            summarye <-
              summarye[order(sortedallpam$distance), ]
            oligoranke <-
              oligorank[order(sortedallpam$distance)]
            summarye <- summarye[order(oligoranke), ]
            oligoranke <- sort(oligoranke)
            for (i in (1:(nrow(summarye)))) {
              if (oligoranke[i] > 5)
              {
                summarye$Rank[i] <-
                  paste0(as.character(i + (
                    length(allpammd) + length(allpammr)
                  )), "*")
              } else {
                summarye$Rank[i] <-
                  paste0(as.character(i + (
                    length(allpammd) + length(allpammr)
                  )))
              }
            }
            colnames(summarye)[colnames(summarye) == "Target"] <-
              sumetarget
            rve$data <- summarye
            csvoutpute <- csvoutput
            csvoutpute <-
              csvoutpute[order(sortedallpam$distance), ]
            oligoranke <-
              oligorank[order(sortedallpam$distance)]
            csvoutpute <- csvoutpute[order(oligoranke), ]
            oligoranke <- sort(oligoranke)
            for (i in (1:(nrow(csvoutpute)))) {
              if (oligoranke[i] > 5)
              {
                csvoutpute$Rank[i] <-
                  paste0(as.character(i + (
                    length(allpammd) + length(allpammr)
                  )), "*")
              } else {
                csvoutpute$Rank[i] <-
                  paste0(as.character(i + (
                    length(allpammd) + length(allpammr)
                  )))
              }
              
            }
            csvoutputec <- csvoutpute
            csvoutputec[nrow(csvoutputec) + 1,] <-
              c("", "", "", "", "", "", "", "")
            csvoutputec[nrow(csvoutputec) + 1,] <-
              c(
                paste0("M1_", rvg$data),
                as.character(forwardoligo()),
                paste(nchar(
                  as.character(forwardoligo())
                ), "nt"),
                "Forward oligo",
                "",
                "",
                "",
                ""
              )
            confinedlist <- NULL
            csvoutputr <<- NULL
            csvoutputrc <- NULL
            extendedlist <-
              list('Extended search space' = csvoutpute,
                   'Forward oligo' = csvforward)
            revreport <<- csvoutpute
            row.names(revreport) <<- revreport[, 4]
            
            # No PAM sites were found even in the extended search space ---------------
            
          } else {
            output$monetitle <- renderText({
              monetitle
            })
            output$monedescr <- renderText({
              monedescr
            })
            output$mtwotitle <- renderText({
              mtwotitle
            })
            output$mtwodescr <- renderText({
              mtwodescr
            })
            output$mtwoext <- renderText({
              mtwoext
            })
            output$searchspacee <-
              renderUI({
                img(src = 'searchspace_e.png',
                    width = 800,
                    align = "right")
              })
            output$mtwocomment <- renderText({
              mtwocomment
            })
            showElement("mtwocomment")
            showElement("extended")
            showElement("inp_ext")
            enable("inp_ext")
            showElement("inp_apply")
            enableActionButton("inp_apply", session)
            output$cpfe <- renderText({
              ""
            })
            output$mtwocommente <-
              renderText({
                paste0(
                  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumVioletRed">',
                  "No PAM sites found within the ",
                  rvei$data,
                  " nt extended search space.",
                  '</span>'
                )
              })
            showElement("mtwocommente")
            output$nextstep <- renderText({
              nextstep
            })
            showElement("nextstep")
            hideElement("downloadcsv")
            hideElement("downloadxlsx")
            hideElement("report")
          }
          
          # Extended search space option is chosen ----------------------------------
          
          observe({
            if ((input$extended == 1) & (nchar(targetseq()) == 403)) {
              #if (input$extended == 1) {
              enable("inp_ext")
              enableActionButton("inp_apply", session)
              
              ntsstop <- aroundstop()(targetseq())
              isolate({
                output$esearchspacetext <- renderText({
                  paste0(
                    '<span style = "font-style: italic; color:MediumBlue">',
                    "The ",
                    rvei$data,
                    " nt extended search space for PAM sites is the following:",
                    '<br>',
                    '<br>',
                    '</span>'
                  )
                })
              })
              isolate({
                output$esearchspaceoligos <- renderText({
                  paste0(
                    '<br>',
                    '<br>',
                    '<span style = "font-style: italic; color:MediumBlue">',
                    "The oligos found in the ",
                    rvei$data,
                    " nt extended search space are the following:",
                    '<br>',
                    '<br>',
                    '</span>'
                  )
                })
              })
              isolate({
                output$nucleotidese <-
                  renderText({
                    # Extended search space visualisation
                    regionde <- paste(
                      as.character(ntsstop()[1:(17 + 10)]),
                      '<span style = "color:red">',
                      strong(substr(targetseq(), 201, 203)),
                      '</span>',
                      '<span style = "color:MediumVioletRed">',
                      substr(targetseq(), 204, 204 + rvei$data - 1),
                      '</span>',
                      substr(targetseq(), (204 + rvei$data), 305),
                      "&nbsp-&nbspdirect&nbspstrand",
                      sep = ""
                    )
                    
                  })
                if ((as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TAA") |
                    (as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TAG") |
                    (as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TGA")) {
                  output$stopwarninge <- renderText({
                    ""
                  })
                } else {
                  output$stopwarninge <-
                    renderText({
                      stopwarning
                    })
                }
              })
              isolate({
                if (rvei$data > 17) {
                  output$nucleotidesre <- renderText({
                    paste(
                      as.character(complement(ntsstop(
                        
                      )[1:10])),
                      as.character(complement(ntsstop(
                        
                      )[11:(17 + 10)])),
                      '<span style = "color:red">',
                      strong(as.character(
                        complement(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)])
                      )),
                      '</span>',
                      as.character(complement(ntsstop(
                        
                      )[(17 + 3 + 10 + 1):(17 + 3 + 10 + 17)])),
                      '<span style = "color:MediumVioletRed">',
                      as.character(complement(
                        DNAString(targetseq())[(203 + 17 + 1):(204 + rvei$data - 1)]
                      )),
                      '</span>',
                      as.character(complement(
                        DNAString(targetseq())[(204 + rvei$data):305]
                      )),
                      "&nbsp-&nbspreverse&nbspstrand",
                      sep = ""
                    )
                  })
                } else {
                  output$nucleotidesre <- renderText({
                    paste(
                      as.character(complement(ntsstop(
                        
                      )[1:10])),
                      as.character(complement(ntsstop(
                        
                      )[11:(17 + 10)])),
                      '<span style = "color:red">',
                      strong(as.character(
                        complement(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)])
                      )),
                      '</span>',
                      as.character(complement(
                        DNAString(targetseq())[204:305]
                      )),
                      " - reverse strand",
                      sep = ""
                    )
                    
                  })
                }
              })
              if (length(allpammdl) + length(allpammrl) > 0) {
                isolate({
                  output$cpfe <- renderTable({
                    rve$data
                  }, sanitize.text.function = function(x)
                    x)
                  rvo$data <- extendedlist
                  revreport <<- rbind(csvoutputr, csvoutpute)
                  row.names(revreport) <<- revreport[, 4]
                  csvoutputec <- csvoutpute
                  csvoutputec[nrow(csvoutputec) + 1,] <-
                    c("", "", "", "", "", "", "", "")
                  csvoutputec[nrow(csvoutputec) + 1,] <-
                    c(
                      paste0("M1_", rvg$data),
                      as.character(forwardoligo()),
                      paste(nchar(
                        as.character(forwardoligo())
                      ), "nt"),
                      "Forward oligo",
                      "",
                      "",
                      "",
                      ""
                    )
                  rvoc$data <-
                    rbind(
                      '17 nt search space' = csvoutputr,
                      'Extended search space' = csvoutputec
                    )
                  output$nextstep <-
                    renderText({
                      paste0(
                        '<br>',
                        '<br>',
                        "We suggest aligning the oligos to the target before ordering them.",
                        '<br>',
                        '<br>',
                        '<br>'
                      )
                    })
                  showElement("nextstep")
                  hideElement("mtwocomment")
                  output$mtwocommente <-
                    renderText({
                      mtwocommenterank
                    })
                  showElement("mtwocommente")
                })
              } else {
                showElement("extended")
                showElement("inp_ext")
                enable("inp_ext")
                showElement("inp_apply")
                enableActionButton("inp_apply", session)
                isolate({
                  output$cpfe <- renderText({
                    ""
                  })
                  output$mtwocommente <-
                    renderText({
                      paste0(
                        '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumVioletRed">',
                        "No PAM sites found within the ",
                        rvei$data,
                        " nt extended search space.",
                        '</span>'
                      )
                    })
                  showElement("mtwocommente")
                  output$nextstep <- renderText({
                    nextstep
                  })
                  showElement("nextstep")
                })
                if (length(allpammd) + length(allpammr) + length(allpammdl) + length(allpammrl) > 0) {
                  showElement("downloadcsv")
                  showElement("downloadxlsx")
                  showElement("report")
                  confinedlist <-
                    list('17 nt search space' = csvoutputr,
                         'Forward oligo' = csvforward)
                  revreport <<- csvoutputr
                  row.names(revreport) <<- revreport[, 4]
                  isolate({
                    rvo$data <- confinedlist
                    rvoc$data <- csvoutputrc
                    output$nextstep <-
                      renderText({
                        paste0(
                          '<br>',
                          '<br>',
                          "We suggest aligning the oligos to the target before ordering them.",
                          '<br>',
                          '<br>',
                          '<br>'
                        )
                      })
                    showElement("nesxtstep")
                    output$mtwoext <- renderText({
                      mtwoextb
                    })
                  })
                } else {
                  hideElement("downloadcsv")
                  hideElement("downloadxlsx")
                  hideElement("report")
                }
              }
              
              # Extended search space option is not chosen ------------------------------
              
            } else {
              isolate({
                output$nucleotidese <- renderText({
                  ""
                })
                output$nucleotidesre <- renderText({
                  ""
                })
                output$cpfe <- renderText({
                  ""
                })
                output$esearchspacetext <- renderText({
                  ""
                })
                disable("inp_ext")
                disableActionButton("inp_apply", session)
                output$mtwocommente <- renderText({
                  ""
                })
                showElement("mtwocommente")
              })
              if (length(allpammd) + length(allpammr) > 0) {
                isolate({
                  output$mtwocomment <- renderText({
                    mtwocommenterank
                  })
                  showElement("mtwocomment")
                  confinedlist <-
                    list('17 nt search space' = csvoutputr,
                         'Forward oligo' = csvforward)
                  revreport <<- csvoutputr
                  row.names(revreport) <<- revreport[, 4]
                  rvo$data <- confinedlist
                  rvoc$data <- csvoutputrc
                  output$nextstep <-
                    renderText({
                      paste0(
                        '<br>',
                        '<br>',
                        "We suggest aligning the oligos to the target before ordering them.",
                        '<br>',
                        '<br>',
                        '<br>'
                      )
                    })
                  showElement("nextstep")
                })
              } else {
                isolate({
                  output$nextstep <- renderText({
                    ""
                  })
                })
              }
            }
          })
        } else {
          isolate({
            output$monetitle <- renderText({
              monetitle
            })
            output$monedescr <- renderText({
              monedescr
            })
            output$mtwotitle <- renderText({
              mtwotitle
            })
            output$mtwodescr <- renderText({
              mtwodescr
            })
            output$mtwoext <- renderText({
              mtwoext
            })
            output$searchspacee <-
              renderUI({
                img(src = 'searchspace_e.png',
                    width = 800,
                    align = "right")
              })
            output$mtwocomment <- renderText({
              mtwocomment
            })
            showElement("mtwocomment")
            showElement("extended")
            showElement("inp_ext")
            enable("inp_ext")
            showElement("inp_apply")
            enableActionButton("inp_apply", session)
            output$cpfe <- renderText({
              ""
            })
            output$mtwocommente <-
              renderText({
                paste0(
                  '<span style = "font-style:italic; font-family: Arial; font-size: 18px; color:MediumVioletRed">',
                  "No PAM sites found within the ",
                  rvei$data,
                  " nt extended search space.",
                  '</span>'
                )
              })
            showElement("mtwocommente")
            output$nextstep <- renderText({
              nextstep
            })
            showElement("nextstep")
          })
          hideElement("downloadcsv")
          hideElement("downloadxlsx")
          hideElement("report")
          if ((input$extended == 1) &
              (nchar(targetseq()) == 403)) {
            ntsstop <- aroundstop()(targetseq())
            isolate({
              output$esearchspacetext <- renderText({
                paste0(
                  '<span style = "font-style: italic; color:MediumBlue">',
                  "The ",
                  rvei$data,
                  " nt extended search space for PAM sites is the following:",
                  '<br>',
                  '<br>',
                  '</span>'
                )
              })
            })
            isolate({
              output$esearchspaceoligos <- renderText({
                paste0(
                  '<br>',
                  '<br>',
                  '<span style = "font-style: italic; color:MediumBlue">',
                  "The oligos found in the ",
                  rvei$data,
                  " nt extended search space are the following:",
                  '<br>',
                  '<br>',
                  '</span>'
                )
              })
            })
            isolate({
              output$nucleotidese <- renderText({
                regionde <- paste(
                  as.character(ntsstop()[1:(17 + 10)]),
                  '<span style = "color:red">',
                  strong(substr(targetseq(), 201, 203)),
                  '</span>',
                  '<span style = "color:MediumVioletRed">',
                  substr(targetseq(), 204, 204 + rvei$data - 1),
                  '</span>',
                  substr(targetseq(), (204 + rvei$data), 305),
                  "&nbsp-&nbspdirect&nbspstrand",
                  sep = ""
                )
              })
              if ((as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TAA") |
                  (as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TAG") |
                  (as.character(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)]) == "TGA")) {
                output$stopwarninge <- renderText({
                  ""
                })
              } else {
                output$stopwarninge <-
                  renderText({
                    stopwarning
                  })
              }
            })
            isolate({
              if (rvei$data > 17) {
                output$nucleotidesre <- renderText({
                  paste(
                    as.character(complement(ntsstop()[1:10])),
                    as.character(complement(ntsstop()[11:(17 + 10)])),
                    '<span style = "color:red">',
                    strong(as.character(
                      complement(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)])
                    )),
                    '</span>',
                    as.character(complement(ntsstop()[(17 + 3 + 10 + 1):(17 + 3 + 10 + 17)])),
                    '<span style = "color:MediumVioletRed">',
                    as.character(complement(
                      DNAString(targetseq())[(203 + 17 + 1):(204 + rvei$data - 1)]
                    )),
                    '</span>',
                    as.character(complement(
                      DNAString(targetseq())[(204 + rvei$data):305]
                    )),
                    "&nbsp-&nbspreverse&nbspstrand",
                    sep = ""
                  )
                })
              } else {
                output$nucleotidesre <- renderText({
                  paste(
                    as.character(complement(ntsstop()[1:10])),
                    as.character(complement(ntsstop()[11:(17 + 10)])),
                    '<span style = "color:red">',
                    strong(as.character(
                      complement(ntsstop()[(17 + 10 + 1):(17 + 10 + 3)])
                    )),
                    '</span>',
                    as.character(complement(
                      DNAString(targetseq())[204:305]
                    )),
                    " - reverse strand",
                    sep = ""
                  )
                })
              }
            })
          }
        }
        output$downloadcsv <- downloadHandler(
          filename = paste0("M_", rvg$data, ".csv"),
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
      }, ignoreInit = TRUE)
      
    }
    #wraptarget <-
    #wrapfw <- str_break(forwardoligo())
    #wraptarget<- cat(str_break(targetseq()), sep = '\n')
    # output$report <- downloadHandler(
    #   filename = function() {
    #     'Mammalian_PCR_tagging_report.pdf'
    #   },
    #   content = function(file) {
    #     src <- normalizePath('report.Rmd')
    #
    #     # temporarily switch to the temp dir, in case you do not have write
    #     # permission to the current working directory
    #     owd <- setwd(tempdir())
    #     on.exit(setwd(owd))
    #     file.copy(src, 'report.Rmd', overwrite = TRUE)
    #     library(rmarkdown)
    #     params <- list(
    #       a = input$inputmethod,
    #       b = str_break(targetseq()),
    #       c = input$genename,
    #       d = input$inp_cpf,
    #       e = input$threeha,
    #       f = input$fiveha,
    #       g = input$phospho,
    #       h = str_break(forwardoligo()),
    #       i = nchar(forwardoligo()),
    #       j = revreport[, c(1,2,3)],
    #       k = input$extended,
    #       l = rvei$data,
    #       m = nrow(csvoutputr)
    #       )
    #     out <- rmarkdown::render('report.Rmd',
    #                              params = params,
    #                              envir = new.env(parent = globalenv()))
    #     file.rename(out, file)
    #   }
    # )
    observe({
      if (input$compute == 0) {
        isolate({
          output$nucleotidese <- renderText({
            ""
          })
          output$nucleotidesre <- renderText({
            ""
          })
          output$cpfe <- renderText({
            ""
          })
        })
      }
    })
  }, ignoreInit = TRUE)
}
# Run the app ----
shinyApp(ui = ui,
         server = server,
         options = list(height = 1080))