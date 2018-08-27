# action to take when submit button is pressed
panel.comment <- fluidPage(
  h3(strong("Add a comment"),
     br(),
     br()
  ),
  splitLayout(cellWidths = c(100, 200),
              h4("Your name:"),
              textInput(
                "commentname",
                label = NULL,
                width = 200
              )
  ),
  h4("Your comment:"),
  textAreaInput(
    "commenttext",
    label = NULL,
    width = 800
  ),
  actionButton("submit", label = "Submit"),
  br(),
  br(),
  br(),
  #tags$style(HTML('#comments td {border-top: 0}')),
  tags$style(HTML('#comments td {vertical-align: middle;}')),
  tags$style(HTML('#comments td {font-size: 30px}')),
  tags$style(HTML('#comments td {height: 100px}')),
  #tags$style(HTML('#comments td {font-family: "Roboto Mono"}')),
  
  htmlOutput("comments")
)