source("global.R", local = TRUE)

server <- function(input, output, session) {

  observe({
    showNavPane(input$tab)
  })

  .modal_about <- modal(
    id = NULL,
    size = "lg",
    header = h5("About"),
    HTML(htmltools::includeMarkdown("inst/about.md"))
  )
  observeEvent(input$about, {
    showModal(.modal_about)
  })

  .modal_funding <- modal(
    id = NULL,
    size = "md",
    header = h5("Funding"),
    p("This open-access webtool is funded by NIH grants U54-HL127365, U24-DK116204 and U54-HL127624.")
  )
  observeEvent(c(input$funding, input$funding2),
    ignoreInit = TRUE, {
    showModal(.modal_funding)
  })

  callModule(
    module = similarityQueryServer,
    id = "similarity"
  )
}

ui <- function(req) {
  tagList(
    page_headers(),
    webpage(
      nav = navbar_ui(),
      nav_content_ui()
    )
  )
}

app <- shinyApp(ui, server, enableBookmarking = "url")
