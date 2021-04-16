NAV_ITEMS <- list(
  similarity = list(icon("circle", class = "similarity--green"), "Similarity")
)

navbar_ui <- function() {
  navbar(
    brand = tags$a(
      href = "http://sorger.med.harvard.edu/",
      tags$img(
        src = "sms/assets/img/logo.png",
        style = "height: 2rem;"
      )
    ),
    class = "navbar-dark bg-dark",
    navInput(
      appearance = "pills",
      id = "tab",
      choices = unname(NAV_ITEMS),
      values = names(NAV_ITEMS)
    ) %>%
      margin(left = "auto"),
    tags$ul(
      class = "yonder-nav nav nav-pills",
      tagList(
        buttonInput(
          id = "about",
          label = "About",
          class = "nav-link btn-link"
        ),
        buttonInput(
          id = "funding",
          label = "Funding",
          class = "nav-link btn-link"
        ),
        tags$a(
          href = "https://forms.gle/dSpCJSsbaavTbCkP6",
          target = "_blank",
          icon("comments", class = "fa-lg"),
          "Feedback",
          class = "nav-link btn btn-link"
        ),
        bookmarkButton(
          label = "Share current view",
          icon = icon("share-square"),
          class = "btn-link nav-link"
        ),
        tags$a(
          href = "https://github.com/labsyspharm/sms-query",
          target = "_blank",
          icon("github", class = "fa-lg"),
          class = "nav-link btn btn-link"
        )
      ) %>%
        map(tags$li, class = "nav-item")
    ) %>%
      margin(left = 0)
  ) %>%
    padding(0, r = 3, l = 3) %>%
    margin(b = 4) %>%
    shadow()
}

similarity_page <- function() {
  container(
    centered = TRUE,
    columns(
      column(
        h2("The Small Molecule Suite Query Server") %>%
          font(align = "center")
      )
    ),
    similarityQueryUI(
      id = "similarity"
    )
  )
}

page_headers <- function() {
  tagList(
    useShinyjs(),
    htmltools::htmlDependency(
      "font-awesome",
      "5.3.1", "www/shared/fontawesome",
      package = "shiny",
      stylesheet = c("css/all.min.css", "css/v4-shims.min.css")
    ),
    tags$head(
      tags$title("Query SMS"),
      tags$link(href = "https://fonts.googleapis.com/css?family=Lato:400,700&display=swap", rel = "stylesheet"),
      tags$link(rel = "stylesheet", type = "text/css", href = "sms/css/slider.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "sms/css/main.css"),
      tags$script(src = "sms/js/main.js"),
      tags$link(rel = "icon", type = "image/png", href = "sms/assets/img/favicon.png")
    )
  )
}

nav_content_ui <- function() {
  navContent(
    navPane(
      id = "similarity",
      similarity_page()
    )
  )
}
