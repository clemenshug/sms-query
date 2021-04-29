all_similarities_table <- function(
  query_ids, target_ids = NULL,
  tas_n_common = 4L,
  pfp_n_common = 4L
) {
  similarity_cols <- c(
    "tas_similarity", "structural_similarity", "phenotypic_correlation"
  )
  similarities <- merge(
    tas_weighted_jaccard(query_ids, target_ids, min_n = tas_n_common),
    chemical_similarity(query_ids, target_ids),
    by = c("target_lspci_id", "query_lspci_id"),
    all = TRUE
  ) %>%
    merge(
      phenotypic_similarity(query_ids, target_ids, min_n = pfp_n_common),
      all = TRUE,
      by = c("target_lspci_id", "query_lspci_id")
    ) %>% {
      .[
        ,
        (similarity_cols) := lapply(.SD, signif, digits = 3), .SDcols = similarity_cols
      ]
    } %>%
    merge_compound_names() %>%
    relocate(
      query_compound, target_compound, all_of(similarity_cols)
    )
  # for (j in similarity_cols)
  #   set(similarities, j = j, value = signif(similarities[[j]], digits = 2))
  similarities
}

split_compounds <- function(x) {
  x %>%
    str_trim() %>%
    str_split("[\\n;]+") %>%
    chuck(1L) %>%
    magrittr::extract(. != "")
}

similarityQueryUI <- function(id) {
  ns <- NS(id)
  tagList(
    columns(
      column(
        deck(
          card(
            tags$h5(
              class = "card-title",
              "Query compounds"
            ),
            p("Enter compound names to query for. By default, similarities between",
              "all query compounds are computed. To select specific target compounds",
              "use the input on the right."),
            textAreaInput(
              inputId = ns("query_compounds"),
              label = NULL,
              width = "100%",
              rows = 10,
              placeholder = "tofacitinib;ruxolitinib;..."
            ),
            p("Enter one compound per line or separate them using semicolons.",
              class = "text-muted"),
            uiOutput(
              outputId = ns("feedback_query_compounds")
            )
          ),
          card(
            tags$h5(
              class = "card-title",
              "Target compounds"
            ),
            radiobarInput(
              id = ns("target_options"),
              class = "btn-group-primary",
              choices = c("Same as query", "Specific targets", "All targets"),
              values = c("same", "separate", "all"),
              selected = "same"
            ),
            textAreaInput(
              inputId = ns("target_compounds"),
              label = NULL,
              width = "100%",
              rows = 10,
              placeholder = "tofacitinib;ruxolitinib;..."
            ),
            uiOutput(
              outputId = ns("feedback_target_compounds")
            )
          )
        )
      )
    ) %>%
      margin(b = 3),
    columns(
      column(
        card(
          actionButton(
            inputId = ns("submit"), label = "Submit", icon = icon("bolt"),
            class = "btn-primary"
          )
        )
      )
    ) %>%
      margin(b = 3),
    columns(
      column(
        card(
          DT::DTOutput(outputId = ns("table")),
          mod_ui_download_button(ns("table_csv_dl"), "Download CSV"),
          mod_ui_download_button(ns("table_xlsx_dl"), "Download Excel")
        )
      )
    )
  ) %>%
    container(centered = TRUE)
}

similarityQueryServer <- function(input, output, session) {
  ns <- session$ns

  observeEvent(input$target_options, {
    switch(
      input$target_options,
      same = disable,
      separate = enable,
      all = disable
    )(id = "target_compounds", selector = "textarea")

  })

  ## Query compounds

  r_query_compounds_raw <- reactive({
    input$query_compounds
  }) %>%
    debounce(500)

  r_query_compounds_original <- reactive({
    req(r_query_compounds_raw())
    split_compounds(r_query_compounds_raw())
  })

  r_query_compounds <- reactive({
    find_compound_ids(r_query_compounds_original())
  })

  ## Target compounds

  r_target_compounds_raw <- reactive({
    input$target_compounds
  }) %>%
    debounce(500)

  r_target_compounds_original <- reactive({
    req(r_target_compounds_raw())
    split_compounds(r_target_compounds_raw())
  })

  r_target_compounds <- reactive({
    if (input$target_options == "all")
      NULL
    else if (input$target_options == "same")
      r_query_compounds()
    else
      find_compound_ids(r_target_compounds_original())
  })

  ## Feedback

  output$feedback_query_compounds <- renderUI({
    r_query_compounds()
    p(
      isolate(length(r_query_compounds_original())), "queries match",
      nrow(r_query_compounds()), "compounds."
    )
  })

  output$feedback_target_compounds <- renderUI({
    if (!input$target_options == "separate")
      NULL
    else {
      r_target_compounds()
      p(
        isolate(length(r_target_compounds_original())), "queries match",
        nrow(r_target_compounds()), "compounds."
      )
    }
  })

  ## Outputs

  r_results <- eventReactive(input$submit, {
    if (!isTruthy(r_query_compounds()))
      data.table()
    else
      all_similarities_table(
        query_ids = r_query_compounds()[["lspci_id"]] %>%
          unique(),
        target_ids = if (is.null(r_target_compounds())) NULL
         else r_target_compounds()[["lspci_id"]] %>% unique()
      )
  })

  r_result_dt <- reactive({
    DT::datatable(
      r_results(),
      style = "bootstrap4",
      selection = "none",
      options = list(
        scrollX = TRUE
      )
    )
  })

  output$table <- DT::renderDataTable({
    r_result_dt()
  })

  # setBookmarkExclude(
  #   table_inputs("table_sim_compound")
  # )

  callModule(mod_server_download_button, "table_xlsx_dl", r_results, "excel")
  callModule(mod_server_download_button, "table_csv_dl", r_results, "csv")

}
