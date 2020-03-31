bar_chart_gadget <- function(data) {

  data_name <- rlang::quo_name(rlang::enquo(data))
  assign(data_name, data)
  vars <- colnames(data)
  numeric_vars <- vars[sapply(data, is.numeric)]
  char_vars <- setdiff(vars, numeric_vars)

  pickerInput <- function(...) {
    opts <- shinyWidgets::pickerOptions(liveSearch = TRUE)
    shinyWidgets::pickerInput(..., options = opts)
  }


  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Bar Chart Creator"),

    miniUI::miniTabstripPanel(

      miniUI::miniTabPanel(
        "Parameters",
        icon = shiny::icon("sliders"),
        miniUI::miniContentPanel(
          pickerInput("x", "X Variable", choices = char_vars),
          pickerInput("y", "Y Variable", choices = numeric_vars),
          pickerInput("color", "Bar Color", choices = grDevices::colors(), selected = "steelblue"),
          shiny::checkboxInput("sort", "Sort", value = TRUE),
          # shinyWidgets::pickerInput("facet", "Facet Variable", choices = vars, selected = NULL),
          shiny::conditionalPanel(
            "input.sort",
            shiny::numericInput("limit", "Limit", min = 1, value = 10)
          )
        )
      ),

      miniUI::miniTabPanel(
        "Visualization",
        icon = shiny::icon("area-chart"),
        miniUI::miniContentPanel(
          shiny::plotOutput("plot", height = "100%")
        )
      ),

      miniUI::miniTabPanel(
        "Code",
        icon = shiny::icon("code"),
        miniUI::miniContentPanel(
          shiny::verbatimTextOutput("code")
        )
      )

    )
  )

  server <- function(input, output, session) {

    call <- shiny::reactive({
      rlang::expr(
        ggcharts::bar_chart(
          data = !!rlang::sym(data_name),
          x = !!rlang::sym(input$x),
          y = !!rlang::sym(input$y),
          bar_color = !!input$color,
          sort = !!input$sort,
          limit = !!as.numeric(input$limit)
        )
      )
    })

    output$plot <- shiny::renderPlot({
      rlang::eval_tidy(call())
    })

    output$code <- shiny::renderText({
      n <- length(call())
      fun_name <- deparse(call()[[1]])
      args <- sapply(call(), deparse)[2:n]
      call_str <- paste0(
        fun_name, "(\n  ",
        paste(names(args), args, sep = " = ", collapse = ",\n  "),
        "\n)"
      )
      call_str
    })

    shiny::observeEvent(input$done, {
      plot <- rlang::eval_tidy(call())
      shiny::stopApp(plot)
    })
  }

  shiny::runGadget(ui, server)
}
