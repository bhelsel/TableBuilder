# R/datasets.R ---------------------------------------------------------------
.builtin_datasets <- function() {
  penguins <- if ("penguins" %in% getNamespaceExports("datasets")) {
    datasets::penguins # R >= 4.5
  } else {
    palmerpenguins::penguins # add palmerpenguins to Imports/Suggests
  }

  list(
    mtcars = mtcars,
    iris = iris,
    penguins = as.data.frame(penguins),
    airquality = airquality,
    ToothGrowth = ToothGrowth,
    mpg = as.data.frame(ggplot2::mpg)
  )
}

# UI -------------------------------------------------------------------------
reportUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "Report Builder",
        width = 3,
        status = "primary",
        solidHeader = TRUE,

        shiny::tags$div(
          class = "sidebar-section",
          shiny::h5(
            class = "section-header",
            shiny::icon("table"),
            " Table Selection"
          ),
          shiny::selectInput(
            ns("redcap_tables"),
            "Dataset:",
            choices = names(.builtin_datasets()),
            selected = "mtcars"
          ),
          shiny::uiOutput(ns("variable_selector")),
          shiny::uiOutput(ns("stratify_selector"))
        ),

        shiny::tags$div(
          class = "sidebar-section",
          shiny::h5(
            class = "section-header",
            shiny::icon("edit"),
            " Customize"
          ),
          shiny::textInput(
            ns("table_title"),
            "Table Title:",
            value = "Build Your Own Custom Table",
            placeholder = "Enter table title"
          ),
          shiny::textInput(
            ns("edit_labels"),
            "Edit Variable Labels",
            placeholder = "e.g., mpg = Miles per gallon, hp = Horsepower"
          ),
          shiny::textInput(
            ns("filter_table"),
            "Filter Table:",
            placeholder = "e.g., cyl == 4"
          ),
          shiny::uiOutput(ns("filter_validation"))
        ),

        shiny::tags$div(
          class = "sidebar-section",
          shiny::h5(
            class = "section-header",
            shiny::icon("clipboard-check"),
            " Actions"
          ),
          shiny::actionButton(
            ns("capture_table"),
            "Add Table to Report",
            icon = shiny::icon("plus"),
            class = "btn-success btn-block capture-btn"
          ),
          shiny::tags$hr(class = "action-divider"),
          shiny::downloadButton(
            ns("download_report"),
            "Download PDF Report",
            class = "btn-primary btn-block download-btn"
          ),
          shiny::tags$small(
            class = "helper-text",
            "Add tables above, then download"
          )
        )
      ),
      shinydashboard::box(
        title = "Program Summary Statistics",
        status = "primary",
        solidHeader = TRUE,
        width = 9,
        shiny::htmlOutput(ns("program_summary_table")), # render_gt pairs with gt_output
        shiny::uiOutput(ns("titles"))
      )
    )
  )
}

# Server ---------------------------------------------------------------------
reportServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    datasets <- .builtin_datasets()

    participant_data <- shiny::reactive({
      shiny::req(input$redcap_tables %in% names(datasets))
      datasets[[input$redcap_tables]]
    })

    # Reset dependent inputs when the dataset changes
    shiny::observeEvent(input$redcap_tables, {
      shiny::updateTextInput(session, "filter_table", value = "")
      shiny::updateTextInput(session, "edit_labels", value = "")
    })

    output$variable_selector <- shiny::renderUI({
      shiny::selectizeInput(
        session$ns("selected_vars"),
        "Choose Variables (blank = all)",
        choices = colnames(participant_data()),
        multiple = TRUE
      )
    })

    output$stratify_selector <- shiny::renderUI({
      d <- participant_data()
      ok <- names(d)[vapply(
        d,
        function(x) {
          n <- dplyr::n_distinct(x, na.rm = TRUE)
          n >= 2 && n <= 6
        },
        logical(1)
      )]
      shiny::selectInput(
        session$ns("stratify_by"),
        "Stratify by:",
        choices = c("None", ok),
        selected = "None"
      )
    })

    # ---- Labels ------------------------------------------------------------
    parse_labels <- function(label_string) {
      if (is.null(label_string) || trimws(label_string) == "") {
        return(NULL)
      }
      pairs <- strsplit(label_string, ",")[[1]]
      parts <- lapply(pairs, function(p) trimws(strsplit(p, "=")[[1]]))
      parts <- Filter(function(p) length(p) == 2 && all(nzchar(p)), parts)
      if (length(parts) == 0) {
        return(NULL)
      }
      stats::setNames(lapply(parts, `[[`, 2), vapply(parts, `[[`, "", 1))
    }

    # ---- Filter ------------------------------------------------------------
    filter_debounced <- shiny::debounce(
      shiny::reactive(input$filter_table),
      millis = 1000
    )

    filter_valid <- shiny::reactive({
      txt <- trimws(filter_debounced() %||% "")
      d <- participant_data()

      if (txt == "") {
        list(valid = TRUE, msg = "", ready = FALSE)
      } else if (!grepl("==|!=|>|<|%in%|&|\\|", txt)) {
        list(
          valid = FALSE,
          msg = "⚠ Incomplete expression (need ==, >, <, etc.)",
          ready = FALSE
        )
      } else {
        tryCatch(
          {
            expr <- rlang::parse_expr(txt)
            missing_vars <- setdiff(all.vars(expr), colnames(d))
            if (length(missing_vars) > 0) {
              list(
                valid = FALSE,
                msg = paste("✗ Unknown:", paste(missing_vars, collapse = ", ")),
                ready = FALSE
              )
            } else {
              n <- nrow(dplyr::filter(d, !!expr))
              list(
                valid = TRUE,
                msg = paste0("✓ Ready (", n, " rows)"),
                ready = TRUE
              )
            }
          },
          error = function(e) {
            list(
              valid = FALSE,
              msg = paste("✗", conditionMessage(e)),
              ready = FALSE
            )
          }
        )
      }
    })

    output$filter_validation <- shiny::renderUI({
      v <- filter_valid()
      if (v$msg == "") {
        return(NULL)
      }
      color <- if (v$valid) {
        "green"
      } else if (startsWith(v$msg, "⚠")) {
        "orange"
      } else {
        "red"
      }
      shiny::tags$small(
        style = paste0(
          "color:",
          color,
          "; text-align:right; display:block; margin:5px 0;"
        ),
        v$msg
      )
    })

    filtered_data <- shiny::reactive({
      d <- participant_data()
      v <- filter_valid()
      if (v$ready && v$valid) {
        d <- dplyr::filter(d, !!rlang::parse_expr(trimws(filter_debounced())))
      }
      d
    })

    # ---- Variable selection ------------------------------------------------
    stratify_var <- shiny::reactive({
      s <- input$stratify_by
      if (is.null(s) || s == "None" || !s %in% colnames(participant_data())) {
        NULL
      } else {
        s
      }
    })

    selected_data <- shiny::reactive({
      d <- filtered_data()
      vars <- if (length(input$selected_vars) > 0) {
        input$selected_vars
      } else {
        colnames(d)
      }
      vars <- union(vars, stratify_var())
      dplyr::select(d, dplyr::all_of(vars))
    })

    # ---- Summary table -----------------------------------------------------
    summary_table <- shiny::reactive({
      d <- selected_data()
      shiny::req(nrow(d) >= 1)
      by <- stratify_var()

      # Columns coded 0/1 (or 1/NA) -> show only the "1" row
      dichotomous <- names(d)[vapply(
        d,
        function(x) {
          is.numeric(x) && any(!is.na(x)) && all(x %in% c(0, 1, NA))
        },
        logical(1)
      )]
      dichotomous <- setdiff(dichotomous, by)

      type_list <- if (length(dichotomous) > 0) {
        list(dplyr::all_of(dichotomous) ~ "dichotomous")
      }
      value_list <- if (length(dichotomous) > 0) {
        list(dplyr::all_of(dichotomous) ~ 1)
      }

      d |>
        dplyr::mutate(dplyr::across(
          dplyr::where(is.factor),
          forcats::fct_drop
        )) |>
        gtsummary::tbl_summary(
          by = dplyr::all_of(by), # all_of(NULL) is fine -> no stratification
          missing = "no",
          label = parse_labels(input$edit_labels),
          statistic = list(
            gtsummary::all_continuous() ~ "{mean} ± {sd}",
            gtsummary::all_categorical() ~ "{n} ({p}%)"
          ),
          digits = gtsummary::all_continuous() ~ 1,
          type = type_list,
          value = value_list
        )
    })

    output$program_summary_table <- gt::render_gt({
      shiny::req(summary_table(), input$table_title)
      summary_table() |>
        gtsummary::as_gt() |>
        gt::tab_header(title = gt::html(input$table_title)) |>
        gt::opt_interactive(
          use_search = TRUE,
          use_filters = TRUE,
          page_size_default = 10
        )
    })

    # ---- Capture + download ------------------------------------------------
    captured <- shiny::reactiveValues(tables = list(), titles = list())

    output$titles <- shiny::renderUI({
      if (length(captured$titles) == 0) {
        return(shiny::tags$p("No tables captured yet"))
      }
      shiny::tags$ul(lapply(captured$titles, shiny::tags$li))
    })

    shiny::observeEvent(input$capture_table, {
      tbl <- gtsummary::as_tibble(summary_table())
      colnames(tbl) <- gsub("\\*", "", colnames(tbl))
      captured$tables <- c(
        captured$tables,
        stats::setNames(list(tbl), input$table_title)
      )
      captured$titles <- c(captured$titles, input$table_title)
    })

    output$download_report <- shiny::downloadHandler(
      filename = function() paste0("test_report_", Sys.Date(), ".pdf"),
      content = function(file) {
        temp_qmd <- file.path(tempdir(), "report_template.qmd")
        file.copy("qmd/temp.qmd", temp_qmd, overwrite = TRUE)

        temp_functions <- file.path(tempdir(), "functions.R")
        file.copy("qmd/functions.R", temp_functions, overwrite = TRUE)

        temp_tables <- file.path(tempdir(), "tables.RData")
        tables <- captured$tables
        save(tables, file = temp_tables)

        withr::with_dir(tempdir(), {
          quarto::quarto_render(
            input = temp_qmd,
            output_format = "pdf",
            output_file = "report_template.pdf",
            execute_params = list(
              tables_path = temp_tables,
              titles = captured$titles
            )
          )
        })
        file.rename(file.path(tempdir(), "report_template.pdf"), file)
      }
    )
  })
}
