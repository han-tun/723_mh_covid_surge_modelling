#' Setup Module
#'
#' A shiny module that renders all of the content for the setup page.
#'
#' @name setup_module
#'
#' @param id An ID string that uniquely identifies an instance of this module
#' @param params reactive object passed in from the main server

#' @rdname setup_module
setup_ui <- function(id) {
  alter_names <- primary_box(
    title = "Alter Names",
    "Alter the names of the population groups and treatments. Each option should appear on a new line",
    textAreaInput(
      NS(id, "population_groups"),
      "Population Groups"
    ),
    textAreaInput(
      NS(id, "treatments"),
      "Treatments"
    )
  )

  configure_g2c <- primary_box(
    title = "Configure Conditions",
    selectInput(
      NS(id, "population_groups_g2c"),
      "Population Group",
      NULL
    ),
    div(id = "conditions_list")
  )

  configure_c2t <- primary_box(
    title = "Configure Treatments",
    selectInput(
      NS(id, "population_groups_c2t"),
      "Population Group",
      NULL
    ),
    selectInput(
      NS(id, "conditions_c2t"),
      "Condition",
      NULL
    ),
    div(id = "treatments_list")
  )

  fluidRow(
    alter_names,
    configure_g2c,
    configure_c2t
  )
}

#' @rdname setup_module
setup_server <- function(id, params) {
  moduleServer(id, function(input, output, session) {
    conditions <- character(0)
    new_params <- reactiveValues(g2c = list(), c2t = list())

    # initialise ====

    init <- observe({
      conditions <<- params %>%
        get_all_conditions()

      input_names <- paste0("condition_",  gsub(" ", "_", conditions))

      conditions_ui <- map2(input_names, conditions, function(.x, .y) {
        i <- checkboxInput(NS(id, .x), .y)

        observeEvent(input[[.x]], {
          g <- req(input$population_groups_g2c)
          v <- input[[.x]]

          if (v) {
            new_params$c2t[[g]][[.y]] <- new_treatments() %>%
              set_names() %>%
              map(~FALSE)
          } else {
            new_params$c2t[[g]][[.y]] <- NULL
          }

          new_params$g2c[[g]][[.y]] <- v
        })

        i
      })

      insertUI("#conditions_list", "beforeEnd", conditions_ui)

      init$destroy()
    })

    # alter names ====

    process_text_area <- function(v) {
      v %>%
        # split by new lines
        strsplit("\n") %>%
        .[[1]] %>%
        # remove any white space at start/end of lines
        gsub("^\\s+|\\s+$", "", .) %>%
        # remove any empty lines
        .[. != ""] %>%
        sort()
    }

    population_groups <- reactive_changes(names(params$groups))
    observeEvent(population_groups(), {
      v <- paste(population_groups(), collapse = "\n")
      updateTextAreaInput(session, "population_groups", value = v)
    })
    new_population_groups <- reactive({
      process_text_area(input$population_groups)
    })

    treatments <- reactive_changes(names(params$treatments))
    observeEvent(treatments(), {
      v <- paste(treatments(), collapse = "\n")
      updateTextAreaInput(session, "treatments", value = v)
    })
    new_treatments <- reactive({
      process_text_area(input$treatments)
    })

    observeEvent(new_population_groups(), {
      new_params$g2c <- req(new_population_groups()) %>%
        set_names() %>%
        map(~set_names(logical(length(conditions)), conditions))

      new_params$c2t <- new_population_groups() %>%
        set_names() %>%
        map(~list())

      updateSelectInput(session, "population_groups_g2c", choices = new_population_groups())
    })

    # g2c ====

    observeEvent(input$population_groups_g2c, {
      req(input$population_groups_g2c)

      new_params$g2c[[input$population_groups_g2c]] %>%
        purrr::iwalk(function(.x, .i) {
          isolate(updateCheckboxInput(session, paste0("condition_", gsub(" ", "_", .i)), value = .x))
        })
    })

    # c2t ====

    groups_c2t <- reactive_changes({
      x <- map_dbl(new_params$g2c, sum)

      if (sum(x) > 0) {
        names(x)[x > 0]
      } else {
        NULL
      }
    })

    observeEvent(groups_c2t(), {
      updateSelectInput(session, "population_groups_c2t", choices = groups_c2t())
    })

    observeEvent(input$population_groups_c2t, {
      if (is.null(groups_c2t)) {
        updateSelectInput(session, "conditions_c2t", choices = NULL)
      }
      req(input$population_groups_c2t)
      x <- names(new_params$c2t[[input$population_groups_c2t]])
      updateSelectInput(session, "conditions_c2t", choices = x)
    })

    tx_obs <- list()
    observeEvent(new_treatments(), {
      t <- new_treatments()
      input_names <- paste0("treatment_",  gsub(" ", "_", t))

      tx_obs %>% walk(~.x$destroy())
      removeUI("#treatments_list > *", TRUE, TRUE)
      tx_obs <<- list()

      treatments_ui <- map2(input_names, t, function(.x, .y) {
        i <- checkboxInput(NS(id, .x), .y)

        tx_obs[[.x]] <- observeEvent(input[[.x]], {
          ig <- input$population_groups_c2t
          ic <- input$conditions_c2t
          req(ig, ic)

          new_params$c2t[[ig]][[ic]][[.y]] <- input[[.x]]
        })

        i
      })

      insertUI("#treatments_list", "beforeEnd", treatments_ui)
    })

    observeEvent(input$conditions_c2t, {
      ig <- req(input$population_groups_c2t)
      ic <- req(input$conditions_c2t)

      new_params$c2t[[ig]][[ic]] %>%
        purrr::iwalk(function(.x, .i) {
          isolate(updateCheckboxInput(session, paste0("treatment_", gsub(" ", "_", .i)), value = .x))
        })
    })
  })
}
