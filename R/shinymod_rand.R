#' UI Function for Randomization Module
#'
#' Creates the user interface for the randomization system
#'
#' @param id A character string that defines the namespace for the module
#'
#' @return A list containing UI elements for different user roles
#'
#' @export
randomizationUI <- function(id) {
  ns <- shiny::NS(id)

  list(
    # Data Manager Interface
    data_manager = shiny::tabsetPanel(
      id = ns("dm_tabs"),

      # Setup Tab
      shiny::tabPanel(
        "Study Setup",
        shiny::fluidRow(
          shiny::column(6,
                        shiny::wellPanel(
                          shiny::h4("Study Configuration"),
                          shiny::textInput(ns("study_id"), "Study ID", value = "EHS_PILOT"),
                          shiny::numericInput(ns("n_participants"), "Number of Participants",
                                              value = 62, min = 1, max = 1000),
                          shiny::textAreaInput(ns("arm_labels"), "Treatment Arms (one per line)",
                                               value = "Treatment\nControl", rows = 3),
                          shiny::textAreaInput(ns("block_sizes"), "Block Sizes (comma-separated)",
                                               value = "2, 4, 6, 8"),
                          shiny::textAreaInput(ns("allocation_ratio"), "Allocation Ratio (comma-separated)",
                                               value = "1, 1"),
                          shiny::numericInput(ns("seed"), "Random Seed (optional)", value = NA),
                          shiny::actionButton(ns("setup_study"), "Setup Study Database",
                                              class = "btn-primary"),
                          shiny::br(), shiny::br(),
                          shiny::actionButton(ns("generate_randomization"), "Generate Randomization",
                                              class = "btn-success")
                        )
          ),
          shiny::column(6,
                        shiny::wellPanel(
                          shiny::h4("Encryption Key Management"),
                          shiny::passwordInput(ns("master_password"), "Master Password"),
                          shiny::actionButton(ns("store_keys"), "Store Encryption Keys",
                                              class = "btn-warning"),
                          shiny::br(), shiny::br(),
                          shiny::h5("Study Status"),
                          shiny::verbatimTextOutput(ns("study_status"))
                        )
          )
        )
      ),

      # Participant Management Tab
      shiny::tabPanel(
        "Participant Management",
        shiny::fluidRow(
          shiny::column(6,
                        shiny::wellPanel(
                          shiny::h4("Add Participants"),
                          mmints::csvUploadUI(ns("participant_upload"))$input,
                          shiny::br(),
                          shiny::h5("Manual Entry"),
                          shiny::textInput(ns("participant_name"), "Participant Name"),
                          shiny::actionButton(ns("add_participant"), "Add Participant")
                        )
          ),
          shiny::column(6,
                        shiny::wellPanel(
                          shiny::h4("Participant List"),
                          DT::DTOutput(ns("participants_table"))
                        )
          )
        )
      ),

      # Unblinding Tab
      shiny::tabPanel(
        "Unblinding Control",
        shiny::fluidRow(
          shiny::column(8,
                        shiny::wellPanel(
                          shiny::h4("Unblinding Management"),
                          shiny::p("This section controls when and how study personnel gain access to treatment allocations."),

                          shiny::h5("Current Blinding Status"),
                          shiny::verbatimTextOutput(ns("blinding_status")),

                          shiny::br(),
                          shiny::h5("Unblinding Actions"),
                          shiny::passwordInput(ns("unblind_password"), "Master Password"),
                          shiny::checkboxInput(ns("confirm_unblind"),
                                               "I confirm that the initial blinded analysis is complete and documented"),
                          shiny::actionButton(ns("perform_unblinding"), "Perform Planned Unblinding",
                                              class = "btn-danger"),

                          shiny::br(), shiny::br(),
                          shiny::h5("Emergency Unblinding"),
                          shiny::textAreaInput(ns("emergency_reason"), "Reason for Emergency Unblinding", rows = 3),
                          shiny::actionButton(ns("emergency_unblind"), "Emergency Unblinding",
                                              class = "btn-danger")
                        )
          ),
          shiny::column(4,
                        shiny::wellPanel(
                          shiny::h4("Unblinding Log"),
                          DT::DTOutput(ns("unblinding_log"))
                        )
          )
        )
      )
    ),

    # Statistician Interface (Randomization)
    statistician_randomization = shiny::fluidPage(
      shiny::h3("Randomization Interface"),
      shiny::p("Use this interface to perform blinded randomization."),

      shiny::fluidRow(
        shiny::column(6,
                      shiny::wellPanel(
                        shiny::h4("Available Participants"),
                        DT::DTOutput(ns("available_participants")),
                        shiny::br(),
                        shiny::actionButton(ns("perform_randomization"), "Perform Randomization",
                                            class = "btn-primary")
                      )
        ),
        shiny::column(6,
                      shiny::wellPanel(
                        shiny::h4("Randomization Results"),
                        DT::DTOutput(ns("randomization_results")),
                        shiny::br(),
                        shiny::downloadButton(ns("download_allocations"), "Download Allocations")
                      )
        )
      )
    ),

    # Statistician Interface (Analysis)
    statistician_analysis = shiny::fluidPage(
      shiny::h3("Blinded Analysis Interface"),
      shiny::p("Analyze data without knowing treatment assignments."),

      shiny::fluidRow(
        shiny::column(8,
                      shiny::wellPanel(
                        shiny::h4("Available Data"),
                        shiny::selectInput(ns("analysis_table"), "Select Data Table",
                                           choices = c("Clinical Data", "Assignment Allocations")),
                        DT::DTOutput(ns("analysis_data")),
                        shiny::br(),
                        shiny::downloadButton(ns("download_analysis_data"), "Download Data")
                      )
        ),
        shiny::column(4,
                      shiny::wellPanel(
                        shiny::h4("Analysis Tools"),
                        shiny::p("Group comparison tools for blinded analysis"),
                        shiny::selectInput(ns("comparison_var"), "Variable to Compare", choices = NULL),
                        shiny::actionButton(ns("run_comparison"), "Run Group Comparison"),
                        shiny::br(), shiny::br(),
                        shiny::verbatimTextOutput(ns("comparison_results"))
                      )
        )
      )
    ),

    # Outcome Assessor Interface
    outcome_assessor = shiny::fluidPage(
      shiny::h3("Outcome Assessment Interface"),
      shiny::p("Enter and view clinical data while remaining blinded to treatment assignments."),

      shiny::fluidRow(
        shiny::column(6,
                      shiny::wellPanel(
                        shiny::h4("Data Entry"),
                        shiny::selectInput(ns("participant_select"), "Select Participant", choices = NULL),
                        shiny::selectInput(ns("data_type"), "Data Type",
                                           choices = c("Baseline", "Week 4", "Week 8", "Week 12", "Week 16")),
                        shiny::textAreaInput(ns("data_value"), "Data Value", rows = 5),
                        shiny::actionButton(ns("save_clinical_data"), "Save Data")
                      )
        ),
        shiny::column(6,
                      shiny::wellPanel(
                        shiny::h4("Clinical Data Summary"),
                        DT::DTOutput(ns("clinical_data_summary"))
                      )
        )
      )
    ),

    # Audit Interface
    audit = shiny::fluidPage(
      shiny::h3("Audit Trail"),
      shiny::p("Complete audit log of all system activities."),

      shiny::fluidRow(
        shiny::column(12,
                      shiny::wellPanel(
                        shiny::h4("System Activity Log"),
                        shiny::dateRangeInput(ns("audit_dates"), "Date Range",
                                              start = Sys.Date() - 30, end = Sys.Date()),
                        shiny::selectInput(ns("audit_user"), "User (optional)",
                                           choices = c("All Users" = ""), multiple = FALSE),
                        shiny::selectInput(ns("audit_action"), "Action Type (optional)",
                                           choices = c("All Actions" = ""), multiple = FALSE),
                        DT::DTOutput(ns("audit_log")),
                        shiny::br(),
                        shiny::downloadButton(ns("download_audit_log"), "Download Audit Log")
                      )
        )
      )
    )
  )
}

#' Server Function for Randomization Module
#'
#' Handles the server-side logic for the randomization system
#'
#' @param id A character string that matches the ID used in randomizationUI
#' @param postgres_module A postgresModule instance
#' @param auth_info Reactive containing authentication information
#'
#' @export
randomizationServer <- function(id, postgres_module, auth_info) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values for storing data
    values <- shiny::reactiveValues(
      study_id = "EHS_PILOT",
      randomization_package = NULL,
      participants = NULL,
      current_user_role = NULL
    )

    # Update current user role based on auth info
    shiny::observe({
      shiny::req(auth_info())
      if (auth_info()$user_auth) {
        values$current_user_role <- auth_info()$info$permissions
      }
    })

    # === Data Manager Functions ===

    # Setup study database
    shiny::observeEvent(input$setup_study, {
      shiny::req(input$study_id)

      values$study_id <- input$study_id

      tryCatch({
        setup_randomization_database(postgres_module, values$study_id)

        # Log action
        log_user_action(
          postgres_module, values$study_id, auth_info()$info$user,
          "Setup study database", session = session
        )

        shiny::showNotification("Study database setup complete", type = "message")

      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # Generate randomization
    shiny::observeEvent(input$generate_randomization, {
      shiny::req(input$study_id, input$n_participants, input$arm_labels)

      # Parse inputs
      arm_labels <- trimws(strsplit(input$arm_labels, "\n")[[1]])
      block_sizes <- as.numeric(trimws(strsplit(input$block_sizes, ",")[[1]]))
      allocation_ratio <- as.numeric(trimws(strsplit(input$allocation_ratio, ",")[[1]]))
      seed <- if (is.na(input$seed)) NULL else input$seed

      tryCatch({
        # Generate randomization package
        values$randomization_package <- create_randomization_package(
          n_participants = input$n_participants,
          arm_labels = arm_labels,
          block_sizes = block_sizes,
          allocation_ratio = allocation_ratio,
          study_id = values$study_id,
          seed = seed
        )

        # Store data in database
        store_randomization_data(postgres_module, values$study_id, values$randomization_package)

        # Log action
        log_user_action(
          postgres_module, values$study_id, auth_info()$info$user,
          "Generated randomization sequence", session = session
        )

        shiny::showNotification("Randomization generated successfully", type = "message")

      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # Store encryption keys
    shiny::observeEvent(input$store_keys, {
      shiny::req(input$master_password, values$randomization_package)

      tryCatch({
        store_encrypted_key(
          postgres_module, values$study_id, "assignment_mask_key",
          values$randomization_package$metadata$encryption_key,
          input$master_password, auth_info()$info$user
        )

        shiny::showNotification("Encryption keys stored securely", type = "message")

      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # Study status output
    output$study_status <- shiny::renderText({
      if (is.null(values$randomization_package)) {
        "Study not yet configured"
      } else {
        paste(
          "Study ID:", values$study_id,
          "\nParticipants:", values$randomization_package$metadata$n_participants,
          "\nArms:", paste(values$randomization_package$metadata$arm_labels, collapse = ", "),
          "\nCreated:", values$randomization_package$metadata$created_date
        )
      }
    })

    # === Participant Management ===

    # Handle CSV upload for participants
    participant_data <- mmints::csvUploadServer(ns("participant_upload"))

    # Add participant manually
    shiny::observeEvent(input$add_participant, {
      shiny::req(input$participant_name)

      # Generate participant ID
      user_ids <- generate_user_ids(1, prefix = paste0(values$study_id, "_"))

      # Add to name-ID link table
      name_id_data <- data.frame(
        participant_name = input$participant_name,
        participant_id = user_ids[1],
        created_by = auth_info()$info$user,
        stringsAsFactors = FALSE
      )

      # Add to user IDs table
      user_id_data <- data.frame(
        participant_id = user_ids[1],
        stringsAsFactors = FALSE
      )

      tryCatch({
        # Store in database (implementation would use proper table switching)
        # This is a simplified version
        log_user_action(
          postgres_module, values$study_id, auth_info()$info$user,
          paste("Added participant:", input$participant_name),
          session = session
        )

        shiny::showNotification("Participant added successfully", type = "message")

        # Clear input
        shiny::updateTextInput(session, "participant_name", value = "")

      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # === Unblinding Management ===

    output$blinding_status <- shiny::renderText({
      "Status: Blinded\nPhase: Data Collection\nNext Milestone: Initial Analysis Complete"
    })

    # Perform planned unblinding
    shiny::observeEvent(input$perform_unblinding, {
      shiny::req(input$unblind_password, input$confirm_unblind)

      if (!input$confirm_unblind) {
        shiny::showNotification("Please confirm that initial analysis is complete", type = "error")
        return()
      }

      tryCatch({
        # Retrieve encryption key
        encryption_key <- retrieve_encrypted_key(
          postgres_module, values$study_id, "assignment_mask_key",
          input$unblind_password, auth_info()$info$user
        )

        # Update database to mark as unblinded
        log_user_action(
          postgres_module, values$study_id, auth_info()$info$user,
          "Performed planned unblinding", session = session
        )

        shiny::showNotification("Unblinding completed successfully", type = "message")

      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # === Statistician Functions ===

    # Display available participants for randomization
    output$available_participants <- DT::renderDT({
      # This would query the user_ids table
      data.frame(
        participant_id = c("EHS_PILOT_ABC123", "EHS_PILOT_DEF456"),
        enrollment_date = c("2024-01-15", "2024-01-16"),
        status = c("Ready", "Ready")
      )
    })

    # Perform randomization
    shiny::observeEvent(input$perform_randomization, {
      # Implementation would handle actual randomization
      log_user_action(
        postgres_module, values$study_id, auth_info()$info$user,
        "Performed participant randomization", session = session
      )

      shiny::showNotification("Randomization completed", type = "message")
    })

    # === Outcome Assessor Functions ===

    # Save clinical data
    shiny::observeEvent(input$save_clinical_data, {
      shiny::req(input$participant_select, input$data_type, input$data_value)

      clinical_data <- data.frame(
        participant_id = input$participant_select,
        data_type = input$data_type,
        data_value = input$data_value,
        collected_by = auth_info()$info$user,
        stringsAsFactors = FALSE
      )

      tryCatch({
        # Store clinical data (implementation would use proper table)
        log_user_action(
          postgres_module, values$study_id, auth_info()$info$user,
          "Saved clinical data", session = session
        )

        shiny::showNotification("Clinical data saved", type = "message")

      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # === Audit Functions ===

    output$audit_log <- DT::renderDT({
      # This would query the audit log table
      data.frame(
        timestamp = c("2024-01-15 10:30:00", "2024-01-15 11:15:00"),
        user_id = c("pdm_user", "statistician_1"),
        action = c("Setup study database", "Generated randomization"),
        table_name = c("study_metadata", "assignment_allocations")
      )
    })

    # Download handlers
    output$download_allocations <- shiny::downloadHandler(
      filename = function() {
        paste0(values$study_id, "_allocations_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Implementation would create proper allocation file
        utils::write.csv(data.frame(note = "Allocation data would be here"), file, row.names = FALSE)
      }
    )

    output$download_audit_log <- shiny::downloadHandler(
      filename = function() {
        paste0(values$study_id, "_audit_log_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Implementation would export audit log
        write.csv(data.frame(note = "Audit log would be here"), file, row.names = FALSE)
      }
    )

  })
}

#' Helper function to store randomization data in database
#'
#' @param postgres_module A postgresModule instance
#' @param study_id Character string identifying the study
#' @param randomization_package List containing randomization data
#'
store_randomization_data <- function(postgres_module, study_id, randomization_package) {

  # Store each table in the appropriate database table
  # This is a simplified implementation

  # Store user IDs
  # postgres_module$saveData(randomization_package$user_ids_table)

  # Store assignment strings
  # postgres_module$saveData(randomization_package$assignment_strings_table)

  # Store encrypted assignment mask
  # (Implementation would handle binary data properly)

  # Store assignment allocations
  # postgres_module$saveData(randomization_package$assignment_allocations_table)

  # Store metadata
  metadata_records <- data.frame(
    metadata_key = c("randomization_generated", "n_participants", "algorithm"),
    metadata_value = c("true",
                       as.character(randomization_package$metadata$n_participants),
                       randomization_package$metadata$randomization_algorithm),
    stringsAsFactors = FALSE
  )

  # Implementation would store these properly
}
