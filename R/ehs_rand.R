#' EHS Pilot Study Randomization Application
#'
#' Complete Shiny application for managing blinded randomization in the EHS pilot study
#'
#' @param dbname Database name
#' @param host Database host
#' @param port Database port
#' @param user Database user
#' @param password Database password
#'
#' @export
ehs_randomization_app <- function(dbname = "ehs_pilot",
                                  host = "localhost",
                                  port = 5432,
                                  user = "postgres",
                                  password = "password") {

  # Define UI
  ui <- shiny::fluidPage(
    # Custom CSS for styling
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .auth-container {
          max-width: 400px;
          margin: 50px auto;
          padding: 20px;
          border: 1px solid #ddd;
          border-radius: 8px;
          background-color: #f9f9f9;
        }
        .auth-container .btn {
          margin: 10px 0;
          width: 100%;
        }
        .pull-right {
          float: right;
        }
        .role-badge {
          background-color: #007bff;
          color: white;
          padding: 5px 10px;
          border-radius: 15px;
          font-size: 0.8em;
          margin-left: 10px;
        }
        .status-indicator {
          padding: 5px 10px;
          border-radius: 3px;
          font-weight: bold;
        }
        .status-blinded {
          background-color: #28a745;
          color: white;
        }
        .status-unblinded {
          background-color: #dc3545;
          color: white;
        }
        .audit-high-priority {
          background-color: #fff3cd;
        }
        .navbar-brand {
          font-weight: bold;
        }
      "))
    ),

    # Navigation bar
    shiny::navbarPage(
      title = shiny::div(
        "EHS Pilot Study - Randomization System",
        shiny::uiOutput("status_indicator", inline = TRUE)
      ),
      id = "main_navbar",

      # Authentication Tab (always visible)
      shiny::tabPanel(
        "Authentication",
        value = "auth_tab",
        shiny::div(
          class = "auth-container",
          shiny::h3("System Access", style = "text-align: center;"),
          mmints::authUI("auth_module"),
          shiny::br(),
          shiny::uiOutput("user_info")
        )
      )
    ),

    # Footer with citations
    shiny::div(
      style = "margin-top: 50px; padding-top: 20px; border-top: 1px solid #eee; text-align: center;",
      mmints::citationUI("citations")$button,
      mmints::citationUI("citations")$output
    )
  )

  # Define Server
  server <- function(input, output, session) {

    # Initialize database modules
    postgres_auth <- mmints::postgresServer(
      "postgres_auth",
      dbname = dbname,
      datatable = "users",  # For authentication
      host = host,
      port = port,
      user = user,
      password = password,
      data = shiny::reactive(NULL)
    )

    postgres_main <- mmints::postgresServer(
      "postgres_main",
      dbname = dbname,
      datatable = "main_data",  # Will be changed dynamically
      host = host,
      port = port,
      user = user,
      password = password,
      data = shiny::reactive(NULL)
    )

    # Initialize authentication
    auth <- mmints::authServer("auth_module", postgres_auth, user_table = "users")

    # Initialize randomization module
    randomization <- randomizationServer("randomization", postgres_main, auth)

    # Reactive values for app state
    values <- shiny::reactiveValues(
      study_phase = "setup",
      blinding_status = "blinded",
      available_tabs = character(0)
    )

    # User info display
    output$user_info <- shiny::renderUI({
      if (!is.null(auth()) && auth()$user_auth) {
        user_info <- auth()$info
        shiny::div(
          shiny::h4("Welcome, ", user_info$user, "!"),
          shiny::p(
            "Role: ", user_info$permissions,
            shiny::span(class = "role-badge", user_info$permissions)
          ),
          shiny::p("Access Level: ", get_access_level(user_info$permissions)),
          shiny::hr(),
          shiny::p("Available interfaces will appear in the navigation menu above.")
        )
      } else {
        shiny::div(
          shiny::p("Please log in or continue as guest to access the system."),
          shiny::p("Different user roles have access to different parts of the system:")
        )
      }
    })

    # Status indicator
    output$status_indicator <- shiny::renderUI({
      if (values$blinding_status == "blinded") {
        shiny::span(class = "status-indicator status-blinded", "BLINDED")
      } else {
        shiny::span(class = "status-indicator status-unblinded", "UNBLINDED")
      }
    })

    # Dynamic tab management based on user role
    shiny::observe({
      shiny::req(auth()$user_auth)

      user_role <- auth()$info$permissions
      new_tabs <- get_user_tabs(user_role)

      # Remove tabs that shouldn't be there
      current_tabs <- values$available_tabs
      for (tab in current_tabs) {
        if (!tab %in% new_tabs) {
          shiny::removeTab("main_navbar", tab)
        }
      }

      # Add tabs that should be there
      for (tab in new_tabs) {
        if (!tab %in% current_tabs) {
          add_user_tab(session, tab, user_role)
        }
      }

      values$available_tabs <- new_tabs
    })

    # Citation system
    citations <- list(
      "R Software" = function() mmints::format_citation(utils::citation()),
      "Block Randomization Reference" = "Efird J. Blocked randomization with randomly selected block sizes. Int J Environ Res Public Health. 2011;8(1):15-20.",
      "Clinical Trial Blinding" = "Monaghan TF, Rahman SN, Agudelo CW, et al. Blinding in Clinical Trials: Seeing the Big Picture. Medicina (Kaunas). 2021;57(7):647.",
      "Sodium Cryptography" = "Ooms J. sodium: A Modern and Easy-to-Use Crypto Library. R package version 1.3.0.",
      "mmints Package" = function() mmints::format_citation(utils::citation("mmints"))
    )

    mmints::citationServer("citations", citations)

    # Session cleanup
    session$onSessionEnded(function() {
      # Log session end
      if (!is.null(auth()) && auth()$user_auth) {
        tryCatch({
          log_user_action(
            postgres_main, "EHS_PILOT", auth()$info$user,
            "Session ended", session = session
          )
        }, error = function(e) {
          # Silent error handling for cleanup
        })
      }
    })
  }

  # Return Shiny app object
  shiny::shinyApp(ui, server)
}

#' Get access level description for user role
#'
#' @param role User role string
#' @return Character description of access level
get_access_level <- function(role) {
  switch(role,
         "data_manager" = "Full system access, unblinding control",
         "statistician_randomization" = "Randomization generation (blinded)",
         "statistician_analysis" = "Data analysis (blinded until unblinding)",
         "outcome_assessor" = "Clinical data entry (blinded)",
         "principal_investigator" = "Study oversight, emergency unblinding",
         "admin" = "System administration",
         "guest" = "Limited read-only access",
         "Limited access"
  )
}

#' Get tabs available for user role
#'
#' @param role User role string
#' @return Character vector of available tab IDs
get_user_tabs <- function(role) {
  base_tabs <- character(0)

  switch(role,
         "data_manager" = c("data_manager_tab", "audit_tab"),
         "statistician_randomization" = c("randomization_tab"),
         "statistician_analysis" = c("analysis_tab"),
         "outcome_assessor" = c("assessment_tab"),
         "principal_investigator" = c("overview_tab", "emergency_tab"),
         "admin" = c("data_manager_tab", "audit_tab", "admin_tab"),
         "guest" = c("overview_tab"),
         character(0)
  )
}

#' Add tab for specific user role
#'
#' @param session Shiny session object
#' @param tab_id Tab identifier
#' @param user_role User role
add_user_tab <- function(session, tab_id, user_role) {

  switch(tab_id,
         "data_manager_tab" = {
           shiny::appendTab(
             "main_navbar",
             shiny::tabPanel(
               "Data Management",
               value = "data_manager_tab",
               randomizationUI("randomization")$data_manager
             )
           )
         },

         "randomization_tab" = {
           shiny::appendTab(
             "main_navbar",
             shiny::tabPanel(
               "Randomization",
               value = "randomization_tab",
               randomizationUI("randomization")$statistician_randomization
             )
           )
         },

         "analysis_tab" = {
           shiny::appendTab(
             "main_navbar",
             shiny::tabPanel(
               "Blinded Analysis",
               value = "analysis_tab",
               randomizationUI("randomization")$statistician_analysis
             )
           )
         },

         "assessment_tab" = {
           shiny::appendTab(
             "main_navbar",
             shiny::tabPanel(
               "Outcome Assessment",
               value = "assessment_tab",
               randomizationUI("randomization")$outcome_assessor
             )
           )
         },

         "audit_tab" = {
           shiny::appendTab(
             "main_navbar",
             shiny::tabPanel(
               "Audit Trail",
               value = "audit_tab",
               randomizationUI("randomization")$audit
             )
           )
         },

         "overview_tab" = {
           shiny::appendTab(
             "main_navbar",
             shiny::tabPanel(
               "Study Overview",
               value = "overview_tab",
               create_overview_tab()
             )
           )
         },

         "emergency_tab" = {
           shiny::appendTab(
             "main_navbar",
             shiny::tabPanel(
               "Emergency Procedures",
               value = "emergency_tab",
               create_emergency_tab()
             )
           )
         }
  )
}

#' Create overview tab content
#'
#' @return UI content for overview tab
create_overview_tab <- function() {
  shiny::fluidPage(
    shiny::h3("EHS Pilot Study Overview"),

    shiny::fluidRow(
      shiny::column(6,
                    shiny::wellPanel(
                      shiny::h4("Study Information"),
                      shiny::p(shiny::strong("Study Title:"), "EHS Pilot Study - 8-week smartphone-based intervention"),
                      shiny::p(shiny::strong("Design:"), "Randomized Controlled Trial (RCT)"),
                      shiny::p(shiny::strong("Allocation:"), "1:1 Treatment:Control"),
                      shiny::p(shiny::strong("Target Sample:"), "62 participants (31 per arm)"),
                      shiny::p(shiny::strong("Randomization:"), "Block randomization with variable block sizes"),
                      shiny::p(shiny::strong("Blinding:"), "Partial blinding with outcome assessors and statisticians blinded")
                    )
      ),

      shiny::column(6,
                    shiny::wellPanel(
                      shiny::h4("Study Timeline"),
                      shiny::tags$ul(
                        shiny::tags$li("Baseline data collection"),
                        shiny::tags$li("Randomization and intervention start"),
                        shiny::tags$li("Week 4: Mid-intervention assessment"),
                        shiny::tags$li("Week 8: Post-intervention assessment"),
                        shiny::tags$li("Week 12: 4-week follow-up"),
                        shiny::tags$li("Week 16: 8-week follow-up")
                      )
                    )
      )
    ),

    shiny::fluidRow(
      shiny::column(12,
                    shiny::wellPanel(
                      shiny::h4("Blinding Strategy"),
                      shiny::p("This study implements a sophisticated blinding strategy to minimize bias:"),
                      shiny::tags$ul(
                        shiny::tags$li("Treatment allocations are encrypted until after initial blinded analysis"),
                        shiny::tags$li("Outcome assessors remain blinded throughout data collection"),
                        shiny::tags$li("Statisticians conducting randomization and analysis are blinded to treatment meaning"),
                        shiny::tags$li("Only the Principal Data Manager has access to unblinded data"),
                        shiny::tags$li("All access and unblinding events are logged for transparency")
                      )
                    )
      )
    )
  )
}

#' Create emergency procedures tab
#'
#' @return UI content for emergency tab
create_emergency_tab <- function() {
  shiny::fluidPage(
    shiny::h3("Emergency Procedures"),

    shiny::div(
      style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
      shiny::h4(style = "color: #856404;", "Emergency Unblinding Protocol"),
      shiny::p(style = "color: #856404;",
               "Emergency unblinding should only be performed in case of medical necessity or serious adverse events requiring knowledge of treatment allocation.")
    ),

    shiny::fluidRow(
      shiny::column(8,
                    shiny::wellPanel(
                      shiny::h4("Emergency Unblinding Request"),
                      shiny::p("Only the Principal Investigator may request emergency unblinding."),

                      shiny::h5("Participant Information"),
                      shiny::textInput("emergency_participant_id", "Participant ID"),

                      shiny::h5("Emergency Details"),
                      shiny::textAreaInput("emergency_description", "Description of Emergency",
                                           rows = 4,
                                           placeholder = "Describe the medical emergency or serious adverse event..."),

                      shiny::h5("Medical Necessity Justification"),
                      shiny::textAreaInput("emergency_justification", "Justification for Unblinding",
                                           rows = 4,
                                           placeholder = "Explain why knowledge of treatment allocation is medically necessary..."),

                      shiny::h5("Authorization"),
                      shiny::checkboxInput("emergency_pi_confirm", "I am the Principal Investigator and confirm this is a medical emergency"),
                      shiny::passwordInput("emergency_pi_password", "PI Password"),

                      shiny::br(),
                      shiny::actionButton("submit_emergency_unblind", "Submit Emergency Unblinding Request",
                                          class = "btn-danger btn-lg")
                    )
      ),

      shiny::column(4,
                    shiny::wellPanel(
                      shiny::h4("Emergency Contacts"),
                      shiny::p(shiny::strong("Principal Investigator:")),
                      shiny::p("Dr. [Name]", shiny::br(), "Phone: XXX-XXX-XXXX"),

                      shiny::p(shiny::strong("Principal Data Manager:")),
                      shiny::p("[Name]", shiny::br(), "Phone: XXX-XXX-XXXX"),

                      shiny::p(shiny::strong("IRB Office:")),
                      shiny::p("Phone: XXX-XXX-XXXX", shiny::br(), "Email: irb@institution.edu"),

                      shiny::hr(),

                      shiny::h5("Reporting Requirements"),
                      shiny::p("All emergency unblinding events must be reported to:"),
                      shiny::tags$ul(
                        shiny::tags$li("IRB within 24 hours"),
                        shiny::tags$li("Safety monitoring committee within 24 hours"),
                        shiny::tags$li("Sponsor (if applicable)")
                      )
                    )
      )
    )
  )
}

#' Launch EHS Randomization App with Default Settings
#'
#' Convenience function to launch the app with environment variable configuration
#'
#' @export
launch_ehs_app <- function() {
  # Get configuration from environment variables
  dbname <- Sys.getenv("EHS_DBNAME", "ehs_pilot")
  host <- Sys.getenv("EHS_HOST", "localhost")
  port <- as.integer(Sys.getenv("EHS_PORT", "5432"))
  user <- Sys.getenv("EHS_USER", "postgres")
  password <- Sys.getenv("EHS_PASSWORD", "password")

  # Launch app
  ehs_randomization_app(
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password
  )
}

#' Example function to setup demo data
#'
#' Creates demo users and initial data for testing the system
#'
#' @param postgres_module A postgresModule instance
#' @export
setup_demo_data <- function(postgres_module) {

  # Create demo users with different roles
  demo_users <- data.frame(
    username = c("pdm_demo", "stat_rand", "stat_analysis", "assessor_1", "pi_demo"),
    password = c(
      sodium::password_store("demo123"),
      sodium::password_store("demo123"),
      sodium::password_store("demo123"),
      sodium::password_store("demo123"),
      sodium::password_store("demo123")
    ),
    email = c("pdm@demo.com", "statr@demo.com", "stata@demo.com", "assess@demo.com", "pi@demo.com"),
    display = c("Principal Data Manager", "Randomization Statistician", "Analysis Statistician", "Outcome Assessor", "Principal Investigator"),
    role = c("data_manager", "statistician_randomization", "statistician_analysis", "outcome_assessor", "principal_investigator"),
    stringsAsFactors = FALSE
  )

  # Save demo users
  postgres_module$saveData(demo_users)

  message("Demo users created:")
  message("- pdm_demo / demo123 (Principal Data Manager)")
  message("- stat_rand / demo123 (Randomization Statistician)")
  message("- stat_analysis / demo123 (Analysis Statistician)")
  message("- assessor_1 / demo123 (Outcome Assessor)")
  message("- pi_demo / demo123 (Principal Investigator)")
}
