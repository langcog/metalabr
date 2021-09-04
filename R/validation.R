get_google_sheet_id <- function(url) {
  str_match(url, "/spreadsheets/d/([a-zA-Z0-9-_]+)")[2]
}


check_key <- function(dataset_meta) {
  if (dataset_meta$key == "") {
    return("\U274C Cannot load dataset, key missing.\n")
  } else {
    return("\U2705 OK!")
  }
}

fetch_dataset <- function(dataset_meta) {
  dataset_url <- sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
    dataset_meta$key, dataset_meta$key
  )

  result <- tryCatch({
    dataset_url %>%
      httr::GET() %>%
      httr::content(col_names = TRUE, col_types = NULL, encoding = "UTF-8")
  },
  error = function(e) {
    return ("error")
  })

  if ("string" %in% class(result)) {
    return("\U274C Cannot load dataset from Google Sheets")
  } else {
    return("\U2705 OK!")
  }
}

get_data <- function(dataset_meta) {
  dataset_url <- sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
    dataset_meta$key, dataset_meta$key
  )

  tryCatch({
    ret <- dataset_url %>%
      httr::GET() %>%
      httr::content(col_names = TRUE, col_types = NULL, encoding = "UTF-8")
  },
  error = function(e) {
    sprintf("\U274C Cannot load dataset '%s' with key '%s'. Exception: %s.\n", dataset_meta$name,
            dataset_meta$key, e)
  })

  ret
}

check_data_exists <- function(dataset_short_name) {
  dataset_meta <- dataset_info %>% filter(dataset_info$short_name == dataset_short_name)
  if (!nrow(dataset_meta)) {
    return(sprintf("\U274C Dataset is not listed in dataset_info metadata")) 
  } else {
    return(sprintf("\U2705 OK!", dataset_meta$name))
  }
}


process_options <- function(options) {
  if (class(options) == "list") {
    opts <- names(unlist(options, recursive = FALSE))
  } else {
    opts <- options
  }
  paste(map_chr(opts, ~sprintf("<code>%s</code>", .x)), collapse = ", ")
}

make_fields <- function(fields) {
  fields_data <- dplyr::data_frame(field = get_property("field"),
                                   description = get_property("description"),
                                   type = get_property("type"),
                                   format = get_property("format"),
                                   options = get_property("options", process_options),
                                   required = get_property("required")) %>%
    tidyr::unite(`format/options`, format, options, sep = "") 
}


googledrive::drive_auth(path = "~/Downloads/metalab-286312-546cfefb0cb2.json",
                        scopes = "https://www.googleapis.com/auth/drive.readonly")

get_revisions <- function(dataset) {
  request <- request_generate(
    endpoint = "drive.revisions.list",
    params = list(
      fileId = dataset$key
    ),
    token = drive_token()
  )

  res <- request_make(request)
  response_process(res)
}


##test <- get_revisions(dataset_info[1,])

current_dataset_revs <- ""

validation_app <- function(dataset_info) {

  ui <- 
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        ## includeCSS("../common/www/custom.css"),

        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        h2("MetaLab Dataset Validation Tool"),
        tabsetPanel(
          tabPanel("MetaLab Data",
                   selectInput("dataset", "Select a dataset to validate", dataset_info$name,
                               selected = NULL, multiple = FALSE,
                               selectize = TRUE, width = NULL, size = NULL),
                   uiOutput("revisionList"),
                   h5("VersionID for datasets.yaml"),
                   textOutput("version_id"),
                   h3("Validation Results"),
                   h4("Dataset Key Present"),
                   textOutput("check_key_m"),
                   h4("Dataset Key Found in metadata.yaml"),
                   textOutput("check_data_exists_m"),
                   h4("Dataset Loaded from Google Sheets"),
                   textOutput("check_fetch_m"),
                   h4("Field Validation Results"),
                   dataTableOutput("field_validation_m")),
          tabPanel("External URL",
                   textInput("google_sheet_url", "Google Sheets URL", ""),
                   actionButton("validate_sheet_button", "Validate Sheet"),
                   h3("Validation Results"),
                   h4("Dataset Loaded from Google Sheets"),
                   textOutput("check_fetch_g"),
                   h4("Field Validation Results"),
                   dataTableOutput("field_validation_g")),
          tabPanel("Upload CSV",
                   fileInput("csv_file", "Choose CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   h3("Validation Results"),
                   h4("Dataset Loaded from Google Sheets"),
                   textOutput("check_fetch_csv"),
                   h4("Field Validation Results"),
                   dataTableOutput("field_validation_csv")))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Dataset Information",
                   h3("Information in",
                      a("datasets.yaml",
                        href = "https://github.com/langcog/metalab/blob/main/metadata/datasets.yaml"),
                      "file"),
                   dataTableOutput("dataset_spec")
                   ),
          tabPanel("Fields Information",
                   dataTableOutput("fields_spec")
                   )
        )
      )
    )
  )


  server <- function(input, output, session) {
  ## from global.R , should be passed in also
  fields <-
    metalabr:::get_metalab_specs("https://raw.githubusercontent.com/langcog/metalab/main/metadata/spec.yaml")

    fields_derived <-
      metalabr:::get_metalab_derived_specs("https://raw.githubusercontent.com/langcog/metalab/main/metadata/spec_derived.yaml")

get_property <- function(property, property_fun = function(x) x) {
  map_chr(fields, function(entry) {
    if (property %in% names(entry) && !is.null(entry[[property]]))
      property_fun(entry[[property]])
    else ""
  })
}

    
  output$dataset_spec <- renderDT(
    data.frame(column = names(dataset_info),
               value = t(dataset_info %>% filter(name == input$dataset))),
    rownames = "", options = list(dom = 't', pageLength = 50)
  )

  output$fields_spec <- renderDT(make_fields(fields))
  output$selected_name <- renderText(input$dataset)

  output$check_key_m <- renderText(check_key(dataset_info %>% filter(name == input$dataset)))
  output$check_data_exists_m <- renderText(check_data_exists(dataset_info %>% filter(name == input$dataset)%>% pull(short_name)))
  output$check_fetch_m <- renderText(fetch_dataset(dataset_info %>% filter(name == input$dataset)))
  
  output$field_validation_m <- renderDT({
    valid_fields <- validate_metalab_data(dataset_info %>% filter(name == input$dataset),
                                     get_data(dataset_info %>% filter(name == input$dataset)),
                                     fields)

    ret_df <- data.frame(Field = unlist(sapply(fields, "[[", "field")),
                         Valid = unlist(valid_fields)) %>%
      mutate(Valid = ifelse(Valid, "\u2705", "No")) %>% 
      arrange(Valid)

    return(datatable(ret_df, options = list(dom = 'tp', pageLength = 50)))
  })

  observeEvent(input$validate_sheet_button,
  {
    df <- get_data(data.frame(key = get_google_sheet_id(input$google_sheet_url),
                              name = "input url"))
    output$field_validation_g <-
      renderDT({
        valid_fields <- validate_metalab_data(data.frame(name = 'from url'), df, fields)

        ret_df <- data.frame(Field = unlist(sapply(fields, "[[", "field")),
                             Valid = unlist(valid_fields)) %>% arrange(Valid)

        return(datatable(ret_df, options = list(dom = 'tp', pageLength = 50)))
      })
    ## shinyjs::toggle("dataset_spec")    
    output$dataset_spec_m <- renderDT(data.frame(column = "Not available for datasets not included in Metalab",
                                                 value = ""))
  })

  rev_df <- reactive({
    metalabr:::get_revs((dataset_info %>% filter(name == input$dataset) %>% select(key))[1,1])
  })

  # Create all dropdown's at once.
  output$revisionList <- renderUI({
    rev_df <- rev_df()
    rev_dates <- rev_df %>% pull(formatted_ts)
    
    selectInput("revision_date", 
                label = "Select revision to validate", 
                choices = rev_dates)
  })

  observeEvent(input$revision_date, {
    output$version_id <- renderText(metalabr:::get_rev_from_ts(rev_df(), input$revision_date))
  })
  
  output$field_validation_csv <- renderDT({
    req(input$csv_file)

    df <- read.csv(input$csv_file$datapath,
                   header = TRUE)
    
    valid_fields <- validate_metalab_data(data.frame(name = 'from csv'), df, fields)

    ret_df <- data.frame(Field = unlist(sapply(fields, "[[", "field")),
                         Valid = unlist(valid_fields)) %>% arrange(Valid)

    return(datatable(ret_df, options = list(dom = 'tp', pageLength = 50)))
  })
}

  shiny::shinyApp(ui, server)
}
