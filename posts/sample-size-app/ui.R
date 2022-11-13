################################################################################
###                           Necessary Libraries                            ###
################################################################################
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
source("C:/Users/miqui/OneDrive/Website/n3uraln3twork.github.io/posts/sample-size-app/InputParameters.R", echo = FALSE)
################################################################################
###                               UI Components                              ###
################################################################################
testsOptions <- list("Correlation" = "correlation", "Regression" = "regression",
                     "Proportion" = "proportion")
test_select <- selectInput(inputId = "tests", label = "Test design", choices =  testsOptions, multiple = FALSE, selected = "proportion")

# Conditional panels:
## For proportion designs:
proportion_panel <- conditionalPanel(
  condition = "input.tests == 'proportion'",
  selectInput(inputId = "proportionDesign",
              label = "Please select which design fits your purpose: ",
              choices = c("Two proportions (diff N)", "Two proportions (same N)", "One proportion")
  )
)

## For correlation designs:
correlation_panel <- conditionalPanel(
  condition = "input.tests == 'correlation'",
  selectInput(inputId = "correlationDesign",
              label = "Please select which design fits your purpose: ",
              choices = c("Two groups")
  )
)


################################################################################
###                                   UI                                     ###
################################################################################
ui <- dashboardPage(
  skin = "black",
  options = list(sidebarExpandOnHover = TRUE),
  dashboardHeader(
    title = span(img(src = "Precision Consulting - Power Analysis Logo.jpg", height = 35), "Power!"),
    titleWidth = 300,
    dropdownMenu(
      type = "notifications",
      headerText = strong("HELP"),
      icon = icon("question"),
      badgeStatus = NULL
    )
  ),
  dashboardSidebar(id = "sidebar", minified = FALSE, collapsed = FALSE,
                   width = 300,
                   test_select,
                   proportion_panel,
                   correlation_panel,
                   one_proportion,
                   two_proportions_sameN,
                   two_proportions_diffN,
                   correlation
                   ),
  body = dashboardBody(
    
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "radar_style.css"
      )
    ),
    
    htmlOutput(outputId = "design", inline = TRUE),
    
    br(),
    
    DT::dataTableOutput(outputId = "results"),
    
    br(),
    
    plotOutput(outputId = "curves"),
    
    # tableOutput("show_inputs"),
    
    br(),
    
    br(),
    
    # Ownership
    tags$strong("Author: Matthias Quinn",
                tags$br(),
                "Project: Power Calculator",
                tags$br(),
                "Date: 11/09/2022"
    ),
    
    # Sources:
    h2("References"),
    a("1. Table Options",
      href = "https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table"),
    br(),
    a("2. Pretty Options",
      href = "https://rdrr.io/cran/shinyWidgets/man/prettyCheckboxGroup.html"),
    br(),
    a("3. Shiny Tutorial",
      href = "https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/"),
    br(),
    a("4. Error Handling in R",
      href = "https://bookdown.org/rdpeng/RProgDA/error-handling-and-generation.html")
  ),
  controlbar = dashboardControlbar(),
  title = "Power Calculator"
)