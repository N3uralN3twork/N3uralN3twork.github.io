################################################################################
###                           Necessary Libraries                            ###
################################################################################
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
source("C:/Users/miqui/OneDrive/Website/n3uraln3twork.github.io/posts/sample-size-app/InputParameters.R", echo = FALSE)
source("C:/Users/miqui/OneDrive/Website/n3uraln3twork.github.io/posts/sample-size-app/Results.R", echo = FALSE)
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

## For regression designs:
regression_panel <- conditionalPanel(
  condition = "input.tests == 'regression'",
  selectInput(inputId = "regressionDesign",
              label = "Please select which design fits your purpose: ",
              choices = c("Linear bivariate regression (one group)", "Linear bivariate regression (two groups, difference between intercepts)", "Linear bivariate regression (two groups, difference between slopes)", "Linear multiple regression (Fixed model, R2 deviation from zero)",
                          "Linear multiple regression (Fixed model, R2 increase)", "Linear multiple regression (Fixed model, single regression coefficient)", "Linear multiple regression (Random Model)", "Logistic regression", "Poisson regression")
  )
)



################################################################################
###                               Functions                                  ###
################################################################################
switchNA <- function(inputID){
  if (is.na(inputID)) {
    switch(inputID, "NA" = NULL, inputID)
  }
  else {
    inputID
  }
}

resultsToDF <- function(results){
  # binding columns together
  df <- data.frame(t(sapply(results, c)))

  return(df)
}

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
                   regression_panel,
                   one_proportion,
                   two_proportions_sameN,
                   two_proportions_diffN
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


################################################################################
###                               Server                                     ###
################################################################################
server <- function(input, output, session){
  design <- reactive({
    if (input$tests == "proportion") {
      design = input$proportionDesign
    }
    else if (input$tests == "correlation") {
      design = input$correlationDesign
    }
    else if (input$tests == "regression") {
      design = input$regressionDesign
    }
  })
  
  tables <- reactive(
    DT::datatable(
      if (input$tests == "proportion" && input$proportionDesign == "Two proportions (same N)") {
        powerAnalysis = pwr.2p.test(h = switchNA(input$effectSize), n = switchNA(input$n),
                                    sig.level = switchNA(input$sig.level), power = switchNA(input$power),
                                    alternative = input$alternative)
        results = data.frame(t(sapply(powerAnalysis, c)))
      }
      else if (input$tests == "proportion" && input$proportionDesign == "One proportion") {
        powerAnalysis = pwr.p.test(h = switchNA(input$effectSize), n = switchNA(input$n),
                                   sig.level = switchNA(input$sig.level), power = switchNA(input$power),
                                   alternative = input$alternative)
        results = data.frame(t(sapply(powerAnalysis, c)))
      }
      else if (input$tests == "proportion" && input$proportionDesign == "Two proportions (diff N)") {
        powerAnalysis = pwr.2p2n.test(h = switchNA(input$effectSize), n1 = switchNA(input$n1), n2 = switchNA(input$n2),
                                      sig.level = switchNA(input$sig.level), power = switchNA(input$power),
                                      alternative = input$alternative)
        results = data.frame(t(sapply(powerAnalysis, c)))
      }
    ))
  
  output$results <- DT::renderDataTable({
    tables()
  })
  
  output$curves <- renderPlot({
    plot(results())
  })
  
  AllInputs <- reactive(
    DT::datatable(
      x = reactiveValuesToList(input) %>%
    
      # Remove the sidebarItemExpanded element from the inputs
      x$sidebarItemExpanded <- NULL %>%
  
      dfInputs <<- data.frame(
        names = names(x),
        values = unlist(x)
      ) %>%
      
      rownames(dfInputs) <<- 1:nrow(dfInputs) %>%
      
      dfInputs <- as.data.frame(dfInputs)
    )
  )
  
  output$show_inputs <- DT::renderDataTable({
    AllInputs()
  })

  output$design <- renderText({
    HTML(paste0("You chose: <b>", input$tests))
  })
}
################################################################################
###                               App                                        ###
################################################################################
shinyApp(ui = ui, server = server)






# ! powerAnalysis <- pwr.p.test(h = 0.30, n = NULL, sig.level = 0.10, power = 0.90, alternative = "greater")
# ! data.frame(t(sapply(powerAnalysis, c)))






