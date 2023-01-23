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
      else if (input$tests == "correlation" && input$correlationDesign == "Two groups") {
        powerAnalysis = pwr.r.test(n = switchNA(input$n), r = switchNA(input$corr),
                                   sig.level = switchNA(input$sig.level), power = switchNA(input$power),
                                   alternative = input$alternative)
        results = data.frame(t(sapply(powerAnalysis, c)))
      }
    ))
  
  output$results <- DT::renderDataTable({
    tables()
  })
  
  results <- reactive({
    if (input$tests == "proportion" && input$proportionDesign == "Two proportions (same N)") {
      powerAnalysis = pwr.2p.test(h = switchNA(input$effectSize), n = switchNA(input$n),
                                  sig.level = switchNA(input$sig.level), power = switchNA(input$power),
                                  alternative = input$alternative)
    }
    else if (input$tests == "proportion" && input$proportionDesign == "One proportion") {
      powerAnalysis = pwr.p.test(h = switchNA(input$effectSize), n = switchNA(input$n),
                                 sig.level = switchNA(input$sig.level), power = switchNA(input$power),
                                 alternative = input$alternative)
    }
    else if (input$tests == "proportion" && input$proportionDesign == "Two proportions (diff N)") {
      powerAnalysis = pwr.2p2n.test(h = switchNA(input$effectSize), n1 = switchNA(input$n1), n2 = switchNA(input$n2),
                                    sig.level = switchNA(input$sig.level), power = switchNA(input$power),
                                    alternative = input$alternative)
    }
    else if (input$tests == "correlation" && input$correlationDesign == "Two groups") {
      powerAnalysis = pwr.r.test(n = switchNA(input$n), r = switchNA(input$corr),
                                 sig.level = switchNA(input$sig.level), power = switchNA(input$power),
                                 alternative = input$alternative) 
    }
  })
  
  output$curves <- renderPlot({
    plot(results(), xlim = c(-100, 100), ylim = c(-100, 100))
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





