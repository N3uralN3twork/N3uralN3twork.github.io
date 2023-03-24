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
                               alternative = input$alternative
                               ) 
  }
})