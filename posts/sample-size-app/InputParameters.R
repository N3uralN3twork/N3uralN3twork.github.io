library(pwr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

## Instead of having inputs for every single type of design,
## Create a list of unique inputs and store as objects:
effectSize <- numericInput(inputId = "effectSize",
             label = "Effect size",
             min = 0,
             max = 1,
             value = 0.5
             )

NObs <- numericInput(inputId = "n",
             label = "Number of Observations",
             min = 0,
             value = NA
             )

alpha <- numericInput(inputId = "sig.level",
             label = "Significance level",
             min = 0,
             max = 0.9999,
             value = 0.10
             )

power <- numericInput(inputId = "power",
             label = "Power of test",
             min = 0,
             max = 0.9999,
             value = 0.80
             )

alternative <- selectInput(inputId = "alternative",
            label = "alternative",
            choices = c("two.sided", "greater", "less"),
            selected = "greater"
            )

n1 <- numericInput(inputId = "n1",
                   label = "N for first sample",
                   min = 1,
                   value = NA
                   )

n2 <- numericInput(inputId = "n2",
                   label = "N for second sample",
                   min = 1,
                   value = 100
                   )
RCorr <- numericInput(inputId = "corr",
                      label = "Linear corr. coef.",
                      min = -0.9999,
                      max = 0.9999,
                      value = NA)

## One Proportion:
one_proportion <- conditionalPanel(
  condition = "input.tests == 'proportion' & input.proportionDesign == 'One proportion'",
  effectSize,
  NObs,
  alpha,
  power,
  alternative
)

## Power for two proportions (same N):
### Same inputs as 1 proportion:
two_proportions_sameN <- conditionalPanel(
  condition = "input.tests == 'proportion' & input.proportionDesign == 'Two proportions (same N)'",
  effectSize,
  NObs,
  alpha,
  power,
  alternative
)

## Power for two proportions (different N):
two_proportions_diffN <- conditionalPanel(
  condition = "input.tests == 'proportion' & input.proportionDesign == 'Two proportions (diff N)'",
  effectSize,
  n1,
  n2,
  alpha,
  power,
  alternative
)

correlation <- conditionalPanel(
  condition = "input.tests == 'correlation' & input.correlationDesign == 'Two groups'",
  NObs,
  RCorr,
  alpha,
  power,
  alternative
)












