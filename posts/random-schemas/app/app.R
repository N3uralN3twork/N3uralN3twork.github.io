"
Title: Randomization Schema
Author: Matt Quinn
Date: September 18th 2020
Finished on: August 10th 2020
Class: STA 635 Consulting and Programming
"

################################################################################
###                           Necessary Libraries                            ###
################################################################################

library(dplyr)        # For the row_number and unite functions
library(tidyr)        # For the row_count function
library(shinythemes)  # For aesthetics
library(shinyWidgets) # For aesthetics
library(DT)           # For displaying the data table
library(randomizr)    # For the block_ra function

################################################################################
###                           NOTES:                                         ###
################################################################################

# INPUT:
# A list of site codes (character)
# The number of subjects per site (integer)
# The randomization ratio (>= 1)
# Number of factors in your experiment (>= 0)

# OUTPUT:
# A sequential list of N codes inside a dataframe object
# An empty data vector for each factor

# For each site do the following:
# Store site in dataframe N times each
# Add numbers (1-K) to end of each site code in dataframe
# Randomly assign (T/C) to end of each site code in dataframe

# Assigning the numbers:
# Based on your row number for each site
# If less than 10, assign a "0" between the code and the number
# Otherwise, assign no space between the code and the number

# Randomization Ratio:
# Number of Treatment subjects == (N/(N+D))*NSubjects:
# Number of Control subjects == (NSubjects - TSubjects)
# (1, 1/2) , (2, 2/3), (3, 3/4), (4, 4/5), etc. = (n, n/n+1)

# If a negative number is input, it will take the absolute value of the input


# Sources:
"https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table"
"https://rdrr.io/cran/shinyWidgets/man/prettyCheckboxGroup.html"
"https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/" # Shiny tutorial
"https://bookdown.org/rdpeng/RProgDA/error-handling-and-generation.html" # Error handling in R
"https://www.datamentor.io/r-programming/for-loop/"
"https://astrostatistics.psu.edu/su07/R/html/base/html/rep.html"
"https://www.rdocumentation.org/packages/tidyft/versions/0.4.5/topics/uncount" # Uncount Function
"https://stackoverflow.com/questions/6558921/boolean-operators-and?noredirect=1&lq=1" # && operator
"https://rdrr.io/cran/shinyWidgets/man/multiInput.html" # MultiInput Widget
"https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html" # Making an R Package
"https://stackoverflow.com/questions/38351820/negation-of-in-in-r" # Negation of %in%
"https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer" # Test if number is an integer
"https://stackoverflow.com/questions/36665492/how-to-combine-two-lists-in-r/36672341" # Combining lists

################################################################################
###                         Creating the Design Schema
###
################################################################################
schema <- function(Sites = NULL, NSubjects, BlockSize = NULL, RRatio = NULL, seed = NULL){
  
  greeks <- list("\U03B1", "\U03B2", "\U03B3", "\U03B4", "\U03B5", "\U03B6",
                 "\U03B7", "\U03B8", "\U03B9", "\U03BA", "\U03BB", "\U03BC",
                 "\U03BD", "\U03BE", "\U03BF", "\U03C0", "\U03C1", "\U03C3",
                 "\U03C4", "\U03C5", "\U03C6", "\U03C7", "\U03C8", "\U03C9")
  
  # Define a helper function to test if input is an integer:
  check.integer <- function(N){
    !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE)) # Looks for whole numbers only using regex
  }
  
  # Set the seed for reproducibility:
  if (!is.null(seed)){
    set.seed(seed)}
  
  # Input morphism:
  if (is.character(Sites)){ # If the input for sites are site prefixes
    NSites = length(Sites)  # Create a variable that tracks the number of prefixes entered
  }
  else {
    NSites = Sites
  }
  
  if (is.character(RRatio)){ # If the input is a character ratio
    nums = as.integer(unlist(str_split(string = RRatio, pattern = ":"))) # Extract the chars and turn into integers
    RRatio = nums[1]/nums[2] # Turn the above ratio into a fraction for later use
  }
  
  if (is.character(NSubjects)){ # Test if the number of subjects is a character value
    NSubjects = as.integer(NSubjects) # If it is: turn the input into an integer
  }
  
  if (is.character(BlockSize)){ # Test if the block size is a character value
    BlockSize = as.integer(BlockSize) # If it is: turn the input into an integer
  }
  
  NBlocks = NSubjects/BlockSize # Compute the number of blocks
  
  ### Error-checking: ###
  # Null value for sites:
  if (is.null(Sites) == TRUE){
    stop("Please enter either an integer or site prefixes")
  }
  
  # Unique site codes:
  test1 = any(duplicated(Sites))
  if (test1 == TRUE){
    stop("Please enter unique site codes")
  }
  
  # Non-positive number of sites:
  if (is.numeric(Sites) && Sites <= 0){
    stop("Please enter a valid number of sites (>=1)")
  }
  
  # Non-positive number of subjects:
  test2 <- any(NSubjects <= 0)
  if (test2 == TRUE){
    stop("Please enter a positive integer for the number of subjects per site")
  }
  
  # Improper Randomization Ratio:
  "  test3 <- check.integer(NSubjects*(RRatio/(RRatio+1)))
  if (test3 == FALSE){
    stop('The following must be an integer: number of subjects * (RRatio/RRatio+1). Check for yourself.')
  }
  "
  if (RRatio <= 0){
    stop("The randomization ratio must be greater than 0")
  }
  
  # Improper Block Size:
  if (BlockSize>0 && NSubjects%%BlockSize != 0){
    stop("The block size must be a multiple of the number of subjects")
  }
  
  if (BlockSize%%RRatio >= 1){
    stop("Block Size and Ratio are Incompatible: cannot assign subjects with given ratio")
  }
  
  
  # Designing the schema:
  # If the input to sites is a NUMERIC number
  
  if (is.numeric(Sites) == TRUE & Sites >= 1 & Sites <= 26){ # Test if a numeric number
    matt = c() # Start with an empty vector
    final = matrix(NA, nrow = Sites*NSubjects, ncol = 1) # Add an empty matrix
    for (letter in LETTERS){ # For each letter in the uppercase(Alphabet):
      result = rep(letter, times = 3) # Repeat each letter 3 times
      result = paste(result, collapse = "") # Combine the result into 1 string
      matt[letter] = result # Assigns the result to the previously empty vector "matt"
    } # Now that we have assigned the letters to the vector "matt"
    matt = data.frame(t(matt)) # Transpose the vector
    matt = matt %>% # For each site code in your input:
      uncount(NSubjects) # Duplicate the site code NSubjects number of times
    matt = matt[1:NSubjects, 1:Sites] # Removing redundant codes
    rownames(matt) = NULL # Aesthetics
  }
  else if (is.numeric(Sites) == TRUE & Sites > 26 & Sites <= 50){ # Test if a numeric number
    matt = c() # Start with an empty vector
    final = matrix(NA, nrow = Sites*NSubjects, ncol = 1) # Add an empty matrix
    for (letter in LETTERS){ # For each letter in the uppercase(Alphabet):
      result = rep(letter, times = 3) # Repeat each letter 3 times
      result = paste(result, collapse = "") # Combine the result into 1 string
      matt[letter] = result # Assigns the result to the previously empty vector "matt"
    } # Now that we have assigned the letters to the vector "matt"
    matt = data.frame(t(matt)) # Transpose the vector
    matt = matt %>% # For each site code in your input:
      uncount(NSubjects) # Duplicate the site code NSubjects number of times
    rownames(matt) = NULL
    gmatt = c() # Start with an empty vector
    for (symbol in greeks){
      result = rep(symbol, times=3)
      result = paste(result, collapse="")
      gmatt[symbol] = result
    }
    gmatt = data.frame(t(gmatt))
    gmatt = gmatt %>% uncount(NSubjects)
    colnames(gmatt) = NULL
    rownames(gmatt) = NULL
    gmatt = gmatt[1:NSubjects, 1:(Sites-26)]
    matt = cbind(matt, gmatt)
  }
  # If the input to sites is a CHARACTER vector
  else if (is.vector(Sites) == TRUE){
    matt = matrix(NA, nrow = length(Sites), ncol = NSubjects)  # Start with an empty data matrix
    dimnames(matt) = list(Sites) # Must include or won't run
    final = matrix(NA, nrow = length(Sites)*NSubjects, ncol = 1) # Start with an empty data matrix
    for (i in Sites){ # For each site:
      for (j in NSubjects){ # And for each subject
        matt[i, ] = rep(i, times = NSubjects) # Row-wise assignment of letters
      }
    }
    matt = as.data.frame(t(matt)) # Transpose and turn into a dataframe for reasonable viewing
    rownames(matt) = NULL # Replace unnecessary rownames
    matt = data.frame(matt)
  }
  # Adding the numbers to the end via a simple if-then-else statement
  if (is.numeric(Sites) && Sites == 1){
    matt = ifelse(row_number(matt) < 10, # If the row-number is less than 10:
                  yes = paste(matt, "0", row_number(matt), sep = ""), # Assign a 0 between letter and number
                  no = paste(matt, row_number(matt), sep = "")) # Otherwise: assign no space between letters and number
    row.names(matt) = NULL
    matt = data.frame(matt)
  }
  else { # For all other cases:
    for (column in matt){ # For each column in the "matt" matrix
      matt[column, ] = if_else(row_number(column) < 10, # Same instructions as above
                               true = paste(column, "0", row_number(column), sep = ""),
                               false = paste(column, row_number(column), sep = ""))
    }
    row.names(matt) = NULL
    matt = matt[NSubjects+1:(nrow(matt)-NSubjects), 1] # Only keep the first column and a subset of rows
  }
  
  # Assigning the Ts and Cs:
  # Start with assigning the blocks:
  vector <- c() # Initiate an empty vector
  for (i in seq(1:(NSites*(NSubjects/BlockSize)))){ # For each block
    for (j in 1:BlockSize){ # Print "BlockSize" times
      vector = append(vector, i)} # Append the results to the previously empty vector
  }
  Ts = RRatio/(RRatio+1) # Compute the proportion of Ts needed
  Cs = 1-Ts # Compute the proportion of Cs needed
  TorC = block_ra(blocks = vector, # Using the first column:
                  conditions = c("T", "C"), # 2 groups are the Ts and Cs
                  prob_each = c(Ts, Cs)) # The prob. that T or C appears
  TLC = data.frame(TorC)
  
  # Shuffle the data randomly:
  "matt = sample(matt)"
  
  # Turn the data matrix into a data.frame:
  matt = as.data.frame(matt)
  
  "# Randomizing the order of the T's and C's for each site:
  ls <- c() # Start with an empty vector
  count <- 1 # Remember, R starts at 1, not 0
  repeat { # Using the repeat function
    ls = append(ls, sample_frac(TLC, 1))
    # Shuffle the T's and C's in the block and append to the once empty vector
    count = count + 1 # Increment the count for each run of the loop by 1
    if (count > NSites){ # Stop when the count reaches the number of sites
      break # Use a break statement to exit the repeat loop, else trouble
    }
  }
  
  TLC <- data.frame(unlist(ls))" # Turn the list of lists into a single object
  
  result <- data.frame(c(matt, TLC)) # Combine TLC and matt into one dataframe
  
  # Use the "unite" function to concatenate both columns into a single column
  final <- result %>% # using the pipe operator from the dplyr syntax
    unite("Code", matt:TorC, sep = "")
  
  # Turn into a dataframe:
  final <- as.data.frame(final)
  
  # Extracting different parts of the codes for easier reading:
  final["Code"] = final[, 1]
  final["Site"] =  substr(final[, 1], 1, 3) # extract first three letters from code
  final["Subject"] =  gsub("[a-zA-Z]+", "", final[, 1]) # remove letters with regex
  final["Group"] = substr(final[, 1], nchar(final[, 1]), nchar(final[, 1]))
  
  # Remove the repetitive column:
  final = final %>% select(Code, Site, Subject, Group) # Using dplyr's built-in pipe (%>%) operator
  
  # Return the end result:
  return(final)
}

################################################################################
###                         Creating the User Interface                      ###
################################################################################

# Define UI for the schema viewer app --->
ui <- fluidPage(

    # Application theme --->
    theme = shinytheme("cerulean"),

    # App title --->
    titlePanel(title = "Project: Randomization Schema"),

    # Sidebar layout with input and output definitions --->
    sidebarLayout(position = "left",
                  fluid = TRUE, # For mobile use

                  # Sidebar panel for inputs --->
                  sidebarPanel(

                      # Input: Text for providing a caption --->
                      # Note: Changes made to the caption in the textInput control
                      # are updated in the output area immediately as you type
                      textInput(inputId = "caption",
                                label = "Title:",
                                value = "Randomization Schema"),

                      selectInput(inputId = "SiteType",
                                  label = "Please select the type of site input you would like:",
                                  choices = c(Numbers = "numeric",
                                              Letters = "character")),
                      conditionalPanel(
                          condition = "input.SiteType == 'numeric'",
                          # Input: Numeric entry for choosing the number of sites --->
                          numericInput(inputId = "NumericSites",
                                       label = "Please enter the number of sites you would like:",
                                       value = 1,
                                       min = 1,
                                       max = 50,
                                       step = 1)),

                      conditionalPanel(
                          condition = "input.SiteType == 'character'",
                          # Multiple input for sites:
                          multiInput(
                              inputId = "CharSites",
                              label = "Please select the site codes you would like:",
                              choices = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG", "HHH",
                                          "III", "JJJ", "KKK", "LLL", "MMM", "NNN", "OOO", "PPP",
                                          "QQQ", "RRR", "SSS", "TTT", "UUU", "VVV", "WWW", "XXX",
                                          "YYY", "ZZZ"),
                              selected = "AAA", width = "350px")),

                      # Input: Numeric entry for choosing the number of subjects per site --->
                      numericInput(inputId = "NSubjects",
                                   label = "Please input the number of subjects at each site:",
                                   value = 10),

                      # Input: Selector for choosing the randomization ratio --->
                      numericInput(inputId = "RRatio",
                                   label = "Please choose a randomization ratio:",
                                   value = 1),

                      # Input: Numeric entry for choosing the block size --->
                      numericInput(inputId = "BlockSize",
                                   label = "Please input the number of subjects per block:",
                                   value = 1),

                      switchInput(inputId = "Copy",
                                  label = "Reproducible",
                                  labelWidth = "80px",
                                  value = FALSE),
                      
                      conditionalPanel(
                        condition = "input.Copy == TRUE",
                        numericInput(inputId = "SeedNum",
                                     label = "Seed:",
                                     value = 123)),
                      
                      # Ownership
                      tags$strong("Author: Matt Quinn",
                                  tags$br(),
                                  "Class: STA 635",
                                  tags$br(),
                                  "Date: 10/1/2020"),

                      br(),

                      # Instructions
                      actionButton(
                          inputId = "Instructions",
                          label = "Instructions",
                          icon = icon("cog"))

                  ),

                  # Main panel for displaying outputs --->
                  mainPanel(

                      # Output: Formatted text for caption --->
                      h2(textOutput(outputId = "caption",
                                    container = span)),

                      # Output: HTML table with requested number of observations --->
                      DT::dataTableOutput("table"),

                      # Sources:
                      # Written in HTML format
                      # a() is for hyperlinks

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
                        href = "https://bookdown.org/rdpeng/RProgDA/error-handling-and-generation.html"),
                      br(),
                      a("5. FOR Loops in R",
                        href = "https://www.datamentor.io/r-programming/for-loop/"),
                      br(),
                      a("6. Substrings in R",
                        href = "https://statisticsglobe.com/r-extract-first-or-last-n-characters-from-string"),
                      br(),
                      a("7. rep() in R",
                        href = "https://astrostatistics.psu.edu/su07/R/html/base/html/rep.html"),
                      br(),
                      a("8. Boolean && Operator",
                        href = "https://stackoverflow.com/questions/6558921/boolean-operators-and?noredirect=1&lq=1"),
                      br(),
                      a("9. Multi User Input in Shiny",
                        href = "https://rdrr.io/cran/shinyWidgets/man/multiInput.html"),
                      br(),
                      a("10. Making an R Package",
                        href = "https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html"),
                      br(),
                      a("11. Negation in R",
                        href = "https://stackoverflow.com/questions/38351820/negation-of-in-in-r")
                  )
    )
)

################################################################################
###                           Creating the Server Side                       ###
################################################################################

# Define server logic to summarize and view selected data set ----
server <- function(input, output, session){

    # Create caption ----
    # The output$caption is computed based on a reactive expression
    # that returns input$caption. When the user changes the
    # "caption" field:
    #
    # 1. This function is automatically called to recompute the output
    # 2. New caption is pushed back to the browser for re-display
    #
    # Note that because the data-oriented reactive expressions
    # below don't depend on input$caption, those expressions are
    # NOT called when input$caption changes
    output$caption <- renderText({
        input$caption
    })

    # Using the above inputs, put into a reactive environment the
    # elements that you created above in the user interface
    FINAL <- reactive({schema(
        if (input$SiteType == "numeric"){
          Sites = input$NumericSites}
        else {
          Sites = input$CharSites
        },
        NSubjects = input$NSubjects,
        RRatio = input$RRatio,
        BlockSize = input$BlockSize,
        if (input$Copy == TRUE){
          seed = input$SeedNum}
        else {
          seed = NULL})})

    # Output the schema:
    output$table <- DT::renderDataTable({
        FINAL()}
    )

    # Instructions via HTML:
    observeEvent(input$Instructions, {
        show_alert(
            title = "Instructions",
            text =  p("1. Choose the sites you would like",
                      br(),
                      "2. Input the number of subjects per site",
                      br(),
                      "3. Input the randomization ratio in the form of a floating-point number",
                      br(),
                      "4. Input the number of subjects per block",
                      br(),
                      "5. You can reproduce your results if you use the handy switch",
                      br(),
                      "Note: The program will automatically update after each change you make")
        )
    })

}

"Wrapping it all together:"
shinyApp(ui = ui, server = server)


