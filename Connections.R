# Fruit: apple, orange, banana, cherry Colors: red, blue, green, yellow

library(shiny)

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Custom Connections Game"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Setup the Game"),
      textAreaInput(
        "categories_words",
        "Enter your categories and words (one category per line, words comma-separated):",
        placeholder = "Example:\nFruit: apple, orange, banana, cherry\nColors: red, blue, green, yellow"
      ),
      actionButton("submit", "Generate Game"),
      br(),
      br(),
      h4("Instructions:"),
      p("- Enter categories and words in the format 'Category: word1, word2, word3'."),
      p("- Press 'Generate Game' to create the puzzle."),
      p("- Select 4 words to form a group and check your solution."),
      p("- Words can only belong to one group."),
      p("- Check your solution when ready!"),
      actionButton("clear_selection", "Clear All Selections")
    ),
    
    mainPanel(
      h3("Solve the Puzzle"),
      uiOutput("game_board"),
      textInput("category_guess", "Enter the name of the category for your selected group:"),
      actionButton("check_solution", "Check Solution"),
      br(),
      textOutput("feedback"),
      br(),
      h3("Completed Groups"),
      uiOutput("completed_groups")
    )
  )
)

# Define the Shiny server
server <- function(input, output, session) {
  words <- reactiveVal(NULL)
  solutions <- reactiveVal(NULL)
  selected_words <- reactiveVal(c())
  completed <- reactiveVal(list())
  incorrect_guesses <- reactiveVal(0)
  pending_group <- reactiveVal(NULL)
  retry_attempts <- reactiveVal(0)
  
  observeEvent(input$submit, {
    # Parse user input into categories and words
    input_text <- input$categories_words
    if (is.null(input_text) || input_text == "") {
      showNotification("Please enter categories and words.", type = "error")
      return()
    }
    
    categories <- strsplit(input_text, "\n")[[1]]
    categories <- trimws(categories)
    
    all_words <- c()
    solution <- list()
    
    for (category in categories) {
      split_line <- unlist(strsplit(category, ":"))
      if (length(split_line) != 2) {
        showNotification("Invalid format. Each line should be 'Category: word1, word2, word3'.", type = "error")
        return()
      }
      
      cat_name <- trimws(split_line[1])
      cat_words <- unlist(strsplit(split_line[2], ","))
      cat_words <- trimws(cat_words)
      
      all_words <- c(all_words, cat_words)
      solution[[cat_name]] <- cat_words
    }
    
    # Shuffle words
    all_words <- sample(all_words)
    
    words(all_words)
    solutions(solution)
    selected_words(c())
    completed(list())
    incorrect_guesses(0)
    pending_group(NULL)
    retry_attempts(0)
    
    showNotification("Game generated! Select words to form groups.", type = "message")
  })
  
  observeEvent(input$clear_selection, {
    updateCheckboxGroupInput(session, "word_selection", selected = character(0))
  })
  
  output$game_board <- renderUI({
    req(words())
    
    tagList(
      checkboxGroupInput(
        "word_selection",
        "Select words to form a group:",
        choices = words(),
        selected = selected_words()
      )
    )
  })
  
  output$completed_groups <- renderUI({
    req(completed())
    completed_groups <- completed()
    if (length(completed_groups) == 0) return(NULL)
    
    tagList(
      lapply(names(completed_groups), function(cat) {
        words <- completed_groups[[cat]]
        tags$div(
          tags$h4(cat),
          tags$p(paste(words, collapse = ", "))
        )
      })
    )
  })
  
  observeEvent(input$check_solution, {
    req(words(), solutions(), input$word_selection)
    
    # Get user-selected words
    selected <- input$word_selection
    if (length(selected) != 4) {
      output$feedback <- renderText("Please select exactly 4 words to form a group.")
      return()
    }
    
    # Check if the selected words match any group in the solutions
    sol <- solutions()
    correct_group <- NULL
    correct_count <- 0
    
    for (cat_name in names(sol)) {
      correct_count <- sum(selected %in% sol[[cat_name]])
      if (correct_count == 4) {
        correct_group <- cat_name
        break
      }
    }
    
    if (!is.null(correct_group)) {
      if (!is.null(pending_group())) {
        if (tolower(input$category_guess) == tolower(correct_group)) {
          # Correct category name guessed
          remaining_words <- setdiff(words(), selected)
          words(remaining_words)
          
          completed_groups <- completed()
          completed_groups[[correct_group]] <- selected
          completed(completed_groups)
          
          pending_group(NULL)
          retry_attempts(0)
          output$feedback <- renderText(paste("Correct! Group", correct_group, "added to completed list."))
          
          if (length(remaining_words) == 0) {
            output$feedback <- renderText("Congratulations, you have won the game... this time!")
          }
        } else {
          attempts <- retry_attempts() + 1
          retry_attempts(attempts)
          
          if (attempts >= 2) {
            # Move on after 2 attempts
            remaining_words <- setdiff(words(), selected)
            words(remaining_words)
            
            completed_groups <- completed()
            completed_groups[[correct_group]] <- selected
            completed(completed_groups)
            
            pending_group(NULL)
            retry_attempts(0)
            output$feedback <- renderText(paste("The words are correct, but the category name is incorrect. The correct category was:", correct_group))
            
            if (length(remaining_words) == 0) {
              output$feedback <- renderText("Congratulations, you have won the game... this time!")
            }
          } else {
            output$feedback <- renderText("The words are correct, but the category name is incorrect. Try again!")
          }
        }
      } else {
        pending_group(list(words = selected, correct_category = correct_group))
        output$feedback <- renderText("The words are correct, but the category name is incorrect. Enter the correct category name.")
      }
    } else {
      if (correct_count == 3) {
        output$feedback <- renderText("You have 3 out of 4 words correct. Try again!")
      } else {
        incorrect_guesses(incorrect_guesses() + 1)
        output$feedback <- renderText("Incorrect group. Try again.")
      }
    }
    
    # Check if the player has exceeded 10 incorrect guesses
    if (incorrect_guesses() >= 10) {
      output$feedback <- renderText("Better luck next time, loser!")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
