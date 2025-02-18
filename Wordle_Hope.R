load_dictionary <- function(filename){
  if(filename== "valid"){
    dictionary <- readLines("valid.txt")
  } else if (filename== "solution") {
    dictionary <- readLines("solution.txt")
  } else {stop("Wrong File")}
  dictionary <- dictionary[dictionary != ""]
  return(dictionary)
}

valid_list <- load_dictionary("valid")
solution_list <- load_dictionary("solution")

solution_list <- intersect(solution_list, valid_list)

pick_solution <- function(solution_list){
  five_letter <- solution_list[nchar(solution_list)==5]
  choice <- sample(five_letter, 1)
  sep_letters <- strsplit(choice, split = "") [[1]]  
  return(sep_letters)
}

solution <- pick_solution(solution_list)

play_wordle <- function(solution, valid_list, num_guesses=6){
  cat("You have", num_guesses, "chances to guess a 5-letter word.\n")
  cat("After each guess, you will receive feedback:\n")
  cat("- Green: Correct letter and correct position.\n")
  cat("- Yellow: Correct letter but wrong position.\n")
  cat("- Grey: Incorrect letter.\n\n")
  
  remaining_letters <- LETTERS
  guess_history <- list()
  
  evaluate_guess <- function(guess, solution){
    feedback <- character(5)
    for(i in 1:5){
      if(guess[i] == solution[i]){
        feedback[i] <- "green"
      } else if(guess[i] %in% solution){
        feedback[i] <- "yellow"
      } else{
        feedback[i] <- "grey"
      }
    }
    return(feedback)
  }
  
  for(guess_num in 1:num_guesses){
    cat("\nAttempt", guess_num, "of", num_guesses, "\n")
    cat("Remaining letters:", paste(remaining_letters, collapse = " "), "\n")
    guess <- readline(prompt = "Enter your guess:")
    guess <- strsplit(guess, split = "")[[1]]
    
    if(length(guess) !=5 || !(paste(guess, collapse = "") %in% valid_list)){
      cat("Invalid guess! Please enter a 5-letter word.\n")
      next
    }
    
    remaining_letters <-setdiff(remaining_letters, guess)
    
    feedback <- evaluate_guess(guess, solution)
    cat("Feedback: ", paste(feedback, collapse = " "), "\n")
    
    guess_history[[guess_num]] <-list(guess= paste(guess, collapse = ""), feedback=feedback)
    
    if (paste(guess, collapse = "") == paste(solution, collapse = "")){
      cat("Congratulations! You Won!\n")
      for(i in 1:guess_num){
        cat("Guess", i, ":", guess_history[[i]]$guess, "Feedback:", paste(guess_history[[i]]$feedback, collapse = " "), "\n")
      }
      return(invisible())
    }
  }
  cat("\nSorry, you are out of guesses. The word was:", paste(solution, collapse = ""), "\n")
  cat("Your guesses and feedback:\n")
  for(i in 1:num_guesses){
    cat("Guess", i, ":", guess_history[[i]]$guess, "Feedback:", paste(guess_history[[i]]$feedback, collapse = " "), "\n")
  }
}
