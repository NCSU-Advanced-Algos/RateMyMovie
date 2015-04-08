#source('RateMyMovie/R/TermDoc2D.R')
#source('RateMyMovie/R/RateMyMovie.R')

ask <- function() {
  invisible(readline("Hit <Return>/<Enter> for next command: "))
}

about <- function() {
  cat("Predictions based on Term Document")
  cat("The model classifies words into good and bad categores based on training 
          set and stores the details in term document.\n\n")
  cat("Further predictions are made by matching the words in the review with the
          term document. Class having maximum number of words is considered as 
          class of review.\n\n")
}


about()
ask()