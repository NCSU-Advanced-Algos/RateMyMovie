source('RateMyMovie/R/TermDoc2D.R')
source('RateMyMovie/R/RateMyMovie.R')

ask <- function() {
  invisible(readline("Hit <Return>/<Enter> for next command: "))
  cat("\014")
}

about <- function() {
  cat("
      Predictions based on Term Document.\n")
  cat("
      The model classifies words into good and bad categores based on training 
        set and stores the details in term document.\n\n")
  cat("
      Further predictions are made by matching the words in the review with the 
        term document. Class having maximum number of words is considered as 
        class of review. Also, model is updated based on feedback obtained. New words 
        are added and existing word counts are updated\n\n\n")
}

trainTermDocumentDemo <- function() {
  cat("
      We first need to train our model. This is done by keeping count of number of times 
        word appears in 'good' or 'bad' reviews. Later, we assign class to word based on 
        class in which word appeared maximum number of times.\n\n")
  cat("
      Creating term document from available training set train.csv and later creating 
        term document from training data\n")
  cat("
      Executing *** traindata = makeTrainSet('RateMyMovie/R/train.csv') *** \n
      Executing *** td = generateTermDocument(traindata) *** \n
      (..generating termdocument.. wait for control to return)\n\n")
  
  traindata = makeTrainSet('RateMyMovie/R/train.csv')
  td = generateTermDocument(traindata)

  ask()
  
  cat("
      Created term document with ",dim(td)[1], "entries.
      Few entries from term document:\n\n")
  print(head(td))
  cat("\n\n\n")
  
  ask()
  
  cat("
      Testing data on movies...
      Fetching comments for 'Dark Knight Rises'
      Executing *** comments = getReviews(movieName='Dark Knight Rises')*** ")
  comments = getReviews(movieName='Dark Knight Rises')
  cat("
      Classifying comments based on term document
      Executing *** output = makePredicts(td, comments$reviews) *** \n")
  output = makePredicts(td, comments$reviews)
  cat("
      Predictions will be saved in output$prediction and updated model 
        from feedback can be collected from output$model.
      Updating our model for future queries.
      Executing *** td = $output$model *** \n")
  cat("
      Updated model has ",dim(output$model)[1]," entries\n\n")
  goodReviews = table(output$prediction)["good"]
  badReviews = table(output$prediction)["bad"]
  cat("
      Based on counts in table$prediction, following conclusions are made.\n")
  cat("
      ",goodReviews," out of ",goodReviews + badReviews," gave 'good' review for movie.\n\n")
}

cat("\014")

about()
ask()
trainTermDocumentDemo()


ask()