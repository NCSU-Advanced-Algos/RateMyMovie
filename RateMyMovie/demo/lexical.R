source('RateMyMovie/R/LexBased.R')
source('RateMyMovie/R/RateMyMovie.R')

about <- function() {
  cat("***** Rate My Movie *****\n")
  cat("-by\n")
  cat("Shubham Bhawsinka (sbhawsi)\n")
  cat("Kumar Utsav (kutsav)\n")
  cat("Kapil Somani (kmsomani)\n")
  cat("George V Mathew (george2)\n")
  cat("Desc : This is a Lexical model which depends on the existing dictionary to analyse text and predict whether the review is good or bad.\n")
}

ask <- function() {
  invisible(readline("Hit <Return>/<Enter> for next command: "))
}

demoStream <- function() {  
  cat("A sample list of movies classified as good or bad are stored in 'movies.txt'. This can be edited or a new
file can be passed as an argument to the method streamTrainingRows(). The streamed comments are stored in 
'train.csv' which is the default file along with the movie classified as good/bad based on the tomato meter 
score obtained from the response JSON while streaming from Rotten Tomatoes.

We first retrieve the movie id based on the name of the movie. For that movieId we retrieve the top 25 comments and use it as our training
data with the class variable as the classification of the movie(as per its rating). We cannot retrieve the
classification of the comment as the API restricts itself to only categorize movies. This is a drawback in
our data collection technique as our data source is not perfect. 

This might take around 0.5*number of movies seconds as the api restricts to 4 calls per second. Hence we needed to insert a sleep while streaming successive
movies.Refer 'http://developer.rottentomatoes.com' for a more detailed information on streaming the movie 
comments.\n\n")
  cat("Running streamTrainingRows('RateMyMovie/R/movies.txt', 'RateMyMovie/R/train.csv')\n\n")
  streamTrainingRows()
}

predictModel <- function() {
  
  cat("In the demo we use a bayes classifier using the 'sentiment' package analyser. So in this case our bayes classifier internally will have 2 gaussians. One gaussian for all the
words classified as good and one for all the words classified as bad. We use the 'sentiment' model to build our 
classifier in this case. \n\n")
  ask()
  
  cat("\nTo test a Movie when we have a model ready we use the api to fetch the top 25 reviews for that movie and we perform
preprocessing on each of the comment to extract each word and then predict if the preprocessed comment is 
categorized as good or bad.  We now predict the review of 'The Dark Knight' \n\n")
  cat("Running predictMovie('The Dark Knight', showContingency=TRUE)\n")
  predictMovie('The Dark Knight', showContingency=TRUE)
  
  ask()
  cat("\n To predict all movies from a file the following command can be executed. It accepts the model and the file name as
the arguments. \n\n")
  cat("Running testNB(nbModel, 'RateMyMovie/R/testMovies.txt')\n")
  LexicalModel('RateMyMovie/R/testMovies.txt')
  
  ask()
  cat("\nFor Streaming movies from rotten tomatoes and predicting review on the fly use the following command. It accepts the model and predict function as 
the arguments. We are not running it as it triggers the stream. Do run it once you build the model.\n\n")
  
  cat("streamMovies(modelName, predictMovieNB)")
}

about()
ask()
demoStream()
ask()
predictModel()