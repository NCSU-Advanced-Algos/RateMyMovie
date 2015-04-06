source('RateMyMovie/R/NBayes.R')
source('RateMyMovie/R/RateMyMovie.R')

about <- function() {
  cat("***** Rate My Movie *****\n")
  cat("-by\n")
  cat("Shubham Bhawsinka (sbhawsi)\n")
  cat("Kumar Utsav (kutsav)\n")
  cat("Kapil Somani (kmsomani)\n")
  cat("George V Mathew (george2)\n")
  cat("Desc : <Write Description Here>\n")
}

ask <- function() {
  invisible(readline("Hit <Return>/<Enter> for next command: "))
}

demoStream <- function() {  
  cat("A sample list of movies classified as good or bad are stored in 'movies.txt'. This can be edited or a new
file can be passed as an argument to the method streamTrainingRows(). The streamed comments are stored in 
'train.csv' which is the default file along with the movie classified as good/bad based on the tomatometer 
score obtained from the response JSON while streaming from Rotten Tomatoes. We first retrieve the movie id 
based on the name of the movie. For that movieId we retrieve the top 25 comments and use it as our training
data with the class variable as the classification of the movie(as per its rating). We cannot retrieve the
classification of the comment as the API restricts itself to only categorize movies. This is a drawback in
our data collection technique as our data source is not perfect. This might take around 0.5*number of movies
seconds as the api restricts to 4 calls per second. Hence we needed to insert a sleep while streaming successive
movies.Refer 'http://developer.rottentomatoes.com' for a more detailed information on streaming the movie 
comments.\n\n")
  cat("Running streamTrainingRows('RateMyMovie/R/movies.txt', 'RateMyMovie/R/train.csv')\n\n")
  streamTrainingRows()
}

demoTrainAndTest <- function() {
  cat("Once we parse fetch comments for the movies from the api, we perform preprocessing on each comment from 'train.csv'. 
We use the 'tm' library for this purpose. The steps involved in preprocessing are converting to lower case, 
remove punctuation symbols, remove numbers and remove stop words in that order respectively. We also tried 
stemming but since it yielded poor results we decided to remove it as our classifier was performing better 
without using it. So each word from the preprocessed comment is inserted into train data with the class variable
as the rating of the comment from train.csv.\n\n")
  cat("Running makeTrainSet('train.csv')\n\n")
  trainSet <- makeTrainSet()
  
  ask()
  
  cat("Once the trainset is built we can send it through any classifier to build a model. In the demo we use a naive bayes
classifier. So in this case our naive bayes classifier internally will have 2 gaussians. One gaussian for all the
words classified as good and one for all the words classified as bad. We use the 'e1071' model to build our 
classifier in this case. \n\n")
  cat("Running trainNB(trainSet)\n\n")
  nbModel <- trainNB(trainSet)
  
  ask()
  
  cat("\nTo test a movie when we have a model ready we use the api to fetch the top 25 reviews for that movie and we perform
preprocessing on each of the comment to extract each word and then predict if the preprocessed comment is 
categorized as good or bad. Since we built the classifier on the word, we only get the posterior probabibilty of
the class given word, but since we are interested in the likelihood probability of the word we access the tables
structure of the model estimate the likelihood of each word given class, multiply them and then the result with
the apriori probability of the class. Whichever class has the greater probability, we say that the comment belongs
to that class. If any word has 0 probability we set it as 0.00000001 to avoid the product being 0. We now predict
the review of 'The Dark Knight' \n\n")
  cat("Running predictMovie(nbModel,'The Dark Knight', showContingency=TRUE)\n")
  predictMovieNB(nbModel,'The Dark Knight', showContingency=TRUE)
  
  ask()
  cat("\n To predic all movies from a file the following command can be executed. It accepts the model and the file name as
the arguments. \n\n")
  cat("Running testNB(nbModel, 'RateMyMovie/R/testMovies.txt')\n")
  testNB(nbModel, 'RateMyMovie/R/testMovies.txt')
  
  ask()
  cat("\nFor Streaming movies from rotten tomatoes and predicting review on the fly use the following command. It accepts the model and predict function as 
the arguments. We are not running it as it triggers the stream. Do run it once you build the model.\n\n")
  
  cat("streamMovies(modelName, predictMovieNB)")
}

about()
ask()
demoStream()
ask()
demoTrainAndTest()