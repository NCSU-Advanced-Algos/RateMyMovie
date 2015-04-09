
library('e1071')

#Method to train the learner.
#Input - Set words and their classification
#Output - Naive Bayes Model
trainNB <- function (words) {
  model = naiveBayes(words$words, words$class)
  model$attributes = colnames(model$tables[[1]])
  return(model)
}

min = 0.00000001

#Method to classify a review as good or bad.
#Each sentence is first preprocessed and split into words. 
#Then the probability of each word is estimated and then we
#compute the probabibilty of the entire sentence
#Input: 
#  model - The naive bayes model used to predict the sentence's class
#  sentence - The review which has to be classified
#Output - good / bad (classification of the sentence)
predictReview <- function (model, sentence) {
  words = preprocess(sentence)
  goods = c()
  bads = c()
  tabs = model$tables[[1]]
  for (word in words) {
    ind = index(model, word)
    if (ind > 0) {
      goods = append(goods, tabs[1, ind])
      bads = append(bads, tabs[2, ind])
    }
  }
  goods = goods+min
  bads = bads+min
  P_good = prod(goods)*model$apriori[1]
  P_bad = prod(bads)*model$apriori[2]
  if (P_good > P_bad) {
    return("good")
  } else {
    return("bad")
  }
}


#Method to predict a movie as good or bad.
#The method internally predicts each review as good or bad
#and returns the total good and bad reviews.
#Input - Set words and their classification
#Output - Naive Bayes Model
predictMovieNB <- function(model, movieName, showContingency=FALSE) {
  reviews = getReviews(movieName = movieName)
  if(length(reviews)==0) {
    return(NULL)
  }
  pred= c()
  for (review in reviews$reviews) {
    pred = append(pred, as.character(predictReview(model, review)))
  }
  if (showContingency) {
    print(table(reviews$rating))  
  }
  tab = table(pred)
  goodCnt = tab[names(tab)=="good"]
  if (length(goodCnt)==0) {
    goodCnt = 0
  }
  badCnt = tab[names(tab)=="bad"] 
  if (length(badCnt)==0) {
    badCnt = 0
  }
  if ( goodCnt > badCnt) {
    rev="good"
  } else {
    rev="bad"    
  }
  print(paste("****",movieName,"****"))
  print(paste(goodCnt,"out of",length(pred),"say the movie is good"))
  print(paste(badCnt,"out of",length(pred),"say the movie is bad"))
  print(paste("Rotten Tomatoes score of", movieName,"is", reviews[1,]$score,"out of 100"))
  cat("\n")
  retFrame = data.frame(act=reviews[1,]$rating, pred=rev)
  return(retFrame)
}


#Utility method to find the index of a word
#in the naive bayes model.
#Input:
#  model - Naive Bayes Model
#  word - the word to be predicted
#Ouptut:
#  index of the word in the model
index <- function(model, word) {
  charWord = as.character(word)
  cols = model$attributes
  ind = which(cols==charWord)
  if (length(ind) > 0) {
    return(ind)
  } else {
    return(0)
  }
}



#Test a list of movies from a file. Predicts 
#Input:
#  model - Naive Bayes Model used to predict
#  moviesFileName - Path of file which contains the list of movies.
#Output:
#  Prints out the statistics for the movies. Like precision, recall, f1 etc
testNB <-function(model, moviesFileName="RateMyMovie/R/testMovies.txt") {
  moviesFile=file(moviesFileName, open = "r")
  movies = readLines(moviesFile)
  close(moviesFile)
  TP=0;TN=0;FP=0;FN=0
  for (movie in movies) {
    ret = predictMovieNB(model, movie)
    Sys.sleep(0.5)
    if (is.null(ret)) {
      next
    }
    if ((ret$act=="good") && (ret$pred=="good")) {
      TP = TP + 1
    } else if ((ret$act=="good") && (ret$pred=="bad")) {
      FN = FN + 1
    } else if ((ret$act=="bad") && (ret$pred=="good")) {
      FP = FP + 1
    } else {
      TN = TN + 1
    }
  }
  modelEval(TP, TN, FP, FN)
}