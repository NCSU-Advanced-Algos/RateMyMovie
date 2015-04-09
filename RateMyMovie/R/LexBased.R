library('RCurl')
library('jsonlite')
library('tm')
library('SnowballC')
library('sentiment')

#Preprocessing the Reviews from the stream data.
preprocesser<-function(sents, stpwrds=stopwords('english')){
  corp=Corpus(VectorSource(sents))
  corp=tm_map(corp, tolower) #To lower case
  corp=tm_map(corp, removePunctuation) # Remove Punctuation
  corp=tm_map(corp, removeNumbers)  # Remove Numbers
  corp=tm_map(corp, removeWords,stpwrds) # Remove Stopwords
  dataframe<-data.frame(text=unlist(sapply(corp, '[')), stringsAsFactors=F)
  return(dataframe)
}

#Prediction of Movie 
predictMovie<- function(movieName, showContingency=FALSE) {
  reviews = getReviews(movieName = movieName)
  if(length(reviews)==0) {
    return(NULL)
  }
  reviews$reviews <- preprocesser(reviews$reviews)
  #Classification of Polarity into 'good' and 'bad' using 'sentiment package' of Stanford NLP
  prediction<-classify_polarity(reviews$reviews,algorithm="bayes",verbose=TRUE)
  prediction<-as.data.frame(prediction)
  a<-c()
  #classification on the basis of good vs ba ratio
  for(word in prediction$"POS/NEG"){
    word<-as.numeric(as.character(word))
    ifelse(word > 1, a <- append(a,"good"), a <- append(a,"bad"))   
  }
  
  if (showContingency) {
    print(table(reviews$rating))  
  }
  tab = table(a)
  # Count of Good reviews
  goodCnt = tab[names(tab)=="good"]
  if (length(goodCnt)==0) {
    goodCnt = 0
  }
  # Count of Bad Reviews
  badCnt = tab[names(tab)=="bad"] 
  if (length(badCnt)==0) {
    badCnt = 0
  }
  # Classification in to good or bad
  if ( goodCnt > badCnt) {
    rev="good"
  } else {
    rev="bad"    
  }
  print(paste("****",movieName,"****"))
  print(paste(goodCnt,"out of",length(a),"say the movie is good."))
  print(paste(badCnt,"out of",length(a),"say the movie is bad"))
  print(paste("Rotten Tomatoes score of", movieName,"is", reviews[1,]$score,"out of 100"))
  cat("\n")
  retFrame = data.frame(act=reviews[1,]$rating, a=rev)
  return(retFrame)
}

#Function to call the list of movies and apply the predictMovie Function
LexicalModel <-function(moviesFileName="RateMyMovie/R/testMovies.txt") {
  moviesFile=file(moviesFileName, open = "r")
  movies = readLines(moviesFile)
  close(moviesFile)
  TP=0;TN=0;FP=0;FN=0
  for (movie in movies) {
    ret = predictMovie(movie)
    Sys.sleep(0.5)
    if (is.null(ret)) {
      next
    }
    
    # Setting the TP,FP,TN,FN values to do the Model Evaluation 
    if ((ret$act=="good") && (ret$a=="good")) {
      TP = TP + 1
    } else if ((ret$act=="good") && (ret$a=="bad")) {
      FN = FN + 1
    } else if ((ret$act=="bad") && (ret$a=="good")) {
      FP = FP + 1
    } else {
      TN = TN + 1
    }
  }
  modelEval(TP, TN, FP, FN)
}