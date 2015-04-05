library('RCurl')
library('jsonlite')
library('tm')
library('SnowballC')
library('sentiment')

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

getRating<-function(score){
  if (score>=50) {
    return("good")
  }  else {
    return("bad")
  }
}

getReviews<-function(movieName="Dark Knight Rises", apiKey="qynq4687htc3z7mq2ec7y67x") {
  movieURL=gsub(" ","+",paste("http://api.rottentomatoes.com/api/public/v1.0/movies.json?q=",
                              movieName,"&page_limit=1&page=1&apikey=", apiKey, sep=""))
  jsonStr=getURLContent(movieURL)
  print(movieName)
  movieObj=fromJSON(jsonStr[1])
  if(length(movieObj$movies) == 0) {
    return(NULL)
  }
  rating = getRating(movieObj$movies$ratings$audience_score)
  id = movieObj$movies$id
  reviewsURL=paste("http://api.rottentomatoes.com/api/public/v1.0/movies/",
                   id, "/reviews.json?review_type=all&page_limit=25&page=1&country=us&apikey=", 
                   apiKey, sep="")
  jsonStr=getURLContent(reviewsURL)
  reviews=gsub("\"|'","",fromJSON(jsonStr[1])$reviews$quote)
  if(length(reviews)==0) {
    return(NULL)
  }
  print(length(reviews))
  movieReview=data.frame(name=movieName,id=id,rating=rating, reviews=reviews)
  return(movieReview)
}

streamTrainingRows<-function(moviesFileName="movies.txt", output="train.csv") {
  if (file.exists(output)) {
    file.remove(output)
  }
  moviesFile=file(moviesFileName, open = "r")
  movies = readLines(moviesFile)
  close(moviesFile)
  doHeader = TRUE
  for (i in 1:length(movies)) {
    reviews = getReviews(movieName=movies[i])
    if (is.null(reviews)) {
      next
    }
    write.table(reviews, file=output, sep=",", col.names=doHeader, row.names=FALSE, append = !doHeader)
    if (doHeader) {
      doHeader = FALSE
    }
    Sys.sleep(0.5)
  }
}

  
  preprocess<-function(sents, stpwrds=stopwords('english')){
  corp=Corpus(VectorSource(sents))
  corp=tm_map(corp, tolower)
  corp=tm_map(corp, removePunctuation)
  corp=tm_map(corp, removeNumbers)
  dataframe<-data.frame(text=unlist(sapply(corp, '[')), stringsAsFactors=F)
  return(dataframe)
}

modelEval<-function(TP,TN,FP,FN){
  Precision <- TP/(TP + FP)
  cat("Precision::")
  print(Precision)
  Accuracy <- (TP + TN)/(TP + FP + TN + FN)
  cat("Accuracy::")
  print(Accuracy)
  Recall <- TP/(TP + FN)
  cat("Recall::")
  print(Recall)
  Sensitivity<- TP/(TP + FN)
  cat("Sensitivity::")
  print(Sensitivity)
  Specificity <- TN/(TN+FP)
  cat("Specificity::")
  print(Specificity)
  F1 <- (2*Precision*Recall)/(Precision+Recall)
  cat("F1 Measure::")
  print(F1)
  
}
 
  getReviews()
  streamTrainingRows()
  train <- read.csv("train.csv")
  train[4]<-preprocess(train[4])
  prediction<-classify_polarity(train[4],algorithm="bayes",verbose=TRUE)
  ansSet<-as.data.frame(prediction)
  words <- ansSet[3]
  a<-c()
  for(word in words$"POS/NEG"){
    word<-as.numeric(as.character(word))
    ifelse(word > 1.5, a <- append(a,"good"), a <- append(a,"bad"))
    
  }
  a<-as.data.frame(a)
  train[5]<-a[1]
  rate<-table(train$rating,train$a)
  rate
  modelEval(rate[1,1],rate[2,2],rate[2,1],rate[1,2] )
