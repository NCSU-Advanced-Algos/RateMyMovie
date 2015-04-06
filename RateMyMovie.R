library('RCurl')
library('jsonlite')
library('tm')
library('SnowballC')

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
API_KEY="qynq4687htc3z7mq2ec7y67x"
getRating<-function(score){
  if (score>=70) {
    return("good")
  } else {
    return("bad")
  }
}

getReviews<-function(movieName="Dark Knight Rises", apiKey=API_KEY) {
  movieURL=gsub(" ","+",paste("http://api.rottentomatoes.com/api/public/v1.0/movies.json?q=",
                 movieName,"&page_limit=1&page=1&apikey=", apiKey, sep=""))
  jsonStr=getURLContent(movieURL)
  movieObj=fromJSON(jsonStr[1])
  if(length(movieObj$movies) == 0) {
    return(NULL)
  }
  score = movieObj$movies$ratings$audience_score
  rating = getRating(score)
  id = movieObj$movies$id
  reviewsURL=paste("http://api.rottentomatoes.com/api/public/v1.0/movies/",
                   id, "/reviews.json?review_type=all&page_limit=25&page=1&country=us&apikey=", 
                   apiKey, sep="")
  jsonStr=getURLContent(reviewsURL)
  reviews=gsub("\"|'","",fromJSON(jsonStr[1])$reviews$quote)
  if(length(reviews)==0) {
    return(NULL)
  }
  movieReview=data.frame(name=movieName, id=id, rating=rating, reviews=reviews,score=score)
  return(movieReview)
}

streamTrainingRows<-function(moviesFileName="trainMovies.txt", output="train.csv") {
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

makeTrainSet <- function(trainFile='train.csv'){
  trains = read.table(file=trainFile,header = TRUE, sep=",")
  goods = c()
  bads = c()
  trains$reviews = as.character(trains$reviews)
  for (i in 1:dim(trains)[1]) {
    if (trains[i,]$rating == "good") {
      goods = append(goods, trains[i,]$reviews)
    } else if (trains[i,]$rating == "bad") {
      bads = append(bads, trains[i,]$reviews)
    }
  }
  gdf = data.frame(words=preprocess(goods),class="good")
  bdf = data.frame(words=preprocess(bads),class="bad")
  trainSet = rbind(gdf, bdf)
  return(trainSet)
}

preprocess<-function(sents, stpwrds=stopwords('english')){
  corp=Corpus(VectorSource(sents))
  corp=tm_map(corp, tolower)
  corp=tm_map(corp, removePunctuation)
  corp=tm_map(corp, removeNumbers)
  corp=tm_map(corp, removeWords,stpwrds)
  #corp=tm_map(corp, stemDocument, language="English")
  dataframe<-data.frame(text=unlist(sapply(corp, '[')), 
                      stringsAsFactors=F)
  words=c()
  dataframe$text = as.character(sapply(dataframe$text, stemWords))
  for(i in 1:dim(dataframe)[1]) {
    splits = strsplit(trim(dataframe$text[i]),"\\s+")
    for (s in splits){
      words = append(words,s)  
    }
  }
  return(words)
}

stemWords=function(str) {
  stemmed = trim(stemDocument(str))
  return(stemmed)
}

streamMovies <- function(model=NULL, predictor=NULL, apiKey=API_KEY, maxPages=5) {
  pageNum=1
  while(pageNum <= maxPages) {
    moviesURL=gsub(" ","+",paste("http://api.rottentomatoes.com/api/public/v1.0/",
                                 "movies.json?q=e&page_limit=25&page=",pageNum,
                                 "&apikey=", apiKey, sep=""))
    jsonStr=getURLContent(moviesURL)
    movieObj=fromJSON(jsonStr[1])
    sapply(movieObj$movies$title, function(movieName){
      predictor(model, movieName)
      Sys.sleep(0.5)
    })
    pageNum = pageNum + 1
  }
}

modelEval<-function(TP,TN,FP,FN){
  Precision <- TP/(TP + FP)
  cat("Precision::")
  print(Precision)
  Accuracy <- (TP + TN)/(TP + FP + TN + FN)
  cat("Accuracy::", Accuracy, "\n")
  Recall <- TP/(TP + FN)
  cat("Recall::", Recall, "\n")
  Sensitivity<- TP/(TP + FN)
  cat("Sensitivity::", Sensitivity, "\n")
  Specificity <- TN/(TN+FP)
  cat("Specificity::", Specificity, "\n")
  F1 <- (2*Precision*Recall)/(Precision+Recall)
  cat("F1 Measure::", F1)
}
