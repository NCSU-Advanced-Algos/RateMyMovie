library('RCurl')
library('jsonlite')

getRating<-function(score){
  if (score>=80) {
    return("good")
  } else if (score>=60) {
    return("average")
  } else {
    return("bad")
  }
}

getReviews<-function(movieName="Dark Knight Rises", apiKey="qynq4687htc3z7mq2ec7y67x") {
  movieURL=gsub(" ","+",paste("http://api.rottentomatoes.com/api/public/v1.0/movies.json?q=",
                 movieName,"&page_limit=1&page=1&apikey=", apiKey, sep=""))
  jsonStr=getURLContent(movieURL)
  movieObj=fromJSON(jsonStr[1])
  if(length(movieObj$movies) == 0) {
    return(NULL)
  }
  rating = getRating(movieObj$movies$ratings$audience_score)
  id = movieObj$movies$id
  reviewsURL=paste("http://api.rottentomatoes.com/api/public/v1.0/movies/",
                   id, "/reviews.json?review_type=all&page_limit=5&page=1&country=us&apikey=", 
                   apiKey, sep="")
  jsonStr=getURLContent(reviewsURL)
  reviews=fromJSON(jsonStr[1])$reviews$quote
  movieReview=data.frame(name=movieName, id=id, rating=rating, reviews=reviews)
  return(movieReview)
}

makeTrainSet<-function(moviesFileName="movies.txt", output="train.csv") {
  if (file.exists(output)) {
    file.remove(output)
  }
  moviesFile=file(moviesFileName, open = "r")
  movies = readLines(moviesFile)
  close(moviesFile)
  doHeader = TRUE
  for (i in 1:length(movies)) {
    reviews = getReviews(movieName=movies[i])
    write.table(reviews, file=output, sep=",", col.names=doHeader, row.names=FALSE, append = !doHeader)
    if (doHeader) {
      doHeader = FALSE
    }
    Sys.sleep(0.5)
  }
}


