
library('e1071')

trainNB <- function (words) {
  model = naiveBayes(words$words, words$class)
  model$attributes = colnames(model$tables[[1]])
  return(model)
}

min = 0.00000001
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