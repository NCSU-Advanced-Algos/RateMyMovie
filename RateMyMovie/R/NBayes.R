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
  print(paste(goodCnt,"out of",length(pred),"say the movie is good"))
  print(paste(badCnt,"out of",length(pred),"say the movie is bad"))
  print(paste("Rotten Tomatoes score of", movieName,"is", reviews[1,]$score,"out of 100"))
  return(rev)
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