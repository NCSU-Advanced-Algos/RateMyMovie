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
  norms = c()
  tabs = model$tables[[1]]
  for (word in words) {
    ind = index(model, word)
    if (ind > 0) {
      goods = append(goods, tabs[1, ind])
      bads = append(bads, tabs[2, ind])
      norms = append(norms, tabs[3, ind])
    }
  }
  goods = goods+min
  bads = bads+min
  norms = norms+min
  P_good = prod(goods)*model$apriori[1]
  P_bad = prod(bads)*model$apriori[2]
  P_norm = prod(norms)*model$apriori[3]
  max_P = max(c(P_good, P_bad, P_norm))
  if (max_P == P_good) {
    return("good")
  } else if (max_P == P_bad) {
    return("bad")
  } else {
    return("average")
  }
}

predictMovie <- function(model, movieName, showContingency=FALSE) {
  reviews = getReviews(movieName = movieName)
  pred= c()
  for (review in reviews$reviews) {
    pred = append(pred, as.character(predictReview(model, review)))
  }
  if (showContingency) {
    print(table(reviews$rating, pred))  
  }
  tab = table(pred)
  maxKey = "good"
  maxVal = tab[names(tab)==maxKey]
  if (tab[names(tab)=="bad"] > maxVal) {
    maxKey="bad"
    maxVal=tab[names(tab)=="bad"]
  }
  if (tab[names(tab)=="average"] > maxVal) {
    maxKey="average"
    maxVal=tab[names(tab)=="average"]
  }
  print(paste("The movie is", toupper(maxKey)))
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