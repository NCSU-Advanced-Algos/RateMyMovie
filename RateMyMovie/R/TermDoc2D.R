# ************************************
# inserts new word into termdocument
#     proper values are incremented for words already present in term document
insertTermDocument = function(entry, dataframe) {
  newrow = dataframe[dataframe$word == entry$words,]
  dataframe = dataframe[dataframe$word != entry$words,]
  if(dim(newrow)[1] == 0 ) {
    newrow = data.frame(word = entry$words, good = 0, bad = 0)
  }
  switch(as.character(entry$class),
         good = (newrow$good = newrow$good + 1),
         bad = (newrow$bad = newrow$bad + 1))
  dataframe = rbind(dataframe, newrow)
  return (dataframe)
}

# ************************************
# sets the class of the word in termdocument, class is determined depending on the class
#     in which the word appeared most number of times
setWordClass = function(dataframe) {
  for(i in 1:dim(dataframe)[1]) {
    goodCount = dataframe[i,"good"]
    badCount = dataframe[i,"bad"]
    maxCountClass = max(goodCount, badCount)
    if(maxCountClass == goodCount) {
      dataframe[i,"class"] = "good"
    }
    else if(maxCountClass == badCount) {
      dataframe[i,"class"] = "bad"
    }
    else {
      dataframe[i,"class"] = "NoClass"
    }
  }
  return(dataframe)
}

# ************************************
# generate a term document based on training data
generateTermDocument = function(inputData) {
  inputData[,1] = as.character(inputData[,1])
  termDocument = data.frame(word = character(), good = numeric(), bad = numeric(), class = character())
  for(i in 1:dim(inputData)[1]) {
    termDocument = insertTermDocument(inputData[i,], termDocument)
  }
  termDocument = setWordClass(termDocument)
  return (termDocument)
}

# ************************************
# NOT IN USE : rate individual review based on sum of wordCounts in each class
rateReviewSum = function(termDoc, review) {
  tokens = data.frame(word = preprocess(review))
  distro = merge(termDoc, tokens, by = colnames(tokens))  
  if(dim(distro)[1] == 0) {
    return ("NotEnoughSamples")
  }
  noGoodRev = sum(distro$good)
  noBadRev = sum(distro$bad)
  maxRev = max(noGoodRev, noBadRev)
  if(maxRev == noGoodRev) {
    return ("good")
  }
  else if(maxRev == noBadRev) {
    return ("bad")
  }
}

# ************************************
# based on count of word of particular class appeared in the review
rateReview = function(termDoc, review) {
  tokens = data.frame(word = preprocess(review))
  distro = merge(termDoc, tokens, by = colnames(tokens))  
  if(dim(distro)[1] == 0) {
    return ("NotEnoughSamples")
  }
  noGoodRev = as.numeric(table(distro$class)["good"])
  noBadRev = as.numeric(table(distro$class)["bad"])
  maxRev = max(noGoodRev, noBadRev, na.rm = TRUE)
  if(!is.na(noGoodRev) && maxRev == noGoodRev) {
    return ("good")
  }
  else if(!is.na(noBadRev) && maxRev == noBadRev) {
    return ("bad")
  }
}

# ************************************
# append new rows in term document based on class of of review determined during execution
# the model keeps on updating, learning new words which can be used for classifying future reviews
addFeedbackRows = function(feedbackTermDoc, termDoc) {
  termDoc[,1] = as.character(termDoc[,1])
  for(i in 1:dim(feedbackTermDoc)[1]) {
    fbWord = (feedbackTermDoc[i,"word"])
    newFbRow = termDoc[termDoc$word == fbWord,]
    if(dim(newFbRow)[1] == 0) {
      termDoc = rbind(termDoc, feedbackTermDoc[i,])
    }
    else {
      termDoc = termDoc[termDoc$word != fbWord,]
      newFbRow$good = newFbRow$good + feedbackTermDoc[i,"good"]
      newFbRow$bad = newFbRow$bad + feedbackTermDoc[i,"bad"]
      newFbRow = setWordClass(newFbRow)
      termDoc = rbind(termDoc, newFbRow)
    }
  }
  return(termDoc)
}

# ************************************
# feedbacks the termdoc to continuously to append newly learnt class of words
feedback = function(review, revClass, termDoc) {
  feedbackData = data.frame(words = preprocess(review), class = revClass)
  feedbackData[,1] = as.character(feedbackData[,1])
  feedbackTermDocument = generateTermDocument(feedbackData)
  addFeedbackRows(feedbackTermDocument, termDoc)
}

# ************************************
# make predictions about class of review w.r.t. term document
makePredicts = function(termDoc, reviewSet) {
  estClass = c()
  for(i in 1:length(reviewSet)) {
    reviewClass = rateReview(termDoc, reviewSet[i])
    estClass = append(estClass, reviewClass)
    if (reviewClass != "NotEnoughSamples") {
      termDoc = feedback(reviewSet[i], reviewClass, termDoc)
    }
  }
  returnVal = c()
  returnVal$prediction = estClass
  returnVal$model = termDoc
  return(returnVal)
}