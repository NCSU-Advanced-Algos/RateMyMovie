#source('RateMyMovie/R/RateMyMovie.R')

#traindata = makeTrainSet('test.csv')
#testdata = read.table(file = 'train.csv', header = TRUE, sep = ",")

# ************************************
# inserts new word into termdocument
# proper values are incremented for words already present in termdocument
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
  #  print("setWordClass")
  for(i in 1:dim(dataframe)[1]) {
    goodCount = dataframe[i,"good"]
#    avgCount = dataframe[i,"average"]
    badCount = dataframe[i,"bad"]
    maxCountClass = max(goodCount, badCount)
    if(maxCountClass == goodCount) {
      dataframe[i,"class"] = "good"
    }
#    else if(maxCountClass == avgCount) {
#      dataframe[i,"class"] = "average"
#    }
    else if(maxCountClass == badCount) {
      dataframe[i,"class"] = "bad"
    }
    else {
      dataframe[i,"class"] = "NoClass"
    }
  }
  #  print("end setWordClass")
  #  print(table(dataframe$class))
  return(dataframe)
}

# ************************************
# generate a term document based ont training set
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
# based on sum of wordCounts in each class
rateReviewSum = function(termDoc, review) {
  tokens = data.frame(word = preprocess(review))
  distro = merge(termDoc, tokens, by = colnames(tokens))  
  if(dim(distro)[1] == 0) {
    return ("NotEnoughSamples")
  }
  noGoodRev = sum(distro$good)
#  noAvgRev = sum(distro$average)
  noBadRev = sum(distro$bad)
  maxRev = max(noGoodRev, noBadRev)
  if(maxRev == noGoodRev) {
    return ("good")
  }
#  else if(maxRev == noAvgRev) {
#    return ("average")
#  }
  else if(maxRev == noBadRev) {
    return ("bad")
  }
  
}

# ************************************
# based on class of words
rateReview = function(termDoc, review) {
  tokens = data.frame(word = preprocess(review))
  distro = merge(termDoc, tokens, by = colnames(tokens))  
  if(dim(distro)[1] == 0) {
    # feedback 
    return ("NotEnoughSamples")
  }
  noGoodRev = as.numeric(table(distro$class)["good"])
 # noAvgRev = as.numeric(table(distro$class)["average"])
  noBadRev = as.numeric(table(distro$class)["bad"])
  maxRev = max(noGoodRev, noBadRev, na.rm = TRUE)
  if(!is.na(noGoodRev) && maxRev == noGoodRev) {
    return ("good")
  }
#  else if(!is.na(noAvgRev) && maxRev == noAvgRev) {
#    return ("average")
#  }
  else if(!is.na(noBadRev) && maxRev == noBadRev) {
    return ("bad")
  }
}

# ************************************
addFeedbackRows = function(feedbackTermDoc, termDoc) {
  termDoc[,1] = as.character(termDoc[,1])
  for(i in 1:dim(feedbackTermDoc)[1]) {
    #print("Loop")
    fbWord = (feedbackTermDoc[i,"word"])
    newFbRow = termDoc[termDoc$word == fbWord,]
    #  if(fbWord == "winters") {
    #    print("Winters")
    #    print(feedbackTermDoc[i,])
    #    print(newFbRow)
    #    print(dim(newFbRow))
    #  }
    #  termDoc = termDoc["word" != fbWord,]
    if(dim(newFbRow)[1] == 0) {
      #      newFbRow = data.frame(word = fbWord, good = 0, bad = 0, average = 0)
      #print("new Row")
      termDoc = rbind(termDoc, feedbackTermDoc[i,])
    }
    else {
      #    print("Updating Row")
      termDoc = termDoc[termDoc$word != fbWord,]
      newFbRow$good = newFbRow$good + feedbackTermDoc[i,"good"]
 #     newFbRow$average = newFbRow$average + feedbackTermDoc[i,"average"]
      newFbRow$bad = newFbRow$bad + feedbackTermDoc[i,"bad"]
      newFbRow = setWordClass(newFbRow)
      termDoc = rbind(termDoc, newFbRow)
    }
    
  }
  #print(dim(termDoc))
  return(termDoc)
}

# ************************************
# feedbacks the termdoc to custinuously build the list
feedback = function(review, revClass, termDoc) {
  feedbackData = data.frame(words = preprocess(review), class = revClass)
  feedbackData[,1] = as.character(feedbackData[,1])
  feedbackTermDocument = generateTermDocument(feedbackData)
  addFeedbackRows(feedbackTermDocument, termDoc)
  #print(feedbackTermDocument)
}

# ************************************
# assign class to review based on term document
makePredicts = function(termDoc, reviewSet) {
  estClass = c()
  for(i in 1:length(reviewSet)) {
    # print(reviewSet[i])
    reviewClass = rateReview(termDoc, reviewSet[i])
    estClass = append(estClass, reviewClass)
    #print(reviewClass)
    termDoc = feedback(reviewSet[i], reviewClass, termDoc)
  }
  returnVal = c()
  returnVal$prediction = estClass
  returnVal$model = termDoc
  return(returnVal)
}


#for(i in 1:dim(traindata)[1]) {
#  if(traindata[i,"class"] == "average") {
#    traindata[i,"class"] = "good"
#  }
#}



#td = generateTermDocument(traindata)
#print(dim(td))
#op = makePredicts(td, testdata[,4])
#print(dim(op$model))
#table(op$prediction, testdata[,3])



