library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(tidytext)
library(dplyr)


GetRawWords <- function(path, male_bool, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", verbose = FALSE)
{
  setwd(path)
  
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  files = list.files(pattern="*.xml")
  
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    author <- gsub(".xml", "", file)
    gender <- truth[truth$author==author,"gender"]
    if (male_bool) {
      if (gender=="male")
      {
        xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
        corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
      }
    }
    else{
      if (gender=="female")
      {
        xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
        corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
      }
    }
  }
  
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }	
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")		
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  return (corpus.preprocessed)
}

GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
	setwd(path)
	
	files = list.files(pattern="*.xml")
	
	corpus.raw <- NULL
	i <- 0
	for (file in files) {
		xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
		corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
		i <- i + 1
		if (verbose) print(paste(i, " ", file))	
	}

	corpus.preprocessed <- corpus.raw
	
	
	if (lowcase) {
		if (verbose) print("Tolower...")
		corpus.preprocessed <- tolower(corpus.preprocessed)
	}	
	
	if (punctuations) {
		if (verbose) print("Removing punctuations...")		
		corpus.preprocessed <- removePunctuation(corpus.preprocessed)
	}

	if (numbers) {
		if (verbose) print("Removing numbers...")
		corpus.preprocessed <- removeNumbers(corpus.preprocessed)
	}

	if (whitespaces) {
		if (verbose) print("Stripping whitestpaces...")
		corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
	}

	if (swlang!="")	{
		if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
		corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
	}
	
	if (swlist!="") {
		if (verbose) print("Removing provided stopwords...")
		corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
	}

	if (verbose) print("Generating frequency terms")
	
	corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
	if (verbose) plot(corpus.frequentterms)
	
	return (corpus.frequentterms)
}


GenerateBoW <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = FALSE) {
	setwd(path)

	truth <- read.csv("truth.txt", sep=":", header=FALSE)
	truth <- truth[,c(1,4,7)]
	colnames(truth) <- c("author", "gender", "variety")

	i <- 0
	bow <- NULL
	files = list.files(pattern="*.xml")
	for (file in files) {
		author <- gsub(".xml", "", file)
		variety <- truth[truth$author==author,"variety"]
		gender <- truth[truth$author==author,"gender"]

		xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
		txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
		

		if (lowcase) {
			txtdata <- tolower(txtdata)
		}

		if (punctuations) {
			txtdata <- removePunctuation(txtdata)
		}

		if (numbers) {
			txtdata <- removeNumbers(txtdata)
		}

		if (whitespaces) {
			txtdata <- stripWhitespace(txtdata)
		}
	
		line <- author
		txtdata <- GetBigrams(txtdata)
		freq <- freq_terms(txtdata, n)
		for (word in vocabulary$WORD) {
			thefreq <- 0
			if (length(freq[freq$WORD==word,"FREQ"])>0) {
				thefreq <- freq[freq$WORD==word,"FREQ"]
			} 
			line <- paste(line, ",", thefreq, sep="")
		}
		if (class=="variety") {
			line <- paste(line, ",", variety, sep="")
		} else {
			line <- paste(line, ",", gender, sep="")
		}

		bow <- rbind(bow, line)

		i <- i + 1

		if (verbose) {
			if (class=="variety") {
				print(paste(i, author, variety))
			} else {
				print(paste(i, author, gender))
			}
		}
	}

	return (bow)
}

GetBigrams <- function(corpus.preprocessed)
{
  bigrams <- c()
  for (each_tweet in corpus.preprocessed)
  {
    each_words <- strsplit(each_tweet, "\\s+")[[1]]
    each_bigrams<-""
    old_word <- ""
    first_word <- TRUE
    for(each_word in each_words)
    {
      if (first_word)
      {
        first_word <- FALSE
      }
      else
      {
        each_bigrams <- paste(each_bigrams, " ", old_word, "_", each_word, sep="")
      }
      
      old_word <- each_word
    }
    
    bigrams<-c(bigrams, each_bigrams)
  }
  
  return (bigrams)
}

n <- 1000
path_training <-  "/home/gabpigon@alumno.upv.es/Descargas/2016-2017 Text Mining en Social Media/pan-ap17-bigdata/training/"  # Your training path
path_test <-  "/home/gabpigon@alumno.upv.es/Descargas/2016-2017 Text Mining en Social Media/pan-ap17-bigdata/test/"	

rawWordsMale <- GetRawWords(path_training, TRUE, swlang="es", verbose = FALSE)
rawWordsMale <-GetBigrams(rawWordsMale)

test_fast <- rawWordsMale[1:3]

result <- GetBigrams(test_fast)
test_fast[1]
result[1]

rawWordsFemale <- GetRawWords(path_training, FALSE, swlang="es", verbose = FALSE)
rawWordsFemale <-GetBigrams(rawWordsFemale)

frequentMale <- freq_terms(rawWordsMale, 2000)
frequentFemale <- freq_terms(rawWordsFemale, 2000)

rawWords <- c(rawWordsMale, frequentMale)

frequentMale$gender <- "male"
frequentFemale$gender <- "female"

#head(frequentMale)
#head(frequentFemale)

freq_matrix_gender <- frequentMale
freq_matrix_gender <- rbind(freq_matrix_gender, frequentFemale)

#head(freq_matrix_gender)
#tail(freq_matrix_gender)

tf_idf <- bind_tf_idf(freq_matrix_gender,WORD, gender, FREQ)

#head(tf_idf)
tf_idf_ordered <- arrange(tf_idf, desc(tf_idf))
#head(tf_idf_ordered)

#vocabulary <- GenerateVocabulary(path_training, n, swlang="es", verbose = FALSE)
vocabulary <- tf_idf_ordered[1:1000,1:2]
#head(vocabulary)
#length(vocabulary$WORD)

bow_training <- GenerateBoW(path_training, vocabulary, length(vocabulary$WORD), class="gender", verbose = FALSE)
bow_test <- GenerateBoW(path_test, vocabulary, length(vocabulary$WORD), class="gender", verbose = FALSE)

#head(bow_training)

#training <- concat.split(bow_training, "V1", ",")
training <-cSplit(bow_training, "V1", ",")
test <- cSplit(bow_test, "V1", ",")

number_columns<- length(names(training))

training <- training[,2:number_columns]
names(training)[number_columns - 1] <- "class"
truth  <- unlist(test[,number_columns:number_columns])
test <- test[,2:(number_columns-1)]

head(bow_training)

#train_control <- trainControl( method="repeatedcv", number = 10 , repeats = 3) 
train_control <- trainControl(method="none")
model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear", preProc = c("center"))

pred_SVM <- predict(model_SVM, test)
confusionMatrix(pred_SVM, truth)






