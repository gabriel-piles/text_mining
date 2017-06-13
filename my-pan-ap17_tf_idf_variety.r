library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(tidytext)
library(dplyr)
library(randomforrest)

path_training <-  "/Users/gabo/Big_Data/pan-ap17-bigdata/training/"	# Your training path
path_test <-  "/Users/gabo/Big_Data/pan-ap17-bigdata/test/"			# Your test path

GetRawWords <- function(path, variety, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", verbose = FALSE)
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
    each_variety <- truth[truth$author==author,"variety"]
    if (each_variety == variety) {
        xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
        corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
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
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (verbose) print("Removing accents...")
  corpus.preprocessed <- chartr("áéíóúàèìòùâêîôû", "aeiouaeiouaeiou", corpus.preprocessed)
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (length(corpus.preprocessed) == 0)
  {
    print("No hay valores")
  }
  
  return (corpus.preprocessed)
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
		
		txtdata <- chartr("áéíóúàèìòùâêîôû", "aeiouaeiouaeiou", txtdata)

		if (whitespaces) {
			txtdata <- stripWhitespace(txtdata)
		}
	
		line <- author
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

rawWordsMexico <- GetRawWords(path_training, "mexico", swlang="es", verbose = FALSE)
rawWordsChile <- GetRawWords(path_training, "chile", swlang="es", verbose = FALSE)
rawWordsPeru <- GetRawWords(path_training, "peru", swlang="es", verbose = FALSE)
rawWordsVenezuela <- GetRawWords(path_training, "venezuela", swlang="es", verbose = FALSE)
rawWordsSpain <- GetRawWords(path_training, "spain", swlang="es", verbose = FALSE)
rawWordsArgentina <- GetRawWords(path_training, "argentina", swlang="es", verbose = FALSE)
rawWordsColombia <- GetRawWords(path_training, "colombia", swlang="es", verbose = FALSE)

number_words_raw <- 1000

frequent1 <- freq_terms(rawWordsMexico, number_words_raw)
frequent2 <- freq_terms(rawWordsChile, number_words_raw)
frequent3 <- freq_terms(rawWordsPeru, number_words_raw)
frequent4 <- freq_terms(rawWordsVenezuela, number_words_raw)
frequent5 <- freq_terms(rawWordsSpain, number_words_raw)
frequent6 <- freq_terms(rawWordsArgentina, number_words_raw)
frequent7 <- freq_terms(rawWordsColombia, number_words_raw)

head(frequent1)

frequent1$variety <- "mexico"
frequent2$variety <- "chile"
frequent3$variety <- "peru"
frequent4$variety <- "venezuela"
frequent5$variety <- "spain"
frequent6$variety <- "argentina"
frequent7$variety <- "colombia"


#head(frequentMale)
#head(frequentFemale)

freq_matrix_variety <- rbind(frequent1, frequent2, frequent3,frequent4,frequent5,frequent6,frequent7)

head(freq_matrix_variety)
#tail(freq_matrix_gender)

tf_idf <- bind_tf_idf(freq_matrix_variety,WORD, variety, FREQ)

#head(tf_idf)
tf_idf_ordered <- arrange(tf_idf, desc(tf_idf))
head(tf_idf_ordered)

#vocabulary <- GenerateVocabulary(path_training, n, swlang="es", verbose = FALSE)
vocabulary <- tf_idf_ordered[1:500,1:2]
#head(vocabulary)
#length(vocabulary$WORD)

bow_training <- GenerateBoW(path_training, vocabulary, length(vocabulary$WORD), verbose = FALSE)
bow_test <- GenerateBoW(path_test, vocabulary, length(vocabulary$WORD), verbose = FALSE)

head(bow_training)

#training <- concat.split(bow_training, "V1", ",")
training <-cSplit(bow_training, "V1", ",")
test <- cSplit(bow_test, "V1", ",")

number_columns<- length(names(training))

training <- training[,2:number_columns]
names(training)[number_columns - 1] <- "class"
truth  <- unlist(test[,number_columns:number_columns])
test <- test[,2:(number_columns-1)]

library(randomForest)
fit <- randomForest(class~., data= training)
#model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")

prediction <- predict(fit, test)
confusionMatrix(prediction, truth)

