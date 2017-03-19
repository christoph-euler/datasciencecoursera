library(RWeka)
library(ggplot2)
library(knitr)
library(tm)
library(downloader)
library(plyr)
library(dplyr)
require(tau)
setwd("D:/Users/ceuler/Documents/2 DataScience/coursera/course_10/Coursera-SwiftKey/final/en_US")

# Check if directory exists
if(!file.exists("./projectData")){
  dir.create("./projectData")
}
Url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# Check if zip has already been downloaded in projectData directory?
if(!file.exists("./projectData/Coursera-SwiftKey.zip")){
  download.file(Url,destfile="./projectData/Coursera-SwiftKey.zip",mode = "wb")
}
# Check if zip has already been unzipped?
if(!file.exists("./projectData/final")){
  unzip(zipfile="./projectData/Coursera-SwiftKey.zip",exdir="./projectData")
}

twitter <- readLines(con <- file ("./projectData/final/en_US/en_US.twitter.txt", "r"), encoding = "UTF-8", skipNul = TRUE)
close(con) 
blogs <- readLines(con <- file ("./projectData/final/en_US/en_US.blogs.txt", "r"), encoding = "UTF-8", skipNul = TRUE)
close(con) 
news <- readLines(con <- file ("./projectData/final/en_US/en_US.news.txt", "r"), encoding = "UTF-8", skipNul = TRUE)
close(con) 

raw_data <- c(sample(blogs, 0.2*length(blogs)),
              sample(news, 0.2*length(news)),
              sample(twitter, 0.2*length(twitter)))
rm(twitter,blogs,news)
gc()
sampled_data <- sapply(raw_data, function(x) iconv(x, "UTF-8", "ASCII", sub=""))
rm(raw_data)
gc()

docs <- Corpus(VectorSource(sampled_data))
docs <- tm_map(docs, removePunctuation) # Remove punctuation
docs <- tm_map(docs, removeNumbers) # Remove numbers
docs <- tm_map(docs, tolower)  # Convert everything to lower case
#docs <- tm_map(docs, removeWords, stopwords("en")) # Remove stopwords, i.e., "a", "and", ...
docs <- tm_map(docs, stripWhitespace) # Remove whitespaces
docs <- tm_map(docs, PlainTextDocument)  # Finish preparation

uniGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
biGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
triGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

uniDocMatrix <- TermDocumentMatrix(docs, control = list(tokenize = uniGramTokenizer))
biDocMatrix  <- TermDocumentMatrix(docs, control = list(tokenize = biGramTokenizer))
triDocMatrix <- TermDocumentMatrix(docs, control = list(tokenize = triGramTokenizer))
quadDocMatrix <- TermDocumentMatrix(docs, control = list(tokenize = quadGramTokenizer))

require(slam)
getFreq <- function(tdm) {
  freq <- sort(rowSums(as.matrix(rollup(tdm, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
  return(data.frame(word = names(freq), freq = freq))
}

freq1 <- getFreq(removeSparseTerms(TermDocumentMatrix(docs), 0.999))
saveRDS(freq1, file="D:/Users/ceuler/Documents/2 DataScience/coursera/course_10/nfreq.f1.2.RData")
freq2 <- getFreq(TermDocumentMatrix(docs, control = list(tokenize = biGramTokenizer, bounds = list(global = c(5, Inf)))))
saveRDS(freq2, file="D:/Users/ceuler/Documents/2 DataScience/coursera/course_10/nfreq.f2.2.RData")
freq3 <- getFreq(TermDocumentMatrix(docs, control = list(tokenize = triGramTokenizer, bounds = list(global = c(3, Inf)))))
saveRDS(freq3, file="D:/Users/ceuler/Documents/2 DataScience/coursera/course_10/nfreq.f3.2.RData")
freq4 <- getFreq(TermDocumentMatrix(docs, control = list(tokenize = quadGramTokenizer, bounds = list(global = c(2, Inf)))))
saveRDS(freq4, file="D:/Users/ceuler/Documents/2 DataScience/coursera/course_10/nfreq.f4.2.RData")