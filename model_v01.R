uniGram.sort <- readRDS("nfreq.f1.RData")
biGram.sort <- readRDS("nfreq.f2.RData")
triGram.sort <- readRDS("nfreq.f3.RData")
quadGram.sort <- readRDS("nfreq.f4.RData")

p.uniGram <- cbind(uniGram.sort, p=uniGram.sort[,2]/sum(uniGram.sort[,2]))
p.biGram <-  cbind(biGram.sort, p=biGram.sort[,2]/sum(biGram.sort[,2]))
p.triGram <- cbind(triGram.sort, p=triGram.sort[,2]/sum(triGram.sort[,2])) 
p.quadGram <- cbind(quadGram.sort, p=quadGram.sort[,2]/sum(quadGram.sort[,2]))

########################################################
# Model
#
# Idea: Bayesian prediction
# Probability of n-gram is n-Gram/sum(n-Gram)
# A n-gram is used to predict the probability of the n-th word of the n-gram using the n-1 previous words
# Therefore, in order to use the previous 1, 2 and 3 words, we need bi- to quadGrams.
#
# Step 1: Find n-grams that contain the previous 1, 2 and 3 words
# Step 2: Determine the likelihood of occurrence of all possible n-th words in these n-grams
# Step 3: Rank these likelihood and print the word with the highest likelihood
########################################################
get_next_word <- function(this_triGram){
  # Clean up input string
  this_triGram <- tolower(this_triGram)
  this_triGram <- gsub("[[:punct:]]", "", this_triGram)
  this_triGram <- gsub("+\\d+", "", this_triGram)
  this_triGram <- gsub("^ *|(?<= ) | *$", "", this_triGram, perl=TRUE)
  
  words <- unlist(strsplit(this_triGram, " "))
  if (length(words)>=3){
    words <- words[(length(words)-2):length(words)]
    prior_3_words <- paste(words, collapse = " ")
    prior_2_words <- paste(words[(length(words)-1):length(words)], collapse = " ")
    prior_1_words <- paste(words[length(words)], collapse = " ")
  }else if (length(words)==2){
    words <- words[(length(words)-1):length(words)]
    prior_3_words <- NA
    prior_2_words <- paste(words[(length(words)-1):length(words)], collapse = " ")
    prior_1_words <- paste(words[length(words)], collapse = " ")
  }else if (length(words)==1){
    prior_3_words <- NA
    prior_2_words <- NA
    prior_1_words <- paste(words[length(words)], collapse = " ")
  }
  
  ########################################################
  # 1. Find n-grams that contain the previous 1, 2 and 3 words
  ########################################################
  p_cond_tri <- p.quadGram[which(substr(quadGram.sort[,1],1,nchar(prior_3_words))==prior_3_words),]
  p_cond_bi <- p.triGram[which(substr(triGram.sort[,1],1,nchar(prior_2_words))==prior_2_words),]
  p_cond_uni <- p.biGram[which(substr(biGram.sort[,1],1,nchar(prior_1_words))==prior_1_words),]
  
  ########################################################
  # 2. Determine the likelihood of occurrence of all possible n-th words in these n-grams 
  # and 3. get word with maximum likelihood for each n
  ########################################################
  # ...from the previous 3-gram
  if (nrow(p_cond_tri)!=0){
    next_word_list_quad <- cbind(word=sapply(p_cond_tri[,1],function(x){strsplit(x %>% toString, split = " ") %>% unlist})[4,],
                                p=p_cond_tri[,3]
    )
    rownames(next_word_list_quad) <- NULL
    next_word_quad <- next_word_list_quad[which(as.numeric(next_word_list_quad[,2])==max(as.numeric(next_word_list_quad[,2]))),]
    if(which(p.uniGram[,1]==next_word_quad[1]) %>% length ==0){
      p_word <- 1
    }else{
      p_word <- p.uniGram[which(p.uniGram[,1]==next_word_quad[1]),3]
    }
    p_ngram <- sum(p.quadGram[which(substr(quadGram.sort[,1],1,nchar(prior_3_words))==prior_3_words),3])
    next_word_quad[2] <- as.numeric(next_word_quad[2]) * p_word / p_ngram
  } else{
    next_word_quad <- NULL
  }
  
  # ...from the previous 2-gram
  if (nrow(p_cond_bi)!=0){
    next_word_list_tri <- cbind(word=sapply(p_cond_bi[,1],function(x){strsplit(x %>% toString, split = " ") %>% unlist})[3,],
          p=p_cond_bi[,3]
    )
    rownames(next_word_list_tri) <- NULL
    next_word_tri <- next_word_list_tri[which(as.numeric(next_word_list_tri[,2])==max(as.numeric(next_word_list_tri[,2]))),]
    if(which(p.uniGram[,1]==next_word_tri[1]) %>% length ==0){
      p_word <- 1
    }else{
      p_word <- p.uniGram[which(p.uniGram[,1]==next_word_tri[1]),3]
    }
    p_ngram <- sum(p.triGram[which(substr(triGram.sort[,1],1,nchar(prior_2_words))==prior_2_words),3])
    next_word_tri[2] <- as.numeric(next_word_tri[2]) * p_word / p_ngram
  } else{
    next_word_tri <- NULL
  }
  
  # ...from the previous 1-gram
  if (nrow(p_cond_uni)!=0){
    next_word_list_bi <- cbind(word=sapply(p_cond_uni[,1],function(x){strsplit(x %>% toString, split = " ") %>% unlist})[2,],
                                p=p_cond_uni[,3]
    )
    rownames(next_word_list_bi) <- NULL
    next_word_bi <- next_word_list_bi[which(as.numeric(next_word_list_bi[,2])==max(as.numeric(next_word_list_bi[,2]))),]
    if(which(p.uniGram[,1]==next_word_bi[1]) %>% length ==0){
      p_word <- 1
    }else{
      p_word <- p.uniGram[which(p.uniGram[,1]==next_word_bi[1]),3]
    }
    p_ngram <- sum(p.biGram[which(substr(biGram.sort[,1],1,nchar(prior_1_words))==prior_1_words),3])
    next_word_bi[2] <- as.numeric(next_word_bi[2]) * p_word / p_ngram
  } else{
    next_word_bi <- NULL
  }
  
  # Combine to data frame
  next_word_list <- rbind(next_word_quad, next_word_tri, next_word_bi)
  
  # Get word with largest posterior probability
  if (is.null(next_word_list)){
    next_word <- p.uniGram[1,1]
  }else{
    next_word <- next_word_list[which(next_word_list[,2] == max(next_word_list[,2]))[1],1]
  }
  return(next_word)
}
require(compiler)
get_next_word <- cmpfun(get_next_word)

this_triGram <- "Chair"
get_next_word(this_triGram)
