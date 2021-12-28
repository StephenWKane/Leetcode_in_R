library(combinat) # a library to get permutations

solve = function(s,words){
  
# s is a string of characters
# words is a vector of strings

s1 = unlist(strsplit(s,split = "")) # turn the input string into a vector of characters
# get a vector of all the permutations of the vector "words"
words_perm = unlist(lapply(permn(words),FUN = function(x){paste0(x,collapse = "")}))
# get the total length of each concatenation of "words"
word_perm_length = nchar(paste0(words,collapse = ""))

found = c(NULL) # keep track of which letters in s are the beginning of one of the permutations
# stop the search before the end of the string
for(i in 1:(length(s1) - (word_perm_length) + 1)){
  
  # check if the current substring is in the vector of permutations
  if(paste0(s1[i:(i + word_perm_length - 1)],collapse = "") %in% words_perm){
    found = c(found,i - 1)
  }
}

found
}

s = "barfoothefoobarman"; words = c("foo","bar")

solve(s,words)

s = "barfoofoobarthefoobarman"; words = c("bar","foo","the")
solve(s,words)

s = "wordgoodgoodgoodbestword"; words = c("word","good","best","word")
solve(s,words)
