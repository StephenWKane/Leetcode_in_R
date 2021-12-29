
solve = function(x){

# convert the input string into a vector of characters
vec_s = unlist(strsplit(x,""))
# make a vector to store substrings 
store_strings = c(NULL)

for(i in 1:length(vec_s)){
  # initialize the running string on the first letter
  if(i == 1){running_string = vec_s[i]} else {
    
    # if the current letter isn't in the running string then append it to the running string
    if(!(vec_s[i] %in% running_string) & i < length(vec_s)){
      running_string = c(running_string,vec_s[i])
    } else {
      # if the current letter is already in the running string then store what was in the running string
      # in our vector of strings and then set the running string to the current letter
      store_strings = c(store_strings,paste0(running_string,collapse = ""))
      running_string = vec_s[i]
    }
  }
}

store_unique_strings_unique = unique(store_strings)
# this makes a vector of numbers corresponding to the number of characters of each string in the vector
num_chars = nchar(store_unique_strings_unique)
# pull the value from the unique strings that corresponds to the string of maximum length.
store_unique_strings_unique[which(num_chars == max(num_chars))]
}

s = "abcabcbb"
solve(s)

s = "iurtewibcdabcbbqwetry"
solve(s)
