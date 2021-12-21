solve = function(s,t){
  
s1 = unlist(strsplit(s,""))
t1 = unlist(strsplit(t,""))
result = data.frame(max = NULL,min = NULL)

# the logic:
# 1. we iterate through starting points and check if the letter is in the target string
# 2. if it is, we remove the letter from t, we track the coordinate of where it was in s, and we continue the search.
# 3. we end our search if we reach the end of s, or if we find all of the remaining letters in t.
# 4. if the search ends, we start a new search with all of the letters in t again.
# 5. At the end, we compare the coordinates across iterations where all of the letters in t were found.

for(i in 1:length(s1)){
  target = t1 # on each iteration we restart the target array
  answer = c(NULL) # record the coordinates of where the letters are in s as we find them in t.
  
  if(s1[i] %in% target){
    
    target = target[-which(target == s1[i])[1]]
    answer = c(answer,i) # store the coordinate of the letter in s
    
    counter = i + 1 # start our search at the next letter

    while(length(target) > 0 & counter <= length(s1)){ 
      if(s1[counter] %in% target){
        target = target[-which(target == s1[counter])[1]]
        answer = c(answer,counter)
      }
      
      if(length(target) == 0){
        result = rbind(result,data.frame(max = max(answer), min =  min(answer)))
      }
      counter = counter + 1
      
    }
    
  }
}

result$diff = result$max - result$min
res = result[which(result$diff == min(result$diff)),]
paste0(s1[res$min:res$max],collapse = "")

}

# the case from the website
s = "ADOBECODEBANC"
t = "ABC" 

solve(s,t)

# an example of a case with duplicates.
s = "ZSPTSRZAFASDFASDFASDFZPTZ"
t = "ZZPT"

solve(s,t)
