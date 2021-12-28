# leetcode 84

height_vector = c(2,1,5,6,2,3) # the case from the problem statement

solve = function(heights){
df = data.frame(start = NULL,end = NULL,area = NULL)
for(i in 1:(length(heights) - 1)){
  starting_height = heights[i]
  for(j in (i + 1):length(heights)){
    height = min(heights[i:j]) 
    area = height*(j - i + 1) 
    df = rbind(df,data.frame(start = i,end = j,area = area)) 
  }
}
df[which(df$area == max(df$area)),]$area
}

solve(height_vector)


