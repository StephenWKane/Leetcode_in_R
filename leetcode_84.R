# leetcode 84

heights = c(2,1,5,6,2,3) # the case from the problem statement
df = data.frame(start = NULL,end = NULL,area = NULL)
for(i in 1:(length(heights) - 1)){
  starting_height = heights[i]
  for(j in (i + 1):length(heights)){
    height = min(heights[i:j]) # the rectangle can't be taller than the smallest value in the rest of the vector
    area = height*(j - i + 1) # the base is (j - (1 - j)) long
    df = rbind(df,data.frame(start = i,end = j,area = area))
  }
}
df[which(df$area == max(df$area)),]$area

