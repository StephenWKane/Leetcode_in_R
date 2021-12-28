# leetcode 221

# 1. For every square from size 1 to the maximum size, do:
# 2. Start the rectangle at (1,1) and move it to (1,n - k + 1)
# 3. Do this for each row from 1 to m - k + 1
# 4. At each position we check if all values are equal to 1, if they are then store it
# 5. at the end we will check the solutions to see which stored rectangle had the biggest area

library(ggplot2)
library(reshape2)

# a function to plot squares and colored base on if the square contains all 1s
plot_func = function(matrix,i,j,k,bool){
  blank_matrix = matrix(nrow = dim(matrix)[1],ncol = dim(matrix)[2],data = 0)
  blank_matrix[j:(j + i - 1),k:(k + i - 1)] = 1
  blank_melt = melt(blank_matrix)
  p = ggplot(melt(matrix), aes(blank_melt$Var1, blank_melt$Var2, fill=cut(blank_melt$value, seq(0, 1)), label=round(value, 1))) + 
    geom_tile() + 
    scale_fill_manual(values=c(if(bool){"skyblue"}else{"lightgreen"}, "white", "red", "white", "black")) + 
    geom_text(color="black") +  theme(legend.position = "none")  + coord_flip() + scale_x_reverse()
  p
}

matrix = rbind(c(1,0,1,0,0),c(1,0,1,1,1),c(1,1,1,1,1),c(1,0,0,1,0))
dim(matrix)
max_square_size = min(dim(matrix))
result_df = data.frame(row = NULL,col = NULL,area = NULL)
for(i in 2:max_square_size){ # skipping 1 because I don't want to press enter too much
  for(j in 1:(nrow(matrix) - i + 1)){
    for(k in 1:(ncol(matrix) - i + 1)){
      if(sum(matrix[j:(j + i - 1),k:(k + i - 1)]) == i^2){
        result_df = rbind(result_df,data.frame(row = j,col = k,area = i^2))
        print(plot_func(matrix,i,j,k,bool = TRUE))
        readline(prompt="Press [enter] to continue")
      } else{
        print(plot_func(matrix,i,j,k,bool = FALSE))
        readline(prompt="Press [enter] to continue")
        }
    }
  }
}






