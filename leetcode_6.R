#leetcode 6

solve = function(s,numRows){
  
s1 = unlist(strsplit(s,split = ""))
i = 1
col = 1
row = 1

# there is probably a formula to calculate how many columns there needs to be
# I  chose values that are at least big enough and they get chopped off at the end
mat = matrix(data = "",nrow = numRows,ncol = numRows^2) 
col_seq = seq(1,numRows^2,by = numRows - 1) 
while(i <= length(s1)){
  
  if(col %in% col_seq & row < numRows){
    # if you are on one of the main columns then you fill the matrix with
    # a letter and increase the row without changing the column
    mat[row,col] = s1[i]
    row = row + 1
    
  } else if(col %in% col_seq & row == numRows){
    # once you reach the bottom you start increasing the columns
    # and decreasing the rows by 1
    mat[row,col] = s1[i]
    col = col + 1
    row = row - 1
   
  } else {
    # if the current column is not in the column sequence 
    # then you are in between columns so you want to fill the matrix and then
    # increase the column and decrease the row by 1 like in the previous case.
    mat[row,col] = s1[i]
    col = col + 1
    row = row - 1
  }
  
  # increase the counter
  i = i + 1
  
}
# the apply makes a vector where each element is a row of the matrix
# pasted into a string
# then you collapse that vector again into one string
paste0(apply(mat,1,paste,collapse=""),collapse = "")
}

s = "PAYPALISHIRING"; numRows = 3
solve(s,numRows)

s = "PAYPALISHIRING"; numRows = 4
solve(s,numRows)
