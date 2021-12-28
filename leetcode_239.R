#leetcode 239

# this might seem like a dumb question but the key is that you can only see the values
# in the sliding window at one time.
# so it means you have to keep memory of the max value that is seen in all windows.

solve = function(v,k){
end = (length(v) - (k - 1)) # the window length determines how far we go
max_value = 0
for(i in 1:end){
  val = max(v[i:(i + k - 1)])
  max_value = max(max_value,val)
}
max_value
}

v = c(1,3,-1,-3,5,3,6,7)
k = 3

solve(v,k)





