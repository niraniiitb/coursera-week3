# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function computes the inverst of a matrix. 
# If the inverse has already been calculated, this should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

# Execution instructions
# 
# sample function to create a matrix of nxn
# > hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# > m1 <- hilbert(2)
# > m1
#      [,1]      [,2]
# [1,]  1.0 0.5000000
# [2,]  0.5 0.3333333
# 
# make cache matrix
# > mcm <- makeCacheMatrix(m1)
# > mcm$get()
#      [,1]      [,2]
# [1,]  1.0 0.5000000
# [2,]  0.5 0.3333333
# 
# cache solve with no cache first run
# 
# > cacheSolve(mcm)
#      [,1] [,2]
# [1,]    4   -6
# [2,]   -6   12
# 
# cache solve retrive from cache 
# > cacheSolve(mcm)
# getting cached data
#      [,1] [,2]
# [1,]    4   -6
# [2,]   -6   12
# 
# validate invere worked
# > round(cacheSolve(mcm) %*% mcm$get())
# getting cached data
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > 
#   End of execution instructions

