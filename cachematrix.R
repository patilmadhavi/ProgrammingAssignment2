## These are the two functions to calcuate the inverse of a matrix and a cache function to retrive a
## value quicker

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
         message("getting cached data")
     return(m)
    }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}

# This is for my own understanding
#a<-makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
#a$get()               Returns original matrix
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#a$getinverse()         
#NULL
#cacheSolve(a)         # Computes, caches, and returns    matrix inverse
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#a$getinverse()        # Returns matrix inverse
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# cacheSolve(a)        # Returns cached matrix inverse using previously computed matrix 
# getting cached data
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#a$set(matrix(c(0,5,99,66), nrow=2, ncol=2))  #modify matrix
#cacheSolve(a) 
#       [,1]      [,2]
#[1,] -0.13333333  0.2
#[2,]  0.01010101  0.0

