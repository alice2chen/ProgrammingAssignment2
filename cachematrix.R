## this is the Week 3 assignment and also the 1st to use Github
## functions do: to make the matrix and put into cache for future use in order to save resource

## Write a short comment describing this function: setup this function matrix and get the results

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function: to read from cache

cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("to get cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}

##below is the test
wk3_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
wk3_Matrix$get()
wk3_Matrix$getinverse()
cacheinverse(wk3_Matrix)
cacheinverse(wk3_Matrix)
