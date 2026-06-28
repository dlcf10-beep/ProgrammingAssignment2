## Calculating an inverse and writing it down so we don't forget it.
## Function 1 stores, and function 2 looks for what's stored vs what needs to be calculated. 

## THE PURPOSE OF THIS FUNCTION IS TO REMEMBER THE MATRIX AND THE INVERSE OF THE MATRIX

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
       
}


## THIS FUNCTION DECIDES IF IT NEEDS TO CALCULATE THE INVERSE OR IF IT CAN USE THE ONE IT PREVIOUSLY REMEMBERED (IN WHICH CASE IT DOESN'T CALCULATE ANYTHING)

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
