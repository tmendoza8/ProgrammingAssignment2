
## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

  # initialize the vector
    cachematrix <- NULL

  # set the value of the vector in  working environment
    set <- function (y) {
           x <<- y
           cachematrix <<- NULL
    }

  # get the value of the vector
    get <- function() x

  # set the inverse value of the vector  and store in the cache
    setinverse <- function(inverse) cachematrix <<- inverse

  # get the inverted value of the vector from cache
    getinverse <- function() cachematrix

  # return the created functions to the working environment
    list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  # compute inverse
    invmatrix <- x$getinverse()
    if (!is.null(invmatrix)) {
    message ("getting cached data")
    return(invmatrix)
    }
 
  # if inverse is not calculated...
    data <- x$get()
    invmatrix <- solve(data, ...)

  # cache the invers
    x$setinverse(invmatrix)

  # return a matrix that is the inverse of 'x'
    invmatrix  

}
