
## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

  # initialize the vector/matrix
    cachematrix <- NULL

  # set the value of the vector/matrix in  working environment
    set <- function (y) {
           x <<- y
           cachematrix <<- NULL
    }

  # get the value of the vector/matrix
    get <- function() x

  # set the inverse value of the vector/matrix  and store in the cache
    setinverse <- function(inverse) cachematrix <<- inverse

  # get the inverted value of the vector/matrix from cache
    getinverse <- function() cachematrix

  # return the created functions to the working environment
    list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  # compute inverse
    cachematrix <- x$getinverse()
    if (!is.null(cachematrix)) {
    message ("getting cached data")
    return(cachematrix)
    }
 
  # if inverse is not calculated...
    data <- x$get()
    cachematrix <- solve(data, ...)

  # cache the inverse
    x$setinverse(cachematrix)

  # return a matrix that is the inverse of 'x'
    cachematrix  

}
