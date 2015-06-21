
## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

# initialize the vector
    cachematrix = NULL

  # set the value of the vector in  working environment
    set <- function (y) {
           x <<- y
           cachematrix <<- NULL
    }

  # get the value of the vector
    get <- function() x

  # set the inverse value of the vector  and store in the cache
    setMatrix <- function(inverse) cachematrix <<- inverse

  # get the inverted value of the vector from cache
    getInverse <- function() cachematrix

  # return the created functions to the working environment
    list(set = set, get = get,
         setMatrix = setMatrix,
         getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

# compute inverse
    invmatrix <- x$getinv()
    if (!is.null(invmatrix)) {
    message ("getting cached data")
    return(invmatrix)
    }

  # if inverse is not calculated..
    data <- x$get()
    invmatrix <- solve(data, ...)

  # cache the inverse
    x$setinv(invmatrix)

  # Return a matrix that is the inverse of 'x' 
    invmatrix

}
