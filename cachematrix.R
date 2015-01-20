## The first function creates a list containing four functions to:
## set and get the value of matrix x, as well as wet and get the inverse of that matrix.


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(sol) i <<- sol
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates the inverse matrix created by previous function.
## It first checks the cache to see if the inverted matrix has been calculated. 
## If not, then it performs the inversion and sets the value of inverted matrix using setinv in cache

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}