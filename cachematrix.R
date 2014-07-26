## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a cacheable matrix that also has an inverse
## e.g. m<-makeCacheMatrix(matrix(1:4,2,2))
## Contains get, set, getinverse, setinverse functions with lexical scoping
makeCacheMatrix <- function(x = matrix(...)) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inversematrix) i <<- inversematrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Solve (find the inverse) of a matrix and check the cache first
## if the inverse is already stored in cache, print a message and 
## get it from cache and return
## e.g. cacheSolve(m)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
