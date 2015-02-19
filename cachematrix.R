## Two functions able to cache the inverse matrix calculation. The objective
## is reduce the time computation in case that this inverse matrix calculation 
## has to be computed many times. 

## 'makeCacheMatrix' is the constructor function, where four other functions are 
## encapsulated: 'get', to obtain the initial matrix value. 
## 'setinverse' to assign the value of the matrix inverse  
## 'getinverse' to obtain the matrix inverse value if has already been calculated and recorded
## One extra function 'set' is added, but not used by cacheSolve, in case the user wants 
## introduce changes in the matrix, can use this function, and reset the inverse matrix cache

makeCacheMatrix <- function(x = matrix()) {
        minverse <- NULL
        set <- function(y) {
                x <<- y
                minverse <<- NULL 
        }
        get <- function() x
        setinverse <- function(matinverse) minverse <<- matinverse
        getinverse <- function() minverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve is the call function that operates with matrices created  with 'makeCacheMatrix'
## check if the matrix inverse has been already calculated, if so returns the cached value, if not 
## the inverse matrix is calculated using 'solve' function and the result is recorded for further calls.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minverse <- x$getinverse()
        if(!is.null(minverse)) {
                message("getting cached data")
                return(minverse)
        }
        data <- x$get()
        minverse <- solve(data, ...)
        x$setinverse(minverse)
        minverse
}
