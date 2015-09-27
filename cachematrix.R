## This pair of functions take in a matrix, and if its inverse 
## has already been computed earlier and has been stored in cache, 
## then the corresponding function outputs the cached inverse; 
## else, the function computes the inverse and then stores it in 
## cache for future use

## The makeCacheMatrix function makes a list of functions to get & set the matrix, 
## as well as to get & set the inverse

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The cacheSolve function checks for the existence of a cached inverse 
## for the input matrix, and outputs it if available; 
## else the function computes the inverse and stores it in 
## cache for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv        
}
