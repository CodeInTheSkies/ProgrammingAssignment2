## This pair of functions take in a matrix, and if its inverse 
## has already been computed earlier and has been stored in cache, 
## then output the cached inverse; else, we compute the inverse and 
## then store it in cache for future use

## This function makes a list of functions to get & set the matrix, as well as to get & set the inverse

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

## This function checks for cached inverse for the input matrix, and outputs it if available, 
## else computes the inverse and stores it in cache for future use

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
