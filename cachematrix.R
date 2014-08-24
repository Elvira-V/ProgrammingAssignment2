## makeCacheMatrix function returns "matrix" object - list of 4 functions that can cache its inverse.
## cacheSolve function finds inverse of "matrix" created by makeCacheMatrix.
## If it has already been calculated (and the matrix has not changed via set() element of "matrix" list),
## then the cachesolve retrieves the inverse from the cache.
## Check solver by running cacheSolve(x)%*%x$get()

## makeCacheMatrix returns a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {                              ## Sets a new matrix for this "object"
                x <<- y
                inv <<- NULL                              ## Sets inverse matrix to empty
        }
        get <- function() x                               ## Returns matrix
        setinv <- function(i) inv <<- i                   ## Sets calculated by cacheSolve inverse
        getinv <- function() inv                          ## Returns inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve checks cache for previously calculated inverse of matrix. If cache is not "empty", then
## result from cache returned. Otherwise solve() function is called. Make sure matrix is invertable!

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {                        ## Checking presence of previously calculated cache
                message("getting cached inverse")  
                return(inv)
        }
        inv <- solve(x$get(), ...)                 ## Solving
        x$setinv(inv)
        inv
}
