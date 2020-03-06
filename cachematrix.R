
## makeCacheMatrix is a function that creates an object to hold the cache of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {   
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv

        list(set = set, 
             get = get, 
             setinv = setinv,
             getinv = getinv
        )
}

## cacheSolve checks if inverse of matrix is in the CacheMatrix (getinv)
## if so, it will return the inverse from the CacheMatrix (getinv)
## if not, is will calculate the inverse of the matrix and store it in setinv.

cacheSolve <- function(x, ...) {
        
        ## Checks is the inv is already calculated in the CacheMatrix, if so it returns inv
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Gets the matrix object from the cacheMatrix and calculates the inverse with solve()
        data <- x$get()
        inv <- solve(data, ...)
        
        ## "Stores" the calculated inverse in the cacheMatrix
        x$setinv(inv)
        
        ## returns the inverse of the matrix object
        inv
}
