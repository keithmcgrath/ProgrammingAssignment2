## The makeCacheMatrix and cacheSolve functions allow a special matrix function
## to be created which computes the inverse of a given matrix. It checks if the
## inverse of the the matrix has already been computed and stored, if so, it
## returns it, else it calculates the inverse, stores it in the cache and then
## returns it.

## The makeCacheMatrix function creates a special matrix function that can cache its
## inverse. It takes in the matrix x, it then clears the m variable.
## Set allows the value of object x to be changed, to the new vlues for x.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes the object x and gets the inverse of it and sets it to m.
## If m is not empty, it retrieves the inverse matrix stored in the cache and
## prints the message "getting cached data".
## Otherwsie it gets the new object x (matrix) and computes m, the inverse of this
## matrix. It then stores this inverse matrix m into the cache and outputs the same
## inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
