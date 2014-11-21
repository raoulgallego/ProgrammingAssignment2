## These are the 2 functions requested on the programming assignment 2
## for week 3 in R Programming course

## The makeCacheMatrix will calculate the inverse of the matrix and
## store it in cache for later usage

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

## The cacheSolve will check if the inverse of the matrix is already
## in cache and will return it. If not, it will call the makeCaheMatrix
## function to calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the Matrix 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data as it was already calculated")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
