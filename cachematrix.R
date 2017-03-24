## Function to cache the inverse of a matrix rather than computing it 
## repeatedly and save on time.

## makeCacheMatrix() function creates a list containing a function to
## (a) set and get the value of the matrix
## (b) set and the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve() function returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If found done, it gets the result and skips the # computation. 
## If found not computed, it computes the inverse, sets the value in the 
## cache via setinverse function.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
