
## The following functions are written to give an overall idea of how 
## caching can be done in functional languages like R. It portrays the 
## beauty of R language and these functions gives us overall idea on 
## closures in R. The caching part and finding the inverse of the passed
## matrix is calculated in cacheSolve method.


## This function creates a "special Matrix" required for our caching 
## example in which we can set, gte the matrix we are passing to this
## function as an argument.

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



## This function takes in the makeCacheMatrix function as the argument 
## and gets the data i.e; matrix in our case and finds the inverse of 
## the matrix using solve(...) and displays it.

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

