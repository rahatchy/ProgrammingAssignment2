## The following pair of functions creates a special object that stores
## a matrix and caches its inverse.

## The function "makeCacheMatrix" creates a special "matrix" object,
## which really is a list containing a function to
## set/get the value of the matrix and set/get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        if(nrow(x)!=ncol(x)){
                message("The matrix is not square and doesn't have a inverse matrix.")
                return()
        }
        m <- NULL
        set <-function(y){
                x <<- y
                m <<- NULL
        }
        get <-function() x
        setSolve <- function(sol) m <<- sol
        getSolve <- function() m
        list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


## The function "cacheSolve" calculates the inverse of the matrix.
## However, if the inverse has been already calculated, it returns
## the cached data and skip the computation.

cacheSolve <- function(x, ...) {
        m<-x$getSolve()
        if (!is.null(m)){
                message("getting a cached data")
                return(m)
        }
        data <- x$get()
        sol <- solve(data)
        x$setSolve(sol)
        sol
}
