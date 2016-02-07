## Matrix inversion is costly so I am told, so what I've done is create a way
## to cache previously inversed values so that they can be retrieved quickly.
## Two functions are exposed, and sub functions are also exposed. 

## makeCacheMatrix exposes four functions via a list
## makeCacheMatrix$set allows you to set the initial matrix value
## makeCacheMatrix$get allows you to retrieve the initial matrix value 
## makeCacheMatrix$setInverse allows you to set the inverse matrix
## makeCacheMatrix$getInverse allows you to retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv 
        getInverse <- function() inverse
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve solves for an inverse matrix of x.
## It first checks if the inverse has been cached. If it has been cached
## it gives you a funny message and sends you the cached inverse. 
## If the value hasn't been cached the function gets the original matrix
## runs solve on it, and then caches the inverse before it returns the value. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("beep boop beep boop getting the cached datas")
                return(inv);
        }
        # Take the red pill neo
        neo <- x$get()
        inv <- solve(neo, ...)
        x$setInverse(inv)
        return(inv)
}
