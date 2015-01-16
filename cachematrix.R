## Put comments here that give an overall description of what your
## functions do

#Given a matrix, these functions will store its value and calculate its inverse, 
#caching it after calculations. The inverse of the matrix will just be
#calculated againg if the matrix was changed.

## Write a short comment describing this function
#It's a function that return a list of functions. With this we can store a matrix
#and its inverse, Also we can get those values. These values are stored into
#function's environment.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y = matrix()){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}


## Write a short comment describing this function
#This function verify if our special "vector" have the matrix'es inverse cached. If it 
#haven't, then we calculate it and cache the value into the special "vector".
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        s <- x$getsolve()
        if(!is.null(s)){
                message("Retrieving cached solve.")
                return(x$getsolve())
        }
        data <- x$get()
        x$setsolve(solve(data, ...))
        s <- x$getsolve()
        s
}
