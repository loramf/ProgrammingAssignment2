## The function set() takes an input matrix, 'y', and assigns it to the object 'x' in the parent environment
## and also assigns the value 'NULL' to the object 'i' in the parent environment; in the global environment 
## of the makeCacheMatrix function.
## The function get() retrieves the matrix stored as object 'x' in the makeCacheMatrix function's 
## global environment and prints it.
## The setinv() function takes an input matrix, 'inv', and assigns it to the object 'i' in the
## parent environment; in the global environment of the makeCacheMatrix function. 
## The getinv() function retrieves the object 'i' stored in the makeCacheMatrix function's global 
## environment and prints it.

## The makeCacheMatrix function takes an input matric, 'x', and creates a list of the functions: set(), 
## get(), setinv(), getinv(). It assigns them the names set, get, setinv and getinv respectively so that 
## when the list is passed into the solveCache function they can be called using '$' notation, e.g. 'x$get()'.
## It also stores an object 'i' in its global environment, assigning it a value of 'NULL'. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                i <<- NULL
                x <<- y
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function uses the output from makeCacheMatrix: a list of functions and objects stored
## in its global environment. First it calls getinv() from the list of functions which retrieves the 
## object 'i' stored in the makeCacheMatric global environment and assigns it to an object of the
## same name, 'i', in the cacheSolve global environment. If it is not 'NULL' then we know that the inverse 
## matrix has already been calculated and cached so print a message saying so and return the inverse matrix. 
## If not then a the matrix is a signed to a new object 'data' by retrieving it using the get() function.
## We then find the inverse of the matrix and assign it to the object 'i' by the function setinv(). Finally 
## we print the inverse matrix, 'i'.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("retrieving cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
