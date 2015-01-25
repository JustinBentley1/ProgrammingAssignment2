## ___________________________________________________________________________________
## makeCacheMatrix coupled with cacheSolve
## using <<- to expose variables/functions/results of functions defined elsewhere 
## in the enviroment and refedine as necessary.
## 
## makeCacheMatrix, cache an invetable matrix
## cacheSolve, take the cached matrix and calculate the inverse
## using <<- to create 'special' objects/functions, able to be called and redefined within other functions.
## ___________________________________________________________________________________

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## placeholder called 'm'
    ## create a function [set] to set the value of x and ensure the inverse result
    ## is empty
    set <- function(y) {
        x <<- y    ## Set the value 
        m <<- NULL # make m nothing, m exists but has no defined value or object type
    }
    ## 'get' the matrix
    get <- function() x
    ## create a function [setinverse] to set the inverse. 
    setInverse <- function(inverse) m <<- inverse
    ## create a function to [get] m 
    getInverse <- function() m
    
    ## fix a list of the four functions of x
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## __________________________________________________________________________________
## calling functions defined in makeCacheMatrix
## to supply matrix for inverse calculation
## __________________________________________________________________________________
cacheSolve <- function(x) {
    ## retrieve the cached inverse value (or null if m not already defined)
    m <- x$getInverse() 
    ## if x$getinvese() NOT IS NULL then simply return result of function x$getinverse
    if(!is.null(m)) {return(m)} ## return the result, subsequent code will be ignored
    ## if NOT IS NULL returned FALSE, x$getinverse == NULL
    ## label result of x$get function [data] 
    data <- x$get()
    ## calculate the inverse matrix of matrix(data), store result in m
    m <- solve(data)
    ## store m in x$setinverse, so when function is called again m will not be null.
    x$setInverse(m)
    ## return [m], the calculated inverse of [data]
    m
    ## the above functions borrowed heavily from examples found on the internet.
    ## particularly from: gustavdelius/datasciencecoursera in github
    ## while efforts have been made to understand the program flow, I do not think
    ## I could have written this from scratch withount reference to others prior 
    ## work. 
}