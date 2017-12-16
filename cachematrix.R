## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The following functions are used to create a special object that 
##stores a matrix and caches its inverse. The first function, 
##makeCacheMatrix creates a special "matrix", which is really a 
##list containing a function to:

    ##1. set the value of the matrix

    ##2. get the value of the matrix

    ##3. set the value of the inverse

    ##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

##We try testing the function using the codes below
##Assume matrix is invertible
A=matrix(c(2,1,5,-1),2,2)
mat.cache=makeCacheMatrix(A) 
a.inv = cacheSolve(mat.cache)
a.inv

## Solution
##    [,1]       [,2]
##[1,] 0.1428571  0.7142857
##[2,] 0.1428571 -0.2857143
