## This programing assignment is for the intro to R programming course on 
## coursera

## The program is designed to cache potentially time-consuming matrix inversion
## If the matrix is not changed, we could use this program to cache the inverse 
## of the matrix so that when we need it, we just look it up in the cache rather
## than recompute it. 

## There are two functions in this program, namely makeCacheMatrix and 
## cacheSolve. The details of the two functions are listed below


## The makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}



## The cacheSolve function calculates the inverse of the special "matrix" 
## created by function makeCacheMatrix. The function first checks to see
## if the inverse has been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it computes the inverse of the
## matrix and sets the value of the inverse in the cache via setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}

