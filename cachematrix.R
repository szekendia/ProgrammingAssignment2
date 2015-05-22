## Coursera : R-Programming : Programming assignment 2
## Github: https://github.com/szekendia/ProgrammingAssignment2

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## Sample run:
## 0. mx <- matrix(c(1,3,2,4,6,7,4,8,12),3,3)

## makeCacheMatrix
## -------------------------------------
## 1. fmx <- makeCacheMatrix()
## 2. fmx$set(mx) ## cache the mx matrix
## 3. fmx$get() ## retrieve the matrix from cache

makeCacheMatrix <- function(cached_matrix = matrix()) {
## in: cached_matrix. Random, numeric, invertible(!) square matrix. Sample run: matrix(1:9,3,3)
    
    cached_inv_matrix <- NULL

    ## Cache the mx matrix. Sample run: fmx$set()
    ## Reset invert matrix variable
    f_set <- function(p_matrix) {
        cached_matrix <<- p_matrix
        cached_inv_matrix <<- NULL
    }

    ## Retrieve cached matrix. Sample run: fmx$get()
    f_get <- function() cached_matrix

    ## Cache the inverted cached matrix into global environment variable. Sample run: fmx$setsolve()
    f_setsolve <- function(solve) cached_inv_matrix <<- solve
    ## in: solve. Cached, numeric, inverse square matrix. Called from function cacheSolve: f$setsolve(inv_matrix)

    ## Retrieve the cached, inverse matrix. Sample run: fmx$getsolve()
    f_getsolve <- function() cached_inv_matrix
    
    list(
        set = f_set,
        get = f_get,
        setsolve = f_setsolve,
        getsolve = f_getsolve
    )
}

## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

## cacheSolve
## -------------------------------------
## Sample run:
## 1. cacheSolve(fmx)

cacheSolve <- function(f, ...) {
## in: f (function). The variable the makeCacheMatrix function is assigned into. (Sample run: fmx)
    ## Set inv_matrix by calling makeCacheMatrix$getsolve
    inv_matrix <- f$getsolve()
    
    ## If inv_matrix has value, then getting cached data
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    ## set inv_matrix by calling makeCacheMatrix$setsolve
    data <- f$get()
    inv_matrix <- solve(data)
    f$setsolve(inv_matrix)
    inv_matrix
}
