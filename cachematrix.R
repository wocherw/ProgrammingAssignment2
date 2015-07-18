## ------------------------------------------------------------------------------------------------------------
## cachematrix.R
## Last Updated : 17-JUL-2015
## ------------------------------------------------------------------------------------------------------------
## This file contains the following functions.
## 		makeCacheMatrix
## 		cacheSolve
## The output from makeCacheMatrix is the argument passed to cacheSolve.
## These two functions are used to store a matrix and it's inversion.
## The process of inverting a matrix can take time.
## If the contents of a matrix do not change very often and if the inverted matrix
## used multiple times in future calculations, it is a good idea to cache both matrices.
## The time to calculate the inversion is done once and the results are cached.
## Any time the information is used, then the results are already memory and it is
## not necessary to invert the matrix again.

## An example of how to use the two functions
## mtrx <- create an invertible matrix
## mtrx_fnc_lst <- makeCacheMatrix(mtrx)

## i_mtrx <- cacheSolve(mtrx_fnc_lst)
##		the first time you execute cacheSolve 
##			it calculates the inverted matrix and caches the results
##				the first time you execute this it will take time since it needs to create the inverted matrix
##			it returns the inverted matrix

## i_mtrx <- cacheSolve(mtrx_fnc_lst)
##		the second,third,fouth,... time you execute cacheSolve
##			it returns the inverted matrix that was cached
##				this is executed more quickly since no calculation needs to be executed.

## ------------------------------------------------------------------------------------------------------------
## makeCacheMatrix is used to create an object that is used by the cacheSolve function.
## makeCacheMatrix assumes that the matrix supplied is always invertible.
## ------------------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  
        ## initialize the object that stores the results
        m <- NULL
        
        ## define a function to initialize the objects
        ## both x and m are objects in a different environment
        ## x stores the original matrix that is being inverted
        ## m is initalized to NULL when the function is first called
        ## m will be used to store the inverted matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## define a function that returns the value of the matrix
        get <- function() x
        
        ## define a function that inverts the matrix
        setsolve <- function(solve) m <<- solve
        
        ## define a function that returns the inverted matrix
        getsolve <- function() m
        
        ## return the functions that were defined
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## ------------------------------------------------------------------------------------------------------------
## cacheSolve is used to get and set the matrix that was returned by calling the cacheSolve function
## ------------------------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## get the value of the inverted matrix stored in a different environment
        ## m is local to this function
        m <- x$getsolve()
        
        ## if the inverted matrix is not null, that means it has already been cached
        ## return the cached matrix and exit out of this function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## this means the matrix is not already cached
        ## get the value of the original matrix
        data <- x$get()
        
        ## invert the matrix and store the results in m
        m <- solve(data, ...)
        
        ## call the function to apply the inverted matrix to the object in a different environment.
        ## once the inverted matrix is cached, then this part of the code will not need to be executed should
        ## that inverted matrix be needed again.
        x$setsolve(m)
        
        ## return the inverted matrix
        m
}
