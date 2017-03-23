## Put comments here that give an overall description of what your
## functions do

# The first function, makeCacheMatrix, creates an special object that 
# stores a matrix, as well as four functions: set, get, setinverse and 
# getinverse, while the second function takes the object created with
# the makeCacheMatrix function, and returns the inverse of the matrix
# specified in the first function, but only when the inverse was not 
# calculated before, if the inverse already exists, it returns its value
# from memory instead of calculating it again.

## Write a short comment describing this function

# makeCacheMatrix receives a matrix as a formal argument, inside the function,
# four other functions are defined and to be able to access them independtly,
# they are stored as a list. When we create an object with the makeCacheMatrix
# function, the parent  environment of that object is the makeCacheMatrix, 
# and we can access and execute its function components with the $ operator.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## note: the <<- operator changes the value of the variable in the parent 
# environment, in this case, in makeCacheMatrix.


## Write a short comment describing this function

# The cacheSolve function, takes the object created with the makeCacheMatrix
# function. First, it gets the inverse of the matrix stored in the object, if 
# the value has not been calculated yet (it is NULL), it gets the stored matrix
# with get() and then calculates its inverse, and assigns that value
# to the variable `inverse´. If the inverse was already calculated, it returns
# a message: "getting cache data", and gets the data from memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
