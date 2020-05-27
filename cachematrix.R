######################################################################################################################
## Author:  Adrián Chavarría Mora
#  Date:    25/05/2020
# The propose of this project is about make a way to inverset a matrix by caching the result within a lexical  
# scope of a function:  "makeCacheMatrix" and "cacheSolve", creating functions to store data within several environments
######################################################################################################################

######################################################################################################################
# The function "makeCacheMatrix" creates a new enviroment variable. The inverse matrix is cached in the variable j
######################################################################################################################
makeCacheMatrix <- function(x = matrix()) {
    j <- NULL # assigns NULL to a variable within the current environment 
    set <- function(y){ # Set matrix value
        x <<- y # cache the matrix - assigns value y from parent environment
        j <<- NULL # search through parent environments for an existing definition of the variable and set to NULL
    }
    get <- function()x  # Get the matrix value cached with setmatrix
    setInverse <- function(inverse) j <<- inverse # Cached value of inverse matrix is saved in j
    getInverse <- function() j # Get the saved value of inverse matrix j that was saved with setinverse
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse) # creates list to house the four functions  
}

######################################################################################################################
## The function "cacheSolve" returns the inverse of the matrix that is returned by makeCacheMatrix function,
######################################################################################################################

cacheSolve <- function(x, ...) {
    m <- x$getinvers() # if an inverse has already been calculated this gets it
    
    if (!is.null(m)) { # check to see if cacheSolve has been run before
        message("getting cached matrix")
        return (m)
    } 
    
    matX <- x$get() # run the getmatrix function to get the value of the input matrix
    m <- solve(matX, ...) # compute the value of the inverse of the input matrix
    x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
    
    m # return the inverse

}
