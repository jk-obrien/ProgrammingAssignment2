## There are two functions in this file.

## The first, makeCacheMatrix, creates a matrix object that holds a numeric
## matrix, that can compute its inverse and that can cache both the matrix and
## its inverse.

## Example of use:

## x <- matrix(sample(16),4,4)

## Define the object
## mcm <- makeCacheMatrix()

## Cache the matrix and the inverse
## mcm$set(x)
## mcm$setinverse(inverse(x))

## Retrieve the matrix and the inverse
## mcm$get()
## mcm$getinverse()



## The second function, cacheSolve, accepts a matrix object as an unnamed
## argument and returns the inverse of the matrix. It will try to find a cached
## copy of the inverse but, if none exists, or if the matrix has changed,  it
## will compute it itself and will cache the result.

## Example of use:

## Use the object created above
## cacheSolve(mcm)




## This function takes a matrix as an argument and returns a four-member named
## list of functions that caches the matrix, returns the matrix, caches the
## inverse of the matrix, and returns the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        i<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i


    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function returns the inverse of a matrix stored in an object created by
## the makeCacheMatrix object above. It will try to retrieve the inverse from
## the cache but will calculate it itself if it can't find it in the cache or
## of the matrix has changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()

    ## Check that the matrix hasn't changed?

    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- inverse(data, ...)
    x$setinverse(i)
    i
}
