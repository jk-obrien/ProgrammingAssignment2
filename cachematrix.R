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

## Change the matrix
## mcm$set(matrix(sample(4),2,2))

## Calculate a new inverse
## cacheSolve(mcm)


## This function takes a matrix as an argument and returns a four-member named
## list of functions that caches the matrix, returns the matrix, caches the
## inverse of the matrix, and returns the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

    # Store the inverse here.
    i <- NULL

    # Define the function to cache the matrix.
    set <- function(y) {

        # Store the matrix in the cache (parent environments).
        x <<- y

        # If the matrix has changed then so has the inverse.
        i <<- NULL
    }

    # Define the function to return the matrix.
    get <- function() x

    # Define the function to cache the inverse of the matrix.
    setinverse <- function(inverse) i <<- inverse

    # Define the function to return the inverse of the matrix.
    getinverse <- function() i

    # Return a named list of the four functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function returns the inverse of a matrix stored in an object created by
## the makeCacheMatrix object above. It will try to retrieve the inverse from
## the cache but will calculate it itself if it can't find it in the cache or
## of the matrix has changed.

cacheSolve <- function(x, ...) {

    # Return a matrix that is the inverse of 'x'.
    i <- x$getinverse()

    ## Check that the matrix hasn't changed?

    # If the cached inverse is there, use it.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    # Otherwise get the matrix from the cache...
    data <- x$get()

    #   ...and calculate the inverse of that.
    i <- solve(data, ...)

    # Cache it!
    x$setinverse(i)

    # Then return it.
    i
}
