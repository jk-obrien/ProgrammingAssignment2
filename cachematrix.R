## There are two functions in this file.

## The first, makeCacheMatrix, creates an object based on a numeric matrix, that
## can compute the inverse of the matrix and that can cache, and retreive, both
## the matrix and its inverse.

## Example of use:

## x <- matrix(sample(16), 4, 4)

## Define the object.
## mcm <- makeCacheMatrix()

## Cache the matrix and its inverse.
## mcm$set_matrix(x)
## mcm$set_inverse(solve(x))

## Retrieve the matrix and its inverse.
## mcm$get_matrix()
## mcm$get_inverse()



## The second function, cacheSolve, accepts a matrix object as an unnamed
## argument and returns the inverse of the matrix. It will try to find a cached
## copy of the inverse but, if none exists, it will compute it itself and will
## cache the result.

## Example of use:

## Use the object created above as the argument.
## cacheSolve(mcm)

## Change the matrix.
## mcm$set_matrix( matrix(sample(4), 2, 2) )

## Calculate a new inverse.
## cacheSolve(mcm)



## makeCacheMatrix
## This function takes a matrix as an argument and returns a four-member named
## list of functions that cache the matrix, return the matrix, cache the
## inverse of the matrix, and return the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

    # Store the inverse here.
    i <- NULL

    # Define the function to cache the matrix.
    set_matrix <- function(y) {

        # Store the matrix in the cache (parent environments).
        x <<- y

        # If the matrix has changed then so has the inverse, so set it to NULL
        # in cache.
        i <<- NULL
    }

    # Define the function to return the matrix.
    get_matrix <- function() x

    # Define the function to cache the inverse of the matrix.
    set_inverse <- function(inverse) i <<- inverse

    # Define the function to return the inverse of the matrix.
    get_inverse <- function() i

    # Return a named list of the four functions.
    list(set_matrix  = set_matrix,
         get_matrix  = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)

}



## cacheSolve
## This function returns the inverse of a matrix stored in an object created by
## the makeCacheMatrix object above. It will try to retrieve the inverse from
## the cache but will calculate it itself if it can't find it in the cache or
## of the matrix has changed.

cacheSolve <- function(x, ...) {

    # Return a matrix that is the inverse of 'x'.
    i <- x$get_inverse()

    # If the cached inverse is there, use it.
    if(!is.null(i)) {
        message("Found inverse in cache.")
        return(i)
    }

    # Otherwise get the matrix from the cache...
    m <- x$get_matrix()

    #   ...and calculate the inverse of that.
    i <- solve(m, ...)

    # Cache it!
    x$set_inverse(i)

    # Then return it.
    i
}
