## There are two functions in this file.

## The first, makeCacheMatrix, creates an object based on a numeric matrix, that
## can cache, and retreive, both the matrix and its inverse. If the matrix
## changes, it will automatically delete the cached inverse.

## Example of use:

## Create the object and then a matrix to use it with.
## mcm <- makeCacheMatrix()
## x   <- matrix(sample(16), 4, 4)

## Cache the matrix and its inverse.
## mcm$set_matrix(x)
## mcm$set_inverse(solve(x))

## Retrieve the matrix and its inverse.
## mcm$get_matrix()
## mcm$get_inverse()



## The second function in this file, cacheSolve, accepts one of the objects
## above as an unnamed argument and returns the inverse of the matrix. It will
## try to find a cached copy of the inverse but, if none exists, it will compute
## it itself and will cache the result.

## Example of use:

## Use the object created above as the argument, and return the inverse.
## cacheSolve(mcm)

## Change the matrix.
## mcm$set_matrix( matrix(sample(4), 2, 2) )

## Calculate, and return, a new inverse.
## cacheSolve(mcm)



## makeCacheMatrix
## This function takes a matrix as an argument and returns a four-member named
## list of functions that cache the matrix, return the matrix, cache the
## inverse of the matrix, and return the inverse of the matrix.

makeCacheMatrix <- function(m = matrix()) {

    # Store the inverse here.
    i <- NULL

    # Define the function to cache the matrix.
    set_matrix <- function(y) {

        # Store the matrix in the cache, as 'm'.
        m <<- y

        # If the matrix has changed then so has the inverse, 'i', so set that to
        # NULL in cache.
        i <<- NULL
    }

    # Define the function to return the matrix.
    get_matrix <- function() m

    # Define the function to cache the inverse of the matrix.
    # Per the specification we don't have to make sure the matrix is invertible.
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
## the cache but will calculate it itself if it can't find it.

cacheSolve <- function(x, ...) {

    # Look for a pre-calculated inverse in the cache.
    i <- x$get_inverse()

    # If the cached inverse is there, use it and we're done.
    if( !is.null(i) ) {
        message("Found inverse in cache.")
        return(i)
    }

    # Otherwise get the matrix from the cache...
    message("No inverse in cache, calculating.")
    m <- x$get_matrix()

    #   ...and calculate the inverse of that.
    i <- solve(m, ...)

    # Cache it!
    x$set_inverse(i)

    # Then return it.
    i
}
