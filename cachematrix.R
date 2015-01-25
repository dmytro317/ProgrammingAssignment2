## Module contains functions that facilitate caching inversed matrix

## Creates a list of functions that give access to a matrix itself
## and to its cached inversion
makeCacheMatrix <- function(x = matrix())
{
    inverse.x <- NULL

    # sets the matrix itself and clears the cache
    set <- function(y)
    {
        x <<- y
        inverse.x <<- NULL
    }

    # returns the matrix itself
    get <- function()
    {
        x
    }

    # sets the inverse of the matrix
    set.inverse <- function(inv)
    {
        inverse.x <<- inv
    }

    # returns inversed matrix
    get.inverse <- function()
    {
        inverse.x
    }

    list (
        set = set,
        get = get,
        set.inverse = set.inverse,
        get.inverse = get.inverse
    )
}

## Computes inverse of a matrix OR extracts it from cache.
cacheSolve <- function(x, ...)
{
    inverse.x <- x$get.inverse()

    if (!is.null(inverse.x))
    {
        message("getting cached data")
        return(inverse.x) ## return a matrix that is the inverse of 'x'
    }

    inverse.x <- solve(x$get(), ...)
    x$set.inverse(inverse.x)

    inverse.x ## return a matrix that is the inverse of 'x'
}
