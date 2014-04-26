## This is a set of utility functions to allow for caching inverted matrixes.
## Solving a matrix is a potentially resource expensive
## operation for example in the case of dealing with huge matrixes.
##
## Example usage:
## > my_matrix <- makeCacheMatrix(matrix(1:4, 2))
## > cacheSolve(my_matrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(my_matrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## Solution notes:
## I have used more verbose variable names than those in the course examples
## which used single letters (x, y, m etc),
## as I would like to make more evident what each variable is for.



## This is a wrapper around the standard matrix type.
## Its purpose is to allow us to store two variables together, 
## a matrix and its inverse that will act as a cache.
makeCacheMatrix <- function(raw_matrix = matrix()) {
        inverse <- NULL
        set <- function(new_matrix) {
                # The <<- operator is for assigning to variable
                # in the parent scope.
                # Also, when setting a new matrix, delete the inverse cache!
                raw_matrix <<- new_matrix
                inverse <<- NULL
        }
        get <- function() raw_matrix
        # `setInverse` and `getInverse` are to be used by
        # the `cacheSolve` function
        setInverse <- function(new_inverse) inverse <<- new_inverse
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns a matrix that is the inverse of the given matrix
## This requires a cache object produced by the `makeCacheMatrix` function
## instead of a normal matrix type.
## This function should only caclulate the inverse matrix once,
## all subsequent calls using the same input should result in retrieving
## cached results.
cacheSolve <- function(cache_matrix, ...) {
        inverse <- cache_matrix$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- cache_matrix$get()
        inverse <- solve(data, ...)
        cache_matrix$setInverse(inverse)
        inverse
}
