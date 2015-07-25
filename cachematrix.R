## The functions below allow the inverse of a square matrix to be cached,
## so that, in the future, the inverted matrix can be retrieved from cache
## rather than be recomputed every time the matrix needs to be inverted. The
## purpose is to save compute time.

## makeCacheMatrix creates functions that can (1) create a matrix object (set),
## (2) retrieve a matrix object (get), (3) cache the inverse of the matrix
## object (setInvMatrix), and (4) retrieve the cached inverse (getInvMatrix).

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y             ## changes value of matrix object
                inv <<- NULL        ## resets cached inverse to NULL
        }
        get <- function() x         ## retrieves matrix object
        setInvMatrix <- function(InvMatrix) inv <<- InvMatrix ## stores inverse 
        getInvMatrix <- function() inv          ## retrieves cached inverse
        list(set = set, get = get,  ## creates named list of functions
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

## cacheSolve returns the inverse of the matrix object returned by
## makeCacheMatrix above. If the inverse has already been calculated,
## the result will be returned from cache; if not, the inverse will be
## calculated and then stored in cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInvMatrix()
        if(!is.null(inv)) {                    ## if inverse exists in cache
                message("getting cached data") ## notify of cache access
                return(inv)                    ## return inverse from cache
        }
        matrixx <- x$get()                     ## else, get matrix object
        inv <- solve(matrixx)                  ## solve for inverse
        x$setInvMatrix(inv)                    ## store inverse in cache
        inv                                    ## return computed inverse
}
