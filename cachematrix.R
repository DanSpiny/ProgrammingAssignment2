# Pair of functions presented in this module
# perform caching the inverse of a matrix.

makeCacheMatrix <- function(stored_mat = matrix()) {
    # Creates a special "matrix" object that can cache its inverse
    #
    # Args: 
    #   x - square invertible matrix
    # Returns: 
    #   "matrix" object that can cashe the inverse version of x
    
    inv <- NULL
    
    set <- function(input_mat) {
        # given the matrix, sets the variables in this closure
        stored_mat <<- input_mat
        inv <<- NULL
    }
    
    get <- function(){
        # Gets the stored matrix
        stored_mat    
    }
    
    set_inv <- function(inv_mat){
        # Sets the cached inverse matrix of the stored one
        inv <<- inv_mat
    }
    
    get_inv <- function() {
        # Gets the cached inverse matrix of the stored one
        inv
    }

    
    # returns the list of getters and setters to work with special "matrix" obj
    list(get = get, set = set, get_inv = get_inv, set_inv = set_inv)
}


cacheSolve <- function(mat_obj, ...) {
    # Computes the inverse of the special "matrix"
    # returned by makeCacheMatrix abovea
    #
    # If the inverse has already been calculated (and the matrix has not changed),
    # then the cachesolve should retrieve the inverse from the cache.
    #
    # Args:
    #   mat_obj - special "matrix" object returned by makeCacheMatrix
    # Returns:
    #   inverse matrix of the one stored in the special "matrix" object mat_obj
    
    inv <- mat_obj$get_inv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mat_obj$get()
    inv <- solve(data, ...)
    mat_obj$set_inv(inv)
    inv
}
