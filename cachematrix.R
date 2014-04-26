makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL # assign NULL value for inverse matrix
    set <- function(y) { # set values through function
            x <<- y
            inv_mat <<- NULL
    }
    get <- function() x # get data from x variable
    set_inv_mat <- function(solve_mat) inv_mat <<- solve_mat 
    # cache inverse matrix 

    get_inv_mat <- function() inv_mat
    # get data from inverse matrix

    list(set = set, get = get, set_inv_mat = set_inv_mat,
         get_inv_mat = get_inv_mat)
    # return a list that contains all nested functions from
    # makeCacheMatrix

}

cacheSolve <- function(x, ...) {
    inv_mat <- x$get_inv_mat() # assign data from get_inv_mat function
    if(!is.null(inv_mat)) {
            message("getting cached data")
            return(inv_mat)
    } # control if inverse matrix is cached in memory with a message

    data <- x$get() # assign x matrix to data
    inv_mat <- solve(data, ...) # solve matrix to inverse matrix
    x$set_inv_mat(inv_mat) # cache inverse matrix through set_inv_mat
    # function
    inv_mat # return inverse matrix
}
