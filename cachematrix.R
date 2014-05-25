##' Create a cached matrix
##'
##'
##' @param x matrix to inverse
##' @return a cachedmatrix object which is a "special" list
##' @examples
##' set.seed(1)
##' mymat <- makeCacheMatrix(matrix(rnorm(1e6), 1e3))
##' system.time(inv1 <- cacheSolve(mymat))
##' system.time(inv2 <- cacheSolve(mymat))
##' @author Ahmadou dicko
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## initialize the inverse

    set <- function(y) {
        x <<- y
        inv <- NULL
    }

    ### Methods
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv

    ### Results is a list
    res <- list(set = set,
                get = get,
                setinv = setinv,
                getinv = getinv)

    ### Create a class which is list (inheritance)
    ### This way our cacheSolve function can operate only on "cachedmatrix"
    class(res) <- c("cachedmatrix", "list")
    res
}


##' Get the inverse of a cached matrix
##'
##'
##' @param x
##' @param ...
##' @examples
##' set.seed(1)
##' mymat <- makeCacheMatrix(matrix(rnorm(1e6), 1e3))
##' system.time(inv1 <- cacheSolve(mymat))
##' system.time(inv2 <- cacheSolve(mymat))
##' @author Ahmadou Dicko
cacheSolve <- function(x, ...) {
    ## Check if x is of class cachedmatrix
    ## if not stop the function
    stopifnot(inherits(x, "cachedmatrix"))
    ## get the inverse if there's one
    inv <- x$getinv()

    ## if inverse is present then cache it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Compute the inverse if not in the cachedmatrix object
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


## Example (uncomment the following)
## set.seed(1)
## mymat <- makeCacheMatrix(matrix(rnorm(1e6), 1e3))
## class(mymat)
## system.time(inv1 <- cacheSolve(mymat))
## system.time(inv2 <- cacheSolve(mymat))
## all.equal(inv1, inv2)
