##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @return
##' @author
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <- NULL
    }

    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv

    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @param ...
##' @return
##' @author
cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## set.seed(1)
## mymat <- makeCacheMatrix(matrix(rnorm(1e6), 1e3))
## system.time(inv1 <- cacheSolve(mymat))
## system.time(inv2 <- cacheSolve(mymat))
## all.equal(inv1, inv2)
