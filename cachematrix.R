## Michael Vorndran
## 19.05.2015
## functions are calculating the inverse of a matrix and stores it in the cache.
## It is checked wheter the matrix has allready be calculated, 
## if so, than the inverted matrix is in the cache will be retrieved rather.
## if not, than the matrix inversion is calculated.


## function creates a special list which provides several function for caching 
## the matrix.
## When this function is called, the matrix inverse (mInv) is set to NULL
##  and other functions are loaded and assambled in a list

makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        get <- function() x
        setinv <- function(inv) mInv <<- inv
        getinv <- function() mInv
        list(get = get,
             setinv = setinv,
             getinv = getinv)
}


## function for calculating the inverse of a matrix.
## it is checked whether the matrix has allready be inverted, if, than take the 
## stored matrix, if not, than calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getinv()
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        mInv <- solve(data)
        x$setinv(mInv)
        mInv
}
