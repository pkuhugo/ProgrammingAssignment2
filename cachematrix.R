## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #define the catch inv
        inv <- NULL
        set <- function(y){ 
                x <<- y ## assign the input matrix y to the variable x
                inv <<- NULL ## re-initialize inv in the parent evironment to NULL
        }
        get <- function() x ## return the matrix x
        setinv <- function(solve) inv<-solve ## set the cache m equal to the inverse of the matrix x
        getinv <- function() inv ## return the cached inverse of x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){ ## check if there's a cashed inverse value of matrix x
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...) ## calculate matrix inverse
        x$setinv(inv) ## set matrix inverse
        inv
}
