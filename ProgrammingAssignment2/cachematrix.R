## Below are two functions that are used to create a special 
## object that stores a numeric vector and cache's its mean.


## The makeVector creates a special "vector", 
## which is a list containing a function to (1) set value vector
## (2) get value vector, (3) set value mean, and (4) get value mean

makeCacheMatrix <- function(x = matrix()) {

        mMat <- NULL
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x in the
                ## parent environment
                mMat <<- NULL ## re-initialize m in the parent environment to null
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) mMat <<- inverse ## set the cache m equal
        ## to the inverse of the matrix x
        getinverse <- function() mMat ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}



## The cacheSolve function calculates the mean of the special "vector" created
## with the makeCacheMatrix function, checking first to see if the mean has 
## already been calculated...

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cSol <- x$getinverse()
        if(!is.null(cSol)) {
                message("getting cached data")
                return(cSol)
        }
        data <- x$get()
        cSol <- solve(data, ...)
        x$setinverse(cSol)
        cSol
}

