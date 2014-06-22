##===================================================================
## makeCacheMatrix and cacheSolve are used together to calculate and
##    then cache the inverse of a matrix
## Assumptions: matrix will be invertible (and hence square)
## Adapted from functions written by rdpeng
## Last modified by drsarahc on June 22, 2014
##===================================================================

##===================================================================
##       makeCacheMatrix
##===================================================================
## Passed a matrix, this function returns a list of 4 functions:
##    $set to initialize x and m
##    $get to simply return x
##    $setinv to call the inversion function and return m
##    $getinv to simply return the cached inverted matrix, m
## No checks are performed as to the size or shape of the matrix
##===================================================================

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(cacheSolve) m <<- cacheSolve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##===================================================================
##       cacheSolve
##===================================================================
##  Passed a list of functions created by makeCacheMatrix,
##  this function returns m, the inverse of invertible matrix x
##  m is calculated or returned from cache based on value of x$getinv
##===================================================================

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    message("inverting and caching")
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}   

##===================================================================
##  Demonstrates calls to makeCacheMatrix and cacheSolve
##===================================================================

mat<-matrix(1:4,2,2)
print("original matrix is")
mat
lis<-makeCacheMatrix(mat)
for (i in 1:3)  {
    print(i)
    invmat<-cacheSolve(lis)
    invmat
    }

##========================= E O F ===================================
