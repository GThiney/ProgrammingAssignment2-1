## The combined functions check if the matrix to invert is the same as the one
## in memory. If so, the inverted matrix has already been calculated and its
## value is simply returned. If not, the inverted matrix is calculated and 
## placed in the cache for further use.

# Example of use: for a 1000x1000 invertible elements matrix (1M elements):
#       mat <- matrix(sample(1:1000000), ncol=1000)
#       mat2 <- makeCacheMatrix(mat)
#       cacheSolve(mat2) 
# ==> returns the inverse of the square matrix mat

# The call of cacheSolve inverts the matrix  
# ==> 3.97s on my computer with the "system.time" command
# 
# If called a second time, cacheSolve reads the matrix in the cache
# ==> "0" seconds on my computer with the same command    


## Write a short comment describing this function
##      makeCacheMatrix takes a square invertible matrix as argument and returns 
##      and returns a list of 4 functions:
##              set sets the value of the matrix
##              get reads the value of the matrix
##              setcache sets the inverse of the matrix  
##              getcache reads the inverse of the matrix

makeCacheMatrix <- function(x=numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setcache <- function(solve) m <<- solve
        getcache <- function() m
        list(set = set, get = get, setcache=setcache, getcache=getcache)
}

## Write a short comment describing this function
##      cacheSolve takes the list returned by makeCacheMatrix as argument,
##      reads the value of the cache (getcache) and checks if it exists 
##      If it does, it returns the value in the cache, i.e. the inverted matrix.
##      IF it doesn't, it reads the initial matrix (get), inverts it and sets it
##      in the cache before returning it.

cacheSolve <- function(x, ...) {
        m <- x$getcache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m <- solve(data, ...)
        x$setcache(m)
        m
}
