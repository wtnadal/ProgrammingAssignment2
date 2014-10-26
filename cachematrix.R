## this function is an example of lexical scoping.  From the website stackoverflow:
## "...in the context of R lexical scoping means that free variables in a function 
## (i.e. variables that are used in a function but not defined in the function) 
## are looked up in the parent environment of the function, as opposed to the caller"
 
## Matrix inversions can be resource and time intensive.  This example will use these scoping 
## rules to cache and perserve state to help optimize certain functions, e.g. matrix inversions.

## makeCacheMatrix: create a special "vector" or list containing a function to 
## set and get the value of the matrix; and set and get the value of the matrix's inversion'

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL               ## if m is NULL solve for the  matrix inversion and cache in m                       
        set <- function(y) {    ## set will initialize the function for a new matrix (y) and inversion
                x <<- y
                m <<- NULL      ## NULL tests ass nothing cached, do the inversion
        }
        
        get <- function() x     
        setinverse <- function(inverse) m <<- inverse       
        getinverse <- function() m       
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {         ## if exists use the cached inversion in m, else solve current matrix                
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)    ## the R solve function performs a matrix inversion
        x$setinverse(m)
        m
}
