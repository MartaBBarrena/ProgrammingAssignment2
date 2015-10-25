# "makeCacheMatrix" function calculates the inverse matrix of a given matrix and keeps fixed its value for
# the future in case it was necessary.

makeCacheMatrix <- function(x = matrix()) {              # Creates the starting function
    m <- NULL                                            # Initialize matrix inverse value (m) as NULL
    
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x                                  
    setinverse <- function(solve) m <<- solve            # Calculates inverse matrix and fix it as m
    getinverse <- function () m
    list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# "cacheSolve" function look for the existing value of the inverse matrix in case it had been already 
#calculated recovering its value if it already exist or calculating if not.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){                  #if the inverse matrix already exists the existing value will be used
        message ("getting cached data")
        return(m)
    }
    data <- x$get()                   #if the inverse matrix don´t exists it will be obtained
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
## Return a matrix that is the inverse of 'x'