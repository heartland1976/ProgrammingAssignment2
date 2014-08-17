## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        ## defines a set function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ## defines a get function
        get <- function() x

        ## defines a set function to set the inverse value of a matrix 
        setmean <- function(mean) m <<- mean

	# defines a get function to get the inverse of a matrix 
        getmean <- function() m

        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Calls the getmean function defined in makeCacheMatrix and checks if the inverse has already been calculated
	m <- x$getmean()

	## If the inverse value is there, we return it from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	## If we reach here, the inverse has not been cached yet
        data <- x$get()

	## We calculate the inverse value for the matrix
        m <- solve(data, ...)

	## We cache this inverse value 
        x$setmean(m)

	## Return the inverse value
        m
}
