## Program that will caclulate the inverse of the matrix
## If the matri has already been used before, it will pull from cache, else
## build cache and inverse

## getter setter for the cachcing the matrix
makeCacheMatrix <- function(x = matrix()) {
        o_Inv <- NULL
		#setter method that will set the value being passed
        set <- function(y) {
                x <<- y
                o_Inv <<- NULL
        }
		# the getter function tht will return the value of the object
        get <- function() x
		# sets that inverse of the matrix
        fnsetinverse <- function(oInv) o_Inv <<- oInv
		# gets the inverse of the matrix
        fngetinverse <- function() o_Inv
        list(set = set, get = get,
             fnsetinverse = fnsetinverse,
             fngetinverse = fngetinverse)
}


## solve command to inverse the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # checking if the cached value is present
        o_Inv <- x$fngetinverse()
        # if the value is cached return the cached value
        if(!is.null(o_Inv)) {
                message("getting cached data")
                return(o_Inv)
        }
        data <- x$get()
        #print(data)
        #calculating the inverse
        o_Inv <- solve(data, ...)
        #setting the value to cache
        x$fnsetinverse(o_Inv)
        #returning the inverse
        o_Inv
}
