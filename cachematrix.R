##Functions that cache the inverse of a matrix

##makeCacheMatrix: matrix object that can cache its inverse

    makeCacheMatrix <- function(x = matrix()) {
            
			##set the value of the matrix
			inv <- NULL                                       
            set <- function(matrix) {
                    x <<- matrix
                    inv <<- NULL
            }
            
			##get the value of the matrix
			get <- function() x                               
            
			##set the value of the inverse
			setinverse <- function(inverse) inv <<- inverse     
            
			##get the value of the inverse
			getinverse <- function() inv                           
            list(set = set, get = get,
                 setmean = setmean,
                 getmean = getmean)
    }

##cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix	
    cacheSolve <- function(x, ...) {
            
			##compute whether inverse is calculated
			inv <- x$getinverse()           
            
			##if yes, get the inverse from the cache and skip computation
			if(!is.null(inv)) {             
                    message("getting cached data")
                    return(inv)
            }
            
			##else, compute inverse of matrix and set the value of the inverse via setinverse
			data <- x$get()                 
            inv <- inverse(data, ...)
            x$setinverse(inv)
            inv
    }
	