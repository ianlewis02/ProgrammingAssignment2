## The following two functions combine to create a square matrix and
## make the inverse of the matrix available in a cache environment
##

## Function 1 - "makeCacheMatrix" creates a special "matrix" object that  
## can cache its inverse and returns functions used by "cacheSolve", to 
## get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {

	## "cacheSolve" utilises the function "solve()", which requires a 
	## square matrix i.e. number of rows equals number of columns.

	if (ncol(x)==nrow(x)){
  		## source matrix is square
		
		## initialise the matrix
		cached_inv <- NULL
		set <- function(y) {
			x <<- y
			cached_inv <<- NULL
		}
		
		## get the matrix
		get <- function() x

		## invert the matrix and store in cache
		setInverse <- function(inverse) cached_inv <<- inverse

		## return the inverted matrix from cache
		getInverse <- function() cached_inv

		## return the functions
		list(set = set, get = get,
			setInverse = setInverse, getInverse = getInverse)
	} else {
		## source matrix is not square		
		
		## issue warning to user	
		message("Error: the matrix is not square")
	}
}

## Function 2 - "cacheSolve" creates the inverse of the special "matrix" object 
## returned by "makeCacheMatrix". If the inverse has already been calculated 
## (and the matrix has not changed), then "cacheSolve" will retrieve the inverse 
## from the cache.

cacheSolve <- function(x = ...) {
	
	## get the inverse of the matrix stored in cache
	cached_inv <- x$getInverse()

	## is the inverted matrix in cache (i.e. "not NULL")?
      if(!is.null(cached_inv)) {
		## the inverted matrix is in the cache
		
		## return the inverted matrix
		return(cached_inv)
	} else {
		## the inverted matrix was not in cache

		## generate a new matrix to cache
		cached_new <- x$get()
		cached_inv <- solve(cached_new)
		
		## set the inverted matrix in cache
		x$setInverse(cached_inv)
		return(x)
	}
}
