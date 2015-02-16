## Mimic approach used in example "makeVector" by creating a "special" matrix object 
## that is actually a set of functions to set and the matrix and to set and get the
## matrix inverse

## For a given matrix x, creates a "special" matrix of functions allowing for easy
## retrevial of a cached value for the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set <- function(y) {
		x<<-y
		inv<<-NULL
	}
	get <- function() x
	setinv <- function(invSolve) inv <<-invSolve
	getinv <- function() inv
	list(set=set,get=get,setinv=setinv, getinv=getinv)
}


## If the matrix inverse is already computed, return the already computed value. 
## Otherwise, compute the inverse

cacheSolve <- function(x, ...) {
	inv<-x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	## Return a matrix that is the inverse of 'x'
	data<-x$get()
	inv<-solve(data)
	x$setinv(inv)
        
}
