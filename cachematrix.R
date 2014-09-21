## [Put comments here that describe what your functions do]
## in your R-prompt run this program by giving as argument an invertible matrix:
## cacheSolve(makeCacheMatrix(matrix(c(4,2,7,6), 2,2)))
#and that should return the inverse of the matrix 
#M
#     [,1] [,2]
#[1,]    4    2
#[2,]    7    6
#M^-1 = 
#     [,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4

#cacheSolve is the main function, and it takes as arguments the 
# function makeCacheMatrix
#that takes an invertible matrix as its own argument
# first cacheSolve runs makeCacheMatrix's getmatrix()
# if this value had already being calculated (ie, the value is not NULL) 
# and cached then 
#it will just return that value and exit the cacheSolve function
# if not, then it will get the matrix with makeCacheMatrix's get()
# invert it with solve
#set the new inversion value with makeCacheMatrix's setmatrix
# and return it 

#makeCacheMatrix caches the inversion value of the input matrix
makeCacheMatrix <- function(x = matrix()) {
	#initialize m, which will be the inverse, to NULL
	m <- NULL

	#read the matrix tht is given as argument
	get <- function() x

	# set m = mat, where mat is an argument
	setmatrix <- function(mat) m <<- mat

	#read the m value, which is the inversion of the ,atrix
	getmatrix <- function() m

	#return a list of your getters and setters to the calling function
	list(get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}

#cacheSolve function is the main function
#that calls the cache, or calculates the inversion value
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
        
	# get the inversion: this will return NULL or some value
      mc <- x$getmatrix()

	#if a value exists for the inversion, just return it and exit this function
      if(!is.null(mc))
      {
      	message("getting cached data")
        	return(mc)
      }

	#if the inversion was NULL,
	#get the matrix
      data <- x$get()

	#calculate the inversion with solve
      mc <- solve(data, ...)

	#use the makeCacheMatrix funtion to set the inversion to cache
      x$setmatrix(mc)

	#return the inversion
      mc
}
