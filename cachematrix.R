## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Object: create a special function to cache the inverse of a matrix
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(cmatrix = matrix(),...) {
	inverseM <- NULL
	setMatrix <- function(x) {
		cmatrix <<- x
		inverseM <- NULL
	}
	getMatrix <- function() cmatrix
	setInverse <- function(m) inverseM <<- m
	getInverse <- function() inverseM
	list(setMatrix = setMatrix, getMatrix = getMatrix,
		setInverse = setInverse, getInverse =  getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(cmatrix,...) {
	M <- cmatrix$getInverse()
	if(!is.null(M)) 
	{
		message("Retriving cached inverse Matrix")
		return(M)
	}
	message("calculating inverse matrix")
	M <- solve(cmatrix$getMatrix())
	cmatrix$setInverse(M)
	M
}
