#!/usr/bin/env Rscript

# Catching an Inverse of a Matrix
# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# Given a matrix
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix
# return a list not a matrix

makeCacheMatrix <- function (x = matrix()) {
	m <- NULL
	set <- function(y) {
	x <<- y
	m <<- NULL
	}
	get <- function() x

	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m 
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix
# check inverse has already been calculated
# Calculate if not or else return the inverse matrix

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
 	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
	
}



