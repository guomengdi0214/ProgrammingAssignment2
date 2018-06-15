## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list (set=set, get=get, 
              setinverse=setinverse, getinverse=getinverse)	
}



cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()
        if (!is.null(inverse)){
                message("getting cached matrix")
                return(inverse)
        }
        matrix<-x$get()
        inverse<-solve(matrix)
        x$setinverse(inverse)
        inverse
}
