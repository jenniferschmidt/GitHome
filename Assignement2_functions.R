## This function is to take take an inverse of a matrix and store this value.
## If the inverse was  not calculated and stored then it will be calculated

## The first step is to get the inverse of the matrix
makeCacheMatrix<-function(x=matrix()){
        set<-function(y){
                x<<y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) m<<-solve
        getinverse<-function()m
        list(set=set, get=get,
             setinverse=setinverse
             getinverse=getinverse)
}
## Then cache the inverse of the maxtrix if not get the inverse. 
## and return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m <- x$setinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
