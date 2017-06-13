#a pair of functions that cache the inverse of a matrix.


#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    Inv<-NULL
    set<-function(y){
        x<<-y
        Inv<<-NULL
    }
    get<-function()x
    setsolve<-function(solve)Inv<<-solve
    getsolve<-function()Inv
    list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    #Return a matrix that is the inverse of 'x'
    Inv<-x$getsolve()
    if(!is.null(Inv)){
        message("getting cached data")
        return(Inv)
    }
    data<-x$get()
    Inv<-solve(data,...)
    x$setsolve(Inv)
    Inv
}
