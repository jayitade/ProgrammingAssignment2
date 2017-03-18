#Matrix inverse is a costly computation. So without repeated calculating
#the same thing here inverse is being cached so that it can be found easily.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv1<-NULL
set<-function(y){
x<<-y
inv1<<-NULL
}
get<-function()x
setinverse<-function(inverse)inv1<<-inverse
getinverse<-function()inv1
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#The following funtion returns the inverse of a matrix. It first checks
#whether inverse is present or not. If not present then compute the inverse
#and sets the value in the cache.

cacheSolve <- function(x, ...) {
       inv1 <- x$getinverse()
    if(!is.null(inv1)) {
        message("getting cached data.")
        return(inv1)
    }
    data <- x$get()
    inv1 <- solve(data)
    x$setinverse(inv1)
    inv1
}
##sample run
##>x<-rbind(c(4,3),c(3,2))
##>p=makeCacheMatrix(x)
##> p$get()
 ##    [,1] [,2]
##[1,]    4    3
##[2,]    3    2
## >cacheSolve(x)

##      [,1] [,2]
##[1,]    -2    3
##[2,]    3    -4



