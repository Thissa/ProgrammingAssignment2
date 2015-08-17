
## These functions together return the inverse of a specifed matrix. If the inverse
## of the matrix has already been calculated, the inverse is retrieved from a stored
## cache as oppose to be recalculated

## makeCacheMatrix is passed a matrix, x and it returns a list of functions that we 
## define within the function.
## We set variable m to be null. 
## "Set" is defined as a function that is passed a variable y, makes x = y and sets m 
## to be null.
## "Get" is defined as a function that returns x
## "Setinverse" is defined as a function that is passed variable inverse and sets m
## equal to inverse
## "Getinverse" is defined as a function that returns m
## Our original function makeCacheMatrix then returns a list called outlist which 
## contains reference to the four functions defined above

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<- function()x
  setinverse<- function(inverse) m<<-inverse
  getinverse<-function()m
  outlist<<-list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
  #returns list
}

## cacheSolve returns the inverse of the matrix x, either by calculating it, or by 
## recalling it from a cache after it was previously computed

## The function is initially passed the matrix, x
## We then set m equal to the inverse of the matrix calculated in makeCacheMatrix by
## referencing "getinverse" in the globally available list, outlist. 
## If m is not null, we display the message 'getting cached data' and return the value
## of m stored in the cache
## If m is null, we set a variable data equal to the original matrix x, by way of 
## calling the "get" function from outlist.
## We then set m = to the inverse of data by using the solve function in R
## We then run the "setinverse" function (referenced from outlist, passing in m which
## we just set to be the inverse of x)
## m (either the just calculated inverse or the retrieved inverse of x) is then written 
## onto the screen

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
m<-outlist$getinverse()
if (!is.null(m)){
  message("getting cached data")
  return(m)
  }
  data<-outlist$get()
m<-solve(data, ...)
outlist$setinverse(m)
m
}

