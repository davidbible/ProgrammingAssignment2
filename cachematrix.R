
## Put comments here that give an overall description
# of what your functions do

# i am creating two functions
# the first function creates a special 'matrix'
# that has the ability to cache it's own inverse,
# assuming a square invertible matrix

# the second function calculates the inverse of
# the special 'matrix' created from the first 
# function

# FUNCTION_1
makeCacheMatrix<-function(x=matrix())
{
  inv1<-NULL
  set<-function(z)
    {
      inv1<<-NULL
      x<<-z
    }
  get<-function() x
  getInverse<-function() inv1
  setInverse<-function(inverse) inv1<<-inverse
  list(set=set,
       setInverse=setInverse,
       get=get,
       getInverse=getInverse)
}


# ------------------------------------------------------------

## Write a short comment describing this function
# this second of two functions calculates the inverse
# of the special 'matrix' created from the first function

# FUNCTION_2
cacheSolve<-function(x, ...)
{
  inv1<-x$getInverse()
  if (!is.null(inv1))
  {
    return(inv1)
  }
  matrx<-x$get()
  inv1<-solve(matrx, ...)
  x$setInverse(inv1)
  inv1
}




