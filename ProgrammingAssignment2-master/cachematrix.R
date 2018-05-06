## I am writing a couple of functions that happen to cache the inverse of  a matrix. This is a programming assignment in R.


## This particular function creates a special matrix that can hopefully cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	## to initialize the inverse property
	i<-NULL
	
	## then to set the matrix up
	set <-function(matrix) {
		  m<<-matrix
		  i<<-NULL
	}
    ## to retrieve the matrix
    get<-function(){
    	## return the matrix as above
    	m
    }
    ## to set the inverse of the matrix
    setInverse<- function(inverse){
    	i<<-inverse
    }
    ##method to get the inverse of the matrix
    getInverse<- function() {
    	##Return the inverse property
    	i
    	##Return a list of the commands
    	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    }
}


## Compute the inverse of the special matrix as identified above. If correctly calculated, then "cachesolve" should retrieve the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        ## return the inverse if already set
        if(!is.null(m)){
        	message("getting cached data")
        }
        ##get the matrix from our object
        data<-x$get()
        ##calculate the inverse using matrix multiplication
        m<-solve(data) %*% data
        ##set the inverse
        x$setInverse(m)
        ##return the matrix
        m
}
