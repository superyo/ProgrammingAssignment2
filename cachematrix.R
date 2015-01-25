
## In order to not having to calculate many times the inverse of a matrix.
## This combination of fuctions caches the inverse to calculate it only onece.






## The function makeCacheMatrix, teakes a matrix as an argument, 
## and creates a list of functions that are related to that matrix
## The value of the matrix and the value of its inverse (once it is calculated) stay in the function environment


makeCacheMatrix<-function(x=matrix()){   
	i<-NULL					#Resets the inverse
	set<-function(y) {			#Function 1: Sets a new value to the matrix (and resets de inverse)
		x<<-y				#The x and i are not in the function "set"'s environment
		i<<-NULL			#x and i are in the parent environment: "makeCacheMatrix"'s environment.
	}
	get<-function() x			#Function 2: Prints the value of the matrix.
	setinv<-function(inv) i<<-inv		#Function 3: Caches a value for the inverse of the matrix.
	getinv<-function() i			#Function 4: Prints the value of the matrix's inverse that was cached.
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The function cacheSolve will take as an argument the list created with the function "makeCacheMatrix"
## That list is related to a matrix.
## cacheSolve will check if the matrix inverse is stored in memory.
## If it is in memory will print it
## If its not in memory, will calculate it, store it and print it.


cacheSolve <- function(x, ...) {		
						
        i <- x$getinv()				
        if(!is.null(i)) {			# Checks if the inverse is in cache ...
                message("getting cached data")
                return(i)			# ...and prints it. End of function.
        }
        data <- x$get()					
        i <- solve(data)			# If it was not in cache, calculates the inverse...
        x$setinv(i)				# ...sets the inverse in cache for next time that is needed...
        i					# ...and prints it.
}




