# File: "cachematrix.R"
#
# This file contains two functions that help avoid recalculating the inverse of a matrix by saving the calculated result in a cache which can be accessed on repeated calls.
#
# IMPORTANT note about ##Optional: There are a few ##Optional features in this source file which are not required by the assignment but may be included to make the functions more robust. For grading the assignment, a reviewer may choose to consider or ignore the ##Optional part of the code. The functions fulfill the assignment requirements even without the ##Optional features. 
#
#
# As specified in the Programming Assignment 2 instructions, the functions are:
#       1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
#
#       This function "makeCacheMatrix" as implemented here ensures that:
#               - The input provided is a matrix     ##Optional check   
#               - The input provided is an invertible matrix    ##Optional check
#               - The input provided is not identical to the existing matrix, before overwriting the existing matrix. If it is identical then it avoids overwriting to save processing.
#
#       2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#
#       This function "cacheSolve" as implemented here ensures that:
#               - Function handles the presence of optional arguments by recalculating the inverse and updating the cache, if the number of arguments change from one function call to the next   
#               - If the number of arguments is not identical with the previous function call (if any) we recalculate the matrix inverse
#               - If the number of arguments is more than one (even if the number of arguments are same as the previous function call), we still need to recalculate inverse including both the cache matrix and the optional arguments
# 
#
#
# Following inline comments describe how the pieces of code work. 
#
#
# Package: "matrixcalc"
# Using the package "matrixcalc" is not required to fulfil the assignment requirements. However, this has been included as an optional feature to check whether the input matrix is invertible. Please note that not all matrices can be inverted and this check helps ensure that we have the right input. 
# 
# First we need to install the package, if it is not already installed. This line is commented as the package is already installed on my computer.
# install.packages("matrixcalc")        ##Optional: used for checking if the input matrix is invertible
# Next we need to load this package using library command
library("matrixcalc")                   ##Optional: used for checking if the input matrix is invertible


# Function: "makeCacheMatrix"
# This function takes a matrix object as an input and returns a special "matrix" object including a set of of variables and functions. The included variables are used to cache the matrix and its inverse, and the included functions are used to access or modify the included variables.
#
# Start function makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) { # The function is declared with a single matrix input

        # The following block of code is optional and it checks if the input is a matrix or not.
        if(!is.matrix(x)) {             ##Optional: used for checking if the input is a matrix
        
                message("Input is not a matrix") # Provides an informative message to help the user identify the problem
                
                return(NULL) # Returns NULL to ensure that any previous values in the returned matrix object are overridden. Without this if we execute: mat <- makeCacheMatrix(x) multiple times for different inputs and in the current execution if input "x" is not a matrix, then "mat" may still contain values from the previous run (if any) of this function. This helps wipe out any previous values in "mat".
        
        } # End if for checking that input is matrix
        
        # The following block of code is optional and it checks if the input matrix is invertible or not. A singular matrix is NOT invertible. The function "is.singular.matrix" exists in the R package "matrixcalc" included above.
        if(is.singular.matrix(x)){      ##Optional: used for checking if the input matrix is invertible
        
                message("Input is not an invertible matrix") # Provides an informative message to help the user identify the problem
                
                return(NULL)  # Returns NULL to ensure that any previous values in the returned matrix object are overridden. Without this if we execute: mat <- makeCacheMatrix(x) multiple times for different inputs and in the current execution if input "x" is not a matrix, then "mat" may still contain values from the previous run (if any) of this function. This helps wipe out any previous values in "mat".
        
        } # End if for checking invertible
        
        inv <- NULL # Initializes the matrix cache variable to NULL. A NULL will help identify if the matrix inverse needs to be calculated or whether it already exists.
        
        # Function: "set"
        # Following function is used to save the input matrix in the cache variable "x".
        # Start function set
        set <- function(y){ # Function declared with input y
                
                # The following 'if' checks if the new input matrix is identical with the already existing matrix in the cache. Helpful when we change the matrix by calling the set function multiple times. This check ensures that if we pass the same input matrix repeatedly, then we do not need to overwrite "x" or compute the "inv" again and the existing values are used. 
                
                if(!identical(x, y)){ # Check if the existing matrix in the cache "x" and the input matrix "y" are not identical before overwriting "x" and "inv"
                        
                        x <<- y # Overwrite existing cache matrix with the new input matrix
                        inv <<- NULL # Reset the inverse cache variable to NULL, to ensure that it is recalculated for the new input
                        
                } # End if for checking identical matrices
                
        } # End function set
        
        # Function: "get"
        # This function is used to extract the matrix stored in the cache variable "x"
        # Start function get
        get <- function() x
        # End function get
        
        # Function: "setinv"
        # This function is used to save the calculated inverse of the matrix in the cache variable "inv"
        # Start function setinv
        setinv <- function(inverse) inv <<- inverse
        # End function setinv
        
        # Function: "getinv"
        # This function is used to extract the matrix inverse stored in the cache variable "inv"
        # Start function getinv
        getinv <- function() inv
        # End function getinv
        
        # The following list is the last object in the function and will be returned as a result of this function
        # All the functions which are required to update and acccess the cache variables for the matrix and its inverse are included here such that they are available to the user
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
} # End function makeCacheMatrix


# Function: "cacheSolve"
# This function takes the special matrix object created by "makeCacheMatrix" function and returns the inverse of the cache matrix variable "x" contained in the input special "matrix" object. It also saves the computed inverse in the cache inverse variable of the input special matrix object. 
#
# Start function cacheSolve
cacheSolve <- function(x, ...) { # This function is declared with a matrix input "x" and a "..." operator in its arguments, which helps optional arguments to be passed to the "solve()" function used in the computation of the matrix inverse.
        
        # Because this function can be called repeatedly with or without optional arguments we need to ensure that it handles each scenario.
        # To provide an example:
        # If we call cacheSolve(a) it computes and saves the inverse of matrix "a" in "inv".
        # Next, if we call cacheSolve(a, b), then we need to check if the number of arguments have changed from 1 to 2 and ensure that both "a" and "b" are passed on as arguments to the function solve() to recompute and save the matrix inverse.
        # Again if we call cacheSolve(a), then the check identifies that the number of arguments have changed again and value of the inverse will need to be recalculated.
        # Please read the documentation for the solve() function to understand the difference in its behaviour in case of 1 or 2 arguments
        #
        # A cache variable "arg" created is used in handling any change in optional arguments between repeated function calls
        if(!exists("arg")) arg <<- nargs() # Creates and sets the cache variable "arg" during the first function call in this function's parent environment (using operator "<<-")
        
        if(identical(arg, nargs())){ # checks if the number of arguments (using "nargs()") in the current function call is identical to the number of arguments ("arg") in the previous function call
                if(arg == 1){ # Given that the number of arguments is same as previous function call, checks if that number of arguments is "1" (which means that we do not need to recalculate the inverse)
                        inv <- x$getinv() # Extract the inverse value from cache of the special matrix object "x"
                        
                        if(!is.null(inv)){ # Checks if the extracted value is not NULL and thus can be used
                              
                                message("getting cached data") # message to user that cached data is being returned
                                
                                return(inv) # Return the cached inverse of the matrix
                                
                        } # End if check for NULL inverse
                        
                }# End if check for a single argument
                
        }# End if check for number of arguments identical with previous function call (if any)
          
        else { # code in the 'else' block executes only if the arguments are not identical between two function calls, if there was a previous call 
                arg <<- nargs() # updates the cache "arg" variable to the current number of arguments using "nargs()"
        } # End else
        
        # Following block of code executes in the following scenarios:
        #       - If the number of arguments is not identical with the previous function call (if any), as we need to recalculate the matrix inverse
        #       - If the number of arguments is more than one (even if the number of arguments are same as the previous function call), as then we need to execute the "solve()" function again including both the cache matrix and the optional arguments
        #
        data <- x$get() # Extract cache matrix
        inv <- solve(data, ...) # Calculate the inverse again
        x$setinv(inv) # Save the calculated inverse in the special matrix object's cache variable "inv"
        return(inv) # return the recalculated inverse of the matrix
        
} # End function cacheSolve
