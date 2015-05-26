#### Make sure that current working directory is where the "ex1data1.txt" is stored
### Reading and making sense of the data

data = read.table("ex1data2.txt", sep = ",") # reading the data with separator being the comma
# 'data.frame':	47 obs. of  3 variables:
 # $ V1: int  2104 1600 2400 1416 3000 1985 1534 1427 1380 1494 ...
 # $ V2: int  3 3 3 2 4 4 3 3 3 3 ...
 # $ V3: int  399900 329900 369000 232000 539900 299900 314900 198999 212000 242500 ...

# Define the Column Names
colnames(data) = c("Size", "NumBedrooms", "Price")

# Define the X
X = data[1:2]

X### Feature Normalization
featureNormalizeLoop <- function(X) {
	# Returns a normalize version of X where the mean value of each feature is 0
	# and the standard deviation is 1. This is often the good preprocessing step
	# to do when working with learning algorithms.
	
	X_norm = X
	
	# ==================== Your CODE HERE ==========================
	
	for(iter in 1:ncol(X_norm)) {
		X_norm[iter] = (X_norm[iter] - mean(as.matrix(X_norm[iter]))) / sd(as.matrix(X_norm[iter]))
	}
	
	# ==============================================================

	# Return X_norm
	X_norm
}

# Load the required package for vectorized implementations
library(matrixStats)

# Vectorized Implementations of Feature Normalization
featureNormalizeVectorized <- function(X) {
	# Returns a normalize version of X where the mean value of each feature is 0
	# and the standard deviation is 1. This is often the good preprocessing step
	# to do when working with learning algorithms.
	
	# This function uses the vectorized implementations instead of iterations
	
	# Convert X into matrix, and store in X
	X = as.matrix(X)
	X_norm = X
	
	# ==================== Your CODE HERE ==========================
	
	X_norm = (X - colMeans(X)) / colSds(X)
	
	# ==============================================================

	# Return X_norm
	X_norm
}

X_norm = featureNormalizeLoop(X)

# Adding the first column to be x0
X_norm = cbind(x0 = 1, X_norm)

### Gradient descent for multiple linear regression
# initialize variables
theta = rep(0, ncol(X_norm))
alpha = 0.01
iterations = 1500

## Define a function to compute the cost J(theta)
computeCostMulti <- function(X, y, theta) {
    # Compute the cost of using theta as the parameter for linear regression
    # to fit the data points in X and y
    
    # initialize some useful values
    m = nrow(y) # Number of training examples
    J = 0;
    
    # ==================== Your CODE HERE ==========================
    
    #Convert X and Y to matrix
    X = as.matrix(X)
    y = as.matrix(y)
    
    # Find the h(x) for each x using dot product multiplication %*%
    h = X %*% theta
    
    # Finding the cost by summing the squared differences
    # of h - y divide by 2m
    J = sum((h-y)^2) / (2*m)
    
    # ==============================================================
    
    # Return J
    J
}

gradientDescentMulti <- function(X, y, theta, alpha, num_iters) {
    # Performs gradient descent to learn theta
    # The algorithm updates theta by taking num_iters gradient steps
    # with learning rate alpha
    
    # Returns a list of theta, and history of cost function per iterations
    
    # Initialize some useful values
    m = nrow(y) # Number of training examples
    J_history = rep(1, num_iters)
    
    for(iter in 1:num_iters) {
        # ==================== Your CODE HERE ==========================
        
        # Convert X and y to matrix
        X = as.matrix(X)
        y = as.matrix(y)
        
        for(feature in 1:ncol(X)) {
            theta[feature] = theta[feature] - alpha * (1/m) * sum(((X %*% theta) - y) * X[, feature])
        }
        
        # ==============================================================
        
        
        # Save the cost of J in every iteration
        J_history[iter] = computeCostMulti(X, y, theta)
        
    }
    
    # Return theta and J_history
    list(theta, J_history)
}

ans = gradientDescentMulti(X_norm, data[3], theta, alpha, iterations)
theta = ans[[1]]

# Load library
library(MASS)
# Create another function for Vectorized Multiple Linear Regression Implementation
gradientDescentMultiV <- function(X, y) {
    # Performs gradient descent based on vectorized implementations
    
    # Convert X and y as matrix
    X = as.matrix(X)
    y = as.matrix(y)
    
    theta = ginv(t(X) %*% X) %*% t(X) %*% y
    theta
}

thetaV = gradientDescentMultiV(X_norm, data[3])

## Compare with multiple linear regression in R
