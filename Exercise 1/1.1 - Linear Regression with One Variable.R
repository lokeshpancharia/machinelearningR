#### Make sure that current working directory is where the "ex1data1.txt" is stored
### Reading and making sense of the data

data = read.table("ex1data1.txt", sep = ",") # reading the data with separator being the comma
str(data)
# 'data.frame':	97 obs. of  2 variables:
 # $ V1: num  6.11 5.53 8.52 7 5.86 ...
 # $ V2: num  17.59 9.13 13.66 11.85 6.82 ...

# Notice that the column names are not provided in the original data.
# To define the column names
colnames(data) = c("Population", "Profit")

# Plotting the data
plot(data$Population, data$Profit)

### Linear Regression with one variable

# Initialize the vector theta, learning rate alpha and iterations
theta = c(0,0)
alpha = 0.01
iterations = 1500

# Adding the first column to be x0
data = cbind(x0 = 1, data)

## Define a function to compute the cost J(theta)
computeCost <- function(X, y, theta) {
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

# Running the computeCost function
computeCost(data[1:2], data[3], theta)

## Running the Gradient Descent Algorithm
gradientDescent <- function(X, y, theta, alpha, num_iters) {
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
		
		theta[1] = theta[1] - alpha * (1/m) * sum(((X %*% theta) - y))
		theta[2] = theta[2] - alpha * (1/m) * sum(((X %*% theta) - y) * X[,2])
		
		# ==============================================================
		
	
		# Save the cost of J in every iteration
		J_history[iter] = computeCost(X, y, theta)
		
	}
	
	# Return theta and J_history
	list(theta, J_history)
	
}

ans = gradientDescent(data[1:2], data[3], theta, alpha, iterations)

# Saving the theta into theta object
theta = ans[[1]]

# Saving the Cost function history
J_history = ans[[2]]


# Source: http://pingax.com/linear-regression-with-r-step-by-step-implementation-part-2/
