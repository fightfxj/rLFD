# Problem discription
# 
# The Perceptron Learning Algorithm
#
# In this problem, you will create your own target function f and data set D 
# to see how the Perceptron Learning Algorithm works. Take d = 2 so you can 
# visualize the # problem, and assume X = [-1; 1] * [-1; 1] with uniform 
# probability of picking each # x \in X.
#
# In each run, choose a random line in the plane as your target function f (do 
# this by taking two random, uniformly distributed points in [-1; 1] * [-1; 1]
# and taking the line passing through them), where one side of the line maps 
# to +1 and the other maps to -1. Choose the inputs x_n of the data set as 
# random points (uniformly in X), and evaluate the target function on each x_n
# to get the corresponding output y_n.
#
# Run the Perceptron Learning Algorithm to find g and measure
# the difference between f and g as Pr(f(x) != g(x)) (you can either calculate
# this exactly, or approximate it by generating a sufficiently large separate 
# set of points to evaluate it). Repeat the experiment for 1000 runs (as 
# speciffied above) and take the average. Start the PLA with the weight vector 
# w being all zeros, and at each iteration have the algorithm choose a point 
# randomly from the set of misclassiffied points.

PLA <- function(N) {
  # generate two points for f (x1, x2, y1, y2)
  # f is determined by two points((f[1], f[3]), (f[2], f[4]))
  f <- runif(4, -1, 1)
  
  # +1 : y >= y1 + (y2 - y1)/(x2 - x1)(x - x1)
  # -1 : y >  y1 + (y2 - y1)/(x2 - x1)(x - x1)
  
  # generate N training data
  X <- runif(N, -1, 1)
  Y <- runif(N, -1, 1)
  label <- 2 * (Y >= f[3] + (f[4] - f[3]) / (f[2] - f[1]) * (X - f[1])) - 1
  
  w <- rep(0, 3)  	# weight in PLA, initial value of w is (0, 0, 0)
  itr.count <- 0		# count the iteration
  repeat {
	itr.count <- itr.count + 1
	
	# h: the results of PLA algorithm on the training data
    h <- sign(w[1] + X * w[2] + Y * w[3])
    if (sum(label != h) == 0)
		break;
	
    mis.X <- X[label != h]
    mis.Y <- Y[label != h]
	mis.label <- label[label != h]
	mis.point <- sample(1 : length(mis.X), 1)
    
	# use a random misclassified sample to improve the weights
	w <- w + mis.label[mis.point] * c(1, mis.X[mis.point], mis.Y[mis.point])
  }
  
  # use simulation to obtain the difference between f and g
  # the result stored in diff
  
  # test with a larger data set
  X <- runif(100 * N, -1, 1)
  Y <- runif(100 * N, -1, 1)
  label <- 2 * (Y >= f[3] + (f[4] - f[3]) / (f[2] - f[1]) * (X - f[1])) - 1
  h <- sign(w[1] + X * w[2] + Y * w[3])
  dif <- sum(label != h) / (100 * N)
  
  c(itr.count, dif)
}

Test.PLA <- function(N, runs = 1000) {
  stat <- matrix(nrow=0, ncol=2)
  for (i in 1 : runs) {
    stat <- rbind(stat, PLA(N))
  }
  apply(stat, 2, mean)
}
