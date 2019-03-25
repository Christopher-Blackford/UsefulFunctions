#GLM

x1 <- rep(c(1:10), each = 4)
x1 <- x1 + runif(length(x1), min = -0.5, max = 0.5)
x1 <- sort(x1); x1


######Linear
y1 <- c((5*x1) + rnorm(length(x1), mean = 0, sd = sqrt(x1)))


######Quadratic
y2 <- c(x1^2 + rnorm(length(x1), mean = 0, sd = sqrt(x1)))

######Parabolic
y3 <- -x1*(x1-10) + rnorm(length(x1), mean = 0, sd = sqrt(x1))


######Logistic growth - 2 ways to do it

###Discrete
K <- 100; r <- 1.4; initial_pop <- 1;
time_steps <- 10

log_growth <- initial_pop
for (i in 1:time_steps){log_growth[i+1] <- log_growth[i]*(1 + r*(K - log_growth[i])/K)}; log_growth <- log_growth[-1]

y4 <- rep(log_growth, each = 4)
y4 <- y4 + rnorm(length(y4), mean = 0, sd = sqrt(y4/2))


###Continuous
require(rootSolve)
require(deSolve)

LogGrowth=function(t,y,parameters){ 
  ## Variables
  N=y[1];
  ## Parameters
  r = parameters[1];
  K = parameters[2];
  ## Ordinary differential equations
  dNdt <- r*N*((K-N)/K)
  return(list(dNdt))}

#Paramater values (expect p and beta because that's what we're testing)
r <- 5
K <- 100
parameters=c(r,K)

## Initial state
variables0=c(N0=1)

output <- lsoda(y = variables0,    # intial values  
             times = seq(from = 1, to = 4, by = 0.1), # time vector
             func = LogGrowth,   # model
             parms = parameters) # constant parameters

output <- data.frame(output)
plot(output$time, output$N0)

x4 <- rep(output$time, each=2); y4 <- rep(output$N0, each=2)

y4 <- y4 + rnorm(length(y4), mean = 0, sd = sqrt(y4/2))


#####Plot

png("glm example output.png", width = 800, height = 600)

par(mfrow = c(2,2), mai = c(0.1, 0.1, 0.1, 0.1), cex = 1.25)

plot(x1, y1, xaxt='n', yaxt='n', xlab = "", ylab = "")
abline(lm(y1~x1))


plot(x1, y2, xaxt='n', yaxt='n', xlab = "", ylab = "")
lines(x1^2~x1)


plot(x1, y3, xaxt='n', yaxt='n', xlab = "", ylab = "")
lines(-x1*(x1-10)~x1)


plot(x4, y4, xaxt='n', yaxt='n', xlab = "", ylab = "")
lines(output$time, output$N0)

dev.off()
