###############################################
###############################################
###############################################
#Distributions.R

#READ.ME:
#This file creates a couple of well-known
#distribution plots (normal and poisson 
#with option of doing more), and presents
#them with respect to number of samples in each curve

#Create directories
if(!dir.exists("./images")){dir.create("./images")}

#Define sample size and number of plots to report
Num_reps <- 1000000
Num_plots <- 100

#Want a couple of very small sample sizes before you go up to dividing Num_reps/Num_plots
Custom_samp_size <- c(5, 7, 9, 11, 15, 20, 30, 50, 100, 200, 300, 400, 500, 1000)



############################################################
############################################################
########Gaussian distribution
Gaus_sd <- 1

#Gaussian directories
if(!dir.exists("./images/Gaussian")){dir.create("./images/Gaussian")}
if(!dir.exists("./images/Gaussian/fixedscale")){dir.create("./images/Gaussian/fixedscale")}
if(!dir.exists("./images/Gaussian/dynamicscale")){dir.create("./images/Gaussian/dynamicscale")}

#Generate a random point from a normal distribution
x <- rnorm(Num_reps, mean=0, sd=Gaus_sd)
length_x <- as.numeric(length(x)) #Need to convert length to numeric to store more values

#Generate sample sizes for each plot
new_size <- NULL
for (j in 1:Num_plots){temp <- length_x*j/Num_plots; new_size <- append(new_size, temp)}; rm(temp) #For full sample size, j = Num_plots

new_size <- append(Custom_samp_size, new_size)
new_size <- unique(new_size)
length_new_size <- as.numeric(length(new_size)) #Need to convert length to numeric to store more values

for (i in 1:length_new_size){
  i <- as.numeric(i) #For very many reps, you need to convert length to numeric to store more values
  
  #Indexing sample size to use for the plot
  x_samp <- sample(x, size=new_size[i], replace = FALSE)
  
  #Making sure histogram encompasses smallest and largest value of x
  if(round(min(x_samp), digits=2) < min(x_samp)){min_break = round(min(x_samp), digits=2)}
  if(round(min(x_samp), digits=2) > min(x_samp)){min_break = round(min(x_samp), digits=2)-0.01}
  
  if(round(max(x_samp), digits=2) > max(x_samp)){max_break = round(max(x_samp), digits=2)}
  if(round(min(x_samp), digits=2) < max(x_samp)){max_break = round(max(x_samp), digits=2)+0.01}
  
  #Write out graphs - Dynamic scale
  png(paste0("./images/Gaussian/dynamicscale/", i, "Gaussian_SampSize", new_size[i], "_sd", Gaus_sd, ".png"), width = 800, height = 600)
  hist(x_samp, breaks = seq(from = min_break, to = max_break, by = 0.01), main = paste0("Sample size ", new_size[i]))
  dev.off()
  
  #Write out graphs - Fixed scale
  x_samp <- x_samp[x_samp < 4 & x_samp > -4]
  png(paste0("./images/Gaussian/fixedscale/", i, "Gaussian_SampSize", new_size[i], "_sd", Gaus_sd, ".png"), width = 800, height = 600)
  hist(x_samp, breaks = seq(from = -4, to = 4, by = 0.01), main = paste0("Sample size ", new_size[i]))
  dev.off()
}


############################################################
############################################################
########Poisson distribution

#Poisson works a bit different in that you get integer outputs and the shape changes more as lambda changes than as samp size changes
pois_lambda <- 1

#Poisson directories
if(!dir.exists("./images/Poisson")){dir.create("./images/Poisson")}
if(!dir.exists("./images/Poisson/fixedscale")){dir.create("./images/Poisson/fixedscale")}
if(!dir.exists("./images/Poisson/dynamicscale")){dir.create("./images/Poisson/dynamicscale")}

#Generate a random point from a normal distribution
x <- rpois(Num_reps, lambda = pois_lambda)
length_x <- as.numeric(length(x)) #Need to convert length to numeric to store more values

#Generate sample sizes for each plot
new_size <- NULL
for (j in 1:Num_plots){temp <- length_x*j/Num_plots; new_size <- append(new_size, temp)}; rm(temp) #For full sample size, j = Num_plots

new_size <- append(Custom_samp_size, new_size)
new_size <- unique(new_size)
length_new_size <- as.numeric(length(new_size)) #Need to convert length to numeric to store more values


for (i in 1:length_new_size){
  i <- as.numeric(i) #For very many reps, you need to convert length to numeric to store more values
  
  #Indexing sample size to use for the plot
  x_samp <- sample(x, size=new_size[i], replace = FALSE)
  
  #Making sure histogram encompasses smallest and largest value of x
  if(round(min(x_samp) == 0)){min_break = min(x_samp)}
  if(round(min(x_samp) != 0)){min_break = min(x_samp)-1}
  
  max_break <- max(x_samp)
  
  #Write out graphs - Dynamic scale
  png(paste0("./images/Poisson/dynamicscale/", i, "Poisson_SampSize", new_size[i], "_lambda", pois_lambda, ".png"), width = 800, height = 600)
  hist(x_samp, breaks = seq(from = min_break, to = max_break, by = 1), main = paste0("Sample size ", new_size[i]))
  dev.off()
  
  #Write out graphs - Fixed scale
  x_samp <- x_samp[x_samp < ceiling(1.5*pois_lambda + 9)]
  png(paste0("./images/Poisson/fixedscale/", i, "Poisson_SampSize", new_size[i], "_lambda", pois_lambda, ".png"), width = 800, height = 600)
  hist(x_samp, breaks = seq(from = 0, to = ceiling(1.5*pois_lambda + 9), by = 1), main = paste0("Sample size ", new_size[i]))
  dev.off()
}



############################################################
############################################################
#######Gamma distribution
#Gamma is a cool one you could try in the future
?rgamma

#y <- rgamma(10000, shape=1, scale = 2)
#hist(y)


############################################################
############################################################
#Binomial is not very informative
?rbinom
#x <- rbinom(100, size = 2, prob=0.5)


