#Distributions

#Create directories
if(!dir.exists("./images")){dir.create("./images")}
if(!dir.exists("./images/Gaussian")){dir.create("./images/Gaussian")}
if(!dir.exists("./images/Gaussian/fixedscale")){dir.create("./images/Gaussian/fixedscale")}
if(!dir.exists("./images/Gaussian/dynamicscale")){dir.create("./images/Gaussian/dynamicscale")}

#Define sample size and number of plots to report
Number_of_reps <- 1000000
Num_plots <- 1000

Custom_samp_size <- c(5, 7, 9, 11, 15, 20, 30, 50, 100, 200, 300, 400, 500)


########Gaussian
Gaus_sd <- 1

#Generate a random point from a normal distribution
x <- rnorm(Number_of_reps, mean=0, sd=Gaus_sd)
length_x <- as.numeric(length(x)) #Need to convert length to numeric to store more values

new_size <- NULL
for (j in 1:Num_plots){
  temp <- length_x*j/Num_plots
  new_size <- append(new_size, temp)}

new_size <- append(Custom_samp_size, new_size)
new_size <- unique(new_size)
length_new_size <- as.numeric(length(new_size)) #Need to convert length to numeric to store more values

for (i in 1:length_new_size){
  
  #For very many reps, you need to convert length to numeric to store more values
  i <- as.numeric(i)
  
  #Potentially move sample size outside of loop so you can get more smaller values and then just index the size inside loop?  
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


#######Poisson

#######Gamma

######Binomial is not very informative
?poisson

?rpois
?rbinom
#x <- rbinom(100, size = 2, prob=0.5)
?rgamma

rgamma(1000, shape=1, scale=2)



