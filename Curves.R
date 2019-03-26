#Curves

Number_of_reps <- 1000

x <- NULL
for (i in 1:Number_of_reps){
  
  x1 <- rnorm(1, mean=0, sd=1)
  
  x <- append(x,x1)
  
  if(length(x) == 100 | length(x) == 1000){
    hist(x, breaks = seq(from = round(min(x), digits=2), 
                         to = round(max(x), digits=2), by = 0.01))
  }
  }


      


