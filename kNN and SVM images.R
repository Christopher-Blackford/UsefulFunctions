
if(!dir.exists("./images")){dir.create("./images")}

######
numb <- 8
stand_dev = 0.15

x_1 <- rnorm(numb, mean = 0.75, sd = stand_dev)
y_1 <- rnorm(numb, mean = 0.75, sd = stand_dev)

x_2 <- rnorm(numb, mean = 1, sd = stand_dev)
y_2 <- rnorm(numb, mean = 1.353553, sd = stand_dev)

x_3 <- rnorm(numb, mean = 1.25, sd = stand_dev)
y_3 <- rnorm(numb, mean = 0.75, sd = stand_dev)


png("./images/kNN_SVM.png", width = 12, height = 8, units = "in", res = 300)

par(bty="l")

plot(x_1, y_1, xlab = "", ylab = "",
     xaxt = "n", yaxt = "n",
     xlim = c(0.25,1.75), ylim = c(0.25,1.75),
     col = "red", pch = 19)

points(x_2, y_2, col = "blue", pch = 19)
points(x_3, y_3, col = "green", pch = 19)

points(1, 1, pch = 8, cex = 1.2)

dev.off()
