# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%% PLOTTING IN R DEMONSTRATION %%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Two sections below (demonstrating colors/tranparency, and demonstraring jitter)
# were taken from a gist shared by Michael Stemkovski; https://github.com/stemkov
# Credit goes to Michael for being the one to originally show me these tricks!

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ----Simulating data and demonstrating colors/transparency----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate dataset
time <- 1:1000
scatter_data <- time + rnorm(1000,1,1)*time*0.4 + rnorm(1000,1,50)
# This function just groups values 1:1000 into whole number bins (1,2,3)
make.groups <- function(x){
  add_noise <- x + rnorm(1,10,50)
  if(add_noise < 200) return(1)
  if(add_noise > 200 & add_noise < 600) return(2)
  else return(3)
}
scatter_data_groups <- sapply(time, make.groups)
# Create random year_data vector, sampling from larger year value set
years <- rep(2010:2018, each=300)
year_data <- years + rnorm(length(years),1,200) + 1:length(years)*0.6
years <- years[-c(400:600)]
year_data <- year_data[-c(400:600)]
# Generating a uniform distribution, and then setting values to binary based on condition
env <- runif(1000,1,100)
alive_or_not <- sapply(env, function(x) if((x + rnorm(1,10,25)) > 50) return(1) else return(0))

# Actual plotting stuff!
plot(scatter_data ~ time)
# Demonstration of pch parameter
plot.pch()
plot(scatter_data ~ time, pch=20)

# Colors, palettes, and transparency
plot(scatter_data ~ time, pch=20, col=rgb(0.3,0.6,0))
plot(scatter_data ~ time, pch=20, col=rgb(0.3,0.6,0,0.4)) # Using transparency (alpha argument)for dense scatter plots

library(RColorBrewer) # A library for some more fancy colors
old_mar <- par("mar") # Capturing default parameters (for "mar")
par(mar=c(3,4,2,2))

display.brewer.all() # Show the palettes from RColorBrewer
par(mar = old_mar)
# Group datasets by color
color_pallet <- brewer.pal(n=3, name="Set2")
group_colors <- sapply(scatter_data_groups, function(x) color_pallet[x])
plot(scatter_data ~ time, pch=20, col=group_colors) # group colors
# Group datasets by color, with transparency
group_colors_transparent <- adjustcolor(group_colors, alpha.f = 0.5)
plot(scatter_data ~ time, pch=20, col=group_colors_transparent) # transparent group colors
# Adding labels and legend
plot(scatter_data ~ time, pch=20, col=group_colors_transparent,
     xlab = "Environmental gradient",
     ylab = "Trait value")
legend("topleft", inset = 0.05,
       legend = c("Species A", "Species B", "Species C"),
       col=color_pallet,
       pch = 20, cex=0.9, pt.cex = 1,
       bty = "n")

# Demonstration of jitter
# Plotting data across discrete values
plot(year_data ~ years)
# Adding color
plot(year_data ~ years, pch=20, col=rgb(0.2,0,0.3,0.2),
     xlab = "Years", ylab = "Trait value")
# Adding jitter, to better show density
plot(year_data ~ jitter(years,0.9),
     pch=20, col=rgb(0.2,0,0.3,0.2),
     xlab = "Years", ylab = "Trait value") # adding jitter

# Plotting discrete values across a continuous variable
plot(alive_or_not ~ env)
# Adding color and jitter
plot(jitter(alive_or_not, 0.5) ~ env, pch=20, col= rgb(0.1,0.5,0.1,0.4),
     xlab = "Environmental gradient", ylab = "Survival")
# Adding prediction line
surv_model <- glm(alive_or_not ~ env, family = binomial(link = "logit"))
lines(predict(surv_model,data.frame(env = 1:100), type="response"),
      lwd=3, col = rgb(0.8,0,0)) # this line is better

# %%%%%%%%%%%%%%%%%%%%%%%
# ----Layout function----
# %%%%%%%%%%%%%%%%%%%%%%%
# Saving the default par setting. Useful if you're going to be messing around with stuff...
def.par <- par(no.readonly = TRUE)
# Define the layout you want as a matrix
lay.mat <- matrix(c(1,1,0,2), 2, 2, byrow = TRUE)
layout(lay.mat)
# Showing the plotting regions
layout.show(2)

# Randomly generate vectors
x <- rnorm(50); y <- rnorm(50)
# Make histogram objects from vectors, but don't plot them
xhist <- hist(x, breaks = seq(-3,3,0.5), plot = FALSE)
yhist <- hist(y, breaks = seq(-3,3,0.5), plot = FALSE)
top <- max(c(xhist$counts, yhist$counts))
xrange <- c(-3, 3); yrange <- c(-3, 3)
lay.mat <- layout(matrix(c(2,0,1,3),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
layout.show(lay.mat)
# Plot scatter plots and histograms, managing the margin parameter value
par(mar = c(3,3,1,1))
plot(x, y, xlim = xrange, ylim = yrange, xlab = "", ylab = "")
par(mar = c(0,3,1,1))
barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
par(mar = c(3,0,1,1))
barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ----Plotting means and standard deviations----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data(iris) # loading in iris trait data
# Calculating means and standard deviations
setosa_means <- colMeans(iris[which(iris$Species == "setosa"),-5])
setosa_sd <- apply(iris[which(iris$Species == "setosa"),-5],2,sd)

versicolor_means <- colMeans(iris[which(iris$Species == "versicolor"),-5])
versicolor_sd <- apply(iris[which(iris$Species == "versicolor"),-5],2,sd)

virginica_means <- colMeans(iris[which(iris$Species == "virginica"),-5])
virginica_sd <- apply(iris[which(iris$Species == "virginica"),-5],2,sd)

color_pallet <- brewer.pal(n=3, name="Dark2") #colors

# Setting plot ranges: these correspond to xlim/ylim values
y_range <- c(0,8)
x_range <- c(0.5,4.5)

# Build the plot, starting with an empty window
par(mfrow=c(1,1),
    mar=c(4.1, 4.1, 2.1, 2.1))
plot(1,type = "n", xaxt = "n",
     xlim = x_range, ylim = y_range,
     xlab = "", ylab = "Trait values")

# x axis
axis(1, at = c(1:4), labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"),
     tick=F)

poly_width <- 0.4
poly_color <- "gray"

# Drawing polygons and mean lines. Mostly aesthetic, except for the mean lines
for(i in 1:4){
  ## ****
  browser() # use to step through loop step-by-step; n = next line, q = quit; help = show all commands
  ## ****
  polygon(x = c(i-poly_width,i+poly_width,i+poly_width,i-poly_width),
          y = c(y_range[1]-3,y_range[1]-3,y_range[2]+3,y_range[2]+3),
          col = poly_color,
          border = F)

  lines(x = c(i-poly_width, i+poly_width),
        y = rep(mean(c(setosa_means[i],versicolor_means[i],virginica_means[i])), 2),
        lty=2)
}

# Plotting mean values
points(x = c(1:4)-0.2, y = setosa_means, pch = 20, col = color_pallet[1], cex=2)
points(x = c(1:4), y = versicolor_means, pch = 20, col = color_pallet[2], cex=2)
points(x = c(1:4)+0.2, y = virginica_means, pch = 20, col = color_pallet[3], cex=2)
# Plotting standard deviations using arrows function
arrows(c(1:4)-0.2,
       setosa_means-(setosa_sd/2),
       c(1:4)-0.2,
       setosa_means+(setosa_sd/2),
       lwd=1.5, angle=90, code=3, length=0.08, col = color_pallet[1])

arrows(c(1:4),
       versicolor_means-(versicolor_sd/2),
       c(1:4),
       versicolor_means+(versicolor_sd/2),
       lwd=1.5, angle=90, code=3, length=0.08, col = color_pallet[2])


arrows(c(1:4)+0.2,
       virginica_means-(virginica_sd/2),
       c(1:4)+0.2,
       virginica_means+(virginica_sd/2),
       lwd=1.5, angle=90, code=3, length=0.08, col = color_pallet[3])

# Adding legend
legend("topright", inset = 0.05, legend = c("Iris setosa","Iris versicolor","Iris virginica"),
       col=color_pallet, pch = c(20,20,20), cex=1, pt.cex = 2)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ----Heatmap for a value matrix----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setwd("/RAID1/IMLS_GCCO/Analysis/Stacks/denovo_finalAssemblies/QUAC/")

# Read in a table of Fst values, and make symmetrical (nrow=ncol)
QUAC.fst.mat <- as.matrix(read.table("output/populations_wild/populations.fst_summary.tsv", header=TRUE, row.names=1, sep = "\t"))
QUAC.fst.mat <- rbind(QUAC.fst.mat, rep(NA,5))
rownames(QUAC.fst.mat) <- colnames(QUAC.fst.mat) # Update row and column names

# Use image command to plot a heatmap
# First two arguments specify the boundaries of the heatmap; z provides actual values
# z is transposed in order to plot numeral values later on
image(x=1:ncol(QUAC.fst.mat), y=1:nrow(QUAC.fst.mat), z=t(QUAC.fst.mat), axes=FALSE, xlab="", ylab="",
      main="QUAC Fst Values: De novo (6,850 loci)")
grid(nx=ncol(QUAC.fst.mat), ny=nrow(QUAC.fst.mat), col="black", lty=1) # Add boundary lines
# Labels
axis(1, 1:ncol(QUAC.fst.mat), colnames(QUAC.fst.mat), cex.axis=1.2, tick=FALSE)
text(1, c(1:5), labels=rownames(QUAC.fst.mat), cex=1.2)
for(x in 1:ncol(QUAC.fst.mat)){
  for(y in 1:nrow(QUAC.fst.mat)){
    text(x, y, QUAC.fst.mat[y,x], cex=1.5)
  }
}

# %%%%%%%%%%%%%%%%%
# ----Materials----
# %%%%%%%%%%%%%%%%%
# This is a handy website that covers most of the "basic" types of R plots
# http://www.sthda.com/english/wiki/r-base-graphs
# Depending on topic, there might be a cheatsheet available for what you want to find out about
# https://www.rstudio.com/resources/cheatsheets/
