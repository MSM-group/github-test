# Remember to open R Project or set working directory using setwd()

# Read in the data
dat <- read.csv("data/iris_petals.csv")
petal_length <- dat$petlen
petal_width <- dat$petwidth

# Plot
plot(petal_length, petal_width)

# Write to file
pdf("output/test_plot.pdf")
plot(petal_length, petal_width)
dev.off()

#Thanks Serina! :)

#Hello
