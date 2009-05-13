source("divide.r")

hdata <- read.csv("happy.csv")
hdata$degree <- factor(hdata$degree, 
  levels = c("lt high school","high school","junior college", "bachelor",
             "graduate"))

pdf("heike-teaser.pdf", width = 8, height = 6)
# Mosaic plot of happiness and education
prodplot(hdata, ~ degree + happy)
# Mosiac plot of happiness, conditional on education
prodplot(hdata, ~ happy, ~ degree)
# Education conditional on happy
prodplot(hdata, ~ degree, ~ happy)

# Add some colour
prodplot(hdata, ~ happy, ~ degree) + aes(fill = happy)
prodplot(hdata, ~ happy, ~ degree) + aes(fill = degree)
prodplot(hdata, ~ happy, ~ degree) + aes(fill = .wt)
prodplot(hdata, ~ happy, ~ degree) + aes(fill = factor(level))

# Use stacked bars instead of mosaic plot
prodplot(hdata, ~ degree + happy, divider = stackedbar())
prodplot(hdata, ~ happy, ~ degree, divider = stackedbar())

# Use nested bars instead of mosaic plot
prodplot(hdata, ~ degree + happy, divider = nestedbar())
prodplot(hdata, ~ happy, ~ degree, divider = nestedbar())

prodplot(hdata, ~ degree + happy, ~ sex)
prodplot(hdata, ~ degree + happy, ~ sex, divider = ddecker())
dev.off()