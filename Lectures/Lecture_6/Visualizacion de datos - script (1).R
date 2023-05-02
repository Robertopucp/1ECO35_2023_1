
#----
# Data visualiazacion
#----

# Example: Bike sharing Chicago

Chicago.agg <- read.csv("Chicago.agg.csv")

# Investigate the data by displaying the structure:
str(Chicago.agg)

#----
# Exploratory graphs
#----

# (Default) histogramm:
hist(Chicago.agg$tripduration)

# (Default) boxplot:
boxplot(tripduration ~ events, data = Chicago.agg)

# (Default) scatterplot:
plot(tripduration ~ temperature, data = Chicago.agg)


#----
# Illustrative graphs
#----

#----
# Customizations
#----

# (As usual) perform some data preparation tasks fist:
# 1. transform fahrenheit to celsius
Chicago.agg$temperature <- (Chicago.agg$temperature - 32) * (5/9)
# 2. reshape the data to weekly observations
temp <- aggregate(temperature ~ week, FUN = max, data = Chicago.agg)
customers <- aggregate(no.customers ~ week, FUN = sum, data = Chicago.agg)
data <- data.frame("week" = temp$week, "temperature" = temp$temperature, 
                   "customers" = customers$no.customers)

plot(customers ~ temperature, data = data)
# Change the title, axis labels, and coordinate system:
plot(customers ~ temperature, main = "Chicago bike trips", 
     xlim = c(0, 40), xlab = "Temperature in Celsius", ylab = "No of customers", 
     data = data)


# Change the text size of data symbols, title and axis labels:
plot(customers ~ temperature, main = "Chicago bike trips", 
     xlim = c(0, 40), xlab = "Temperature in Celsius", ylab = "No of customers", 
     cex = 0.8, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5, 
     data = data)


# Change the plotting symbol:
plot(customers ~ temperature, main = "Chicago bike trips", 
     xlim = c(0, 40), xlab = "Temperature in Celsius", ylab = "No of customers", 
     cex = 2, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5, 
     pch = 19,
     data = data)


# Change the plot regions and the figure margins:

par()                   # list of default graphics state settings
old.par <- par()        # save default settings as `old.par`

par(mar = c(5.1, 0, 4.1, 0) + 0.1) # set left and right figure margins to zero

plot(customers ~ temperature, main = "Chicago bike trips", 
     xlim = c(0, 40), xlab = "Temperature in Celsius", ylab = "No of customers", 
     cex = 2, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5, 
     pch = 19,
     data = data)

par(old.par)             # switch back to default settings


# Change the number of figures on a page:
par(mfrow = c(1,2))      # fill up figure regions row-wise 

hist(Chicago.agg$tripduration)
plot(tripduration ~ temperature, data = Chicago.agg)

par(old.par)            # again, switch back to default settings


#----
# Annotations
#----

# Adding colors:
plot(customers ~ temperature, main = "Chicago bike trips", 
     xlim = c(0, 40), xlab = "Temperature in Celsius", ylab = "No of customers", 
     cex = 2, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5, 
     pch = 19,
     col = "peru",
     data = data)

colors()               # list of all named colors in R

col2rgb("red")         # obtain rgb codes for "red"
rgb2hsv(c(255,0,0))    # obtain hsv codes for "red"

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255) # function to convert rgb to hex
rgb2hex(255, 0, 0 )    # obtain hex code for "red"

# Built-in color sets:
n <- 5
rainbow(n)      # returns hex codes

# Example: a rainbow color wheel
pie(rep(1, 12), col = rainbow(12))

# helper function to the display color palettes next:
# (note: functions will be covered in more detail in week 6)
pal <- function(col, border = "light gray"){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

# display the differen color palettes:
pal(rainbow(n)) 
pal(terrain.colors(n))
pal(heat.colors(n))
pal(topo.colors(n))
pal(cm.colors(n))


# RColorBrewer palettes
library("RColorBrewer")
?RColorBrewer

# qualitative color sets
brewer.pal(n, "Accent")
pal(brewer.pal(n, "Accent"))

# sequential color sets
brewer.pal(n, "Blues")
pal(brewer.pal(n, "Blues"))

# diverging color sets
brewer.pal(n, "Spectral")
pal(brewer.pal(n, "Spectral"))


# Rcolorspace palettes
library(colorspace)
?colorspaces

# qualitative color sets
pal(rainbow_hcl(n))

# sequential color sets
pal(sequential_hcl(n))

# diverging color sets
pal(diverge_hcl(n))


# Wes Anderson movie palettes
library(wesanderson) 
?wesanderson

wes_palettes

wes_palette("Zissou1", n = 5)
pal(wes_palette("Zissou1", n = 5))


# Do-it-your self sequential palettes
colorRampPalette(c("#FFD700", "gray30"))(5)
ramp <- colorRampPalette(c("#FFD700", "gray30"))(5)
pal(ramp)


# Adding data:

# again, aggregate over observations:
temp <- aggregate(temperature ~ week, FUN = max, data = Chicago.agg)
customers <- aggregate(no.customers ~ week, FUN = sum, data = Chicago.agg)

# plot the number of customers over 31 weeks:
plot(no.customers/10000 ~ week, ylim = c(0, 35), 
     col = "#FFD700", type = "l", cex = 2, 
     data = customers)

# add temperature line (curve):
lines(temperature ~ week, lty="solid", lwd = 1, 
      data = temp)

points(temperature ~ week, cex=2, 
      data = temp)


# Adding legends:
plot(no.customers/10000 ~ week, ylim = c(0, 35), ylab = "",
     col = "#FFD700", pch = 19, cex = 2, 
     data = customers)

lines(temperature ~ week, lty="dashed", lwd = 2, 
      data = temp)

legend("topleft", legend = c("no. of customers", "temperature"), 
       cex=1.25, fill=c("#FFD700", "black"), bty = "n")


#----
# `ggplot` and the grammer of graphics
#----

library(ggplot2)

#----
# Exploratory graphs
#----

# (Default) densityplot:
ggplot(Chicago.agg, aes(x = tripduration)) + 
  geom_density()  

# (Default) boxplot:
ggplot(Chicago.agg, aes(x = events, y = tripduration)) + 
  geom_boxplot()  

# (Default) scatterplot:
ggplot(Chicago.agg, aes(x = temperature, y = tripduration)) + 
  geom_point()

#----
# Customizations & Annotations
#----

# Change outlier (color, shape and size)
p <- ggplot(Chicago.agg, aes(x = events, y = tripduration)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) 
p

# Add coordiante system flip (rotate the boxes)
p + coord_flip()    

# Change box plot line colors by groups, add scale labels and plot title
p <- ggplot(Chicago.agg, aes(x = events, y = tripduration, color = events)) +
  geom_boxplot() +
  labs(title = "Boxplot", x = "Events", y = "Tripduration")  

# Add box line colors by groups using brewer color palettes
p + scale_color_brewer(palette = "Dark2")  

