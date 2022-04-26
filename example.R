source("~/../repo/National-Wages/Source/Prelim.R")
source("~/../repo/National-Wages/Source/binscatter.R")

library(pacman)
p_load('AER', 'lfe', 'pryr', 'magrittr')

data("Guns", package = "AER")
Guns$ln_violent <- log(Guns$violent)
Guns$ln_prisoners <- log(Guns$prisoners)

binscatter(formula="ln_violent ~ ln_prisoners", key_var = "ln_prisoners",
           data=Guns, bins=10, partial=FALSE)

binscatter(formula="ln_violent ~ ln_prisoners + year | state | 0 | state" , key_var = "ln_prisoners",
           data=Guns, bins=10, partial=TRUE)

data("CPS1988", package = "AER")
CPS1988$ln_wage <- log(CPS1988$wage)
CPS1988$experience2 <- (CPS1988$experience)^2

graph <- binscatter(formula=" ln_wage ~ education" , key_var = "education",
           data=CPS1988, bins=10, partial=FALSE, xlabel = "Log Wage", ylabel = "Education", savedata = "data.csv")

binscatter(formula=" ln_wage ~ education + ethnicity" , key_var = "education",
           data=CPS1988, bins=10, partial=TRUE, savedata = "data2.csv")

# Reload data for plotting
bs1_data <- read.csv("data.csv")
bs2_data <- read.csv("data2.csv")
new_graph <-  ggplot() +
    #geom_segment(aes(x = min_x, y = min_y, xend = max_x, yend = max_y),
    #            color=cea_blue, size=1) +
    geom_smooth(method = lm, se = FALSE, data = bs1_data, aes(x=bs1_data[, 1], y = bs1_data[, 2]), color = cea_blue) +
    geom_point(data = bs1_data, aes(x=bs1_data[, 1], y = bs1_data[, 2]), color = cea_blue) +
    geom_smooth(method = lm, se = FALSE, data = bs2_data, aes(x=bs2_data[, 1], y = bs2_data[, 2]), color = cea_blue) +
    geom_point(data = bs2_data, aes(x=bs2_data[, 1], y = bs2_data[, 2]), color = cea_blue)
    #+
    #labs(x = xlabel, y = ylabel)

#print(new_graph)

print(head(CPS1988))

another_graph <- binscatter(formula=" ln_wage ~ education + ethnicity | experience + experience2" , key_var = "education",
           data=CPS1988, bins=10, partial=TRUE, savedata = "data3.csv")

print(another_graph)
