# Load necessary library
library(ggplot2)
library(scales)
library(gridExtra)

# Define parameters for Weibull distribution
shape1 <- 1.5
scale1 <- 10
shape2 <- 2.5
scale2 <- 15

# Generate data
Years <- seq(0, 30, by=0.5)
Survival1 <- pweibull(Years, shape=shape1, scale=scale1, lower.tail=FALSE)
Survival2 <- pweibull(Years, shape=shape2, scale=scale2, lower.tail=FALSE)

data <- data.frame(Years, Survival1, Survival2)

# Split data into two parts: before and after x_intersect
data_before <- subset(data, Years <= 10)
data_after <- subset(data, Years > 10)

# Calculate survival probabilities at x = 10
x_intersect <- 10
survival1_at_10 <- pweibull(x_intersect, shape=shape1, scale=scale1, lower.tail=FALSE)
survival2_at_10 <- pweibull(x_intersect, shape=shape2, scale=scale2, lower.tail=FALSE)

# calculate ARD
ARD <- sprintf("%.1f", (survival2_at_10 - survival1_at_10)*100)

# calculate dRMST
# Define the Weibull survival functions
weibull_survival1 <- function(t) {
  exp(-(t / scale1)^shape1)
}

weibull_survival2 <- function(t) {
  exp(-(t / scale2)^shape2)
}

# Calculate RMST for each group
rmst1 <- integrate(weibull_survival1, lower = 0, upper = x_intersect)$value
rmst2 <- integrate(weibull_survival2, lower = 0, upper = x_intersect)$value

# Calculate the difference in RMST
dRMST <- sprintf("%.1f", rmst2 - rmst1)

# calculate LOLE
LE1 <- scale1 * gamma(1 + 1 / shape1) # same as: integrate(weibull_survival1, lower = 0, upper = Inf)$value
LE2 <- scale2 * gamma(1 + 1 / shape2) # same as: integrate(weibull_survival2, lower = 0, upper = Inf)$value
LOLE <- sprintf("%.1f", LE2 - LE1)

# colors
green <- "#A5D867"
orange <- "#EC7A08"
pink <- "#C30072"

# make three plots
ARD.plot <- ggplot() +
  geom_line(data=data_before, aes(x=Years, y=Survival1), color=green, linewidth=1, linetype="solid") +
  geom_line(data=data_after, aes(x=Years, y=Survival1), color=green, linewidth=1, linetype="dashed") +
  geom_line(data=data_before, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="solid") +
  geom_line(data=data_after, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="dashed") +
  geom_vline(xintercept=x_intersect, linetype="dashed", linewidth=0.5) + 
  geom_segment(aes(x=x_intersect, xend=x_intersect, 
                   y=survival1_at_10, yend=survival2_at_10),
               linewidth=1,
               arrow=arrow(length=unit(0.1, "inches"))) +
  geom_segment(aes(x=x_intersect, xend=x_intersect, 
                   y=survival2_at_10, yend=survival1_at_10),
               linewidth=1,
               arrow=arrow(length=unit(0.1, "inches"))) +
  annotate("text", x=11.3, y=0.5, label="ARD", color="black", size=3) +
  annotate("text", x=12, y=0.9, 
           label=paste0(x_intersect, "-year ARD = ", ARD, "%"), 
           color="black", size=5, hjust=0) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), 
                     breaks=seq(0, 1, by=0.2)) +
  scale_x_continuous(breaks=seq(0, 30, by=5)) +
  labs(x="Years since diagnosis",
       y="Survival Proportion") +
  theme_classic() +
  theme(
    text = element_text(size = 16), 
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 20)
  )
show(ARD.plot)

dRMST.plot <- ggplot() +
  geom_line(data=data_before, aes(x=Years, y=Survival1), color=green, linewidth=1, linetype="solid") +
  geom_line(data=data_after, aes(x=Years, y=Survival1), color=green, linewidth=1, linetype="dashed") +
  geom_line(data=data_before, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="solid") +
  geom_line(data=data_after, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="dashed") +
  geom_vline(xintercept=x_intersect, linetype="dashed", linewidth=0.5) + 
  geom_ribbon(aes(x=Years, ymin=Survival1, ymax=Survival2), 
              fill=orange, alpha=0.1, 
              data=subset(data, Years <= x_intersect)) +
  annotate("text", x=6, y=0.8, label="\u0394RMST", color="black", size=3) +
  annotate("text", x=12, y=0.9, 
           label=paste0(x_intersect, "-year \u0394RMST = ", dRMST, " years"), 
           color="black", size=5, hjust=0) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), 
                     breaks=seq(0, 1, by=0.2)) +
  scale_x_continuous(breaks=seq(0, 30, by=5)) +
  labs(x="Years since diagnosis",
       y="Survival Proportion") +
  theme_classic() +
  theme(
    text = element_text(size = 16), 
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 20)
  )
show(dRMST.plot)

LOLE.plot <- ggplot() +
  geom_line(data=data_before, aes(x=Years, y=Survival1), color=green, linewidth=1, linetype="solid") +
  geom_line(data=data_after, aes(x=Years, y=Survival1), color=green, linewidth=1, linetype="dashed") +
  geom_line(data=data_before, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="solid") +
  geom_line(data=data_after, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="dashed") +
  geom_ribbon(aes(x=Years, ymin=Survival1, ymax=Survival2), 
              fill=orange, alpha=0.1) +
  annotate("text", x=10.5, y=0.5, label="LOLE", color="black", size=3) +
  annotate("text", x=12, y=0.9, label=paste("LOLE =", LOLE, "years"), 
           color="black", size=5, hjust=0) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), 
                     breaks=seq(0, 1, by=0.2)) +
  scale_x_continuous(breaks=seq(0, 30, by=5)) +
  labs(x="Years since diagnosis",
       y="Survival Proportion") +
  theme_classic() +
  theme(
    text = element_text(size = 16), 
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 20)
  )
show(LOLE.plot)

# Generate plot for ARD, dRMST, and LOLE
ggsave(filename="C:/Users/carol/OneDrive - Erasmus MC/Dissertation/Figures General Introduction/Illustration.TE.png",
       width=15, height=5,
       grid.arrange(ARD.plot, dRMST.plot, LOLE.plot, ncol=3))