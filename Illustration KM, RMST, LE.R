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
S.10 <- sprintf("%.1f", survival2_at_10*100)

# calculate RMST
# Define the Weibull survival functions
weibull_survival2 <- function(t) {
  exp(-(t / scale2)^shape2)
}

# Calculate RMST for each group
rmst1 <- integrate(weibull_survival1, lower = 0, upper = x_intersect)$value
rmst2 <- integrate(weibull_survival2, lower = 0, upper = x_intersect)$value

# Calculate the difference in RMST
RMST <- sprintf("%.1f", rmst2)

# calculate LE
LE2 <- scale2 * gamma(1 + 1 / shape2) # same as: integrate(weibull_survival2, lower = 0, upper = Inf)$value
LE <- sprintf("%.1f", LE2)

# colors
green <- "#A5D867"
orange <- "#EC7A08"
pink <- "#C30072"

# make three plots
ARD.plot <- ggplot() +
  geom_line(data=data_before, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="solid") +
  geom_line(data=data_after, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="dashed") +
  geom_vline(xintercept=x_intersect, linetype="dashed", linewidth=0.5) + 
  annotate("text", x=12, y=0.9, 
           label=paste0(x_intersect, "-year survival = ", S.10, "%"), 
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

RMST.plot <- ggplot() +
  geom_line(data=data_before, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="solid") +
  geom_line(data=data_after, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="dashed") +
  geom_vline(xintercept=x_intersect, linetype="dashed", linewidth=0.5) + 
  geom_ribbon(aes(x=Years, ymin=0, ymax=Survival2), 
              fill=orange, alpha=0.1, 
              data=subset(data, Years <= x_intersect)) +
  annotate("text", x=5, y=0.6, label="RMST", color="black", size=5) +
  annotate("text", x=12, y=0.9, 
           label=paste0(x_intersect, "-year RMST = ", RMST, " years"), 
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
show(RMST.plot)

LE.plot <- ggplot() +
  geom_line(data=data_before, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="solid") +
  geom_line(data=data_after, aes(x=Years, y=Survival2), color=pink, linewidth=1, linetype="dashed") +
  geom_ribbon(aes(x=Years, ymin=0, ymax=Survival2), 
              fill=orange, alpha=0.1) +
  annotate("text", x=5, y=0.6, label="LE", color="black", size=5) +
  annotate("text", x=12, y=0.9, label=paste("LE =", LE, "years"), 
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
show(LE.plot)

# Generate plot for ARD, RMST, and LE
ggsave(filename="C:/Users/carol/OneDrive - Erasmus MC/Dissertation/Figures General Introduction/Illustration.prognosis.png",
       width=15, height=5,
       grid.arrange(ARD.plot, RMST.plot, LE.plot, ncol=3))