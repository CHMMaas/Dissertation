rm(list=ls(all.names=TRUE))
library(ggplot2)
library(dplyr)
library(gridExtra)
library(rms)
set.seed(1)

# Number of observations per group
n <- 10000

# Simulate covariates
OR.risk <- 1
x <- rnorm(n, mean=0.5, sd=1)
lp <- OR.risk*x

# Simulate treatment assignment
treatment <- rbinom(n=n, size=1, prob=0.5)

# Simulate baseline risk and treatment effect
baseline_risk <- -2
treatment_effect <- 2

# Calculate predicted probabilities
lp.y <- baseline_risk + treatment_effect * treatment + lp
y <- rbinom(n, 1, plogis(lp.y))
model.true <- rms::lrm(y ~ treatment + lp)

# alter coefficients to show 
# 1) underestimation, 2) overestimation, 3) underfitting, 4) overfitting
true <- coef(model.true)

# change coefficient of intercept to create underestimation and overestimation
Underestimation <- c(-2.5, true[2:3])
Overestimation <- c(-1.5, true[2:3])

# change coefficient of linear predictor to create underfitting and overfitting
Underfitting <- c(true[1:2], 0.5)
Overfitting <- c(true[1:2], 2)

nr.plot <- 1
for (type in c("true", "Underestimation", "Overestimation",
               "Underfitting", "Overfitting")){
  # obtain predictions
  coef <- eval(parse(text=type))
  lp <- cbind(rep(1, n), treatment, x) %*% coef
  probability <- plogis(lp)
  
  # Create data frame
  data <- data.frame(treatment, y, probability)
  
  ###
  ### HISTOGRAM OF PREDICTED PROBABILITIES
  ###
  green <- "#A5D867"
  orange <- "#EC7A08"
  pink <- "#C30072"
  histogram <- ggplot(data, aes(x = probability, 
                                fill = factor(treatment))) +
    geom_histogram(position = "dodge", bins = 30) +
    labs(title = "",
         x = "Predicted Probability",
         y = "Count",
         fill = "Treatment Group") +
    scale_fill_manual(values = c("0" = green, "1" = pink)) + 
    theme_void() +
    theme(legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 1.4), "cm"))
  
  ###
  ### CALIBRATION PLOT
  ### 
  # Create deciles of predicted probabilities
  g <- 10
  data <- data %>%
    mutate(decile = ntile(probability, g))
  
  # Calculate mean predicted, mean observed, and standard deviation for each decile
  calibration_data <- data %>%
    group_by(decile) %>%
    summarise(
      mean_predicted = mean(probability),
      mean_observed = mean(y),
      n = n()
    ) %>%
    mutate(
      se = sqrt((mean_observed * (1 - mean_observed)) / n),
      lower_ci = mean_observed - 1.96 * se,
      upper_ci = mean_observed + 1.96 * se
    )
  
  # Calibration plot with confidence intervals
  calibration.intercept <- sprintf("%.2f", 
                                   as.numeric(coef(glm(y ~ offset(lp), 
                                                       family="binomial"))[1]))
  calibration.slope <- sprintf("%.2f", 
                               as.numeric(coef(glm(y ~ lp, 
                                                   family="binomial"))[2]))
  C.index <- sprintf("%.2f", Hmisc::rcorr.cens(x=probability,
                                               S=y)["C Index"])
  calibration.plot <- ggplot(calibration_data, 
                             aes(x = mean_predicted, y = mean_observed)) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                  width = 0.02, color = pink) +
    geom_point(color = pink, size = 1.5) +
    geom_line(color = pink, alpha = 0.25) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = green) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    annotate("text", x=0, y=1, hjust=0, vjust=1,
             label=paste0("Calibration intercept = ", calibration.intercept, "\n",
                          "Calibration slope = ", calibration.slope, "\n",
                          "C-statistic = ", C.index, "\n")) +
    labs(title = ifelse(type=="true", "Perfect calibration", type),
         tags = LETTERS[nr.plot],
         x = "Predicted Probability",
         y = "Observed Probability") +
    theme_classic()
  
  nr.plot <- nr.plot + 1
  
  plot <- grid.arrange(calibration.plot, histogram, 
                       heights=c(4, 1),
                       nrow=2, ncol=1)
  assign(paste0("plot.", type), plot)
  # val.prob(p=probability, y=y)
}

ggsave(file="C:/Users/carol/OneDrive - Erasmus MC/Dissertation/Figures General Introduction/Calibration.png",
       plot=grid.arrange(plot.true,
                         grid.arrange(plot.Underestimation, plot.Overestimation,
                                      plot.Underfitting, plot.Overfitting,
                                      nrow=2, ncol=2),
                         nrow=1, ncol=2),
       width=16, height=8, dpi=300)