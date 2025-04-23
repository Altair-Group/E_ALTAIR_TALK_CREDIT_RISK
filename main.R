source("lib.R")
# 1) Discriminatory Power ----

# Rating system 1
ND_1 <- c(rep(5, 150), rep(4, 200), rep(3, 185), rep(2, 215), rep(1, 200))
D_1 <- c(rep(5, 27), rep(4, 14), rep(3, 2), rep(2, 5), rep(1, 2))

true_labels_1 <- c(rep(0, length(ND_1)), rep(1, length(D_1)))
scores_1 <- c(ND_1, D_1)

table_1 <- table(scores_1, true_labels_1)
LR_1 <- (table_1[, 1] / sum(table_1[, 1])) / (table_1[, 2] / sum(table_1[, 2]))
print(table_1)

# Rating system 2
ND_2 <- c(rep(5, 180), rep(4, 200), rep(3, 210), rep(2, 215), rep(1, 145))
D_2 <- c(rep(5, 28), rep(4, 11), rep(3, 5), rep(2, 4), rep(1, 2))

true_labels_2 <- c(rep(0, length(ND_2)), rep(1, length(D_2)))
scores_2 <- c(ND_2, D_2)

table_2 <- table(scores_2, true_labels_2)
LR_2 <- (table_2[, 1] / sum(table_2[, 1])) / (table_2[, 2] / sum(table_2[, 2]))
print(table_2)

##  1.1) Cumulative Accuracy Profile (CAP) ----
hit_rate_1 <- c(0, cumsum(rev(table_1[, 2])) / sum(rev(table_1[, 2])))
alarm_rate_1 <- c(0, cumsum(rev(rowSums(table_1))) / sum(rev(rowSums(table_1))))

hit_rate_2 <- c(0, cumsum(rev(table_2[, 2])) / sum(rev(table_2[, 2])))
alarm_rate_2 <- c(0, cumsum(rev(rowSums(table_2))) / sum(rev(rowSums(table_2))))

p <- sum(true_labels_1) / length(true_labels_1)

# Plot the CAP curve for Rating System 1
plot(alarm_rate_1, hit_rate_1, type = "l", col = "#7570b3", main = "CAP Curves", 
     xlab = "Alarm Rate", ylab = "Hit Rate", lwd = 2, xlim = c(0, 1), ylim = c(0, 1))

# Add Rating System 2 CAP Curve
lines(alarm_rate_2, hit_rate_2, col = "#e7298a", lwd = 2, lty = 2)

# Add Perfect Model (Ideal Classifier) - Green (Perfect CAP curve)
lines(c(0, p, 1), c(0, 1, 1), col = "#1b9e77", lwd = 2, lty = 3)

# Add Random Model (Diagonal Line) - Black (Random CAP curve)
lines(c(0, 1), c(0, 1), col = "#666666", lwd = 2, lty = 4)

# Add Legend
legend("bottomright",
       legend = c("Rating System 1", "Rating System 2", "Perfect Model", "Random Model"),
       col = c("#7570b3", "#e7298a", "#1b9e77", "#666666"),
       lty = c(1, 2, 3, 4),
       lwd = 2,
       bty = "n")

## 1.2) Receiver Operating Characteristic (ROC) ----

false_alarm_rate_1 <- c(0, cumsum(rev(table_1[, 1])) / sum(rev(table_1[, 1])))
false_alarm_rate_2 <- c(0, cumsum(rev(table_2[, 1])) / sum(rev(table_2[, 1])))

# Plot the ROC curve for Rating System 1
plot(false_alarm_rate_1, hit_rate_1, type = "l", col = "#7570b3", main = "ROC Curves", 
     xlab = "False Alarm Rate", ylab = "Hit Rate", lwd = 2, xlim = c(0, 1), ylim = c(0, 1))

# Add Rating System 2 ROC Curve
lines(false_alarm_rate_2, hit_rate_2, col = "#e7298a", lwd = 2, lty = 2)

# Add Perfect Model (Ideal Classifier) - Green (Perfect CAP curve)
lines(c(0, 0, 1), c(0, 1, 1), col = "#1b9e77", lwd = 2, lty = 3)

# Add Random Model (Diagonal Line) - Black (Random CAP curve)
lines(c(0, 1), c(0, 1), col = "#666666", lwd = 2, lty = 4)

# Add Legend
legend("bottomright",
       legend = c("Rating System 1", "Rating System 2", "Perfect Model", "Random Model"),
       col = c("#7570b3", "#e7298a", "#1b9e77", "#666666"),
       lty = c(1, 2, 3, 4),
       lwd = 2,
       bty = "n")

##  1.3) Area Under the Curve (AUC) ----

## Test on Rating system 1
U_1 <- wilcox.test(D_1, ND_1)
print(U_1)

AUC_1 <- U_1$statistic[[1]]/(length(ND_1)*length(D_1))
print(AUC_1)

roc_1 <- roc(true_labels_1, scores_1)
print(as.numeric(auc(roc_1)))

# Create summary table table
table_roc_1 <- data.frame(
  AUC = auc(roc_1),
  SE =  sqrt(var(roc_1)),
  `95% CI LB` = ci(roc_1)[1],
  `95% CI UB` = ci(roc_1)[3],
  `p-value` = U_1$p.value
)
print(table_roc_1)

## Test on Rating system 2
U_2 <- wilcox.test(D_2, ND_2)
print(U_2)

AUC_2 <- U_2$statistic[[1]]/(length(ND_2)*length(D_2))
print(AUC_2)

roc_2 <- roc(true_labels_2, scores_2)
print(as.numeric(auc(roc_2)))

# Create summary table table
table_roc_2 <- data.frame(
  AUC = auc(roc_2),
  SE =  sqrt(var(roc_2)),
  `95% CI LB` = ci(roc_2)[1],
  `95% CI UB` = ci(roc_2)[3],
  `p-value` = U_2$p.value
)
print(table_roc_2)

## DeLong comparison test
roc.test(roc_1, roc_2, method = "delong")

# 2) Calibration Accuracy ----
set.seed(42)

rho_vec <- c(0.05, 0.2)
p <- 0.01
p_vec <- c(0.005, 0.01, 0.015, 0.02, 0.025)
m_vec <- c(50, 250, 1000)
a <- 0.05
B <- 10000

par(mfrow = c(2, 3))

for (k in 1:2){
  
  rho <- rho_vec[k]
  
  for (j in 1:3){
    
    m <- m_vec[j]
    
    R_mean <- p
    R_var <- (m - 1) / m * pnorm2d(qnorm(p), qnorm(p), rho = rho) + p / m - p^2
    
    a_shape <- (R_mean / R_var) * (R_mean * (1 - R_mean) - R_var)
    b_shape <- ((1 - R_mean) / R_var) * (R_mean * (1 - R_mean) - R_var)
    
    q_bin <- qbinom(1 - a, m, p)
    q_vasiceck <- ceiling(pnorm((qnorm(p) + sqrt(rho) * qnorm(1 - a)) / sqrt(1 - rho)) * m)
    q_mm <- ceiling(qbeta(1 - a, a_shape, b_shape) * m)
    
    n_def <- c()
    
    # Part 1 : Critical Values Comparison
    
    for (b in 1:B) {
      e <- rnorm(m)
      f <- rnorm(1)
      av <- sqrt(rho) * f + sqrt(1 - rho) * e
      n_def[b] <- sum(av <= qnorm(p))
    }
    
    q_true <- quantile(n_def, probs = 1 - a)
    
    hist(n_def, xlim = c(0, p * 10 * m + p * 10 * (1 - p * 10) * m), freq = FALSE,
         main = paste("m = ", m, ", \u03C1 = ", rho),
         xlab = "Number of defaults")
    abline(v = q_true, col = "#7570b3")
    abline(v = q_bin, col = "#e7298a")
    abline(v = q_vasiceck, col = "#1b9e77")
    abline(v = q_mm, col = "#666666")
    legend("topright",
           legend = c(
             paste("MC: ", q_true), 
             paste("Binomial: ", q_bin), 
             paste("Vasicek:", q_vasiceck),
             paste("MM:", q_mm)),
           col = c("#7570b3", "#e7298a", "#1b9e77", "#666666"),
           lwd = 2,
           bty = "n")
    
    # Part 2 : Test Power Comparison
    
    res <- matrix(, nrow = length(p_vec), ncol = 4)
    
    for(i in 1:length(p_vec)){
      
      q = p_vec[i]
      tmp <- matrix(, nrow = B, ncol = 4)
      
      for(b in 1:B){
        e <- rnorm(m)
        f <- rnorm(1)
        av <- sqrt(rho) * f + sqrt(1 - rho) * e
        n_def <- sum(av <= qnorm(q))
        
        tmp[b, 1] <- q_true < n_def
        tmp[b, 2] <- q_bin < n_def
        tmp[b, 3] <- q_vasiceck < n_def
        tmp[b, 4] <- q_mm < n_def
      }
      
      res[i, 1] <- sum(tmp[, 1]) / B
      res[i, 2] <- sum(tmp[, 2]) / B
      res[i, 3] <- sum(tmp[, 3]) / B
      res[i, 4] <- sum(tmp[, 4]) / B
      
    }
    
    res_df <- data.frame(res, row.names = p_vec)
    colnames(res_df) <- c('True', 'Binomial', 'Vasiceck', 'MM')
    print(res_df)
    
  }
}

##  2.1) LGD - Concentration Curve (CC) ----

set.seed(111)
n <- 1000
phi <- 1

LGD <- c()
Y <- c()
for (i in 1:n) {
  LGD <- c(LGD, runif(1))
  Y <- c(Y, rbeta(1, LGD*phi, (1-LGD)*phi))
}

hist(Y)

Y_sorted <- Y[order(LGD, decreasing = TRUE)]
Y_perfect <- Y[order(Y, decreasing = TRUE)]

x <- c(0, cumsum(1:n) / sum(1:n))
CC <- c(0, cumsum(Y_sorted) / sum(Y_sorted))

# Plot the CAP curve for Rating System 1
plot(x, CC, type = "l", col = "#7570b3", main = "Concentration Curves", 
     xlab = "u", ylab = "CC(u)", lwd = 2, xlim = c(0, 1), ylim = c(0, 1))

# Add Perfect Model (Ideal Classifier) - Green (Perfect CAP curve)
lines(x, c(0, cumsum(Y_perfect) / sum(Y_perfect)), col = "#1b9e77", lwd = 2, lty = 3)

# Add Random Model (Diagonal Line) - Black (Random CAP curve)
lines(c(0, 1), c(0, 1), col = "#666666", lwd = 2, lty = 4)

# Add Legend
legend("bottomright",
       legend = c("LGD", "Perfect Model", "Random Model"),
       col = c("#7570b3", "#1b9e77", "#666666"),
       lty = c(1, 2, 3, 4),
       lwd = 2,
       bty = "n")