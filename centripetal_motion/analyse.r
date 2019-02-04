library(ggplot2)

U_T = 0.02
U_r = 0.01

M = 1e-3 * (59.69 + 59.70 + 59.67) / 3
m = 1e-3 * (8.99 + 8.9 + 8.90) / 3

time_df <- read.table("data.csv", sep=",")
colnames(time_df) <- c("r_mm", "10T1", "10T2", "10T3")
time_df$T_avg <- rowMeans(time_df[c("10T1", "10T2", "10T3")]) / 10
time_df$r <- time_df$r / 1000

time_df$x = time_df$T_avg^2 / (4 * pi ^2 * m / M)
time_df$y = time_df$r

model <- lm(y ~ x, data=time_df)
summary(model)

qplot(x, y, data=time_df,
      main="Period against mass for a bung in circular motion",
      xlab="T^2 * M / (4 pi ^ 2 * m) / s^2", ylab="r / m") +
      geom_errorbarh(data=time_df,
                     mapping=aes(xmin=(T_avg - U_T)^2 / (4 * pi ^ 2 * m / M),
                                 xmax=(T_avg + U_T)^2 / (4 * pi ^ 2 * m / M))) +
      geom_errorbar(data=time_df, mapping=aes(ymin=r-U_r, ymax=r+U_r)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      geom_smooth(method="lm")
