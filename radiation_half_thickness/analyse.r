library(ggplot2)

dt = 10
C_bg = (3 + 0 + 2) / 3

ratio_U_C = 1.1
U_x = 0.5e-3 / 100

N = 500

beta_df <- read.table("data.csv", sep=",", header=TRUE)
beta_df$x <- beta_df$x * 1e-3
beta_df$I <- (rowMeans(beta_df[c("C1", "C2", "C3")]) - C_bg) / dt

show(beta_df)
show(log(beta_df$I))

model <- lm(log(I) ~ x, data=beta_df)
summary(model)
mu <- coef(summary(model))["x", "Estimate"]
x_half = log(2) / mu
print(paste("Half thickness: ", x_half))

qplot(x, log(I), data=beta_df,
      main="Linearised intensity vs thickness of absorbent aluminium",
      xlab="x/m", ylab="ln(I/Bq)") +
      geom_errorbarh(data=beta_df,
                     mapping=aes(xmin=x-U_x,
                                 xmax=x+U_x)) +
      geom_errorbar(data=beta_df,
                    mapping=aes(ymin=log(I / ratio_U_C),
                                ymax=log(I * ratio_U_C))) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      geom_smooth(method="lm")

prediction <- data.frame(
    x = c(beta_df$x, seq(min(beta_df$x), max(beta_df$x), length.out=N)),
    I = c(beta_df$I, rep(NA, N))
)
prediction$pred <- exp(predict(model, prediction))

#show(prediction)

qplot(x, I, data=beta_df,
      main="Intensity vs thickness of absorbent aluminium",
      xlab="x/m", ylab="I/Bq") +
      geom_errorbarh(data=beta_df,
                     mapping=aes(xmin=x-U_x,
                                 xmax=x+U_x)) +
      geom_errorbar(data=beta_df,
                    mapping=aes(ymin=(I / ratio_U_C),
                                ymax=(I * ratio_U_C))) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      geom_line(size=1, color="steelblue", data = prediction, aes(y = pred))
