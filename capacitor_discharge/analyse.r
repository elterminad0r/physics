library(ggplot2)

U_I <- 0.002
U_t <- 1
charge_df <- read.table("data.csv", sep=",")
colnames(charge_df) <- c("t", "I1", "I2", "I3", "I4", "I5")
charge_df$I_avg <- rowMeans(charge_df[c("I1", "I2", "I3", "I4", "I5")])
m <- nls(I_avg~I0*exp(-t/tau), start=list(I0=head(charge_df$I_avg, n=1),tau=25), data=charge_df)
m
qplot(t, I_avg, data=charge_df, main="Current in RC circuit over time",
      xlab="t / s", ylab="I / A") +
      geom_errorbar(data=charge_df, mapping=aes(ymin=I_avg-U_I, ymax=I_avg+U_I)) +
      geom_errorbarh(data=charge_df, mapping=aes(xmin=t-U_t, xmax=t+U_t)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      scale_y_continuous(minor_breaks = seq(0, 0.12, 0.002), breaks = seq(0, 0.12, 0.01)) +
      scale_x_continuous(minor_breaks = seq(0, 110, 2), breaks = seq(0, 110, 10)) +
      geom_line(aes(y = predict(m)), color="steelblue")
