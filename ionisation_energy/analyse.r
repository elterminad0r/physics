library(ggplot2)

k = 1.3806488e-23

U_I = 0.01e-3
U_T = 0.2

V_values <- c(5.11, 5.08, 5.15, 5.27)
V <- mean(V_values)
V

current_df <- read.table("data.csv", sep=",")
colnames(current_df) <- c("T_celsius", "I_ma")

current_df$T <- current_df$T_celsius + 273.15
current_df$I <- current_df$I_ma / 1000

current_df$T_recip = 1 / current_df$T
current_df$ln_I = log(current_df$I)

pc_U_I = U_I / median(current_df$I)
pc_U_T = U_T / median(current_df$T)

pc_U_I
pc_U_T

current_df

nl_model <- nls(I~A*exp(-E/(k * T)),
                start=list(A=4, E=0.3 * 1.6e-19), data=current_df)
nl_model

qplot(T, I, data=current_df,
      main="Current through thermistor against temperature",
      xlab="T / K", ylab="I / A") +
      geom_errorbar(data=current_df, mapping=aes(ymin=I-U_I, ymax=I+U_I)) +
      geom_errorbarh(data=current_df, mapping=aes(xmin=T-U_T, xmax=T+U_T)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      scale_y_continuous(minor_breaks = seq(0.1e-3, 1.1e-3, 0.2e-4),
                         breaks = seq(0.1e-3, 1.1e-3, 0.1e-3)) +
      scale_x_continuous(minor_breaks = seq(280, 320, 1),
                         breaks = seq(280, 320, 5)) +
      geom_line(aes(y = predict(nl_model)), color="steelblue")

model <- lm(ln_I ~ T_recip, data=current_df)
model

qplot(T_recip, ln_I, data=current_df,
      main="ln(I) against 1/T",
      xlab="K / T", ylab="ln(I) - ln(A)") +
      geom_errorbar(data=current_df,
                    mapping=aes(ymin=ln_I - pc_U_I,
                                ymax=ln_I + pc_U_I)) +
      geom_errorbarh(data=current_df,
                     mapping=aes(xmin=(1 - pc_U_T) * T_recip,
                                 xmax=(1 + pc_U_T) * T_recip)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      scale_y_continuous(minor_breaks = seq(-10, -5, 0.05),
                         breaks = seq(-10, -5, 0.2)) +
      scale_x_continuous(minor_breaks = seq(0.003, 0.004, 0.00002),
                         breaks = seq(0.003, 0.004, 0.0001)) +
      geom_line(aes(y = predict(model)), color="steelblue")
