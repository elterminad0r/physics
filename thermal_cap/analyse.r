library(ggplot2)
library(mgcv)
library(pracma)

U_temp <- 0.2
U_time <- 5
PRED_RES <- 0.1

V <- 12.2
I <- 3.41

m <- 1007

t_heat = 210

data <- read.table("data.csv", header=TRUE, sep=",")

t_start <- head(data$time, n=1)
t_end <- tail(data$time, n=1)

temp_start <- head(data$temp, n=1)

smooth_model <- gam(temp ~ s(time, bs = "cs"), data=data)

smooth_vals <- data.frame(time=seq(t_start, t_end, PRED_RES))
smooth_vals$temp <- predict.gam(smooth_model, smooth_vals)
#smooth_model
#smooth_vals

t_1_ind <- which.max(smooth_vals$temp)
t_1 <- smooth_vals$time[t_1_ind]
t_2 <- t_end - t_1

message(sprintf("t_1 = %.1f", t_1))
message(sprintf("t_2 = %.1f", t_2))

A_1_data <- subset(smooth_vals, time <= t_1)
A_1 <- trapz(A_1_data$time, A_1_data$temp - temp_start)

A_2_data <- subset(smooth_vals, time >= t_1)
A_2 <- trapz(A_2_data$time, A_2_data$temp - temp_start)

str(A_1_data)
str(A_2_data)

message(sprintf("A_1 = %.1f", A_1))
message(sprintf("A_2 = %.1f", A_2))

temp_corr_max <- max(data$temp) + A_1 / A_2 * (max(data$temp) - tail(data$temp, n=1))

message(sprintf("t_c_max = %.1f", temp_corr_max))
message(sprintf("t_start = %.1f", temp_start))
message(sprintf("t_heat = %.1f", t_heat))

c <- V * I * t_heat / (m * (temp_corr_max - temp_start))

message(sprintf("c = %.5f", c))

qplot(time, temp, data=data,
      main="Temperature of block heated by immersion heater over time",
      xlab="t / s",
      ylab=paste("temp / C ", "")) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
      geom_errorbar(data=data, mapping=aes(ymin=temp-U_temp, ymax=temp+U_temp)) +
      geom_errorbarh(data=data, mapping=aes(xmin=time-U_time, xmax=time+U_time)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      scale_y_continuous(minor_breaks = seq(20 , 45, 0.5), breaks = seq(20, 45, 5)) +
      scale_x_continuous(minor_breaks = seq(0, 2000, 50), breaks = seq(0, 2000, 500))
