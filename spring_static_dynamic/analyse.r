library(ggplot2)

U_x = 0.002
pc_U_m = 5
U_20T = 0.2
U_T = U_20T / 20

g = 9.80665

static_df <- read.table("static.csv", sep=",", header=TRUE)

static_df$x <- rowMeans(subset(static_df, select = c(x1, x2, x3, x4, x5)))
static_df$F <- g * static_df$m

static_df

static_model <- lm(F ~ x, data=static_df)
summary(static_model)

qplot(x, F, data=static_df,
      main="Force against extension in a spring (Hooke's Law)",
      xlab="x / m", ylab="F / N") +
      geom_errorbar(mapping=aes(ymin=g * (m * (100 + pc_U_m) / 100),
                                ymax=g * (m * (100 - pc_U_m) / 100),
                                width=0.005)) +
      geom_errorbarh(mapping=aes(xmin=x - U_x,
                                 xmax=x + U_x,
                                 height=0.1)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      scale_y_continuous(minor_breaks = seq(0, 7, 0.2),
                         breaks = seq(0, 7, 1)) +
      scale_x_continuous(minor_breaks = seq(0.5, 1, 0.01),
                         breaks = seq(0.5, 1, 0.05)) +
      geom_smooth(method = "lm", color = "red", size=.1)

dynamic_df <- read.table("dynamic.csv", sep=",", header=TRUE)
dynamic_df$T <- rowMeans(subset(dynamic_df, select=c(T1, T2, T3, T4, T5))) / 20
dynamic_df$plot_y <- (2 * pi / dynamic_df$T) ^ 2
dynamic_df$plot_x <- 1 / dynamic_df$m

dynamic_df

dynamic_model <- lm(plot_y ~ plot_x, data=dynamic_df)
summary(dynamic_model)

qplot(plot_x, plot_y, data=dynamic_df,
      main="Dynamic analysis of spring constant: Period against mass",
      xlab="kg / m", ylab="(Angular Frequency)^2/Hz^2") +
      geom_errorbar(mapping=aes(ymin=(2 * pi / (T + U_T))^2,
                                ymax=(2 * pi / (T - U_T))^2),
                                width=0.2) +
      geom_errorbarh(mapping=aes(xmin=1 / (m * (100 + pc_U_m) / 100),
                                 xmax=1 / (m * (100 - pc_U_m) / 100))) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      scale_y_continuous(minor_breaks = seq(0, 250, 10),
                         breaks = seq(0, 250, 50)) +
      scale_x_continuous(minor_breaks = seq(0, 11, 0.4),
                         breaks = seq(0, 11, 2)) +
      geom_smooth(method = "lm", color = "steelblue", size=.1)
