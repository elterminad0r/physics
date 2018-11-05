library(ggplot2)

U_s = 3
U_t = 0.1

s_df_1 <- read.table("data/1.csv", sep=",", header=TRUE)
s_df_2 <- read.table("data/2.csv", sep=",", header=TRUE)
s_df_3 <- read.table("data/3.csv", sep=",", header=TRUE)
s_df_4 <- read.table("data/4.csv", sep=",", header=TRUE)

s_df_1
s_df_2
s_df_3
s_df_4

t_0_1 = head(s_df_1$t, n=1)
s_0_1 = 654
A_1 = head(s_df_1$s, n=1) - s_0_1
m_1 <- nls(s ~ s_0_1 + A_1 * exp(-(t - t_0_1) / tau),
           start=list(s_0_1 = s_0_1, A_1 = A_1, tau = 30), data=s_df_1)
summary(m_1)

t_0_2 = head(s_df_2$t, n=1)
s_0_2 = 656
A_2 = head(s_df_2$s, n=1) - s_0_2
m_2 <- nls(s ~ s_0_2 + A_2 * exp(-(t - t_0_2) / tau),
           start=list(s_0_2 = s_0_2, A_2 = A_2, tau = 19), data=s_df_2)
summary(m_2)

t_0_3 = head(s_df_3$t, n=1)
s_0_3 = 608
A_3 = head(s_df_3$s, n=1) - s_0_3
m_3 <- nls(s ~ s_0_3 + A_3 * exp(-(t - t_0_3) / tau),
           start=list(s_0_3 = s_0_3, A_3 = A_3, tau = 17), data=s_df_3)
summary(m_3)

t_0_4 = head(s_df_4$t, n=1)
s_0_4 = 567
A_4 = head(s_df_4$s, n=1) - s_0_4
m_4 <- nls(s ~ s_0_4 + A_4 * exp(-(t - t_0_4) / tau),
           start=list(s_0_4 = s_0_4, A_4 = A_4, tau = 10), data=s_df_4)
summary(m_4)

m_1_per <- lm(t ~ seq_along(t), data=s_df_1)
summary(m_1_per)

m_2_per <- lm(t ~ seq_along(t), data=s_df_2)
summary(m_2_per)

m_3_per <- lm(t ~ seq_along(t), data=s_df_3)
summary(m_3_per)

m_4_per <- lm(t ~ seq_along(t), data=s_df_4)
summary(m_4_per)

qplot(t, s, data=s_df_1, main="Peaks of damped oscillator",
      xlab="t / s", ylab="s / mm") +
      geom_errorbar(data=s_df_1, mapping=aes(ymin=s-U_s, ymax=s+U_s)) +
      geom_errorbarh(data=s_df_1, mapping=aes(xmin=t-U_t, xmax=t+U_t)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      #scale_y_continuous(minor_breaks = seq(0, 0.12, 0.002), breaks = seq(0, 0.12, 0.01)) +
      #scale_x_continuous(minor_breaks = seq(0, 110, 2), breaks = seq(0, 110, 10)) +
      geom_line(aes(y = predict(m_1)), color="steelblue", size=1)

qplot(seq_along(t), t, data=s_df_1, main="Peak times",
      xlab="n", ylab="t_n / s") +
      geom_errorbar(data=s_df_1, mapping=aes(ymin=t-U_t, ymax=t+U_t)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      #scale_y_continuous(minor_breaks = seq(0, 0.12, 0.002), breaks = seq(0, 0.12, 0.01)) +
      #scale_x_continuous(minor_breaks = seq(0, 110, 2), breaks = seq(0, 110, 10)) +
      geom_smooth(method = "lm", color="steelblue")

qplot(t, s, data=s_df_2, main="Peaks of damped oscillator",
      xlab="t / s", ylab="s / mm") +
      geom_errorbar(data=s_df_2, mapping=aes(ymin=s-U_s, ymax=s+U_s)) +
      geom_errorbarh(data=s_df_2, mapping=aes(xmin=t-U_t, xmax=t+U_t)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      #scale_y_continuous(minor_breaks = seq(0, 0.12, 0.002), breaks = seq(0, 0.12, 0.01)) +
      #scale_x_continuous(minor_breaks = seq(0, 110, 2), breaks = seq(0, 110, 10)) +
      geom_line(aes(y = predict(m_2)), color="steelblue", size=1)

qplot(seq_along(t), t, data=s_df_2, main="Peak times",
      xlab="n", ylab="t_n / s") +
      geom_errorbar(data=s_df_2, mapping=aes(ymin=t-U_t, ymax=t+U_t)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      #scale_y_continuous(minor_breaks = seq(0, 0.12, 0.002), breaks = seq(0, 0.12, 0.01)) +
      #scale_x_continuous(minor_breaks = seq(0, 110, 2), breaks = seq(0, 110, 10)) +
      geom_smooth(method = "lm", color="steelblue")

qplot(t, s, data=s_df_3, main="Peaks of damped oscillator",
      xlab="t / s", ylab="s / mm") +
      geom_errorbar(data=s_df_3, mapping=aes(ymin=s-U_s, ymax=s+U_s)) +
      geom_errorbarh(data=s_df_3, mapping=aes(xmin=t-U_t, xmax=t+U_t)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      #scale_y_continuous(minor_breaks = seq(0, 0.12, 0.002), breaks = seq(0, 0.12, 0.01)) +
      #scale_x_continuous(minor_breaks = seq(0, 110, 2), breaks = seq(0, 110, 10)) +
      geom_line(aes(y = predict(m_3)), color="steelblue", size=1)

qplot(seq_along(t), t, data=s_df_3, main="Peak times",
      xlab="n", ylab="t_n / s") +
      geom_errorbar(data=s_df_3, mapping=aes(ymin=t-U_t, ymax=t+U_t)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      #scale_y_continuous(minor_breaks = seq(0, 0.12, 0.002), breaks = seq(0, 0.12, 0.01)) +
      #scale_x_continuous(minor_breaks = seq(0, 110, 2), breaks = seq(0, 110, 10)) +
      geom_smooth(method = "lm", color="steelblue")

qplot(t, s, data=s_df_4, main="Peaks of damped oscillator",
      xlab="t / s", ylab="s / mm") +
      geom_errorbar(data=s_df_4, mapping=aes(ymin=s-U_s, ymax=s+U_s)) +
      geom_errorbarh(data=s_df_4, mapping=aes(xmin=t-U_t, xmax=t+U_t)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      #scale_y_continuous(minor_breaks = seq(0, 0.12, 0.002), breaks = seq(0, 0.12, 0.01)) +
      #scale_x_continuous(minor_breaks = seq(0, 110, 2), breaks = seq(0, 110, 10)) +
      geom_line(aes(y = predict(m_4)), color="steelblue", size=1)

qplot(seq_along(t), t, data=s_df_4, main="Peak times",
      xlab="n", ylab="t_n / s") +
      geom_errorbar(data=s_df_4, mapping=aes(ymin=t-U_t, ymax=t+U_t)) +
      theme(panel.grid.minor = element_line(colour="gray", size=0.4),
            panel.grid.major = element_line(colour="gray", size=1),
            panel.background = element_blank()) +
      #scale_y_continuous(minor_breaks = seq(0, 0.12, 0.002), breaks = seq(0, 0.12, 0.01)) +
      #scale_x_continuous(minor_breaks = seq(0, 110, 2), breaks = seq(0, 110, 10)) +
      geom_smooth(method = "lm", color="steelblue")
