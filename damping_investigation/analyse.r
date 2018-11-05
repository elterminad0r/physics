library(ggplot2)

U_s = 3
U_t = 0.1

s_0s = c(654, 656, 608, 567)
taus = c(30, 19, 17, 10)

cumul_df = data.frame()
s_dfs = c()
per_models = c()

for (i in 1:4) {
    message(paste("Analysing Experiment", i))
    s_df <- read.table(paste0("data/", i, ".csv"), sep=",", header=TRUE)
    s_df$n <- seq_along(s_df$t)
    s_df$i <- i
    s_dfs[[i]] <- s_df
    cumul_df <- rbind(cumul_df, s_df)
    print(s_df)

    t_0 = head(s_df$t, n=1)
    s_0 = s_0s[i]
    A = head(s_df$s, n=1) - s_0
    m_exp <- nls(s ~ s_0 + A * exp(-(t - t_0) / tau),
                 start=list(s_0 = s_0, A = A, tau = taus[i]), data=s_df)
    coef_exp <- coef(summary(m_exp))
    print(coef_exp)

    m_per <- lm(t ~ n, data=s_df)
    coef_per <- coef(summary(m_per))
    print(coef_per)
    per_models[[i]] <- m_per

    message(paste("So, Q = pi * tau / T =",
                   pi * coef_exp["tau", "Estimate"]
                      / coef_per["n", "Estimate"]))

    print(qplot(t, s, data=s_df,
          main=paste0("Local maximum displacement of damped oscillator (",
                       i, ")"),
          xlab="t / s", ylab="s / mm") +
          geom_errorbar(data=s_df, mapping=aes(ymin=s-U_s, ymax=s+U_s)) +
          geom_errorbarh(data=s_df, mapping=aes(xmin=t-U_t, xmax=t+U_t)) +
          theme(panel.grid.minor = element_line(colour="gray", size=0.4),
                panel.grid.major = element_line(colour="gray", size=1),
                panel.background = element_blank()) +
          geom_line(aes(y = predict(m_exp)), color="steelblue", size=1))
}

p_plot <- ggplot(cumul_df, aes(x = n, y = t)) +
    geom_errorbar(aes(ymin=t-U_t, ymax=t+U_t, color=factor(i))) +
    geom_errorbarh(aes(height=2, xmin=n, xmax=n, color=factor(i))) +
    labs(x="n", y="t / s",
         title="Period of oscillation of different damped oscillators") +
    theme(panel.grid.minor = element_line(colour="gray", size=0.4),
          panel.grid.major = element_line(colour="gray", size=1),
          panel.background = element_blank()) +
    scale_color_discrete(name = "Experiment") +
    geom_point(aes(color=factor(i)))

for (i in 1:4) {
    p_plot <- p_plot + geom_line(y = predict(per_models[[i]]), data=s_dfs[[i]],
                                 aes(color=factor(i)))
}

p_plot
