library(ggplot2)
library(splines)

g <- 9.81

my_theme <- theme(panel.grid.minor = element_line(colour="gray", size=0.4),
                panel.grid.major = element_line(colour="gray", size=1),
                panel.background = element_blank())

area_df <- data.frame(displ=numeric(0), A1=numeric(0), A2=numeric(0),
                      A_t=numeric(0))
peak_df <- data.frame(displ=numeric(0), max=numeric(0), min=numeric(0),
                      vel=numeric(0))

for (file in Sys.glob("*.csv")) {
s_df <- read.table(file, sep=",", header=TRUE)
colnames(s_df) <- c("V1", "V2", "V3")
s_df$t <- seq_along(s_df$V1)
d_start = 1
d_stop = nrow(s_df)

spl <- strsplit(file, "[._]")[[1]]
displ <- abs(strtoi(spl[1]) - strtoi(spl[2]))

for (i in 1:3) {
    message(paste(file, "run", i))
    message(paste("Displacement:", displ))

    V <- s_df[,i]
    my_glm <- glm(V ~ ns(t, df = 100), data = s_df)

    pred_func <- function(t) predict(my_glm, data.frame(t=t))[1]

    min_opt <- optimize(pred_func, lower=d_start, upper=d_stop, maximum=FALSE)
    max_opt <- optimize(pred_func, lower=d_start, upper=d_stop, maximum=TRUE)
    mid_opt <- optimize(function(t) abs(pred_func(t)), upper=min_opt$minimum,
                                                       lower=max_opt$maximum)

    int_1 <- integrate(Vectorize(pred_func), d_start, mid_opt$minimum)
    int_2 <- integrate(Vectorize(pred_func), mid_opt$minimum, d_stop)
    int_t <- integrate(Vectorize(pred_func), d_start, d_stop)

    peak_df[nrow(peak_df) + 1,] <-
        list(displ, max_opt$objective, min_opt$objective, sqrt(2 * g * displ))

    area_df[nrow(area_df) + 1,] <-
        list(displ, int_1$value, int_2$value, int_t$value)

    print(ggplot(s_df, aes(x = t, y = V)) +
          ggtitle(paste("Faraday's Law induced EMF", file, "run", i,
                        "Displacement", displ)) +
          labs(x="t / (s * k)", y="V / V") +
          geom_point(size=0.07, color="red") +
          my_theme +
          geom_line(aes(y = predict(my_glm)), color="steelblue",
                    size=2, alpha=0.5))
}}

my_theme2 <- function(ggp) ggp +  geom_point(color = "red") + my_theme +
             geom_smooth(method="lm", formula=y ~ x)

my_theme2(ggplot(area_df, aes(x = displ, y = A1)) +
    ggtitle("Area under first peak vs displacement") +
    labs(x="s / mm", y="A/(k * Wb)"))

my_theme2(ggplot(area_df, aes(x = displ, y = A2)) +
    ggtitle("Area under second peak vs displacement") +
    labs(x="s / mm", y="A/(k * Wb)"))

my_theme2(ggplot(area_df, aes(x = displ, y = A_t)) +
    ggtitle("Total area vs displacement") +
    labs(x="s / mm", y="A/(k * Wb)"))

my_theme2(ggplot(peak_df, aes(x = vel, y = max)) +
    ggtitle("First peak height vs velocity") +
    labs(x="v / (mm / s)", y="E/V"))

my_theme2(ggplot(peak_df, aes(x = vel, y = min)) +
    ggtitle("Second peak height vs velocity") +
    labs(x="s / (mm / s)", y="E/V"))
