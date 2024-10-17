data <- read.csv("C:/Users/cstra/data/EuroVsCopa.csv")
View(data)
summary(data)

data$goals <- data$home_goals + data$away_goals
data$XG <- data$Home.Expected.goals.xG. + data$Away.Expected.goals.xG.
data$shots <- data$Home.Total.shots + data$Away.Total.shots
data$fouls <- data$Home.Fouls.committed + data$Away.Fouls.committed
data$yellows <- data$Home.Yellow.cards + data$Away.Yellow.cards
data$reds <- data$Home.Red.cards + data$Away.Red.cards
data$passes <- data$Home.Passes + data$Away.Passes
data$passes.own.half <- data$Home.Own.half + data$Away.Own.half
data$passes.opp.half <- data$Home.Opposition.half + data$Away.Opposition.half


euro <- data[1:51,]
copa <- data[52:81,]
View(euro)
View(copa)
summary(euro)
summary(copa)


t.test(euro$goals, copa$goals)
t.test(euro$XG, copa$XG)
t.test(euro$shots, copa$shots)
t.test(euro$fouls, copa$fouls)
t.test(euro$yellows, copa$yellows)
t.test(euro$reds, copa$reds)
t.test(euro$passes, copa$passes)
t.test(euro$passes.own.half, copa$passes.own.half)
t.test(euro$passes.opp.half, copa$passes.opp.half)

#shoutout copilot 

# Load necessary libraries
library(ggplot2)

# Perform t-tests and store results
t_tests <- list(
  goals = t.test(euro$goals, copa$goals),
  XG = t.test(euro$XG, copa$XG),
  shots = t.test(euro$shots, copa$shots),
  fouls = t.test(euro$fouls, copa$fouls),
  yellows = t.test(euro$yellows, copa$yellows),
  reds = t.test(euro$reds, copa$reds),
  passes = t.test(euro$passes, copa$passes),
  passes_own_half = t.test(euro$passes.own.half, copa$passes.own.half),
  passes_opp_half = t.test(euro$passes.opp.half, copa$passes.opp.half)
)

# Function to create plot for each t-test
create_plot <- function(data, t_test_result, title) {
  plot_data <- data.frame(
    group = c("Euro", "Copa"),
    mean = c(mean(data[[1]]), mean(data[[2]])),
    sd = c(sd(data[[1]]), sd(data[[2]])),
    se = c(sd(data[[1]])/sqrt(length(data[[1]])), sd(data[[2]])/sqrt(length(data[[2]])))
  )
  
  ggplot(plot_data, aes(x = group, y = mean)) +
    geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    labs(title = paste(title, "\n(t =", round(t_test_result$statistic, 2), ", p =", round(t_test_result$p.value, 4), ")"),
         x = "Tournament", y = "Mean") +
    theme_minimal()
}

# Create plots
plots <- list(
  goals = create_plot(list(euro$goals, copa$goals), t_tests$goals, "Comparison of Goals between Euro and Copa"),
  XG = create_plot(list(euro$XG, copa$XG), t_tests$XG, "Comparison of Expected Goals (XG) between Euro and Copa"),
  shots = create_plot(list(euro$shots, copa$shots), t_tests$shots, "Comparison of Shots between Euro and Copa"),
  fouls = create_plot(list(euro$fouls, copa$fouls), t_tests$fouls, "Comparison of Fouls between Euro and Copa"),
  yellows = create_plot(list(euro$yellows, copa$yellows), t_tests$yellows, "Comparison of Yellow Cards between Euro and Copa"),
  reds = create_plot(list(euro$reds, copa$reds), t_tests$reds, "Comparison of Red Cards between Euro and Copa"),
  passes = create_plot(list(euro$passes, copa$passes), t_tests$passes, "Comparison of Passes between Euro and Copa"),
  passes_own_half = create_plot(list(euro$passes.own.half, copa$passes.own.half), t_tests$passes_own_half, "Comparison of Passes in Own Half between Euro and Copa"),
  passes_opp_half = create_plot(list(euro$passes.opp.half, copa$passes.opp.half), t_tests$passes_opp_half, "Comparison of Passes in Opponent's Half between Euro and Copa")
)

# Display plots
plots$goals
plots$XG
plots$shots
plots$fouls
plots$yellows
plots$reds
plots$passes
plots$passes_own_half
plots$passes_opp_half






