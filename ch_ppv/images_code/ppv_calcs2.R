library(tidyverse)


get_dots <- function(rows, columns) {
  dots = expand.grid(rows, columns)
  dots <- as.data.frame(dots)
  names(dots) <- c("y", "x")
  return(dots)
}

get_stats <- function(prob_true, power, n = 1000) {
  
  alpha <- .05

  num_true <- n* prob_true 
  num_true_sig <- power * num_true
  
  num_false <- n * (1- prob_true)
  num_false_sig <- num_false * alpha
  
  output <- list(num_false_sig = num_false_sig,
                 num_true_sig = num_true_sig)
  return(output)
}

yellow = "#fbbc41"
green = "#779803"
dark_green = "#2d4704"
red = "#ee2b01"


prob_true  = .4
power = .3

xs<-get_stats(prob_true = prob_true, power = power, n = 1000 )
print(xs)

x <- seq(1, 50)
y <- seq(1,20)

n_effect_rows <- prob_true * 1000 / 50 #8 
n_noeffect_rows <- (1-prob_true) * 1000 / 50 #12



noeffect_squares <- get_dots(y, x)
noeffect_squares_sig1 <- get_dots(seq(9, 20), seq(49, 50)) #24
noeffect_squares_sig2 <- get_dots(seq(9, 14), 48) #6

effect_squares   <- get_dots(seq(1,8), x)
effect_squares_nonsig <- get_dots(seq(1,8), seq(1,35))


# just prob_true
sqplot1 = ggplot(data = noeffect_squares, aes(x = x, y = y)) +
  geom_point(shape = 15, size = 3, color = yellow) +
  geom_point(data = effect_squares, shape = 15, size = 3, color = green) +
  annotate(geom = "text", x = -.5, y = 7, label = "Alternative", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 5, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 3, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 16, label = "Null", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 12, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -12, y = 19, label = "Population-level Reality", hjust = 1, angle = 90, size = 5) +
  annotate(geom = "text", x = -10, y = 14, label = "", hjust = 1) +
  coord_fixed() +
  theme_void()

#prob true + false positive
sqplot2 = ggplot(data = noeffect_squares, aes(x = x, y = y)) +
  geom_point(shape = 15, size = 3, color = yellow) +
  geom_point(data = effect_squares, shape = 15, size = 3, color = green) +
  geom_point(data = noeffect_squares_sig1, shape = 15, size = 3, color = dark_green) +
  geom_point(data = noeffect_squares_sig2, shape = 15, size = 3, color = dark_green) +
  annotate(geom = "text", x = -.5, y = 7, label = "Alternative", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 5, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 3, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 16, label = "Null", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 12, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -12, y = 19, label = "Population-level Reality", hjust = 1, angle = 90, size = 5) +
  annotate(geom = "text", x = -10, y = 14, label = "", hjust = 1) +
  coord_fixed() +
  theme_void()



#prob true + false positive + power
sqplot3 =ggplot(data = noeffect_squares, aes(x = x, y = y)) +
  geom_point(shape = 15, size = 3, color = yellow) +
  geom_point(data = effect_squares, shape = 15, size = 3, color = green) +
  geom_point(data = effect_squares_nonsig, shape = 15, size = 3, color = red) +
  geom_point(data = noeffect_squares_sig1, shape = 15, size = 3, color = dark_green) +
  geom_point(data = noeffect_squares_sig2, shape = 15, size = 3, color = dark_green) +
  annotate(geom = "text", x = -.5, y = 7, label = "Alternative", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 5, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 3, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 16, label = "Null", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 12, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -12, y = 19, label = "Population-level Reality", hjust = 1, angle = 90, size = 5) +
  annotate(geom = "text", x = -10, y = 14, label = "", hjust = 1) +
  coord_fixed() +
  theme_void()


ggsave("sqplot1.png", sqplot1, width = 8, height = 8, dpi = "print")
ggsave("sqplot2.png", sqplot2, width = 8, height = 8, dpi = "print")
ggsave("sqplot3.png", sqplot3, width = 8, height = 8, dpi = "print")

#prob true
areaplot1 = ggplot(data = noeffect_squares, aes(x = x, y = y)) +
  annotate(geom = "rect", xmin = 0, xmax = 20, ymin = 0, ymax = 20*prob_true , fill = green) +
  annotate(geom = "rect", xmin = 0, xmax = 20, ymin = 20*prob_true, ymax = 20, fill = yellow) +
  annotate(geom = "text", x = -.5, y = 5.8, label = "Alternative", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 5, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 4.2, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14.8, label = "Null", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 13.2, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -5, y = 16, label = "Population-level Reality", hjust = 1, angle = 90, size = 7) +
  coord_fixed() +
  theme_void()


#prob true + false positive
areaplot2 = ggplot(data = noeffect_squares, aes(x = x, y = y)) +
  annotate(geom = "rect", xmin = 0, xmax = 20, ymin = 0, ymax = 20*prob_true , fill = green) +
  annotate(geom = "rect", xmin = 0, xmax = 20, ymin = 20*prob_true, ymax = 20, fill = yellow) +
  annotate(geom = "rect", xmin = .95*20, xmax = 20, ymin = 20*prob_true, ymax = 20, fill = dark_green) +
  annotate(geom = "text", x = -.5, y = 5.8, label = "Alternative", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 5, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 4.2, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14.8, label = "Null", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 13.2, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -5, y = 16, label = "Population-level Reality", hjust = 1, angle = 90, size = 7) +
  annotate(geom = "text", x = 19.5, y = 14, label = "False Positive", size = 5, colour = "white", angle = 90) +
  annotate(geom = "text", x = 9, y = 15, label = "True Negative", size = 5, colour = "white") +
  annotate(geom = "text", x = 9, y = 14, label = "(Correct Decision)", size = 5, colour = "white") +
  coord_fixed() +
  theme_void()


#prob true + false positive
areaplot3 = ggplot(data = noeffect_squares, aes(x = x, y = y)) +
  annotate(geom = "rect", xmin = 0, xmax = 20, ymin = 0, ymax = 20*prob_true , fill = green) +
  annotate(geom = "rect", xmin = 0, xmax = 20, ymin = 20*prob_true, ymax = 20, fill = yellow) +
  annotate(geom = "rect", xmin = .95*20, xmax = 20, ymin = 20*prob_true, ymax = 20, fill = dark_green) +
  annotate(geom = "rect", xmin = 0, xmax = .7*20, ymin = 0, ymax = 20*prob_true, fill = red) +
  annotate(geom = "text", x = -.5, y = 5.8, label = "Alternative", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 5, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 4.2, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14.8, label = "Null", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 13.2, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -5, y = 16, label = "Population-level Reality", hjust = 1, angle = 90, size = 7) +
  annotate(geom = "text", x = 19.5, y = 14, label = "False Positive", size = 5, colour = "white", angle = 90) +
  annotate(geom = "text", x = 7, y = 5, label = "False Negative", size = 5, colour = "white") +
  annotate(geom = "text", x = 17, y = 5, label = "True Positive", size = 5, colour = "white") +
  annotate(geom = "text", x = 17, y = 4, label = "(Correct Decision)", size = 5, colour = "white") +
  annotate(geom = "text", x = 9, y = 15, label = "True Negative", size = 5, colour = "white") +
  annotate(geom = "text", x = 9, y = 14, label = "(Correct Decision)", size = 5, colour = "white") +
    coord_fixed() +
  theme_void()



ggsave("areaplot1.png", areaplot1, width = 8, height = 8, dpi = "print")
ggsave("areaplot2.png", areaplot2, width = 8, height = 8, dpi = "print")
ggsave("areaplot3.png", areaplot3, width = 8, height = 8, dpi = "print")



yellow2 = "#fbbc41"
green2 = "#779803"
dark_green2 = "#2d4704"
red2 = "#ee2b01"




#prob true + false positive
areaplot4 = ggplot(data = noeffect_squares, aes(x = x, y = y)) +
  annotate(geom = "rect", xmin = 0, xmax = 20, ymin = 0, ymax = 10 , fill = green) +
  annotate(geom = "rect", xmin = 0, xmax = 20, ymin = 10, ymax = 20, fill = yellow) +
  annotate(geom = "rect", xmin = 10, xmax = 20, ymin = 10, ymax = 20, fill = dark_green) +
  annotate(geom = "rect", xmin = 0, xmax = 10, ymin = 0, ymax = 10, fill = red) +
  annotate(geom = "text", x = -.5, y = 5.8, label = "Alternative", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 5, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 4.2, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14.8, label = "Null", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 14, label = "hypothesis", hjust = 1, size = 5) +
  annotate(geom = "text", x = -.5, y = 13.2, label = "is true", hjust = 1, size = 5) +
  annotate(geom = "text", x = -5, y = 16, label = "Population-level Reality", hjust = 1, angle = 90, size = 7) +
  annotate(geom = "text", x = 10, y = 24, label = "Test Conclusion", size = 7) +
  annotate(geom = "text", x = 15, y = 22, label = "Reject", size = 5) +
  annotate(geom = "text", x = 15, y = 21, label = "Null", size = 5) +
  annotate(geom = "text", x = 5, y = 22, label = "Fail to", size = 5) +
  annotate(geom = "text", x = 5, y = 21, label = "Reject Null", size = 5) +
  annotate(geom = "text", x = 15, y = 15, label = "False Positive", size = 5, colour = "white") +
  annotate(geom = "text", x = 5, y = 5, label = "False Negative", size = 5, colour = "white") +
  annotate(geom = "text", x = 15, y = 5, label = "True Positive", size = 5, colour = "white") +
  annotate(geom = "text", x = 15, y = 4, label = "(Correct Decision)", size = 5, colour = "white") +
  annotate(geom = "text", x = 5, y = 15, label = "True Negative", size = 5, colour = "white") +
  annotate(geom = "text", x = 5, y = 14, label = "(Correct Decision)", size = 5, colour = "white") +
  coord_fixed() +
  theme_void()

ggsave("areaplot4.png", areaplot4, width = 8, height = 8, dpi = "print")

