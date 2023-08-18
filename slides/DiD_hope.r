library(tidyverse)
library(broom)
library(haven)
library(kableExtra)
library(patchwork)
library(fontawesome)
library(gapminder)
library(ggthemes)
library(scales)
library(infer)
library(ggdag)
library(dagitty)
library(modelsummary)
#' 
ggplot(data = tibble(x=c(0,10),
                     y=c(0,10)))+
  aes(x = x,
      y = y)+
  geom_text(x=1,y=0.75, label="Correlation", size =10, color = "#fde0dd")+
  geom_text(x=5,y=0.75, label="Causation", size = 10, color = "#7a0177")+
  
  
  geom_text(x=1,y=1.5, label="Differences", size =5, color = "#fde0dd")+
  geom_text(x=1,y=1.25, label="Pre-Post", size =5, color = "#fde0dd")+
  geom_text(x=2,y=1.75, label="Multiple Regression", size =5, color = "#fbb4b9")+
  geom_text(x=2,y=1.25, label="Matching", size =5, color = "#fbb4b9")+
    geom_text(x=3,y=2, label="Fixed Effects", size =5, color = "#f768a1")+
    geom_text(x=3.5,y=2.25, label="Diff-in-Diff", size =5, color = "#c51b8a")+
  geom_text(x=4,y=1.25, label="Natural Experiments", size =5, color = "#c51b8a")+
  geom_text(x=3.5,y=1.5, label="Regression Discontinuity", size =5, color = "#c51b8a")+
  geom_text(x=5,y=1.75, label="RCTs", size =5, color = "#7a0177")+
  annotate("segment", x = 1, xend = 5, y = 1, yend = 1, colour = "black", size=2, alpha=1, arrow=arrow(length=unit(0.5,"cm"), ends="last", type="closed"))+
  scale_x_continuous(breaks=seq(0,6,1),
                     limits=c(0,6))+
  scale_y_continuous(breaks=seq(0,3,1),
                     limits=c(0.5,2.3))+
  theme_void(base_family="Fira Sans Condensed", base_size = 14)

#' 

hotdogs <- tribble(
  ~price, ~cheese, ~chili,
  2, 0, 0,
  2.35, 1, 0,
  2.35, 0, 1,
  2.7, 1, 1
)
hotdogs

#' 

lm(price ~ cheese + chili + cheese*chili,
   data = hotdogs) %>%
  tidy() %>%
  select(term, estimate) %>%
  mutate(estimate = round(estimate,2))
#' 

control<-tribble(
  ~x, ~y,
  1, 1,
  2, 3
)
treatment<-tribble(
  ~x, ~y,
  1, 4,
  2, 8
)

#' 

ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_text(x=0.9,y=1, color = "red", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 2
  geom_text(x=2.1,y=2, color = "red", label=expression(hat(beta)[2]))+

  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_text(x=0.9,y=1, color = "red", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 2
  geom_text(x=2.1,y=2, color = "red", label=expression(hat(beta)[2]))+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # beta 0 plus beta 1 
  geom_text(x=0.8,y=4, color = "green", label=expression(hat(beta)[0]+hat(beta)[1]))+
  
    # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  # beta 2
  geom_text(x=2.1,y=5, color = "red",label=expression(hat(beta)[2]))+

  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#| fig-align: center
ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_text(x=0.9,y=1, color = "red", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 2
  geom_text(x=2.1,y=2, color = "red", label=expression(hat(beta)[2]))+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # beta 0 plus beta 1 
  geom_text(x=0.8,y=4, color = "green", label=expression(hat(beta)[0]+hat(beta)[1]))+
  
  
  # beta 1 dif
  geom_segment(x=1, y=1, xend=1, yend=4, color="black", linetype = "dashed", size = 1)+
  geom_text(x=0.9,y=2.5, color = "black", label=expression(hat(beta)[1]))+

    # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  # beta 2
  geom_text(x=2.1,y=5, color = "red",label=expression(hat(beta)[2]))+

  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#| fig-align: center
ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_text(x=0.9,y=1, color = "red", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 2
  geom_text(x=2.1,y=2, color = "red", label=expression(hat(beta)[2]))+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # beta 0 plus beta 1 
  geom_text(x=0.8,y=4, color = "green", label=expression(hat(beta)[0]+hat(beta)[1]))+
  
  
  # beta 1 dif
  geom_segment(x=1, y=1, xend=1, yend=4, color="black", linetype = "dashed", size = 1)+
  geom_text(x=0.9,y=2.5, color = "black", label=expression(hat(beta)[1]))+

    # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  geom_segment(x=2, y=6, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  # beta 2
  geom_text(x=2.1,y=5, color = "red",label=expression(hat(beta)[2]))+

  # beta 3
  geom_text(x=2.1,y=7, color = "blue", label=expression(hat(beta)[3]))+

  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#| fig-align: center
ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_label(x=0.9,y=1, color = "black", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  geom_segment(x=2, y=6, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#' :::
#| fig-align: center
ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_label(x=0.9,y=1, color = "black", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 1+2
  geom_label(x=2.2,y=3, color = "black", label=expression(hat(beta)[0]+hat(beta)[2]))+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  geom_segment(x=2, y=6, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#| fig-align: center
ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_label(x=0.9,y=1, color = "black", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 1+2
  geom_label(x=2.2,y=3, color = "black", label=expression(hat(beta)[0]+hat(beta)[2]))+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # beta 0 plus beta 1 
  geom_label(x=0.8,y=4, color = "black", label=expression(hat(beta)[0]+hat(beta)[1]))+
  

  # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  geom_segment(x=2, y=6, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#' :::
ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_label(x=0.9,y=1, color = "black", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 1+2
  geom_label(x=2.2,y=3, color = "black", label=expression(hat(beta)[0]+hat(beta)[2]))+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # beta 0 plus beta 1 
  geom_label(x=0.8,y=4, color = "black", label=expression(hat(beta)[0]+hat(beta)[1]))+
  

  # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  geom_segment(x=2, y=6, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  # beta 3
  geom_label(x=2.25,y=8, color = "black",
             label=expression(hat(beta)[0]+hat(beta)[1]+hat(beta)[2]+hat(beta)[3]))+

  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#| fig-align: center
ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_label(x=0.9,y=1, color = "black", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 1+2
  geom_label(x=2.2,y=3, color = "black", label=expression(hat(beta)[0]+hat(beta)[2]))+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # beta 0 plus beta 1 
  geom_label(x=0.8,y=4, color = "black", label=expression(hat(beta)[0]+hat(beta)[1]))+
  

  # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  geom_segment(x=2, y=6, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  # beta 3
  geom_label(x=2.25,y=8, color = "black",
             label=expression(hat(beta)[0]+hat(beta)[1]+hat(beta)[2]+hat(beta)[3]))+

  # beta 1 dif
  annotate("segment", x = 1, xend = 1, y = 1, yend = 4, colour = "black", size=2, alpha=1, arrow=arrow(length=unit(0.5,"cm"), ends="both", type="closed"))+
  geom_label(x=1,y=2.5, size = 5, color = "black", label=expression(hat(beta)[1]))+
  
    scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#' :::
ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_label(x=0.9,y=1, color = "black", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 1+2
  geom_label(x=2.2,y=3, color = "black", label=expression(hat(beta)[0]+hat(beta)[2]))+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # beta 0 plus beta 1 
  geom_label(x=0.8,y=4, color = "black", label=expression(hat(beta)[0]+hat(beta)[1]))+
  

  # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  geom_segment(x=2, y=6, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  # beta 3
  geom_label(x=2.25,y=8, color = "black",
             label=expression(hat(beta)[0]+hat(beta)[1]+hat(beta)[2]+hat(beta)[3]))+

  # beta 1 dif
  annotate("segment", x = 1, xend = 1, y = 1, yend = 4, colour = "black", size=2, alpha=1, arrow=arrow(length=unit(0.5,"cm"), ends="both", type="closed"))+
  geom_label(x=1,y=2.5, size = 5, color = "black", label=expression(hat(beta)[1]))+
  
  # beta 2 dif
  annotate("segment", x = 2, xend = 2, y = 1, yend = 3, colour = "black", size=2, alpha=1, arrow=arrow(length=unit(0.5,"cm"), ends="both", type="closed"))+
  geom_label(x=2,y=2, size = 5, color = "black", label=expression(hat(beta)[2]))+
  
  # beta 2 dif
  annotate("segment", x = 2, xend = 2, y = 4, yend = 6, colour = "black", size=2, alpha=1, arrow=arrow(length=unit(0.5,"cm"), ends="both", type="closed"))+
  geom_label(x=2,y=5, size = 5, color = "black", label=expression(hat(beta)[2]))+
  
    scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#| fig-align: center
ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # beta 0 
  geom_label(x=0.9,y=1, color = "black", label=expression(hat(beta)[0]))+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+
  # beta 1+2
  geom_label(x=2.2,y=3, color = "black", label=expression(hat(beta)[0]+hat(beta)[2]))+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # beta 0 plus beta 1 
  geom_label(x=0.8,y=4, color = "black", label=expression(hat(beta)[0]+hat(beta)[1]))+
  

  # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+

  geom_segment(x=2, y=6, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  # beta 3
  geom_label(x=2.25,y=8, color = "black",
             label=expression(hat(beta)[0]+hat(beta)[1]+hat(beta)[2]+hat(beta)[3]))+

  # beta 1 dif
  annotate("segment", x = 1, xend = 1, y = 1, yend = 4, colour = "black", size=2, alpha=1, arrow=arrow(length=unit(0.5,"cm"), ends="both", type="closed"))+
  geom_label(x=1,y=2.5, size = 5, color = "black", label=expression(hat(beta)[1]))+
  
  # beta 2 dif
  annotate("segment", x = 2, xend = 2, y = 1, yend = 3, colour = "black", size=2, alpha=1, arrow=arrow(length=unit(0.5,"cm"), ends="both", type="closed"))+
  geom_label(x=2,y=2, size = 5, color = "black", label=expression(hat(beta)[2]))+
  
  # beta 2 dif
  annotate("segment", x = 2, xend = 2, y = 4, yend = 6, colour = "black", size=2, alpha=1, arrow=arrow(length=unit(0.5,"cm"), ends="both", type="closed"))+
  geom_label(x=2,y=5, size = 5, color = "black", label=expression(hat(beta)[2]))+
  
  # beta 2 dif
  annotate("segment", x = 2, xend = 2, y = 6, yend = 8, colour = "blue", size=2, alpha=1, arrow=arrow(length=unit(0.5,"cm"), ends="both", type="closed"))+
  geom_label(x=2,y=7, size = 5, color = "blue", label=expression(hat(beta)[3]))+

    scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#' :::
#' ::: {.column width="50%"}

ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=6, color="red", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=6, color="red", linetype = "dashed", size = 1)+

  geom_segment(x=2, y=6, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)


ggplot(data = tibble(x=1,3))+
  aes(x = x)+
  # control
  geom_point(data = control, aes(x=x,y=y), size = 3, color = "red")+
  geom_text(x=1,y=0.5, color = "red", label=expression(C[1]))+
  geom_text(x=2,y=3.5, color = "red", label=expression(C[2]))+
  
  # line
  geom_segment(x=1, y=1, xend=2, yend=3, color="red", size = 2)+
  
  # line slope
  geom_segment(x=1, y=1, xend=2, yend=1, color="red", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=1, xend=2, yend=3, color="red", linetype = "dotted", size = 1)+

  # treatment
  geom_point(data = treatment, aes(x=x,y=y), size = 3, color = "green")+
  geom_text(x=1,y=4.5, color = "green", label=expression(T[1]))+
  geom_text(x=2,y=8.5, color = "green", label=expression(T[2]))+
  
    # line
  geom_segment(x=1, y=4, xend=2, yend=8, color="green", size = 2)+

  # line slope
  geom_segment(x=1, y=4, xend=2, yend=4, color="green", linetype = "dotted", size = 1)+
  geom_segment(x=2, y=4, xend=2, yend=4.5, color="purple", linetype = "dotted", size = 1)+
  
  # diag
  geom_segment(x=1, y=4, xend=2, yend=4.5, color="purple", linetype = "dashed", size = 1)+

  geom_segment(x=2, y=4.5, xend=2, yend=8, color="blue", linetype = "dashed", size = 1)+
  
  scale_x_continuous(breaks=c(1,2),
                  labels=c(expression(t[before]),expression(t[after])),
                  limits=c(0.5,2.5))+
  scale_y_continuous(breaks=NULL,
                     limits=c(0,9))+
  labs(x = "Time",
       y = "Y")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=20)

#' 

hope <- read_csv("https://metricsf22.classes.ryansafner.com/files/data/HOPE.csv")


hope |> glimpse()


hope <- hope %>%
  mutate_at(c("StateCode", "Year"),factor)

#' 
hope


dagify(
  Enroll ~ HOPE + GA + After,
  HOPE ~ GA + After,
  exposure = "HOPE",
  outcome = "Enroll",
  coords = list(x = c(HOPE = 1, Enroll = 3, GA = 2, After = 2),
                y = c(HOPE = 1, Enroll = 1, GA = 2, After = 0))) %>%
  tidy_dagitty(seed = 2) %>%
  ggdag_status()+
  theme_dag()+
  theme(legend.position = "none")

DND_reg <- lm(InCollege ~ Georgia + After + Georgia*After, data = hope)
DND_reg %>% tidy()

#' 
# group mean for non-Georgian before 1992
hope %>%
  filter(Georgia == 0,
         After == 0) %>%
  summarize(prob = mean(InCollege))

# group mean for non-Georgian AFTER 1992
hope %>%
  filter(Georgia == 0,
         After == 1) %>%
  summarize(prob = mean(InCollege))

# group mean for Georgian before 1992
hope %>%
  filter(Georgia == 1,
         After == 0) %>%
  summarize(prob = mean(InCollege))

#' :::
hope %>%
  filter(Georgia == 1,
         After == 1) %>%
  summarize(prob = mean(InCollege))

plot<-hope %>%
  ggplot(data=.)+
  aes(x = After,
      y = InCollege,
      color = factor(Georgia))+
  geom_smooth(method="lm", se=FALSE, size = 2)+
  #scale_y_continuous(breaks=seq(0,1,0.125), limits=c(0,0.05))+
  #geom_abline(intercept = 0.301, slope = -0.004, linetype = "dashed", color = "gray",
  #            xlim = c(0,1))+
  scale_x_continuous(breaks=seq(0,1,1), labels=c("Before", "After"))+
scale_color_manual(name = "State",
                   labels = c("Neighbors", "Georgia"), values = c("blue", "red"))+
  labs(x = "Before or After HOPE",
       y = "Probability of Being Enrolled in College")+
  theme_classic(base_family = "Fira Sans Condensed",
           base_size=16)+
  #scale_y_continuous(limits = c(0.2,0.6),
  #                   expand = c(0,0))+
  theme(legend.position = "top")
plot

#' 
plot1 <- plot+ geom_segment(x = 0, xend = 1, y = 0.301, yend = 0.297, linetype = "dashed", color = "gray", size = 2)

plot+plot1

#' 
DND_fe <- lm(InCollege ~ Georgia*After + factor(StateCode) + factor(Year),
           data = hope)
DND_fe %>% tidy()

#' 
library(fixest)
DND_fe_2 <- feols(InCollege ~ Georgia*After | factor(StateCode) + factor(Year),
           data = hope)
DND_fe_2 %>% tidy()

#' 
DND_fe_controls <- lm(InCollege ~ Georgia*After + factor(StateCode) + factor(Year) + Black + LowIncome,
           data = hope)
DND_fe_controls %>% tidy()

#' 
DND_fe_controls_2 <- feols(InCollege ~ Georgia*After + Black + LowIncome | factor(StateCode) + factor(Year),
           data = hope)
DND_fe_controls_2 %>% tidy()

#' 
library(modelsummary)
modelsummary(models = list("No FE" = DND_reg,
                           "TWFE" = DND_fe_2,
                           "TWFE" = DND_fe_controls_2),
             fmt = 5, # round to 2 decimals
             output = "docs",
             coef_rename = c("(Intercept)" = "Constant",
                             "Georgia:After" = "Georgia x After",
                             "LowIcome" = "Low Income"),
             coef_omit = "html",
             gof_map = list(
               list("raw" = "nobs", "clean" = "n", "fmt" = 0),
               #list("raw" = "r.squared", "clean" = "R<sup>2</sup>", "fmt" = 2),
               list("raw" = "adj.r.squared", "clean" = "Adj. R<sup>2</sup>", "fmt" = 2),
               list("raw" = "rmse", "clean" = "SER", "fmt" = 2)
             ),
             escape = FALSE,
             stars = c('*' = .1, '**' = .05, '***' = 0.01)
)

#' 
