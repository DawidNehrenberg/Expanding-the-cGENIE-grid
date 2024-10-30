#
library(lhs)
library(tidyverse)
library(reshape)
library(httpgd)
#
set.seed(2004)
Param_val <- randomLHS(56, 5, preserveDraw = TRUE)
Param_val <- as.data.frame(Param_val)
colnames(Param_val) <- c("Horiz", "Vmax", "Ea(1)", "POC", "Vert")
 horiz <- 1.22
 vmax <- 6
 ea1 <- 7
 poc <- 0.03
 vert <- 0.5
Param_val <- Param_val %>%
  transmute(
    Horiz = 1494.438354492187500000 * (1.78 - (Horiz * horiz)),
    Vmax = 10 - (Vmax * vmax),
    Ea1 = 60 - (`Ea(1)` * ea1),
    POC = 0.032 - (POC * poc),
    Vert = 0.000025363247914356 * (1.25 - (Vert * vert))
  )
ggplot(Param_val, aes(x = Horiz, y = Vert)) +
    geom_point() +
    theme_bw()
write.csv(Param_val, file = "Initial_Parametrisation.csv", row.names = FALSE)
