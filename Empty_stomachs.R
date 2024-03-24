library(glm2)
library(lm)

empty.df <- read.csv("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Mathias & Leah/empty_stomachs.csv", header=TRUE)

model1.glm <- glm(Empt1.1.0.~ Seaso0, family=binomial,
                data=empty.df)

summary(model1.glm)

anova(model1.glm, test ='Chi')


