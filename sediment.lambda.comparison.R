#size of sediment (percent) compare lambdas

#wrangle quantification of sediment and jeff lambdas together
lambdas = read.csv("lambdas.csv")
sediment_size = readRDS("mod_posteriors_unstd")

#model relationship between the two 

lss.m1 <- brm(
formula = lambda ~ sediment size, 
data = 
)