sqrt_breaks = c(1,4,9,16,25,36,50,64,81)
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

SEsmooth=F
my_alpha=0.5
bun_limits = c(2,50)
crea_limits = c(20,500)

mortality_threshold = 0.05
