

inMidtown <- function(px, py) {
    return(
        ifelse(-4000<px & px < -1000 & 26000 < py & py < 31000, 1, 0)
    )
}

dim(inMid)


univ1month <- univ1month %>% dplyr::select(-id, -pickup_nyct2010_gid, -dropoff_nyct2010_gid) %>% mutate( inMid = inMidtown(px,py) )
master <- univ1month  %>% filter( -7000 < px & px < 3000 & 26000 < py & py < 31000) #%>% sample_n(300000)


master2 <- univ1month  %>% filter( -7000 < px & px < 3000 & 26000 < py & py < 32000) %>% sample_n(100000)
# %>% : pipe function
# 






# mini <- univ1month %>% sample_n(10000) %>% mutate( inMidtown = ifelse(inMid==1,"in","out") ) 
# small <- univ1month %>% filter( inMid  == 1 ) %>% sample_n(50000)
# ggplot(mini %>% filter( trip_distance < 25) ) + geom_histogram(aes(trip_distance), binwidth=.3) # 3 modals
# ggplot(mini %>% filter( hpay < 50) ) + geom_histogram(aes(hpay)) # two modals
# 
# plot(mini$px,mini$py)
# ggplot(master, aes(x = px, y = py)) + geom_point(aes(color=inMid))

# Hmmm
# scaled <- master %>% mutate( pxs = ((px) - mean(px)) / sd(px),
#                              pys = ((py) - mean(py)) / sd(py))
# ggplot(scaled, aes(pxs, pys)) + geom_point()
# ggplot(scaled, aes(pxs, pys, color=isHigh)) + geom_point()

pdf('EDA/streets-histograms.pdf', width = 10,height=10)
for (sample_y in seq(26000, 31000, by=50) ) {
    p <-
    ggplot(master %>% filter(px > -4500 & px < 1250 &
                             py > sample_y-25 &
                             py < sample_y+25 )) + geom_histogram(aes(px), binwidth=80) +
        labs(title = paste0("y = ", sample_y))
    print(p)
    message(sample_y)
}
dev.off()

# gaussian mixture model
ggplot(master) + geom_point(aes(px, py), size=.5) + scale_y_continuous(breaks=seq(26000,31000,by=100)) + scale_x_continuous(breaks=seq(-8000,3000,by=500))

ggplot(master %>% filter(px < -1600) ) + geom_point(aes(px, py), size=.5) + scale_y_continuous(breaks=seq(26000,31000,by=100)) + scale_x_continuous(breaks=seq(-8000,3000,by=500))

ggplot(master %>% filter( 29750 < py & py < 30800 & -2500 < px & px < 0 ) ) + geom_point(aes(px, py))

mixed <- master %>% filter( -8000 < px & px < -3000 & 26500 < py & py < 27000)
mixed <- master %>% filter( 29750 < py & py < 30800 & -2500 < px & px < 0 )
mixed_well_separated<- master %>% filter( 26550 < py & py < 30250 & -5800 < px & px < -5200 )
mixed <- master %>% filter( -2250 < px & px < -1500 & 26600 < py & py < 30250  )

png('EDA/GAUSSIAN-overview.png', height=1440, width=1440)
ggplot(master2,aes(px,py)) + geom_point(size=.1, color="black", alpha=.9) + annotate("rect",xmin=min(mixed$px), xmax=max(mixed$px), ymin=min(mixed$py), ymax=max(mixed$py), fill="skyblue", alpha=.5, color="black") #geom_point(data=mixed, color="blue", size=.05, alpha=.5)
dev.off()

ggplot(mixed) + geom_point(aes(px, py), size=.5)
ggplot(mixed) + geom_histogram(aes(px), binwidth=80)
ggplot(mixed) + geom_histogram(aes(py), binwidth=30)
# http://docs.ggplot2.org/0.9.3.1/geom_histogram.html

library(stats)
library(mixtools) # https://www.jstatsoft.org/article/view/v032i06
first_mu<-seq(26668,30152,by=268)
emed <- normalmixEM(mixed$px, lambda = .2, mu = c(-7000,-6000,-5000,-4000,-3000), sigma = 5)
emed <- normalmixEM(mixed$px, lambda = .2, mu = c(-2500,-1400,-800,-400), sigma = c(5,3,2,2) )
emed_well_separated <- normalmixEM(mixed_well_separated$py, lambda = 1/length(first_mu), mu = first_mu, sigma = 5, verb = TRUE, epsilon = 5e-2)
emed <- normalmixEM(mixed$py, lambda = 1/length(first_mu), mu = first_mu, sigma = 5, verb = TRUE, epsilon = 5e-2)
emed_diffsigma <- normalmixEM(mixed$py, lambda = 1/length(first_mu), mu = first_mu, verb = TRUE, epsilon = 5e-2, sd.constr=rep(NA,length(first_mu)) )

plot(emed, density = TRUE, cex.axis = 1.4, cex.lab = 1.5, cex.main = 1.5, breaks=100,
     main2 = "Membership Density of Avenues", xlab2 = "Rotated x-axis")
# pretty good
abline(v=first_mu, lwd=1, lty=2)


plot(emed_diffsigma, density = TRUE, cex.axis = 1.4, cex.lab = 1.5, cex.main = 1.5, breaks=100,
     main2 = "Membership Density of Avenues", xlab2 = "Rotated x-axis")
# not reasonable


emed[c("lambda", "mu", "sigma")]

plot(mixed$px, mixed$py)
emed$mu

png("EDA/GAUSSIAN-means.png", width = 960, height=960)
with(master %>% filter(px < -1600) %>% sample_n(100000), plot(px,py, pch=18, cex=1,
                                                              main = "Estimated 14 Gaussian Means (after 36 iterations)"))
abline(h=emed$mu,col="red",lwd=3)
dev.off()

png("EDA/GAUSSIAN-means-blue.png", width = 960, height=960)
with(master %>% filter(px < -1600) %>% sample_n(100000), plot(px,py, pch=18, cex=.5,
                                                              main = "Initial Values of 14 Gaussian Means"))
abline(h=first_mu, lwd=3, col="blue", lty=2)
dev.off()


emed$sigma

# credit: http://stackoverflow.com/questions/25313578/any-suggestions-for-how-i-can-plot-mixem-type-data-using-ggplot2
gg.mixEM <- function(EM, breaks=50, npoly = 500, density=TRUE, bincolor="gray80", cons=NULL) {
    b <- breaks
    require(ggplot2)
    x       <- with(EM,seq(min(x),max(x),len=npoly))
    x_inter <- x[2] - x[1]
    pars    <- with(EM,data.frame(comp=colnames(posterior), mu, sigma,lambda))
    em.df   <- data.frame(x=rep(x,each=nrow(pars)),pars)
    if (is.null(cons))
        em.df$y <- with(em.df,lambda*dnorm(x,mean=mu,sd=sigma))
    else
        em.df$y <- with(em.df,cons*dnorm(x,mean=mu,sd=sigma))
    
    em.df <- em.df %>% mutate( y = ifelse((x==min(x)|x==max(x)) & y > 10^-06,0,y))
    
    # tooHighMin <- em.df %>% filter( x == min(x) & y > 10^-06)
    # tooHighMax <- em.df %>% filter( x == max(x) & y > 10^-06)
    # 
    # em.df <- bind_rows(em.df,
    #                    tooHighMin %>% mutate( y = 0, x = x - .1 * sd(x) ),
    #                    tooHighMax %>% mutate( y = 0, x = x + .1 * sd(x) )
    #                    )
    p <- ggplot(data.frame(x=EM$x),aes(x,y=..density..)) + 
        geom_histogram(fill=NA, bins=b, color=bincolor)+
        scale_fill_discrete("Component\nMeans",labels=format(em.df$mu,digits=3))+
        theme_bw()
    if (density)
        p + geom_polygon(data=em.df%>%arrange(x),aes(x,y,fill=comp),color="grey50", alpha=0.5)
    else
        p
    
}

mytheme <- scale_x_continuous(breaks=seq(26000,31000,by=200))

png("EDA/GAUSSIAN-avenue-membership-nodensity.png", width=960, height = 320)
    gg.mixEM(emed, density = FALSE, breaks=200, bincolor = "gray40") + mytheme
dev.off()

png("EDA/GAUSSIAN-avenue-membership.png", width=960, height = 320)
    gg.mixEM(emed, density = TRUE, breaks=200, bincolor = "gray60") + theme(legend.position="none")  + mytheme
dev.off()

png("EDA/GAUSSIAN-avenue-membership-same.png", width=960, height = 320)
    gg.mixEM(emed, density = TRUE, breaks=200, bincolor = "gray60", cons=.1) + theme(legend.position="none")  + mytheme
dev.off()
    

png("EDA/GAUSSIAN-well-membership-nodensity.png", width=960, height = 320)
gg.mixEM(emed_well_separated, density = FALSE, breaks=200, bincolor = "gray40") + mytheme
dev.off()

png("EDA/GAUSSIAN-well-membership.png", width=960, height = 320)
gg.mixEM(emed_well_separated, density = TRUE, breaks=200, bincolor = "gray60") + theme(legend.position="none")  + mytheme
dev.off()

png("EDA/GAUSSIAN-well-membership-same.png", width=960, height = 320)
gg.mixEM(emed_well_separated, density = TRUE, breaks=200, bincolor = "gray60", cons=.1) + theme(legend.position="none")  + mytheme
dev.off()


png("EDA/GAUSSIAN-diff-membership-nodensity.png", width=960, height = 320)
gg.mixEM(emed_diffsigma, density = FALSE, breaks=200, bincolor = "gray40") + mytheme
dev.off()

png("EDA/GAUSSIAN-diff-membership.png", width=960, height = 320)
gg.mixEM(emed_diffsigma, density = TRUE, breaks=200, bincolor = "gray60") + theme(legend.position="none")  + mytheme
dev.off()

png("EDA/GAUSSIAN-diff-membership-same.png", width=960, height = 320)
gg.mixEM(emed_diffsigma, density = TRUE, breaks=200, bincolor = "gray60", cons=.1) + theme(legend.position="none")  + mytheme
dev.off()


str(emed)

n_points <- 1000
x       <- seq(min(emed$x),max(emed$x),len=n_points)

tmp    <- data.frame(comp = colnames(emed$posterior),
                      mu = emed$mu,
                      sigma = emed$sigma,
                      lambda = emed$lambda)
em.df   <- data.frame(x=rep(x,each=nrow(tmp)),tmp)
em.df$y <- em.df$lambda * dnorm(em.df$x, mean=em.df$mu,sd=em.df$sigma)

ggplot(em.df ) + geom_bar(aes(x=x, y=y, fill=comp), position="fill", stat="identity")

ggplot(em.df %>% filter( -5000 < x & x < -4500  ) ) + geom_bar(aes(x=x, y=y, fill=comp), position="fill", stat="identity")

ggplot(em.df %>% filter( -5000 < x & x < -4500  ) ) + geom_bar(aes(x=x, y=y, fill=comp), position="fill", stat="identity")


decision_boundary <- em.df %>% group_by(x) %>% mutate( ratio =  y / sum(y) ) %>% arrange(desc(y)) %>% slice(1:2) %>% group_by(x) %>%  filter( min(ratio) > .49) %>% slice(1) %>% group_by() %>% .$x

plot(mixed$px, mixed$py)
abline(v=decision_boundary)

# multivariate normal

data("Waterdata")
cutpts <- 10.5*(-6:6)
watermult <- makemultdata(Waterdata, cuts = cutpts)

mult_x <- mixed %>% dplyr::select(px,py) %>% as.matrix

mved <- mvnormalmixEM(mult_x, mu = c(-2500,-1400,-800,-400), k = 4, arbmean = TRUE, arbvar = TRUE, verb = TRUE, epsilon = 7e-1)

mved$mu
nnn <- 1000
tmp <- 
bind_rows(
    cbind(rmvnorm(nnn, mved$mu[[1]], mved$sigma[[1]] ), comp = 'c1') %>% data.frame,
    cbind(rmvnorm(nnn, mved$mu[[2]], mved$sigma[[2]] ), comp = 'c2') %>% data.frame
) %>% mutate_each(funs(as.numeric), V1, V2)
ggplot(tmp) + geom_point(aes(x=V1,y=V2,color=comp))


bivn <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, .5, .5, 1), 2))
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
contour(bivn.kde)
image(bivn.kde)
persp(bivn.kde, phi = 45, theta = 30)
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA)

# http://mi.eng.cam.ac.uk/~mjfg/local/4F10/lect2.pdf

#demo(persp)

bivn


pmat = persp(x=1:5,y=1:3,z=matrix(3:17,ncol=3), xlab="X", theta=-60, ylab="Y", zlab="Z", ticktype="detailed", zlim = c(-1, 15) )
lines(trans3d(c(1,5), y=2:3, z= -1, pm = pmat), col = 3, lwd=4)





gaussian_params <- bind_rows(
    data.frame(lambda=0.011272, mu=8150.000000, sigma=185.539298),
    data.frame(lambda=0.145615, mu=8230.000000, sigma=1029.423415),
    data.frame(lambda=0.124117, mu=8310.000000, sigma=3231.427880),
    data.frame(lambda=0.011550, mu=8390.000000, sigma=6027.728843),
    data.frame(lambda=0.011215, mu=8470.000000, sigma=10532.436106),
    data.frame(lambda=0.023994, mu=8550.000000, sigma=17714.305149),
    data.frame(lambda=0.079880, mu=8630.000000, sigma=3824.382463),
    data.frame(lambda=0.195180, mu=8710.000000, sigma=5363.908043),
    data.frame(lambda=0.071077, mu=8790.000000, sigma=17541.390330),
    data.frame(lambda=0.038122, mu=8870.000000, sigma=13660.073926),
    data.frame(lambda=0.089617, mu=8950.000000, sigma=1815.763201),
    data.frame(lambda=0.092370, mu=9030.000000, sigma=4029.656484),
    data.frame(lambda=0.037928, mu=9110.000000, sigma=864.376077),
    data.frame(lambda=0.068063, mu=9190.000000, sigma=419.833916)
)
    

gaussian_params <- bind_rows(
    data.frame(lambda=0.018486, mu=8150.000000, sigma=221.792704),
    data.frame(lambda=0.116990, mu=8230.000000, sigma=819.869670),
    data.frame(lambda=0.122727, mu=8310.000000, sigma=2880.261984),
    data.frame(lambda=0.011777, mu=8390.000000, sigma=6414.157580),
    data.frame(lambda=0.012808, mu=8470.000000, sigma=15003.945386),
    data.frame(lambda=0.029430, mu=8550.000000, sigma=18068.980503),
    data.frame(lambda=0.070685, mu=8630.000000, sigma=10101.291456),
    data.frame(lambda=0.202362, mu=8710.000000, sigma=5287.882987),
    data.frame(lambda=0.076006, mu=8790.000000, sigma=20573.492889),
    data.frame(lambda=0.040758, mu=8870.000000, sigma=16394.046168),
    data.frame(lambda=0.095228, mu=8950.000000, sigma=2232.774360),
    data.frame(lambda=0.082019, mu=9030.000000, sigma=4461.613704),
    data.frame(lambda=0.042136, mu=9110.000000, sigma=546.128168),
    data.frame(lambda=0.078588, mu=9190.000000, sigma=430.476489)
)






gaussian_params <- bind_rows(
    data.frame(lambda=0.029342, mu=8659.051171, sigma=20277.251631),
    data.frame(lambda=0.032938, mu=8710.057950, sigma=18008.025764),
    data.frame(lambda=0.048345, mu=8364.354041, sigma=1917.888006),
    data.frame(lambda=0.035458, mu=8718.545013, sigma=16301.784332),
    data.frame(lambda=0.028884, mu=8650.695532, sigma=19977.399309),
    data.frame(lambda=0.031871, mu=8686.239044, sigma=20033.256599),
    data.frame(lambda=0.086478, mu=8725.494146, sigma=7720.714435),
    data.frame(lambda=0.033286, mu=8702.134938, sigma=18914.380704),
    data.frame(lambda=0.184734, mu=8983.828383, sigma=3847.528636),
    data.frame(lambda=0.119646, mu=9165.046448, sigma=1381.365598),
    data.frame(lambda=0.034617, mu=8715.554854, sigma=17080.235850),
    data.frame(lambda=0.036512, mu=8720.663584, sigma=15358.085736),
    data.frame(lambda=0.069528, mu=8677.190084, sigma=1084.693684),
    data.frame(lambda=0.228362, mu=8244.650064, sigma=2494.598312)
)