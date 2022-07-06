library(metafor)
library(openxlsx)
library(netmeta)

dtbase <- read.xlsx(file.path( "data", "meta3.xlsx"))

dtbase <- dtbase[c(2:60),c(1:3,13:15)]
colnames(dtbase) <- c("Study","Treatment","duration","N","Mean","SD")
dtbase$Mean <- as.character(dtbase$Mean)
dtbase$Mean <- as.numeric(dtbase$Mean)
dtbase$SD <- as.character(dtbase$SD)
dtbase$SD <- as.numeric(dtbase$SD)
dtbase$N <- as.character(dtbase$N)
dtbase$N <- as.numeric(dtbase$N)

# warning: we need to trim empty characters before transforming to factor
dtbase$Treatment <- stringr::str_trim(dtbase$Treatment)
dtbase$Treatment <- as.factor(dtbase$Treatment)
dtbase$Treatment[which(dtbase$Treatment %in% c("CR","CER "))] <- "CER"
dtbase$Treatment[which(dtbase$Treatment %in% c("IF 5:2 ", "IF 5:2  ", "IER"))] <- "IF 5:2 "
dtbase$Treatment[which(dtbase$Treatment %in% c("TRE","TRF "))] <- "TRF"
dtbase$Treatment[which(dtbase$Treatment %in% c("Control","control"))] <- "Control"
dtbase$Treatment <- droplevels(dtbase$Treatment)
summary(dtbase$Treatment)

View(dtbase)

## Direct comparison
CER_MADF <- rbind(cbind(dtbase[2,c(1,4,5,6)],dtbase[3,c(4,5,6)]),
                  cbind(dtbase[6,c(1,4,5,6)],dtbase[7,c(4,5,6)]),
                  cbind(dtbase[8,c(1,4,5,6)],dtbase[9,c(4,5,6)]),
                  cbind(dtbase[17,c(1,4,5,6)],dtbase[18,c(4,5,6)]),
                  cbind(dtbase[19,c(1,4,5,6)],dtbase[20,c(4,5,6)]),
                  cbind(dtbase[21,c(1,4,5,6)],dtbase[22,c(4,5,6)]),
                  cbind(dtbase[23,c(1,4,5,6)],dtbase[24,c(4,5,6)]),
                  cbind(dtbase[25,c(1,4,5,6)],dtbase[26,c(4,5,6)]),
                  cbind(dtbase[27,c(1,4,5,6)],dtbase[28,c(4,5,6)]))
                  

colnames(CER_MADF) <- c("s","N0","M0", "SD0", "N1", "M1", "SD1")
View(CER_MADF)

res_eta1 <- rma(n1i = N0, n2i = N1,m1i = M0, m2i = M1,sd1i = SD0, sd2i = SD1, 
               measure = "MD", slab = s, data = CER_MADF, method = "FE",
               weighted = TRUE, level = 95)
res_eta1
windows(10,10)
metafor::forest(res_eta1, main = "Forest plot - CER vs MADF")

res_eta1 <- rma(n1i = N0, n2i = N1,m1i = M0, m2i = M1,sd1i = SD0, sd2i = SD1, 
                measure = "MD", slab = s, data = CER_MADF, method = "ML",
                weighted = TRUE, level = 95)

res_eta1
windows(10,10)
metafor::forest(res_eta1, main = "Forest plot - CER vs MADF")


CER_IF52 <- rbind(cbind(dtbase[30,c(1,4,5,6)],dtbase[31,c(4,5,6)]),
                  cbind(dtbase[32,c(1,4,5,6)],dtbase[33,c(4,5,6)]),
                  cbind(dtbase[36,c(1,4,5,6)],dtbase[37,c(4,5,6)]),
                  cbind(dtbase[38,c(1,4,5,6)],dtbase[39,c(4,5,6)]),
                  cbind(dtbase[41,c(1,4,5,6)],dtbase[42,c(4,5,6)]),
                  cbind(dtbase[43,c(1,4,5,6)],dtbase[44,c(4,5,6)]),
                  cbind(dtbase[45,c(1,4,5,6)],dtbase[46,c(4,5,6)]),
                  cbind(dtbase[47,c(1,4,5,6)],dtbase[48,c(4,5,6)]))

colnames(CER_IF52) <- c("s","N0","M0", "SD0", "N1", "M1", "SD1")
View(CER_IF52)

res_eta2 <- rma(n1i = N0, n2i = N1,m1i = M0, m2i = M1,sd1i = SD0, sd2i = SD1, 
               measure = "MD", slab = s, data = CER_IF52, method = "FE",
               weighted = TRUE, level = 95)
res_eta2
metafor::forest(res_eta2, main = "Forest plot - CER vs IF 5:2")

res_eta2 <- rma(n1i = N0, n2i = N1,m1i = M0, m2i = M1,sd1i = SD0, sd2i = SD1, 
                measure = "MD", slab = s, data = CER_IF52, method = "REML",
                weighted = TRUE, level = 95)
res_eta2
windows(10,10)
metafor::forest(res_eta2, main = "Forest plot - CER vs IF 5:2")


control_MADF <- rbind(cbind(dtbase[4,c(1,4,5,6)],dtbase[5,c(4,5,6)]),
                      cbind(dtbase[10,c(1,4,5,6)],dtbase[9,c(4,5,6)]),
                      cbind(dtbase[11,c(1,4,5,6)],dtbase[12,c(4,5,6)]),
                      cbind(dtbase[13,c(1,4,5,6)],dtbase[14,c(4,5,6)]), 
                      cbind(dtbase[15,c(1,4,5,6)],dtbase[16,c(4,5,6)]))


colnames(control_MADF) <- c("s","N0","M0", "SD0", "N1", "M1", "SD1")
View(control_MADF)

res_eta3 <- rma(n1i = N0, n2i = N1,m1i = M0, m2i = M1,sd1i = SD0, sd2i = SD1, 
               measure = "MD", slab = s, data = control_MADF, method = "ML",
               weighted = TRUE, level = 95)
res_eta3
windows(10,10)
metafor::forest(res_eta3, main = "Forest plot - control vs MADF")


control_IF52 <- rbind(cbind(dtbase[34,c(1,4,5,6)],dtbase[35,c(4,5,6)]),
                      cbind(dtbase[40,c(1,4,5,6)],dtbase[39,c(4,5,6)]))
                   

colnames(control_IF52) <- c("s","N0","M0", "SD0", "N1", "M1", "SD1")
View(control_IF52)

res_eta4 <- rma(n1i = N0, n2i = N1,m1i = M0, m2i = M1,sd1i = SD0, sd2i = SD1, 
               measure = "MD", slab = s, data = control_IF52, method = "ML",
               weighted = TRUE, level = 95)
res_eta4
metafor::forest(res_eta4, main = "Forest plot - control vs IF 5:2")

res_eta4 <- rma(n1i = N0, n2i = N1,m1i = M0, m2i = M1,sd1i = SD0, sd2i = SD1, 
                measure = "MD", slab = s, data = control_IF52, method = "ML",
                weighted = TRUE, level = 95)
res_eta4
metafor::forest(res_eta4, main = "Forest plot - control vs IF 5:2")



control_TRF <- rbind(cbind(dtbase[50,c(1,4,5,6)],dtbase[51,c(4,5,6)]),
                     cbind(dtbase[52,c(1,4,5,6)],dtbase[53,c(4,5,6)]),
                     cbind(dtbase[54,c(1,4,5,6)],dtbase[55,c(4,5,6)]),
                     cbind(dtbase[56,c(1,4,5,6)],dtbase[57,c(4,5,6)]))
                    
                     

colnames(control_TRF) <- c("s","N0","M0", "SD0", "N1", "M1", "SD1")
View(control_TRF)

res_eta5 <- rma(n1i = N0, n2i = N1,m1i = M0, m2i = M1,sd1i = SD0, sd2i = SD1, 
               measure = "MD", slab = s, data = control_TRF, method = "FE",
               weighted = TRUE, level = 95)
res_eta5
metafor::forest(res_eta5, main = "Forest plot - control vs TRF")

res_eta5 <- rma(n1i = N0, n2i = N1,m1i = M0, m2i = M1,sd1i = SD0, sd2i = SD1, 
                measure = "MD", slab = s, data = control_TRF, method = "ML",
                weighted = TRUE, level = 95)
res_eta5
windows(10,10)
metafor::forest(res_eta5, main = "Forest plot - control vs TRF")


#Result

results <- c(res_eta1$b,res_eta1$se,res_eta1$ci.lb,res_eta1$ci.ub,res_eta1$pval,
             res_eta2$b,res_eta2$se,res_eta2$ci.lb,res_eta2$ci.ub,res_eta2$pval,
             res_eta3$b,res_eta3$se,res_eta3$ci.lb,res_eta3$ci.ub,res_eta3$pval,
             res_eta4$b,res_eta4$se,res_eta4$ci.lb,res_eta4$ci.ub,res_eta4$pval,
             res_eta5$b,res_eta5$se,res_eta5$ci.lb,res_eta5$ci.ub,res_eta5$pval)
results <- as.data.frame(matrix(round(results,3),nrow=5,byrow = T))
colnames(results) <- c("Mean difference","SE","CIl","CIu","p-value")
results$`p-value`[which(results$`p-value`==0.000)]<-"<0.001"
rownames(results) <- c("CER vs MADF", "CER vs IF 5:2", "control vs MADF",
                       "control vs IF 5:2", "control vs TRF")
results

forest(res_eta1, main = "Forest plot - CER vs MADF")
forest(res_eta2, main = "Forest plot - CER vs IF 5:2")
forest(res_eta3, main = "Forest plot - control vs MADF")
forest(res_eta4, main = "Forest plot - control vs IF 5:2")
forest(res_eta5, main = "Forest plot - control vs TRF")

######################NETWORKING#####################

library(gemtc)
library(rjags)

treatments <- read.table(textConnection('
                                        id  description
                                        A   "CER"
                                        B   "MADF & ADF"
                                        C   "IF52"
                                        D   "control"
                                        E   "TRF"'), header=T)


data <- read.table(textConnection('
                                  study treatment mean std.dev sampleSize
                                  01  A  1.91 0.16  12
                                  01  B  0.52 1.87  12
                                  02  A  7.10 2.45  12
                                  02  B  8.20 2.29  13
                                  03  A  3.90 1.44  24
                                  03  B  5.40 1.77  22
                                  04  A  4.40 2.14  22
                                  04  B  4.90 2.14  24
                                  05  A  11.8 9.42  14
                                  05  B  13.9 9.54  14
                                  06  A  11.2 0.60  68
                                  06  B  10.7 0.50  67
                                  07  A  4.88 2.80  25
                                  07  B  5.52 2.37  29
                                  08  A  9.42 6.95  17
                                  08  B  7.77 4.52  18
                                  09  A  2.20 0.70  08
                                  09  C  2.50 1.00  08
                                  10  A  3.00 2.30  22
                                  10  C  1.80 2.24  21
                                  11  A  4.47 0.66  12
                                  11  C  4.71 0.73  15
                                  12  A  4.81 2.75  49
                                  12  C  6.84 3.34  49
                                  13  A  4.50 3.48  47
                                  13  C  5.70 3.35  42
                                  14  A  5.50 4.30  12
                                  14  C  5.30 3.00  11
                                  15  A  5.10 6.95  104
                                  15  C  3.70 6.22  118
                                  16  D  0.20 1.10  28
                                  16  B  3.50 1.48  29
                                  17  D  0.40 1.41  11
                                  17  B  5.40 1.77  22
                                  18  D  0.60 3.56  15
                                  18  B  5.20 3.01  15
                                  19  D  0.60 1.30  10
                                  19  B  2.40 3.10  13
                                  20  D  0.01 0.01  16
                                  20  B  3.00 3.54  25
                                  21  D  0.40 1.40  12
                                  21  C  1.30 1.80  10
                                  22  D  3.08 2.85  52
                                  22  C  6.84 3.34  49
                                  23  D  0.09 0.98  14
                                  23  E  3.17 1.22  19
                                  24  D  8.90 4.20  30
                                  24  E  10.7 4.10  30
                                  25  D  0.68 3.93  57
                                  25  E  0.94 4.06  59
                                  26  D  0.80 1.75   9
                                  26  E  3.03 1.90  11
                                  '),header=T)

network <- mtc.network(data)

network.mod_f <- mtc.model(network, linearModel = "fixed", n.chain = 2, likelihood=NULL,link=NULL)
network.mod_r <- mtc.model(network, linearModel = "random", n.chain = 2)

result_f <- mtc.run(network.mod_f, sampler = NA, n.adapt = 5000, n.iter=20000, thin=1)
result_r <- mtc.run(network.mod_r, sampler = NA, n.adapt = 5000, n.iter=20000, thin=1)
rank_f <- rank.probability(result_f)
rank_r <- rank.probability(result_r)
C<- mtc.anohe(network)

par(mfrow=c(2,3))
plot(rank_r[1,],type="l",xlab = "Rank", ylab = "Probability", main="CER (A)")
plot(rank_r[2,],type="l",xlab = "Rank", ylab = "Probability", main="MADF & ADF (B)")
plot(rank_r[3,],type="l",xlab = "Rank", ylab = "Probability", main="IF52 (C)")
plot(rank_r[4,],type="l",xlab = "Rank", ylab = "Probability", main="control (D)")
plot(rank_r[5,],type="l",xlab = "Rank", ylab = "Probability", main="TRF (E)")
dev.off()
print(rank_r)
plot(network)

##network analysis nodesplit
nodesplit <- mtc.nodesplit(network, 
                           linearModel = "random", 
                           likelihood = "normal",
                           link = "identity",
                           n.adapt = 5000, 
                           n.iter = 1e5, 
                           thin = 10)
plot(summary(nodesplit))


##saving calculation/plot 
##D versus C is control versus IF 5:2 
##A versus B is CER versus MADF
##A versus C is CER versus IF 5:2 
##D versus B is control versus MADF 
##A versus E is CER versus control no significance all ok 
##D versus E is TRF versus control no significance all ok  

saveRDS(nodesplit)
##network netsplit
windows(10,10)
funnel(res, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0, legend=TRUE)



#################SUBGROUP ANALYSIS################
##CER versus treatments 
####Arrange dataset according to each study one row 
dat<-rbind(cbind(dtbase[2,c(1,3,4,5,6)],dtbase[3,c(3,4,5,6)]),
           cbind(dtbase[6,c(1,3,4,5,6)],dtbase[7,c(3,4,5,6)]),
           cbind(dtbase[8,c(1,3,4,5,6)],dtbase[9,c(3,4,5,6)]),
           cbind(dtbase[17,c(1,3,4,5,6)],dtbase[18,c(3,4,5,6)]),
           cbind(dtbase[19,c(1,3,4,5,6)],dtbase[20,c(3,4,5,6)]),
           cbind(dtbase[23,c(1,3,4,5,6)],dtbase[24,c(3,4,5,6)]),
           cbind(dtbase[25,c(1,3,4,5,6)],dtbase[26,c(3,4,5,6)]),
           cbind(dtbase[27,c(1,3,4,5,6)],dtbase[28,c(3,4,5,6)]),
           cbind(dtbase[29,c(1,3,4,5,6)],dtbase[30,c(3,4,5,6)]),
           cbind(dtbase[32,c(1,3,4,5,6)],dtbase[33,c(3,4,5,6)]),
           cbind(dtbase[34,c(1,3,4,5,6)],dtbase[35,c(3,4,5,6)]),
           cbind(dtbase[38,c(1,3,4,5,6)],dtbase[39,c(3,4,5,6)]),
           cbind(dtbase[40,c(1,3,4,5,6)],dtbase[41,c(3,4,5,6)]),
           cbind(dtbase[43,c(1,3,4,5,6)],dtbase[44,c(3,4,5,6)]),
           cbind(dtbase[45,c(1,3,4,5,6)],dtbase[46,c(3,4,5,6)]),
           cbind(dtbase[47,c(1,3,4,5,6)],dtbase[48,c(3,4,5,6)]), 
           cbind(dtbase[49,c(1,3,4,5,6)],dtbase[50,c(3,4,5,6)]))


dat$regimen<-c(rep("MADF & ADF",9),rep("IF 5:2",8))
dat$regimen<-as.factor(dat$regimen)
colnames(dat)<-c("s","D","N0","M0", "SD0", "D","N1", "M1", "SD1","regimen")
dat$M0<-round(dat$M0,2)
dat$M1<-round(dat$M1,2)
dat$SD0<-round(dat$SD0,2)
dat$SD1<-round(dat$SD1,2)

### calculate mean difference (and use the 'slab' argument to 
###store study labels as part of the data frame)
dat <- escalc(measure="MD", m1i=M0, m2i=M1, sd1i=SD0, sd2i=SD1, n1i=N0, n2i=N1, data=dat,
              slab=s)

### fit random-effects model
res<-metafor::rma(yi, vi,mods~D,data=dat)

### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    ", df = ", .(res$k - res$p),
                    ", p ", .(metafor:::.pval(res$pval, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}

windows(10,10)
metafor::forest(res, xlim=c(-26,10), at=log(c(0.03,0.2,1,5,50)),
       ilab=cbind(dat$N0, dat$M0, dat$SD0, dat$N1,dat$M1,dat$SD1),
       ilab.xpos=c(-13,-11.5,-10,-8,-6.5,-5), cex=0.90, ylim=c(-1, 27),
       order=dat$regimen, rows=c(3:10,15:23),
       mlab=mlabfun("RE Model for All Studies", res),
       psize=1, header="Author(s) and Year")

### set font expansion factor (as in forest() above) and use a bold font
op<- par(cex=0.75, font=2)

### add additional column headings to the plot
text(c(-13,-11.5,-10,-8,-6.5,-5),26, c("N","M","SD","N","M","SD"))
text(c(-11.5,-6.5),27, c("CER", "Treatment"))
### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)


### switch to bold italic font
par(font=4)

### add text for the subgroups
text(-26, c(11,24), pos=4, c("IF 5:2",
                               "MADF & ADF"))

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
res.s <- rma(yi, vi,mods~D,subset=(regimen=="MADF & ADF"),data=dat)
res.r <- rma(yi, vi,mods~D,subset=(regimen=="IF 5:2"),data=dat)

### add summary polygons for the three subgroups
addpoly(res.s, row=12.5, cex=0.75, mlab=mlabfun("RE Model for Subgroup", res.s))
addpoly(res.r, row= 1.5, cex=0.75, mlab=mlabfun("RE Model for Subgroup", res.r))

### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ regimen, data=dat)
#inf<- influence(res) # res is your rma object
##windows(10,10)
##par(mar=c(1,1,1,1))
#plot(inf,layout=c(8,1))


### add text for the test of subgroup differences
text(-26, -1.9, pos=4, cex=0.9, bquote(paste("Test for Subgroup Differences: ",
                                              Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p-1),
                                              ", p = ", .(formatC(res$pval, digits=2, format="f")))))


###save in tiff
name=paste0("plots/","_fig1.tiff")
dev.print(tiff,name, width = 10, height=10, units = 'in', res = 600,
          compression = "lzw") 


#################SUBGROUP ANALYSIS################
##Control versus treatments 
####Arrange dataset according to each study one row 
dat1<-rbind(cbind(dtbase[4,c(1,3,4,5,6)],dtbase[5,c(3,4,5,6)]),
            cbind(dtbase[10,c(1,3,4,5,6)],dtbase[9,c(3,4,5,6)]),
            cbind(dtbase[11,c(1,3,4,5,6)],dtbase[12,c(3,4,5,6)]),
            cbind(dtbase[13,c(1,3,4,5,6)],dtbase[14,c(3,4,5,6)]), 
            cbind(dtbase[15,c(1,3,4,5,6)],dtbase[16,c(3,4,5,6)]),
            cbind(dtbase[21,c(1,3,4,5,6)],dtbase[22,c(3,4,5,6)]),
            cbind(dtbase[36,c(1,3,4,5,6)],dtbase[37,c(3,4,5,6)]),
            cbind(dtbase[42,c(1,3,4,5,6)],dtbase[41,c(3,4,5,6)]),
            cbind(dtbase[52,c(1,3,4,5,6)],dtbase[53,c(3,4,5,6)]),
            cbind(dtbase[54,c(1,3,4,5,6)],dtbase[55,c(3,4,5,6)]),
            cbind(dtbase[56,c(1,3,4,5,6)],dtbase[57,c(3,4,5,6)]),
            cbind(dtbase[58,c(1,3,4,5,6)],dtbase[59,c(3,4,5,6)]))
  

dat1$regimen<-c(rep("MADF & ADF",6),rep("IF 5:2",2),rep("TRF",4))
dat1$regimen<-factor(dat1$regimen,levels=c("MADF & ADF","IF 5:2","TRF"))
colnames(dat1)<-c("s","D","N0","M0", "SD0", "D","N1", "M1", "SD1","regimen")
dat1$M0<-round(dat1$M0,2)
dat1$M1<-round(dat1$M1,2)
dat1$SD0<-round(dat1$SD0,2)
dat1$SD1<-round(dat1$SD1,2)

### calculate mean difference (and use the 'slab' argument to 
###store study labels as part of the data frame)
dat1<- escalc(measure="MD", m1i=M0, m2i=M1, sd1i=SD0, sd2i=SD1, n1i=N0, n2i=N1, data=dat1,
              slab=s)

### fit random-effects model
res <- rma(yi, vi,mods~D,data=dat1)

### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, res) {
  list(bquote(paste(.(text),
                    " (Q = ", .(formatC(res$QE, digits=2, format="f")),
                    ", df = ", .(res$k - res$p),
                    ", p ", .(metafor:::.pval(res$pval, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}

### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)
metafor::forest(res, xlim=c(-26,10), at=log(c(0.2,1,10,85)),
       ilab=cbind(dat1$N0, dat1$M0, dat1$SD0, dat1$N1,dat1$M1,dat1$SD1),
       ilab.xpos=c(-12,-10.5,-9,-7,-5.5,-4), cex=0.90, ylim=c(-1, 25),
       order=dat1$regimen, rows=c(16:21,11:12,3:6),
       mlab=mlabfun("RE Model for All Studies", res),
       psize=1, header="Author(s) and Year")

### set font expansion factor (as in forest() above) and use a bold font
op<- par(cex=0.75, font=2)

### add additional column headings to the plot
text(c(-12,-10.5,-9,-7,-5.5,-4),24, c("N","M","SD","N","M","SD"))
text(c(-10.5,-5.5),25, c("Control", "Treatment"))

### switch to bold italic font
par(font=4)

### add text for the subgroups
text(-26, c(22,13,7), pos=4, c("MADF & ADF",
                            "IF 5:2","TRF"))

### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
res.s <- rma(yi, vi,mods~D,subset=(regimen=="MADF & ADF"), data=dat1)
res.r <- rma(yi, vi,mods~D,subset=(regimen=="IF 5:2"),data=dat1)
res.t <- rma(yi, vi,mods~D,subset=(regimen=="TRF"),data=dat1)

### add summary polygons for the three subgroups
addpoly(res.s, row=1.5, cex=0.75, mlab=mlabfun("RE Model for Subgroup", res.s))
addpoly(res.r, row= 9.5, cex=0.75,mlab=mlabfun("RE Model for Subgroup", res.r))
addpoly(res.t, row= 14.5, cex=0.75,mlab=mlabfun("RE Model for Subgroup", res.t))

### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ regimen, data=dat1)
inf<- influence(res) # res is your rma object
#windows(10,10)
#par(mar=c(1,1,1,1))
#plot(inf,layout=c(8,1))


### add text for the test of subgroup differences
text(-26, -1.9, pos=4, cex=0.9, bquote(paste("Test for Subgroup Differences: ",
                                              Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p-1),
                                              ", p = ", .(formatC(res$QMp, digits=2, format="f")))))

###save in tiff
name=paste0("plots/","_fig2.tiff")
dev.print(tiff,name, width = 10, height=10, units = 'in', res = 600,
          compression = "lzw") 

#########Funnel plot ###########
##CER versus treatments 
### set up 2x2 array for plotting
par(mfrow=c(2,2))

### draw funnel plots
windows(10,10)
funnel(res, main="Standard Error",xlim=c(-10,10))
###save in tiff
name=paste0("plots/","_fig4.tiff")
dev.print(tiff,name, width = 10, height=10, units = 'in', res = 600,
          compression = "lzw") 
funnel(res, yaxis="vi", main="Sampling Variance")
funnel(res, yaxis="seinv", main="Inverse Standard Error")
funnel(res, yaxis="vinv", main="Inverse Sampling Variance")

### classical Egger test
##regtest(res, model="lm")

### random/mixed-effects version of the Egger test
regtest(res)
