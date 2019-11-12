# Joining the Burden

library(gridExtra)
rm(list=ls(all.names=TRUE))
setwd ("C:/Users/zmc13/Desktop/ChaModel/Results")

TOL<- read.csv("Data_out Tolima .csv")
ANT<- read.csv("Data_out Antioquia .csv")
ARA<- read.csv("Data_out Arauca .csv")
BOL<- read.csv("Data_out Bolivar .csv")
BOY<- read.csv("Data_out Boyaca .csv")
CAS<- read.csv("Data_out Casanare .csv")
CES<- read.csv("Data_out Cesar .csv")
CUN<- read.csv("Data_out Cundinamarca .csv")
GUA<- read.csv("Data_out Guainia .csv")
MAG<- read.csv("Data_out Magdalena .csv")
MET<- read.csv("Data_out Meta .csv")
NST<- read.csv("Data_out NorteSantander .csv")
SAN<- read.csv("Data_out Santander .csv")
SUC<- read.csv("Data_out Sucre .csv")
SAP<- read.csv("Data_out SanAndres .csv")
VAU<- read.csv("Data_out Vaupes .csv")
NAR<- read.csv("Data_out Narino .csv")
QUI<- read.csv("Data_out Quindio .csv")
CAU<- read.csv("Data_out Cauca .csv")
VLL<- read.csv("Data_out Valle .csv")
PUT<- read.csv("Data_out Putumayo .csv")
COR<- read.csv("Data_out Cordoba .csv")
RIS<- read.csv("Data_out Risaralda .csv")
VIC<- read.csv("Data_out Vichada .csv")
ATL<- read.csv("Data_out Atlantico .csv")
CAL<- read.csv("Data_out Caldas .csv")
AMA<- read.csv("Data_out Amazonas .csv")
GUJ<- read.csv("Data_out Guajira .csv")
CHO<- read.csv("Data_out Choco .csv")
CAQ<- read.csv("Data_out Caqueta .csv")
GVR<- read.csv("Data_out Guaviare .csv")
HUI<- read.csv("Data_out Huila .csv")
BOG<- read.csv("Data_out Bogota .csv")




COLb<- rbind(TOL, ANT, ARA, BOL, BOY, CAS, CES, CUN, GUA, MAG, MET, NST, SAN, SUC, 
             SAP, VAU, NAR, QUI, CAU, VLL, PUT, COR, RIS, VIC, ATL, CAL, AMA, GUJ,
             CHO,CAQ, GVR, HUI, BOG)

agec<- rep(seq(5, 85, 5), (dim(COLb)[1])/17)
COLOMBIA<- cbind(agec, COLb)
rm(COLb, agec)

table(COLOMBIA$place)
table(COLOMBIA$metric)

## COLOMBIA$region<-NA
COLOMBIA$region[COLOMBIA$place== "Antioquia"] = "departamento de antioquia"
COLOMBIA$region[COLOMBIA$place== "Arauca"] = "departamento de arauca"
COLOMBIA$region[COLOMBIA$place== "Bolivar"] = 'departamento de bolivar'
COLOMBIA$region[COLOMBIA$place== "Boyaca"] = 'departamento de boyaca'
COLOMBIA$region[COLOMBIA$place== "Caldas"] = 'departamento de caldas'
COLOMBIA$region[COLOMBIA$place== "Casanare"] =  'departamento de casanare'
COLOMBIA$region[COLOMBIA$place== "Cordoba"] = 'departamento de cordoba'
COLOMBIA$region[COLOMBIA$place== "Cundinamarca"] = 'departamento de cundinamarca'
COLOMBIA$region[COLOMBIA$place== "Guajira"] = 'departamento de la guajira'
COLOMBIA$region[COLOMBIA$place== "Narino"] = 'departamento de narino'
COLOMBIA$region[COLOMBIA$place== "NorteSantander"] = 'departamento de norte de santander'
COLOMBIA$region[COLOMBIA$place== "Risaralda"] = 'departamento de risaralda'
COLOMBIA$region[COLOMBIA$place== "Santander"] = 'departamento de santander'
COLOMBIA$region[COLOMBIA$place== "Sucre"] = 'departamento de sucre'
COLOMBIA$region[COLOMBIA$place== "Tolima"] = 'departamento de tolima'
COLOMBIA$region[COLOMBIA$place== "Amazonas"] = 'departamento del amazonas'
COLOMBIA$region[COLOMBIA$place== "Atlantico"] = 'departamento del atlantico'
COLOMBIA$region[COLOMBIA$place== "Caqueta"] = 'departamento del caqueta'
COLOMBIA$region[COLOMBIA$place== "Cauca"] = 'departamento del cauca'
COLOMBIA$region[COLOMBIA$place== "Cesar"] = 'departamento del cesar'
COLOMBIA$region[COLOMBIA$place== "Choco"] = 'departamento del choco'
COLOMBIA$region[COLOMBIA$place== "Guainia"] = 'departamento del guainia'
COLOMBIA$region[COLOMBIA$place== "Guaviare"] = 'departamento del guaviare'
COLOMBIA$region[COLOMBIA$place== "Huila"] = 'departamento del huila'
COLOMBIA$region[COLOMBIA$place== "Magdalena"] = 'departamento del magdalena'
COLOMBIA$region[COLOMBIA$place== "Meta"] = 'departamento del meta'
COLOMBIA$region[COLOMBIA$place== "Putumayo"] = 'departamento del putumayo'
COLOMBIA$region[COLOMBIA$place== "Valle"] = 'departamento del valle del cauca'
COLOMBIA$region[COLOMBIA$place== "Vaupes"] = 'departamento del vaupes'
COLOMBIA$region[COLOMBIA$place== "Vichada"] = 'departamento del vichada'
COLOMBIA$region[COLOMBIA$place== "Bogota"] = 'distrito capital de bogota'
COLOMBIA$region[COLOMBIA$place== "SanAndres"] = 'providencia y santa catalina, departamento de archipielago de san andres'
COLOMBIA$region[COLOMBIA$place== "Quindio"] = 'quindio department'
COLOMBIA$region[COLOMBIA$place== "Bogota"] = 'distrito capital de bogota'


#
names(COLOMBIA)

## Reading Map fucntions

setwd ("C:/Users/zmc13/Desktop/ChaModel")
source('Rscript/MapsFunctions.r')       # 1 Source



# 
METRIC<- "Cases.Total"
DT<- COLOMBIA[which(COLOMBIA$metric == METRIC),]; dt=data.frame(aggregate(X2020M ~ region, data = DT, sum)); 
names(dt)<- c('region', 'value'); 
admin1_choropleth(country.name = "colombia", df = dt, title = METRIC, 
     legend = "Number", num_colors = 4, reference_map = TRUE)
tab<- cbind( data.frame(aggregate(X2020M ~ place, data = DT, sum)),
                   data.frame(aggregate(X2020L ~ place, data = DT, sum)[,2]),
                   data.frame(aggregate(X2020U ~ place, data = DT, sum))[,2]); names(tab)<- c("Depto", "Median", "Lower", "Upper"); 
CASES<- DT; tab_tcases<- tab; rm(DT, dt, tab)

# 
METRIC<- "Cases.Acute"
DT<- COLOMBIA[which(COLOMBIA$metric == METRIC),]; dt=data.frame(aggregate(X2020M ~ region, data = DT, sum)); 
names(dt)<- c('region', 'value'); 
admin1_choropleth(country.name = "colombia", df = dt, title = METRIC, 
                  legend = "Number", num_colors = 5, reference_map = TRUE)
tab<- cbind( data.frame(aggregate(X2020M ~ place, data = DT, sum)),
                   data.frame(aggregate(X2020L ~ place, data = DT, sum)[,2]),
                   data.frame(aggregate(X2020U ~ place, data = DT, sum))[,2]); names(tab)<- c("Depto", "Median", "Lower", "Upper"); 
ACUTE<- DT; tab_acute<- tab; rm(DT, dt, tab)


METRIC<- "Cases.Asymptomatic"
DT<- COLOMBIA[which(COLOMBIA$metric == METRIC),]; dt=data.frame(aggregate(X2010M ~ region, data = DT, sum)); 
names(dt)<- c('region', 'value'); 
admin1_choropleth(country.name = "colombia", df = dt, title = METRIC, 
                  legend = "Number", num_colors = 5, reference_map = TRUE)
tab<- cbind( data.frame(aggregate(X2020M ~ place, data = DT, sum)),
             data.frame(aggregate(X2020L ~ place, data = DT, sum)[,2]),
             data.frame(aggregate(X2020U ~ place, data = DT, sum))[,2]); names(tab)<- c("Depto", "Median", "Lower", "Upper"); 
ASYMPT<- DT; tab_asymp<- tab; rm(DT, dt, tab)

METRIC<- "Cases.ChronicMild"
DT<- COLOMBIA[which(COLOMBIA$metric == METRIC),]; dt=data.frame(aggregate(X2010M ~ region, data = DT, sum)); 
names(dt)<- c('region', 'value'); 
admin1_choropleth(country.name = "colombia", df = dt, title = METRIC, 
                  legend = "Number", num_colors = 5, reference_map = TRUE)
tab<- cbind( data.frame(aggregate(X2020M ~ place, data = DT, sum)),
                   data.frame(aggregate(X2020L ~ place, data = DT, sum)[,2]),
                   data.frame(aggregate(X2020U ~ place, data = DT, sum))[,2]); names(tab)<- c("Depto", "Median", "Lower", "Upper"); 
CHRO.MILD<- DT; tab_mild<- tab; rm(DT, dt, tab)


METRIC<- "Cases.ChronicSevere"
DT<- COLOMBIA[which(COLOMBIA$metric == METRIC),]; dt=data.frame(aggregate(X2010M ~ region, data = DT, sum)); 
names(dt)<- c('region', 'value'); 
admin1_choropleth(country.name = "colombia", df = dt, title = METRIC, 
                  legend = "Number", num_colors = 5, reference_map = TRUE)
tab<- cbind( data.frame(aggregate(X2020M ~ place, data = DT, sum)),
                   data.frame(aggregate(X2020L ~ place, data = DT, sum)[,2]),
                   data.frame(aggregate(X2020U ~ place, data = DT, sum))[,2]); names(tab)<- c("Depto", "Median", "Lower", "Upper"); 
CHRO.SEVERE<- DT; tab_severe<- tab; rm(DT, dt, tab)

METRIC<- "Deaths.ChrSymptoms"
DT<- COLOMBIA[which(COLOMBIA$metric == METRIC),]; dt=data.frame(aggregate(X2010M ~ region, data = DT, sum)); 
names(dt)<- c('region', 'value'); 
admin1_choropleth(country.name = "colombia", df = dt, title = "Deaths", 
                  legend = "Number", num_colors = 1, reference_map = TRUE)
tab<- cbind( data.frame(aggregate(X2020M ~ place, data = DT, sum)),
                   data.frame(aggregate(X2020L ~ place, data = DT, sum)[,2]),
                   data.frame(aggregate(X2020U ~ place, data = DT, sum))[,2]); names(tab)<- c("Depto", "Median", "Lower", "Upper"); 
DEATHS<- DT; tab_deaths<- tab; rm(DT, dt, tab)

# DALYs
METRIC<- "DALY"
DT<- COLOMBIA[which(COLOMBIA$metric == METRIC),]; dt=data.frame(aggregate(X2010M ~ region, data = DT, sum)); 
names(dt)<- c('region', 'value'); 
admin1_choropleth(country.name = "colombia", df = dt, title = METRIC, 
                  legend = "Number", num_colors = 1, reference_map = TRUE)
tab<- cbind( data.frame(aggregate(X2020M ~ place, data = DT, sum)),
                   data.frame(aggregate(X2020L ~ place, data = DT, sum)[,2]),
                   data.frame(aggregate(X2020U ~ place, data = DT, sum))[,2]); names(tab)<- c("Depto", "Median", "Lower", "Upper")
DALY<- DT; tab_dalys<- tab; rm(DT, dt, tab)




Tab1<- cbind(data.frame(c("Total cases", "Acute", "Asymptomatic", "Mild/Moderate", "Severe", "Deaths", "DALYs")),
 rbind(colSums(tab_tcases[,2:4]),
     colSums(tab_acute[,2:4]),
     colSums(tab_asymp[,2:4]),
     colSums(tab_mild[,2:4]),
     colSums(tab_severe[,2:4]),
     colSums(tab_deaths[,2:4]),
     colSums(tab_dalys[,2:4])))
names(Tab1)<- c("Metric", "Median", "Lower", "Upper")
Tab1

par(mar=c(1,12,4,2))
plot(1, type="n", axes=F, xlab="", ylab="")
#pdf("Tab1 .pdf", height=11, width=8.5)
grid.table(Tab1)

library(Hmisc)
(par(mfrow=c(1,1)))
par(las=2) # make label text perpendicular to axis
par(mar=c(1,12,4,2)) 
bp<- (barplot((Tab1[,2]), ylim=c(0,max(Tab1[,4])), 
             main = c("Burden of Chagas Disease 2020"), col=c(2:8), cex.names=.5))
errbar(x=bp[,1], Tab1[,2], Tab1[,4],Tab1[,3],, add=T, xlab="")
legend("topright", as.character(Tab1[,1]), fill = c(2:8), bty = "n", cex = 1)

names(tab_tcases)<- c("Depto", "M", "L", "U")
tab_tcases
#pdf("TotalCases .pdf", height=11, width=8.5)
(par(mfrow=c(1,1)))
plot(1, type="n", axes=F, xlab="", ylab="")
grid.table(tab_tcases)

(par(mfrow=c(1,1)))
par(las=2) # make label text perpendicular to axis
par(mar=c(10, 7,4,2)) 
TAB <- data.frame(tab_tcases[order(-tab_tcases$M),])
bp<- (barplot(TAB[,2],main="",  ylim=c(0, max(TAB[,4])), names.arg=TAB[,1],
             col="lightblue"))
errbar(x=bp[,1], TAB[,2], TAB[,4],TAB[,3], add=T, xlab="", col="darkblue")
title(main="B", adj=0, cex.main=1.5, family="Calibri Light")







# Calculating Adult prevalence in Blood Donors

CASES_Donors <- subset(CASES, CASES$agec >= 20 | CASES$agec < 70)
CasesDonorsM<- data.frame(aggregate(X2020M ~ place, data = CASES_Donors, sum))
CasesDonorsL<- data.frame(aggregate(X2020L ~ place, data = CASES_Donors, sum))
CasesDonorsU<- data.frame(aggregate(X2020U ~ place, data = CASES_Donors, sum))
CasesT<- cbind(CasesDonorsM, CasesDonorsL, CasesDonorsU)
CasesT2<- CasesT[, c(1,2,4,6)]
DF<- CasesT2[order(as.character(CasesT2$place)),] 

setwd ("C:/Users/zmc13/Desktop/ChaModel/Robject")
react<- read.csv("Reactividad_1995-2010.csv")
reactA<- data.frame(aggregate(prev ~Depto, data = react, mean))
vp<- data.frame("Vaupes", 0); names(vp)<- c("Depto", "prev")
reactAv<- rbind(reactA, vp)
reactYY1<- reactAv[c(-14, -35),]
reactYY<- reactYY1[order(as.character(reactYY1$Depto)),]

Pop<- read.csv("Obs.Alive.Totalx5.csv")
PopR<- read.csv("Obs.Alive.Ruralx5.csv")
Pop_Donors<- subset(Pop, Pop$agec >= 3 | Pop$agec < 14)
dta<- data.frame(aggregate(X2020 ~ dept, data = Pop_Donors, sum))[-14,]
dtb<- cbind(dta, reactYY)

BloodD.Prev<- cbind(DF,dtb[,2], (DF[,2]/dtb[,2]), (DF[,3]/dtb[,2]), (DF[,4]/dtb[,2]), (dtb[,4]))
colnames(BloodD.Prev)<- c("Depto", "M", "L", "U", "Pop", "X.M", "X.L", "X.U" ,"reactivity")
TAB <- data.frame(BloodD.Prev[order(-BloodD.Prev$X.M),])

par(mfrow=c(1,1))
par(adj=.5); par(mgp = c(6, 1, 0))
par(las=2) #las=2 makes label text perpendicular to axis
par(mar=c(10,8,2,2)) 

bp=barplot(TAB[,"X.M"],  
             ylim=c(0,.10), names.arg=TAB[,1],
             col="plum2", cex.axis=2, cex.lab=2, cex.names=1.5, ylab="Prevalence of infection")
errbar(x=bp[,1], TAB[,"X.M"], TAB[,"X.U"],TAB[,"X.L"], add=T, xlab="", col="pink",pch=10, errbar.col="purple")
errbar(x=bp[,1], TAB[,"reactivity"], TAB[,"X.U"],TAB[,"X.L"], add=T, xlab="", col="royalblue4", pch=15)
legend("topright",c("Estimated (Model)", "Reported (Blood Banks)"), fill=c("plum2","royalblue4"),bty="n", cex=2)


#####
setwd ("C:/Users/zmc13/Desktop/ChaModel/Results") # Summarised Data 

TOLb<- read.csv("Data36y Tolima .csv")
ANTb<- read.csv("Data36y Antioquia .csv")
ARAb<- read.csv("Data36y Arauca .csv")
BOLb<- read.csv("Data36y Bolivar .csv")
BOYb<- read.csv("Data36y Boyaca .csv")
CASb<- read.csv("Data36y Casanare .csv")
CESb<- read.csv("Data36y Cesar .csv")
CUNb<- read.csv("Data36y Cundinamarca .csv")
GUAb<- read.csv("Data36y Guainia .csv")
MAGb<- read.csv("Data36y Magdalena .csv")
METb<- read.csv("Data36y Meta .csv")
NSTb<- read.csv("Data36y NorteSantander .csv")
SANb<- read.csv("Data36y Santander .csv")
SUCb<- read.csv("Data36y Sucre .csv")
SAPb<- read.csv("Data36y SanAndres .csv")
VAUb<- read.csv("Data36y Vaupes .csv")
NARb<- read.csv("Data36y Narino .csv")
QUIb<- read.csv("Data36y Quindio .csv")
CAUb<- read.csv("Data36y Cauca .csv")
VLLb<- read.csv("Data36y Valle .csv")
PUTb<- read.csv("Data36y Putumayo .csv")
CORb<- read.csv("Data36y Cordoba .csv")
RISb<- read.csv("Data36y Risaralda .csv")
VICb<- read.csv("Data36y Vichada .csv")
ATLb<- read.csv("Data36y Atlantico .csv")
CALb<- read.csv("Data36y Caldas .csv")
AMAb<- read.csv("Data36y Amazonas .csv")
GUJb<- read.csv("Data36y Guajira .csv")
CHOb<- read.csv("Data36y Choco .csv")
CAQb<- read.csv("Data36y Caqueta .csv")
GVRb<- read.csv("Data36y Guaviare .csv")
HUIb<- read.csv("Data36y Huila .csv")
BOGb<- read.csv("Data36y Bogota .csv")


COLb<- rbind(TOLb, ANTb, ARAb, BOLb, BOYb, CASb, CESb, CUNb, GUAb, MAGb, METb, NSTb, SANb, SUCb, 
             SAPb, VAUb, NARb, QUIb, CAUb, VLLb, PUTb, CORb, RISb, VICb, ATLb, CALb, AMAb, GUJb,
             CHOb, CAQb, GVRb, HUIb, BOGb)

colDALY.M<- colSums((COLb[which(COLb$Data== "DALY.M"),])[,4:39])
colDALY.L<- colSums((COLb[which(COLb$Data== "DALY.L"),])[,4:39])
colDALY.U<- colSums((COLb[which(COLb$Data== "DALY.U"),])[,4:39])


Cases.M<- colSums((COLb[which(COLb$Data== "CasesM"),])[,4:39])
Cases.L<- colSums((COLb[which(COLb$Data== "CasesL"),])[,4:39])
Cases.U<- colSums((COLb[which(COLb$Data== "CasesU"),])[,4:39])
years<-1985:2020  


Cases.M<- colSums((COLb[which(COLb$Data== "CasesM"),])[,4:39])
Cases.L<- colSums((COLb[which(COLb$Data== "CasesL"),])[,4:39])
Cases.U<- colSums((COLb[which(COLb$Data== "CasesU"),])[,4:39])
years<-1985:2020  


#####
setwd ("C:/Users/zmc13/Desktop/ChaModel/Results") # Whole thing

TOLc<- read.csv("Data_out36 Tolima .csv")
ANTc<- read.csv("Data_out36 Antioquia .csv")
ARAc<- read.csv("Data_out36 Arauca .csv")
BOLc<- read.csv("Data_out36 Bolivar .csv")
BOYc<- read.csv("Data_out36 Boyaca .csv")
CASc<- read.csv("Data_out36 Casanare .csv")
CESc<- read.csv("Data_out36 Cesar .csv")
CUNc<- read.csv("Data_out36 Cundinamarca .csv")
GUAc<- read.csv("Data_out36 Guainia .csv")
MAGc<- read.csv("Data_out36 Magdalena .csv")
METc<- read.csv("Data_out36 Meta .csv")
NSTc<- read.csv("Data_out36 NorteSantander .csv")
SANc<- read.csv("Data_out36 Santander .csv")
SUCc<- read.csv("Data_out36 Sucre .csv")
SAPc<- read.csv("Data_out36 SanAndres .csv")
VAUc<- read.csv("Data_out36 Vaupes .csv")
NARc<- read.csv("Data_out36 Narino .csv")
QUIc<- read.csv("Data_out36 Quindio .csv")
CAUc<- read.csv("Data_out36 Cauca .csv")
VLLc<- read.csv("Data_out36 Valle .csv")
PUTc<- read.csv("Data_out36 Putumayo .csv")
CORc<- read.csv("Data_out36 Cordoba .csv")
RISc<- read.csv("Data_out36 Risaralda .csv")
VICc<- read.csv("Data_out36 Vichada .csv")
ATLc<- read.csv("Data_out36 Atlantico .csv")
CALc<- read.csv("Data_out36 Caldas .csv")
AMAc<- read.csv("Data_out36 Amazonas .csv")
GUJc<- read.csv("Data_out36 Guajira .csv")
CHOc<- read.csv("Data_out36 Choco .csv")
CAQc<- read.csv("Data_out36 Caqueta .csv")
GVRc<- read.csv("Data_out36 Guaviare .csv")
HUIc<- read.csv("Data_out36 Huila .csv")
BOGc<- read.csv("Data_out36 Bogota .csv")


COLc<- rbind(TOLc, ANTc, ARAc, BOLc, BOYc, CASc, CESc, CUNc, GUAc, MAGc, METc, NSTc, SANc, SUCc, 
             SAPc, VAUc, NARc, QUIc, CAUc, VLLc, PUTc, CORc, RISc, VICc, ATLc, CALc, AMAc, GUJc,
             CHOc, CAQc, GVRc, HUIc, BOGc)

Deaths.M<- colSums((COLc[which(COLc$bound== "Median" & COLc$metric=="Deaths.Total"),])[,5:40])
Deaths.L<- colSums((COLc[which(COLc$bound== "Low" & COLc$metric=="Deaths.Total"),])[,5:40])
Deaths.U<- colSums((COLc[which(COLc$bound== "Upper" & COLc$metric=="Deaths.Total"),])[,5:40])


# PLOT 
# Figure 5.3: Temporal Changes in the burden of Chagas disease in Colombia from 1985 through 2020

par(mfrow=c(1,3))
par(adj=.5); par(mgp = c(6.5, 1, 0))
par(las=1) #las=2 makes label text perpendicular to axis
par(mar=c(4,9.5,4,2)) 

## Cases
plot(years,Cases.U, lty=0, type="l", ylim=c(0,max(Cases.U)), ylab= "",
     col=4,lwd=1, yaxt="n", xaxt="n", cex.lab=2, font.main=10)
axis(2, at =c(0, 350000, 700000), cex.axis=2)
axis(1, at =c(1985, 2000, 2020), cex.axis=2)
polygon(c(years,rev(years)),c(Cases.L, rev(Cases.U)),
        border = F, col="lightskyblue1", main=NA)
lines(years,Cases.M, col=4, lwd=3)
title(ylab="Total Cases", mgp=c(8,1,0), cex.lab=2, family="Calibri Light")
title(main="A", adj=0, cex.main=2, family="Calibri Light")


# ## Deaths
plot(years,Deaths.U, lty=0, type="l", ylim=c(0,max(Deaths.U)), ylab= "",
     col=4,lwd=1, yaxt="n", xaxt="n",  cex.lab=2, font.main=10)
axis(2, at =c(0, 2500, 5000), cex.axis=2)
axis(1, at =c(1985, 2000, 2020), cex.axis=2)
polygon(c(years,rev(years)),c(Deaths.L, rev(Deaths.U)),
        border = F, col="gray84", main=NA)
lines(years, Deaths.M, col="black", lwd=3)
title(ylab="Deaths", mgp=c(6.5,1,0), cex.lab=2, family="Calibri Light")
title(main="B", adj=0, cex.main=2, family="Calibri Light")


## DALYS
plot(years,colDALY.U, lty=0, type="l", ylim=c(0,max(colDALY.U)), ylab= "",
     col=4,lwd=1, yaxt="n", xaxt="n", cex.axis=2, cex.lab=2, font.main=10)
polygon(c(years,rev(years)),c(colDALY.L, rev(colDALY.U)),
        border = F, col="lightpink", main=NA)
axis(2, at =c(0, 70000, 140000), cex.axis=2)
axis(1, at =c(1985, 2000, 2020), cex.axis=2)
lines(years, colDALY.M, col=2, lwd=3)
title(ylab="DALYs", mgp=c(6.5,1,0), cex.lab=2, family="Calibri Light")
title(main="C", adj=0, cex.main=2, family="Calibri Light")


# PLOT 
###### Comparison with other sources
# Comparison of my estimates against other authors from 1985 - 2015

others<- read.csv("Others.csv")
GBD<- others[which(others$author=="GBD"),]
PAHO1<- others[which(others$author=="PAHO1"),]
PAHO2<- others[which(others$author=="PAHO2"),]
Moncayo<- others[which(others$author=="Moncayo"),]

library(binom); library(plotrix) ;library(sfsmisc); library(Hmisc)
par(mfrow=c(1,1))
par(adj=.5); par(mgp = c(8, 1, 0))
par(las=1) #las=2 makes label text perpendicular to axis
par(mar=c(5.1, 10, 2, 17), xpd=TRUE)

#tiff(paste("Others.png"), width = 10, height = 10, units = 'in', res = 300)
plot(years,Cases.U, lty=0, type="l", ylim=c(0,1000000), 
     ylab= "Cases", xlab= "year",
     col=4,lwd=1, yaxt="n", xaxt="n", cex.lab=2, font.main=10)
axis(2, at =c(0, 150000, 300000, 450000, 600000, 750000, 900000), cex.axis=2)
axis(1, at =c(1985, 1995, 2005, 2015), cex.axis=2)
polygon(c(years,rev(years)),c(Cases.L, rev(Cases.U)),
        border = F, col="lightskyblue2", main=NA)
lines(years,Cases.M, col=4, lwd=3)
points(Moncayo$year, Moncayo$total, pch=18, cex=3, col="darkolivegreen4")
points(PAHO1$year, PAHO1$total, pch=17, col="brown", cex=3)
points(PAHO2$year, PAHO2$total, pch=16, col="purple", cex=3)
par(new=T)
plotCI(GBD$year, GBD$total, ui=GBD$upper, li=GBD$low,
       xlim=c(1984,2020),ylim=c(0,1e6), cex=2,
       ylab=NA,xlab=NA, pch=19,main=NA,xaxt="n", yaxt="n", scol="red", col="salmon1",sfrac=0.0001)
legend("topright", inset=c(-.5,0), legend = c("Moncayo (2009)", "PAHO (2006)", "WHO (2015)", "GBD (2015)", "Cucunubá (2017)"), 
                                        col = c("darkolivegreen4", "brown", "purple", "salmon1", "lightskyblue2" ),
                                        pch = c(18, 17, 16, 19, 22), lwd=c(0,0,0,1),
                                        bty = "n", cex = 2, pt.cex=3,horiz = FALSE)

#dev.off()






### Plot on Age distribution of outcomes
COLc$region[COLc$place== "Antioquia"] = "departamento de antioquia"
COLc$region[COLc$place== "Arauca"] = "departamento de arauca"
COLc$region[COLc$place== "Bolivar"] = 'departamento de bolivar'
COLc$region[COLc$place== "Boyaca"] = 'departamento de boyaca'
COLc$region[COLc$place== "Caldas"] = 'departamento de caldas'
COLc$region[COLc$place== "Casanare"] =  'departamento de casanare'
COLc$region[COLc$place== "Cordoba"] = 'departamento de cordoba'
COLc$region[COLc$place== "Cundinamarca"] = 'departamento de cundinamarca'
COLc$region[COLc$place== "Guajira"] = 'departamento de la guajira'
COLc$region[COLc$place== "Narino"] = 'departamento de narino'
COLc$region[COLc$place== "NorteSantander"] = 'departamento de norte de santander'
COLc$region[COLc$place== "Risaralda"] = 'departamento de risaralda'
COLc$region[COLc$place== "Santander"] = 'departamento de santander'
COLc$region[COLc$place== "Sucre"] = 'departamento de sucre'
COLc$region[COLc$place== "Tolima"] = 'departamento de tolima'
COLc$region[COLc$place== "Amazonas"] = 'departamento del amazonas'
COLc$region[COLc$place== "Atlantico"] = 'departamento del atlantico'
COLc$region[COLc$place== "Caqueta"] = 'departamento del caqueta'
COLc$region[COLc$place== "Cauca"] = 'departamento del cauca'
COLc$region[COLc$place== "Cesar"] = 'departamento del cesar'
COLc$region[COLc$place== "Choco"] = 'departamento del choco'
COLc$region[COLc$place== "Guainia"] = 'departamento del guainia'
COLc$region[COLc$place== "Guaviare"] = 'departamento del guaviare'
COLc$region[COLc$place== "Huila"] = 'departamento del huila'
COLc$region[COLc$place== "Magdalena"] = 'departamento del magdalena'
COLc$region[COLc$place== "Meta"] = 'departamento del meta'
COLc$region[COLc$place== "Putumayo"] = 'departamento del putumayo'
COLc$region[COLc$place== "Valle"] = 'departamento del valle del cauca'
COLc$region[COLc$place== "Vaupes"] = 'departamento del vaupes'
COLc$region[COLc$place== "Vichada"] = 'departamento del vichada'
COLc$region[COLc$place== "Bogota"] = 'distrito capital de bogota'
COLc$region[COLc$place== "SanAndres"] = 'providencia y santa catalina, departamento de archipielago de san andres'
COLc$region[COLc$place== "Quindio"] = 'quindio department'
COLc$region[COLc$place== "Bogota"] = 'distrito capital de bogota'

names(COLc)

agec<- rep(seq(5, 85, 5), (dim(COLc)[1])/17)
COLe<- cbind(agec, COLc)
rm(COLc, agec)

ASYMPTOMATIC<- COLe[which(COLe$metric == "Cases.Asymptomatic" &   COLe$bound=="Median"),]; 
ACUTE_1<- COLe[which(COLe$metric == "Cases.Acute" &   COLe$bound=="Median"),]; 
CHRONIC.M<- COLe[which(COLe$metric == "Cases.ChronicMild" &   COLe$bound=="Median"),]; 
CHRONIC.S<- COLe[which(COLe$metric == "Cases.ChronicSevere" &   COLe$bound=="Median"),]; 
DEATHS.T<- COLe[which(COLe$metric == "Deaths.Total" &   COLe$bound=="Median"),];
DALYs.T<- COLe[which(COLe$metric == "DALY" &   COLe$bound=="Median"),];

m<- rbind(ACUTE_1, ASYMPTOMATIC, CHRONIC.M, CHRONIC.S, DEATHS.T, DALYs.T)
names(m)[6:41]<-1985:2020
mvars<- names(m)%in% c("X", "bound", "region")
Dat<- m[!mvars]
library(reshape)
m1<- melt(Dat, id= c("place", "metric", "agec"))
m1<-m1[which(m1$agec<90),]


ggplot(data=m1, aes(x=agec, y=value, fill=metric))+
  facet_wrap(~variable)+
  geom_bar(stat="identity")+
  xlab("Age distribution (years)")+
  ylab("Absolute number of cases")+
  guides(fill=guide_legend(title=NULL))+
   scale_fill_discrete(labels=c("Acute Infections", "C.Asymptomatic", "C.Mild/Moderate", "C.Severe"))




# Comparison between acute and chronic cases
m2<- m1[which(m1$metric=="Cases.Acute"),]
m3<-m2
mean.age<- numeric(36)
for(i in 1985:2020){
     mean.age[(i-2020 +36)]<- round(median(rep((m3[which(m3$variable==i),])$agec,
              (m3[which(m3$variable==i),])$value)),0)
}

m3$age.mean<-numeric(dim(m3)[1])
for (t in 1985:2020){
  m3$age.mean[which(m3$variable==t)]<- mean.age[t-1984]
}

m4 <- m3
m4$years<- numeric(dim(m4)[1])
m4$years[which(m4$variable==1985)]<- 1
m4$years[which(m4$variable==2000)]<- 1
m4$years[which(m4$variable==2015)]<- 1

m5<-m4[which(m4$years==1 & m4$agec<85),]

p1<- ggplot(data=m5, aes(x=agec, y=value), colour='lightblue')+#, fill=variable))+
  facet_wrap(~variable)+
  geom_bar(stat="identity", fill="lightblue")+
  xlab("Age distribution (years)")+
  ylab('Acute Cases')+
  geom_vline(aes(xintercept = (age.mean)), 
             linetype = 2, colour = "blue", size=1, show.legend =  T)

#m2<- m1[which(m1$metric=="Cases.ChronicSevere" | m1$metric=="Cases.ChronicMild"),]
m2<- m1[which(m1$metric=="Cases.ChronicMild"),]
m3<-m2
mean.age<- numeric(36)
for(i in 1985:2020){
  mean.age[(i-2020 +36)]<- round(mean(rep((m3[which(m3$variable==i),])$agec,
                                            (m3[which(m3$variable==i),])$value)),0)
}

m3$age.mean<-numeric(dim(m3)[1])
for (t in 1985:2020){
  m3$age.mean[which(m3$variable==t)]<- mean.age[t-1984]
}

m4 <- m3
m4$years<- numeric(dim(m4)[1])
m4$years[which(m4$variable==1985)]<- 1
m4$years[which(m4$variable==2000)]<- 1
m4$years[which(m4$variable==2015)]<- 1

m5<-m4[which(m4$years==1 & m4$agec<85),]

p2<- ggplot(data=m5, aes(x=agec, y=value), colour='lightblue')+#, fill=variable))+
  facet_wrap(~variable)+
  geom_bar(stat="identity", fill="lightpink")+
  xlab("Age distribution (years)")+
  ylab('Chronic Heart Disease')+
  geom_vline(aes(xintercept = (age.mean)), 
             linetype = 2, colour = "red", size=1, show.legend =  T)                   

m2<- m1[which(m1$metric=="Cases.ChronicSevere"),]
m3<-m2
mean.age<- numeric(36)
for(i in 1985:2020){
  mean.age[(i-2020 +36)]<- round(mean(rep((m3[which(m3$variable==i),])$agec,
                                          (m3[which(m3$variable==i),])$value)),0)}
m3$age.mean<-numeric(dim(m3)[1])
for (t in 1985:2020){m3$age.mean[which(m3$variable==t)]<- mean.age[t-1984]}
m4 <- m3
m4$years<- numeric(dim(m4)[1])
m4$years[which(m4$variable==1985)]<- 1
m4$years[which(m4$variable==2000)]<- 1
m4$years[which(m4$variable==2015)]<- 1
m5<-m4[which(m4$years==1 & m4$agec<85),]
p3<- ggplot(data=m5, aes(x=agec, y=value), colour='lightblue')+#, fill=variable))+
  facet_wrap(~variable)+
  geom_bar(stat="identity", fill="lightpink")+
  xlab("Age distribution (years)")+
  ylab('C.Severe')+
  geom_vline(aes(xintercept = (age.mean)), 
             linetype = 2, colour = "red", size=1, show.legend =  T) 

m2<- m1[which(m1$metric=='Deaths.Total'),]
m3<-m2
mean.age<- numeric(36)
for(i in 1985:2020){
  mean.age[(i-2020 +36)]<- round(mean(rep((m3[which(m3$variable==i),])$agec,
                                            (m3[which(m3$variable==i),])$value)),0)}
m3$age.mean<-numeric(dim(m3)[1])
for (t in 1985:2020){m3$age.mean[which(m3$variable==t)]<- mean.age[t-1984]}
m4 <- m3
m4$years<- numeric(dim(m4)[1])
m4$years[which(m4$variable==1985)]<- 1
m4$years[which(m4$variable==2000)]<- 1
m4$years[which(m4$variable==2015)]<- 1
m5<-m4[which(m4$years==1 & m4$agec<85),]
p4<- ggplot(data=m5, aes(x=agec, y=value), colour='lightblue')+#, fill=variable))+
  facet_wrap(~variable)+
  geom_bar(stat="identity", fill="gray")+
  xlab("Age distribution (years)")+
  ylab('Deaths')+
  geom_vline(aes(xintercept = (age.mean)), 
             linetype = 2, colour = "black", size=1, show.legend =  T) 



grid.arrange(p1,p2,p4)#'', p4)#,p4,p5,p6,nrow=6,ncol=1,heights=c(0.15,0.15,0.15,0.15,0.15,0.15),left="Power (KW)")  


ggplot(data=m3, aes(x=agec, y=value))+#, fill=variable))+
  facet_wrap(~variable)+
  geom_bar(stat="identity")+
  xlab("Age distribution (years)")+
  ylab("Absolute number of cases")+
  geom_vline(aes(xintercept = (age.mean)))

#####################################################
#====================================================
#             SENSITIVITY ANALYSIS           
#====================================================
#####################################################
#####
setwd ("C:/Users/zmc13/Desktop/ChaModel/Results") # Correlation
TOLd<- read.csv("SEN Tolima .csv")
ANTd<- read.csv("SEN Antioquia .csv")
ARAd<- read.csv("SEN Arauca .csv")
BOLd<- read.csv("SEN Bolivar .csv")
BOYd<- read.csv("SEN Boyaca .csv")
CASd<- read.csv("SEN Casanare .csv")
CESd<- read.csv("SEN Cesar .csv")
CUNd<- read.csv("SEN Cundinamarca .csv")
GUAd<- read.csv("SEN Guainia .csv")
MAGd<- read.csv("SEN Magdalena .csv")
METd<- read.csv("SEN Meta .csv")
NSTd<- read.csv("SEN NorteSantander .csv")
SANd<- read.csv("SEN Santander .csv")
SUCd<- read.csv("SEN Sucre .csv")
SAPd<- read.csv("SEN SanAndres .csv")
VAUd<- read.csv("SEN Vaupes .csv")
NARd<- read.csv("SEN Narino .csv")
QUId<- read.csv("SEN Quindio .csv")
CAUd<- read.csv("SEN Cauca .csv")
VLLd<- read.csv("SEN Valle .csv")
PUTd<- read.csv("SEN Putumayo .csv")
CORd<- read.csv("SEN Cordoba .csv")
RISd<- read.csv("SEN Risaralda .csv")
VICd<- read.csv("SEN Vichada .csv")
ATLd<- read.csv("SEN Atlantico .csv")
CALd<- read.csv("SEN Caldas .csv")
AMAd<- read.csv("SEN Amazonas .csv")
GUJd<- read.csv("SEN Guajira .csv")
CHOd<- read.csv("SEN Choco .csv")
CAQd<- read.csv("SEN Caqueta .csv")
GVRd<- read.csv("SEN Guaviare .csv")
HUId<- read.csv("SEN Huila .csv")
BOGd<- read.csv("SEN Bogota .csv")


COLd<- rbind(TOLd, ANTd, ARAd, BOLd, BOYd, CASd, CESd, CUNd, GUAd, MAGd, METd, NSTd, SANd, SUCd, 
             SAPd, VAUd, NARd, QUId, CAUd, VLLd, PUTd, CORd, RISd, VICd, ATLd, CALd, AMAd, GUJd,
             CHOd, CAQd, GVRd, HUId, BOGd)

SEN<- COLd


library("sensitivity")
X<- SEN[,c("lambda", "mu_a", "mu_cm", "mu_cs", "RRm", "gamma", "delta", "alpha",  "beta",   "RRp")] #,    "wam",    "was",    "wcm",    "wcs")]
letters<-c(expression(paste(lambda)), expression(paste(mu,'a')), expression(paste(mu,'cm')), expression(paste(mu,'cs')),
           expression(RRm), expression(gamma), expression(delta), expression(alpha), expression(beta),
           expression(RRp))


X<- SEN[,c("mu_a", "mu_cm", "mu_cs", "RRm", "gamma", "delta", "alpha",  "beta",   "RRp")] #,    "wam",    "was",    "wcm",    "wcs")]
letters<-c(expression(paste(mu,'a')), expression(paste(mu,'cm')), expression(paste(mu,'cs')),
           expression(RRm), expression(gamma), expression(delta), expression(alpha), expression(beta),
           expression(RRp))



names(X)<- letters
## Partial Rank Correlation Coeficient
library(epiR)
dat<- data.frame(cbind(X, Y = SEN$DALY))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-.1,.25), horiz=T,
        names=letters, main="DALY", las=2, xlab="PRCC")

names(X)<- letters
## Partial Rank Correlation Coeficient
library(epiR)
dat<- data.frame(cbind(X, Y = SEN$CasesCha))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-.3,.3), horiz=T,
        names=letters, main="CAses", las=2, xlab="PRCC")


names(X)<- letters
## Partial Rank Correlation Coeficient
library(epiR)
dat<- data.frame(cbind(X, Y = SEN$Chronic.Mild))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-.3,.3), horiz=T,
        names=letters, main="Cm", las=2, xlab="PRCC")




names(X)<- letters
## Partial Rank Correlation Coeficient
library(epiR)
dat<- data.frame(cbind(X, Y = SEN$Deaths))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-.3,.3), horiz=T,
        names=letters, main="Deaths", las=2, xlab="PRCC")
#####
setwd ("C:/Users/zmc13/Desktop/ChaModel/Results") # Correlation
TOLd<- read.csv("COR Tolima .csv")
ANTd<- read.csv("COR Antioquia .csv")
ARAd<- read.csv("COR Arauca .csv")
BOLd<- read.csv("COR Bolivar .csv")
BOYd<- read.csv("COR Boyaca .csv")
CASd<- read.csv("COR Casanare .csv")
CESd<- read.csv("COR Cesar .csv")
CUNd<- read.csv("COR Cundinamarca .csv")
GUAd<- read.csv("COR Guainia .csv")
MAGd<- read.csv("COR Magdalena .csv")
METd<- read.csv("COR Meta .csv")
NSTd<- read.csv("COR NorteSantander .csv")
SANd<- read.csv("COR Santander .csv")
SUCd<- read.csv("COR Sucre .csv")
SAPd<- read.csv("COR SanAndres .csv")
VAUd<- read.csv("COR Vaupes .csv")
NARd<- read.csv("COR Narino .csv")
QUId<- read.csv("COR Quindio .csv")
CAUd<- read.csv("COR Cauca .csv")
VLLd<- read.csv("COR Valle .csv")
PUTd<- read.csv("COR Putumayo .csv")
CORd<- read.csv("COR Cordoba .csv")
RISd<- read.csv("COR Risaralda .csv")
VICd<- read.csv("COR Vichada .csv")
ATLd<- read.csv("COR Atlantico .csv")
CALd<- read.csv("COR Caldas .csv")
AMAd<- read.csv("COR Amazonas .csv")
GUJd<- read.csv("COR Guajira .csv")
CHOd<- read.csv("COR Choco .csv")
CAQd<- read.csv("COR Caqueta .csv")
GVRd<- read.csv("COR Guaviare .csv")
HUId<- read.csv("COR Huila .csv")
BOGd<- read.csv("COR Bogota .csv")


COLd<- rbind(TOLd, ANTd, ARAd, BOLd, BOYd, CASd, CESd, CUNd, GUAd, MAGd, METd, NSTd, SANd, SUCd, 
             SAPd, VAUd, NARd, QUId, CAUd, VLLd, PUTd, CORd, RISd, VICd, ATLd, CALd, AMAd, GUJd,
             CHOd, CAQd, GVRd, HUId, BOGd)

COR<- ((COLd[which(COLd$X== "DALY"),])[,2:16]); colnames(COR)<- colnames(TOLd)[2:16]
CORq<- matrix(NA, 3, 15); colnames(CORq)<- colnames(TOLd)[2:16]
for(n in 1:15){    CORq[,n]<- quantile(COR[,n], c(.25, .5, .75))}
COR.DALY<- COR

COR<- ((COLd[which(COLd$X== "YLL"),])[,2:16]); colnames(COR)<- colnames(TOLd)[2:16]
CORq<- matrix(NA, 3, 15); colnames(CORq)<- colnames(TOLd)[2:16]
for(n in 1:15){    CORq[,n]<- quantile(COR[,n], c(.25, .5, .75), na.rm=T)}
COR.YLL<- COR

COR<- ((COLd[which(COLd$X== "YLD"),])[,2:16]); colnames(COR)<- colnames(TOLd)[2:16]
CORq<- matrix(NA, 3, 15); colnames(CORq)<- colnames(TOLd)[2:16]
for(n in 1:15){    CORq[,n]<- quantile(COR[,n], c(.25, .5, .75), na.rm=T)}
COR.YLD<- COR

COR<- ((COLd[which(COLd$X== "CasesCha"),])[,2:16]); colnames(COR)<- colnames(TOLd)[2:16]
CORq<- matrix(NA, 3, 15); colnames(CORq)<- colnames(TOLd)[2:16]
for(n in 1:15){    CORq[,n]<- quantile(COR[,n], c(.25, .5, .75))}
COR.Tot<- COR

COR<- ((COLd[which(COLd$X== "Acute"),])[,2:16]); colnames(COR)<- colnames(TOLd)[2:16]
CORq<- matrix(NA, 3, 15); colnames(CORq)<- colnames(TOLd)[2:16]
for(n in 1:15){    CORq[,n]<- quantile(COR[,n], c(.25, .5, .75))}
COR.Acute<- COR

COR<- ((COLd[which(COLd$X== "Asymptomatic"),])[,2:16]); colnames(COR)<- colnames(TOLd)[2:16]
CORq<- matrix(NA, 3, 15); colnames(CORq)<- colnames(TOLd)[2:16]
for(n in 1:15){    CORq[,n]<- quantile(COR[,n], c(.25, .5, .75))}
COR.I<- COR

COR<- ((COLd[which(COLd$X== "Chronic.Mild"),])[,2:16]); colnames(COR)<- colnames(TOLd)[2:16]
CORq<- matrix(NA, 3, 15); colnames(CORq)<- colnames(TOLd)[2:16]
for(n in 1:15){    CORq[,n]<- quantile(COR[,n], c(.25, .5, .75), na.rm=T)}
COR.Cm<- COR

COR<- ((COLd[which(COLd$X== "Chronic.Severe"),])[,2:16]); colnames(COR)<- colnames(TOLd)[2:16]
CORq<- matrix(NA, 3, 15); colnames(CORq)<- colnames(TOLd)[2:16]
for(n in 1:15){    CORq[,n]<- quantile(COR[,n], c(.25, .5, .75), na.rm=T)}
COR.Cs<- COR

COR<- ((COLd[which(COLd$X== "Deaths"),])[,2:16]); colnames(COR)<- colnames(TOLd)[2:16]
CORq<- matrix(NA, 3, 15); colnames(CORq)<- colnames(TOLd)[2:16]
for(n in 1:15){    CORq[,n]<- quantile(COR[,n], c(.25, .5, .75), na.rm=T)}
COR.Deaths<- COR


par(mar=c(3, 5, 2, 1), xpd=F)
(par(mfrow=c(3,3)))
par(las=1) #las=2 makes label text perpendicular to axis

boxplot(COR.Tot, horizontal=T, main="Total cases", col="lightblue", ylim=c(-1,1), boxlty = 0,  whisklty = 1, medcol = "blue", medlty=5, staplecol=T); abline(v=0, col="Red")
boxplot(COR.Acute, horizontal=T, main="Acute", col="lightblue", ylim=c(-1,1), boxlty = 0,  whisklty = 1, medcol = "blue", medlty=5, staplecol=T); abline(v=0, col="Red")
boxplot(COR.I, horizontal=T, main="Asymptomatic", col="lightblue", ylim=c(-1,1), boxlty = 0,  whisklty = 1, medcol = "blue", medlty=5, staplecol=T); abline(v=0, col="Red")
boxplot(COR.Cm, horizontal=T, main="Chronic Mild", col="lightblue", ylim=c(-1,1), boxlty = 0,  whisklty = 1, medcol = "blue", medlty=5, staplecol=T); abline(v=0, col="Red")
boxplot(COR.Cs, horizontal=T, main="Chronic Severe", col="lightblue", ylim=c(-1,1), boxlty = 0,  whisklty = 1, medcol = "blue", medlty=5, staplecol=T); abline(v=0, col="Red")
boxplot(COR.Deaths, horizontal=T, main="Deaths", col="lightblue", ylim=c(-1,1), boxlty = 0,  whisklty = 1, medcol = "blue", medlty=5, staplecol=T); abline(v=0, col="Red")
boxplot(COR.DALY, horizontal=T, main="DALYs", col="lightblue", ylim=c(-1,1), boxlty = 0,  whisklty = 1, medcol = "blue", medlty=5, staplecol=T); abline(v=0, col="Red")
boxplot(COR.YLL, horizontal=T, main="YLLs", col="lightblue", ylim=c(-1,1), boxlty = 0,  whisklty = 1, medcol = "blue", medlty=5, staplecol=T); abline(v=0, col="Red")
boxplot(COR.YLD, horizontal=T, main="YLDs", col="lightblue", ylim=c(-1,1), boxlty = 0,  whisklty = 1, medcol = "blue", medlty=5, staplecol=T); abline(v=0, col="Red")
#plot(1, type="n", axes=F, xlab="", ylab="")


# Table for joining with costs
names(COLe)[6:41]<-1985:2020


CA<- rbind(cbind(colSums(COLe[which(COLe$bound=="Median" & COLe$metric == "Cases.Asymptomatic"),31:35]),
colSums(COLe[which(COLe$bound=="Low" & COLe$metric == "Cases.Asymptomatic"),31:35]),
colSums(COLe[which(COLe$bound=="Upper" & COLe$metric == "Cases.Asymptomatic"),31:35])),

cbind(colSums(COLe[which(COLe$bound=="Median" & COLe$metric == "Cases.ChronicMild"),31:35]),
colSums(COLe[which(COLe$bound=="Low" & COLe$metric == "Cases.ChronicMild"),31:35]),
colSums(COLe[which(COLe$bound=="Upper" & COLe$metric == "Cases.ChronicMild"),31:35])),

cbind(colSums(COLe[which(COLe$bound=="Median" & COLe$metric == "Cases.ChronicSevere"),31:35]),
colSums(COLe[which(COLe$bound=="Low" & COLe$metric == "Cases.ChronicSevere"),31:35]),
colSums(COLe[which(COLe$bound=="Upper" & COLe$metric == "Cases.ChronicSevere"),31:35])))


### ======================================== STANDARISED MORTALITY RATES =============================

### FOR CHAGAS

# Using COLe: Total dataset
Mortality_Chagas<- ((COLe[which(COLe$bound== "Median" & COLe$metric=="Deaths.Total"),]))[,c(1,6:41)]
Mortality_Others<- ((COLe[which(COLe$bound== "Median" & COLe$metric=="Deaths.OtherHD"),]))[,c(1,6:41)]
names(Mortality_Chagas)[2:37]<-1985:2020; names(Mortality_Others)[2:37]<-1985:2020
MortCh<- melt(Mortality_Chagas,id=("agec"))
MortOt<- melt(Mortality_Others,id=("agec"))

MortiCh<-numeric(17*36)
MortiOt<-numeric(17*36)
for (t in 1985:2020){
MortiCh[]<- aggregate(value ~ agec,  data=MortCh[which(MortCh$variable==t),], sum)[,2]
MortiOt[]<- aggregate(value ~ agec,  data=MortOt[which(MortOt$variable==t),], sum)[,2]
}


Pop <- read.csv("C:/Users/zmc13/Desktop/ChaModel/Robject/Obs.Alive.Totalx5.csv")
deathsxyearCh<- matrix(MortiCh, 17,36)
deathsxyearOt<- matrix(MortiOt, 17,36)
Pop.Col<- (Pop[which(Pop$dept=="Colombia"),])[,2:37]
StandPop <- read.csv("C:/Users/zmc13/Desktop/ChaModel/Robject/Age_Stand_Pop.csv")
MortCh_GBD <- read.csv("C:/Users/zmc13/Desktop/ChaModel/Robject/Mortality_Chagas_GBD.csv")


library(epitools)
AdjMortCh<-matrix(NA, 36, 4)
AdjMortOt<-matrix(NA, 36, 4)
for (h in 1:36){
  year<- h
  AdjMortCh[h,]<- ageadjust.direct(count=deathsxyearCh[,year], pop=Pop.Col[,year], stdpop=StandPop[,3], conf.level = 0.95)
  AdjMortOt[h,]<- ageadjust.direct(count=deathsxyearOt[,year], pop=Pop.Col[,year], stdpop=StandPop[,3], conf.level = 0.95)
}

par(mfrow=c(1,2))
par(adj=.5); par(mgp = c(3.5, .8, 0))
par(las=1) #las=2 makes label text perpendicular to axis
par(mar=c(4,6,3,2)) 
AdjMortYearCh<-cbind(1985:2020, AdjMortCh*1e5)[1:31,]
AdjMortYearOt<-cbind(1985:2020, AdjMortOt*1e5)[1:31,]
# PlotA
plot(AdjMortYearCh[,1],AdjMortYearCh[,3], ylab="AMR / 100,000", 
     main="A", cex.main=2,
     ylim=c(0,50), col="purple", pch=16, cex=2, cex.axis=2, cex.lab=2, xlab="")
legend("topright", "Model", col="purple", pch=16, bty="n", cex=2)

# PlotB
plot(MortCh_GBD[,1], MortCh_GBD[,2],col="mediumseagreen", pch=18, ylim=c(0,0.5), cex.axis=2,  
     main="B", cex.main=2,
     xlim=c(1985, 2015), xlab="", ylab="AMR / 100,000", cex=2, cex.axis=2, cex.lab=2)
legend("topright", "Reported", col="mediumseagreen", pch=18, bty="n", cex=2)

 # plot(AdjMortYearOt[,1],AdjMortYearOt[,3], main="Specific CVD Mortality",
 #      ylim=c(0,1000), col="red", pch=10, cex=2, cex.axis=2)
 # legend("topright", "Reported", col="red", pch=10, bty="n", cex=2)


#~~~~~~~~~~~~~~ PCH ~~~~~~~~~~~~~~~~~~~~~
par(mfrow=c(5,5)); par(mar=c(2,2,2,2)); for (g in 1:25){  plot(1:5, cex=3, col=2, pch=g, main=g, axes=FALSE)}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#### DATA USED FOR COSTS CALCULATIONS
# Data for Costs

d4c<- COLe
d4c10<- data.frame(cbind(aggregate(`2010` ~ bound + metric, data= d4c, sum), rep(2010, 39)))
names(d4c10)[3:4]<- c("n", "year")
d4c11<- data.frame(cbind(aggregate(`2011` ~ bound + metric, data= d4c, sum), rep(2011, 39)))
names(d4c11)[3:4]<- c("n", "year")
d4c12<- data.frame(cbind(aggregate(`2012` ~ bound + metric, data= d4c, sum), rep(2012, 39)))
names(d4c12)[3:4]<- c("n", "year")
d4c13<- data.frame(cbind(aggregate(`2013` ~ bound + metric, data= d4c, sum), rep(2013, 39)))
names(d4c13)[3:4]<- c("n", "year")
d4c14<- data.frame(cbind(aggregate(`2014` ~ bound + metric, data= d4c, sum), rep(2014, 39)))
names(d4c14)[3:4]<- c("n", "year")

dfcT<- data.frame(rbind(d4c10, d4c11, d4c12, d4c13, d4c14)) 

write.csv(dfcT, "Data4Costsb.csv")
getwd()
