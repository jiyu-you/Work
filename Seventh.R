install.packages("foreign")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

Daedeokgu_CCTV<-read.csv("D:/Workspace/R/Daejeon_Daedeokgu_CCTV_20190331.csv")
str(Daedeokgu_CCTV)
dim(Daedeokgu_CCTV)
View(Daedeokgu_CCTV)

Donggu_CCTV<-read.csv("D:/Workspace/R/Daejeon_Donggu_CCTV_20190321.csv")
str(Donggu_CCTV)
dim(Donggu_CCTV)
View(Donggu_CCTV)

Junggu_CCTV<-read.csv("D:/Workspace/R/Daejeon_Junggu_CCTV_20190527.csv")
str(Junggu_CCTV)
dim(Junggu_CCTV)
View(Junggu_CCTV)

Seogu_CCTV<-read.csv("D:/Workspace/R/Daejeon_Seogu_CCTV_20190106.csv")
str(Seogu_CCTV)
dim(Seogu_CCTV)
View(Seogu_CCTV)

Yuseonggu_CCTV<-read.csv("D:/Workspace/R/Daejeon_Yuseonggu_CCTV_20190520.csv")
str(Yuseonggu_CCTV)
dim(Yuseonggu_CCTV)
View(Yuseonggu_CCTV)

Daedeokgu<-select( Daedeokgu_CCTV, 汲摹格利备盒 )
Donggu<-select( Donggu_CCTV, 汲摹格利备盒 )
Junggu<-select( Junggu_CCTV, 汲摹格利备盒 )
Seogu<-select( Seogu_CCTV, 汲摹格利备盒 )
Yuseonggu<-select( Yuseonggu_CCTV, 汲摹格利备盒 )

class(Daedeokgu$汲摹格利备盒)
table(Daedeokgu$汲摹格利备盒)

class(Donggu$汲摹格利备盒)
table(Donggu$汲摹格利备盒)

class(Junggu$汲摹格利备盒)
table(Junggu$汲摹格利备盒)

class(Seogu$汲摹格利备盒)
table(Seogu$汲摹格利备盒)

class(Yuseonggu$汲摹格利备盒)
table(Yuseonggu$汲摹格利备盒)

Daedeokgu_CCTV %>%
  group_by( 汲摹格利备盒 )%>%
  filter(汲摹格利备盒 == "积劝规裹")

Daedeokgu_CCTV$汲摹格利备盒 <- as.character( Daedeokgu_CCTV$汲摹格利备盒 )
str( Daedeokgu_CCTV )  

Daedeokgu <- Daedeokgu_CCTV %>%
  select( 包府扁包疙, 汲摹格利备盒 ) %>%
  filter( substr( 汲摹格利备盒, 1, 4 ) == '积劝规裹' )
Daedeokgu
str( Daedeokgu )
View( Daedeokgu )
AA <- table( Daedeokgu )
AA

Donggu <- Donggu_CCTV %>%
  select( 包府扁包疙, 汲摹格利备盒 ) %>%
  filter( substr( 汲摹格利备盒, 1, 4 ) == '积劝规裹' )
Donggu
str( Donggu )
View( Donggu )
BB <- table( Donggu )
BB

Junggu <- Junggu_CCTV %>%
  select( 包府扁包疙, 汲摹格利备盒 ) %>%
  filter( substr( 汲摹格利备盒, 1, 4 ) == '积劝规裹' )
Junggu
str( Junggu )
View( Junggu )
CC <- table( Junggu )
CC

Seogu <- Seogu_CCTV %>%
  select( 包府扁包疙, 汲摹格利备盒 ) %>%
  filter( substr( 汲摹格利备盒, 1, 4 ) == '积劝规裹' )
Seogu
str( Seogu )
View( Seogu )
DD <- table( Seogu )
DD

Yuseonggu <- Yuseonggu_CCTV %>%
  select( 包府扁包疙, 汲摹格利备盒 ) %>%
  filter( substr( 汲摹格利备盒, 1, 4 ) == '积劝规裹' )
str( Yuseonggu )
View( Yuseonggu )
EE <- table( Yuseonggu )
EE

Daejeon <- bind_rows(Daedeokgu, Donggu, Junggu, Seogu, Yuseonggu)
Daejeon

Daejeon_CCTV <- count(Daejeon)
Daejeon_CCTV

Daedeok<-Daedeokgu%>%
  count(Daedeokgu,Daejeon_CCTV)%>%
  group_by(Daedeokgu)
  mutate(pct=round(n/sum(n)*100, 1))
Daedeok





Daejeon_CCTV_install<-Daejeon_CCTV%>%
  mutate(pct=round(n/sum(n)*100, 1))
Daejeon_CCTV_install

ggplot(data=Daejeon_CCTV_install,
         aes(x=Daejeon, y=pct, fill="积劝规裹"))+
  geom_col()+
  coord_flip()
  
ggplot(data=Daejeon_CCTV_install,
         aes(x=Daejeon, y=pct, fill="积劝规裹"))+
  geom_bar()+
  coord_polar
