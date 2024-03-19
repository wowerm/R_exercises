#zad1####
#a
c(5,6,2)
#b
c(1:20)
#c
c(20:1)
#d
c(1:20,19:1)

#zad2####
#a
seq(10,100,10)
#b
seq(1,5,0.5)
#c
seq(-3,3,0.3)
#d
seq(10,0,-2)

#zad3####
x <- (1:10)
#a
x^2
#b
round(1/x,4)
#c
round(1/x^2,4)

#zad4####
#a
mean(x)
#b
sum(x)
#c
x-mean(x)
#d
(x-mean(x))^2
#e
sum(x-mean(x))
#f
sum((x-mean(x))^2)

#zad5####
#a
sum(abs(x-mean(x)))
#b
sqrt(sum((x-mean(x))^2))

#zad6####
y <- 1:10%%3+1
#a
x+y
#b
x*y
sum(x/y)

#zad7####
w1 <- c(2,1)
w2 <- c(6,4)
sqrt(sum((abs(w1-w2))^2))

#zad8####
set.seed(42) #The ulimate question of life, the universe and everything :)
v <- sample.int(365,80,replace=TRUE)
length(unique(v))

#zad9####
#a
plot(x,y)
#b
plot(x,y,type = "l")
#c
plot(x,y,type = "l",col="red",lty=2)

#zad10####
x <- -10:20
y <- x ^ 2 - 5*x-5
plot(x,y,type="l",col="red")
abline(h = 0)
abline(v = 0)

#b
x <- -15:10
y <- x^3+10*x^2-10*x-50
plot(x,y,type="l",col="red")
abline(v = 0)
abline(h = 0)

#c
x <-seq(0,10,0.01)
y <- sqrt(x)
plot(x,y,type="l",col="red")
abline(v = 0)
abline(h = 0)

#d
x <- seq(-5,5,0.1)
y <- 1/x
plot(x,y,type="l",col="red")
abline(v = 0)
abline(h = 0)

#e
y <- 1/(x-1)-1
plot(x,y,type="l",col="red")
abline(v = 0)
abline(h = 0)

#zad11####

(?rivers)
#a
mean(rivers)

#b
mean(rivers)*1.609344

#zad12####

(?lynx)
x <- 1821:1934
plot(lynx,type="b",col="red",xlab="rok",ylab="liczba",main="Liczba upolowanych rysiów w Kanadzie w latach 1821-1934")

#zad13####

rep(1,times=5)
rep("ala",times=10)
rep(c(1,2,3),times=3)
rep(c(1,2),times=c(2,3))
rep(c(1,2),times=2,each=4)

#a
rep(6,times=80)

#b
rep("kajak",times=20)

#c
rep(c(5,6,2),times=20)

#d
rep(c(5,6,2),times=c(10,15,7))

#e
rep(c("a","b"),times=c(5,7))

#zad14####

quantile(rivers)
?quantile

#a
quantile(rivers,c(0.25,0.5,0.75))

#b
quantile(rivers,seq(0.1,0.9,0.1))

#c
quantile(rivers,c(0.05,0.95))

#zad15####

x <- seq(2,6,0.1)
y <- exp(x)*cos(x)
plot(x,y,type="l",xlab="x",ylab=expression("e"^x*cos(x)))
abline(h = 0, col="red")

#zad16####
x <- c(6,4,2,7,1,9,0,6)

#a
x == 6
#b
x != 6
#c
x>4
#d
x<3 | x>6

#zad17####

#a
x[x>6]
#b
x[x<2|x>7]
#c
x[x>4&x<9]

#zad18####

#a
rivers[rivers>1000]
#b
sum(rivers<300)
#c
sum(rivers>mean(rivers))
#d
sum(rivers<mean(rivers))/length(rivers)
#e
sum(rivers<=quantile(rivers,0.25))

#zad19####
set.seed(42)
x <- rnorm(100000,100,15)

#a
mean(x)
#b
sd(x)
#c
sum(x<mean(x))/length(x)*100
#d
sum(x<mean(x)-sd(x))/length(x)*100
#e
sum(x>mean(x)-2*sd(x)&x<mean(x)+2*sd(x))/length(x)*100

#zad20####

#a
max(mtcars$wt)
#b
mean(mtcars$wt)
#c
table(mtcars$am)
#d
table(mtcars$am)/length(mtcars$am)*100
#e
mean(mtcars$mpg[mtcars$gear==4])
#f
mean(mtcars$qsec[mtcars$wt<2])

#zad21####
plot(mtcars$wt,mtcars$mpg)

#zad22####
#a
mtcars[1:4,]
#b
mtcars[mtcars$carb==1,]
#c
mtcars[mtcars$wt>4,]
#d
mtcars[mtcars$hp>150&mtcars$wt>5,]

#zad23####
#a
mtcars[mtcars$wt<2.2,c("wt","hp","cyl","disp")]
#b
mtcars[mtcars$hp<80,c("hp","cyl","mpg")]
#c
?mtcars
mtcars[mtcars$am==1&mtcars$qsec<16,c("qsec","am")]

#zad24####
mtcars_am0 <- mtcars[mtcars$am==0,]
#a
dim(mtcars_am0)
#b
plot(mtcars_am0$wt,mtcars_am0$hp)

#zad25####
hist(mtcars_am0$disp,main ="Histogram zmiennej disp")

#zad26####
boxplot(mtcars$hp ~ mtcars$am,col="lightsalmon")

#zad27####
data("InsectSprays")
str(InsectSprays)
head(InsectSprays)
?InsectSprays
#b
table(InsectSprays$spray)
#c
mean(InsectSprays$count[InsectSprays$spray=="A"])
#d
mean(InsectSprays$count[InsectSprays$spray=="C"])
#e
boxplot(InsectSprays$count~InsectSprays$spray,xlab="typ preparatu",ylab="ilość insektów")

#zad28####
arr <- USArrests
arr$State <- rownames(USArrests)
arr

#a
arr[arr$UrbanPop<50,"State"]
#b
arr[arr$Rape==max(arr$Rape),"State"]
#c
arr[arr$Murder==min(arr$Murder),"State"]
#d
sum(arr$Assault>200)
#e
boxplot(arr$Assault,horizontal=TRUE)

#zad29####
v <- c(5,NA,2,NA,7)
is.na(v)
!is.na(v)
set.seed(10)
grades <- sample(c(5,4.5,4,3.5,3,NA),58,replace = TRUE)

#a
is.na(grades)
#b
sum(!is.na(grades))
#c
grades[!is.na(grades)]
#d
sum(grades==5,na.rm = TRUE)
#e
mean(grades,na.rm = TRUE)

#zad30####
top12 <- head(arr$State[sort(arr$UrbanPop,decreasing = TRUE)],12)
top12
plot()

#zad31####
tt <- read.csv(file = "./data/titanic.csv")
tt$Survived <- factor(tt$Survived, levels = c(0, 1),
                      labels = c("not survived", "survived"))
tt$Pclass <- factor(tt$Pclass)
tt$Sex <- factor(tt$Sex)
tt$Embarked <- factor(tt$Embarked)

#a
nrow(tt)
#b
table(tt$Sex)
#c
mean(tt$Fare)
#d
mean(tt$Fare[tt$Sex=="female"])
#e
sum(tt$Fare>100)

#zad32####

#a
table(tt$Pclass)
#b
mean(tt$Fare[tt$Pclass==1])
#c
tapply(tt$Fare,tt$Pclass,mean)
#d
sum(tt$Pclass==2&tt$Fare<20)

#zad33####

#a
mean(tt$Age,na.rm = TRUE)
#b
summary(tt$Age)
#c
mean(tt$Age[tt$Sex=="male"],na.rm = TRUE)
#d
tapply(tt$Age,tt$Sex,function(x)mean(x,na.rm=TRUE))

#zad34####

#a
table(tt$Sex[!is.na(tt$Age)==FALSE])
#b
tt70 <- tt[tt$Age>=70&!is.na(tt$Age),]
tt70[,c("Name","Sex","Pclass","Age")]
#c
tapply(tt$Age,tt$Pclass,function(x) max(x,na.rm=TRUE))

#zad35####

#a
table(tt$Embarked)
#b
tt[tt$Embarked=="",c("Name","Sex","Age","Pclass","Embarked","Cabin")] #!!!!!!#

#zad36####

#a
table(tt$Survived)
#b
prop.table(table(tt$Survived))*100    
#c
table(tt$Survived,tt$Pclass)
#d
prop.table(table(tt$Survived,tt$Pclass))*100   

#zad37####

#a
prop.table(table(tt$Survived,tt$Pclass),margin = 1)*100
#b
prop.table(table(tt$Survived,tt$Pclass),margin = 2)*100

#zad38####

prop.table(table(tt$Survived,complete.cases(tt)),margin = 2)*100

#zad39####

tt$Relatives <- tt$SibSp+tt$Parch
tt$Relatives

#a
sum(tt$Relatives==0)
#b
round(prop.table(table(tt$Relatives))*100,2)

#zad40####

boxplot(tt$Age~tt$Sex,col="lemonchiffon")

#zad41####

#a
boxplot(tt$Fare~tt$Survived,col="lightslateblue",horizontal = TRUE)

#b
boxplot(tt$Fare~tt$Survived,col="lightslateblue",horizontal = FALSE, outline=FALSE)

#zad42####

#a
head(sort(tt$Age,decreasing = TRUE),20)

#b
head(tt[order(tt$Age),c("Name","Age","Sex","Survived")],10)

#zad43####

#a
sum(grepl("Ann",tt$Name))

#b
tt$Name[grepl("Ann",tt$Name)]

#zad44####

#a
mean(tt$Age[grepl("Master",tt$Name)],na.rm = TRUE)

#b
mean(tt$Age[grepl("Mr.",tt$Name,fixed = TRUE)],na.rm = TRUE)

#c
tt[grepl("Rev.",tt$Name,fixed = TRUE),c("Name","Age","Pclass","Survived")]

#zad45####

mov <- read.csv2(file="./data/movies.csv")
mov
mov$genre <- factor(mov$genre)

#a
str(mov)

#b
summary(mov)

#zad46####

#a
table(mov$year)

#b
tapply(mov$gross,mov$year,mean)

#zad47####

plot(unique(mov$year),tapply(mov$gross,mov$year,mean)/1000000,type="b",xlab="rok",ylab="przychód (w milionach)")

#zad48####

pokemon1_4 <- read.csv("./data/pokemon1_4.csv", skip = 3)
pokemon5_6 <- read.csv("./data/pokemon5_6.csv")

#a
#pierwsze trzy wiersze to nie dane#

#b
pokemon <- rbind(pokemon1_4,pokemon5_6)

#c
pokemon <- pokemon[,2:13]

#d
names(pokemon)[names(pokemon)=="Type.1"] <- "Type1"
names(pokemon)[names(pokemon)=="Type.2"] <- "Type2"
names(pokemon)[names(pokemon)=="Sp..Atk"] <- "SpAtk"
names(pokemon)[names(pokemon)=="Sp..Def"] <- "SpDef"

#e
pokemon$Legendary <- factor(pokemon$Legendary,levels=c("False","True"),labels=c("no","yes"))

#zad49####

#a
boxplot(pokemon$Attack~pokemon$Legendary,xlab="Legendary",ylab="Attack")

#b
plot(pokemon$Attack,pokemon$Defense,pch=19,col=factor(pokemon$Legendary))

#zad50####

#???????????????????????#

mov <- read.csv2(file = "./data/movies.csv")
mov$genre <- factor(mov$genre)
mov$year <- factor(mov$year)

str(mov)

#zad51####

set.seed(10)
x <- rnorm(100, mean = 5, sd = 2)
x.interval <- cut(x, breaks = c(0,2.5,5,7.5,10))
table(x.interval)

mov$duration.interval <- cut(mov$duration, breaks = c(60,90,120,Inf))
table(mov$duration.interval)

#zad52####

#a
barplot(table(mov$genre), space = 0.2)

#b
barplot(table(mov$duration.interval), col = "orange2")

#zad53####

#a
hist(mov$duration,breaks = 20,xlim=c(0,300),xlab="czas trwania",ylab="liczba",main="")

#b
boxplot(mov$duration,ylim=c(0,300),ylab="czas trwania",horizontal = TRUE)

#zad54####

plot(mov$budget,mov$gross,pch=23,cex=0.8,bg="salmon")

#zad55####

plot(mov$budget,mov$gross,col=c("red","gold1","lightgreen","cornflowerblue")[factor(mov$genre)],pch=18,cex=1)
legend("topleft",legend=levels(factor(mov$genre)),fill=c("red","gold1","lightgreen","cornflowerblue"),cex=0.7)

#zad56####

head(mov[order(mov$gross,decreasing = TRUE),c("title","gross","year")],10)

#zad57####

barplot(tapply(mov$rating, mov$genre, mean),col="brown",space = 1.3)

#zad58####
mov$profit <- mov$gross - mov$budget

plot(mov$duration,mov$profit,pch=16,col=c("red","yellow","blue")[factor(mov$duration.interval)])

#zad59####

head(mov[order(mov$profit),c("title","gross","genre","director","year","profit")],10)

#zad60####

#a
boxplot(mov$profit~mov$genre)

#b
#funkcja tapply mówi nam, że Action i Adventure mają znacznie bardziej zróżnicowaną zyskowność
tapply(mov$profit,mov$genre,sd)

#zad61####

plot(mov$budget/1E6,mov$profit/1E6,col=c("red","gold1","lightgreen","cornflowerblue")[factor(mov$genre)],xlab="budżet",ylab="zysk")
abline(h=0)
legend("topright",legend=levels(factor(mov$genre)),fill=c("red","gold1","lightgreen","cornflowerblue"),cex=0.7)

#zad62####

mov$profitable <- mov$profit>=0                              
mov$profitable <- factor(mov$profitable)
boxplot(mov$rating~mov$profitable)
#zad63####

prop.table(table(mov$profitable,mov$genre),margin = 2)

#zad64####

bikes <- read.csv2(file = "./data/bikes.csv")
str(bikes)                        
summary(bikes)

bikes$yr <- factor(bikes$yr, levels = c(0,1),labels = c(2011,2012))
bikes$season <- factor(bikes$season,levels = 1:4,labels = c("winter","spring","summer","autumn"))
table(bikes$season)

#zad65####

bikes$weekday <- factor(bikes$weekday,levels = c(1:6,0),labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
table(bikes$weekday)

#zad66####

bikes$weathersit <- factor(bikes$weathersit,levels = 1:3,labels = c("clear","mist","rain/snow"))
table(bikes$weathersit)

#zad67####

bikes$mnth <- factor(bikes$mnth)
bikes$holiday <- factor(bikes$holiday)
bikes$workingday <- factor(bikes$workingday)

str(bikes)

#zad68####

table(bikes$registered+bikes$casual==bikes$cnt)

#zad69####

tapply(bikes$cnt,bikes$yr,mean)

#zad70####

plot(bikes$instant,bikes$cnt,col=c("cornflowerblue","springgreen2","yellow","orange3")[factor(bikes$season)])

#zad71####

plot(bikes$temp[bikes$yr==2012],bikes$cnt[bikes$yr==2012],pch=20,
     col=c("cornflowerblue","springgreen2","yellow","orange3")[factor(bikes$season)],
     xlab = "temperatura",ylab="liczbawypożyczeń",main="Temperatura i wypożyczenia w 2012 roku")
legend("topleft",legend=levels(factor(bikes$season)),fill = c("cornflowerblue","springgreen2","yellow","orange3"),cex=0.7)

#zad72####

boxplot(bikes$registered~bikes$weekday,ylim=c(0,7000))
boxplot(bikes$casual~bikes$weekday,ylim=c(0,7000))

#zarejestrowani użytkownicy jeżdża częściej w tygodniu, a niezarejestrowani użytkownicy częściej w weekendy#

#zad73####

bikes[bikes$casual>bikes$registered,c("dteday","casual","registered","holiday","workingday","weekday")]

#zad74####

boxplot(bikes$hum~bikes$weathersit)

#zgodny z oczekiwaniami#

#zad75####

imdb <- read.csv2(file = "./data/imdb.csv")
str(imdb)
tmp <- table(trimws(unlist(strsplit(imdb$Actors,","))))
head(tmp[order(tmp,decreasing = T)],20)

imdb <- read.csv2(file = "./data/imdb.csv")

imdb <- imdb[complete.cases(imdb),]
imdb <- imdb[,-1]
str(imdb)

#zad76####

#a
summary(nchar(imdb$Title))

#b
hist(nchar(imdb$Description),breaks = 20,xlim=c(0,500),main = "",xlab="")

#zad77####

#a
grep("^[0-9]",imdb$Title,value = TRUE)

#b
grep(" [2-9]$",imdb$Title,value = T)

#zad78####

#a
gsub("&","and",imdb$Title[grepl("&",imdb$Title)])

#b
gsub("  "," ",imdb$Title[grepl("  ",imdb$Title)])

#zad79####

#a

imdb$dummy_comedy <- ifelse(grepl("Comedy",imdb$Genre),"1","0")
head(imdb,5) #sprawdzenie#

#b

barplot(table(imdb$dummy_comedy),ylim=c(0,600),xlab = "czy komedia?")

#zad80####

sum(grepl("Action",imdb$Genre))

#zad81####

#a
imdb$Title[grepl("Ryan Gosling",imdb$Actors)]

#b
imdb$Title[grepl("Emma Stone",imdb$Actors)&grepl("Ryan Gosling",imdb$Actors)]

#c
imdb[grepl("Adam Sandler",imdb$Actors),c("Title","Rating","Revenue.Millions.")]

#zad82####

imdb$czy_sifi <- ifelse(grepl("Sci-Fi",imdb$Genre),"Sci-Fi","nie Sci-Fi")

table(imdb$czy_sifi)

#zad83####

#a
tapply(imdb$Rating,imdb$czy_sifi,mean)

#b
tapply(imdb$Revenue.Millions.,imdb$czy_sifi,mean)

#zad84####

plot(imdb$Rating[imdb$czy_sifi=="Sci-Fi"],imdb$Metascore[imdb$czy_sifi=="Sci-Fi"],pch=16,col="lightblue")

#zad85####

imdb[imdb$czy_sifi=="Sci-Fi"&imdb$Rating<=5,c("Title","Year","Director","Rating","Metascore")]

#zad86####

vgs <- read.csv2(file = "./data/vgs.csv")

#a
str(vgs)

#b
summary(vgs)

#c
sum(complete.cases(vgs))

#zad87####

plot(table(vgs$year),type="l")

#zad88####

head(sort(tapply(vgs$global_sales,vgs$platform,sum),decreasing = T),10)

#zad89####

platform <- data.frame(vgs$platform[vgs$genre=="Platform"],vgs$name[vgs$genre=="Platform"],vgs$genre[vgs$genre=="Platform"])
platformx <- data.frame(platform[platform=="XB"|platform=="X360"|platform=="XOne"])
prop.table(table(platformx))

#zad90####

grep("[:blank:]?Mario[:blank:]?",vgs$name)     #?????????chuja nie działa#

#zad91####

tt <- read.csv(file = "./data/titanic.csv")
tt <- tt[complete.cases(tt),]
tt$Survived <- factor(tt$Survived,levels = c(0,1),labels = c("no","yes"))
tapply(tt$Age[tt$Pclass==1],tt$Survived[tt$Pclass==1],mean)

#zad92####
#przed tym zadaniem trzebva jeszcze raz tt wgrac bez complete.cases#
tt$Fare_interval <- cut(tt$Fare,breaks = c(0,10,20,50,100,500,Inf))

round(prop.table(table(tt$Pclass,tt$Fare_interval),margin = 1)*100,2)

#zad93####

tapply(tt$Fare,tt$Embarked,mean)[2:4]
#zad94####

mov <- read.csv2(file = "./data/movies.csv")

plot(unique(mov$year),tapply(mov$duration,mov$year,mean),type = "b",col=c("orange"),pch=19,cex=1.5,ylab="sredni czas trwania",xlab="rok")

#zad95####

head(sort(tapply(mov$rating,mov$director,mean),decreasing = T),3)

#zad96####

mov[mov$director=="Damien Chazelle"|mov$director=="Christopher Nolan"|mov$director=="Lee Unkrich",c("director","title","rating")]

#zad97####

bikes <- read.csv2(file = "./data/bikes.csv")
bikes$season <- factor(bikes$season,levels = c(1,2,3,4),labels = c("winter","spring","summer","autumn"))
bikes$yr <- factor(bikes$yr,levels = 0:1,labels = c(2011,2012))
bikes2012 <- bikes[bikes$yr==2012,]
boxplot(bikes2012$registered~bikes2012$season,col="white")

#zad98####

bikes$weathersit <- factor(bikes$weathersit,levels=1:3,labels=c("clear","mist","snow or rain"))
bikes2011.04 <- bikes[bikes$mnth==4&bikes$yr==2011,]
tapply(bikes2011.04$cnt,bikes2011.04$weathersit,mean)

#zad99####

vgs1 <- vgs[vgs$genre=="Adventure"|vgs$genre=="Role-Playing"|vgs$genre=="Strategy",]
vgs1$genre <- factor(vgs1$genre)
plot(vgs1$na_sales,vgs1$eu_sales,col=c("red","green","yellow")[factor(vgs1$genre)],pch=16)
legend("topleft",legend=levels(factor(vgs1$genre)),fill =c("red","green","yellow" ),cex = 1.2)

#zad100####
zad100 <- mov[table(mov$director)>=6,c("gross","director")]
barplot(sort(tapply(zad100$gross/1000000,zad100$director,mean),decreasing = F),horiz = T,
        main="Najbardziej dochodowi rezyserzy",xlab="sredni przychod filmu (mln)",
        names.arg = zad100$director)

mov <- read.csv2(file = "./data/movies.csv")
mov$genre <- factor(mov$genre)

#zad101####

#a
mov %>% 
  filter(duration > 180) %>%
  select(title,genre,year,duration)

#b
mov %>% 
  filter(rating>8.5|gross>600000000) %>% 
  select(title,year,gross,rating)

#zad102####

#a
mov %>% 
  filter(rating>=8.2 & year %in% seq(2011,2016,1)) %>% 
  arrange(rating) %>% 
  select(title,year,rating)

#b
mov %>% 
  filter(genre=="Comedy"&budget>=9E7) %>% 
  arrange(desc(budget)) %>% 
  select(title,year,genre,budget)

#zad103####

#a
mov %>% 
  group_by(year) %>% 
  summarise(mean.duration=mean(duration),
            mean.rating=mean(rating))

#b
mov %>% 
  group_by(genre) %>% 
  summarise(mean.budget=mean(budget),
            mean.gross=mean(gross))

#zad104####

mov %>% 
  filter(year==2011|year==2010) %>% 
  group_by(year,genre) %>% 
  summarise(mean.reviews=mean(reviews))

#zad105####

#a
mov %>% 
  mutate(budget_mln=round(budget/1E6,2),gross_mln=round(gross/1E6,2)) %>%
  select(title,year,budget_mln,gross_mln) %>% 
  head(10)

#b
mov %>% 
  mutate(profit=gross-budget) %>% 
  select(title,profit) %>% 
  arrange(desc(profit)) %>% 
  head(10)

#zad106####

mov %>% 
  mutate(duration2=cut(duration,c(-Inf,90,120,180,Inf))) %>% 
  select(title,rating,duration,duration2) %>% 
  arrange(desc(rating)) %>% 
  head(10)

#zad107####

mov.action10 <- mov %>% 
  filter(genre=="Action") %>% 
  select(title,director,year,gross) %>% 
  arrange(desc(gross)) %>% 
  head(10)

#zad108####

mov.genre.rating <- mov %>% 
  group_by(genre) %>% 
  summarise(mean.rating=mean(rating),median.rating=median(rating),max.rating=max(rating),min.rating=min(rating))

#zad109####

barplot(mov.genre.rating$median.rating,names.arg = mov.genre.rating$genre,col="orange",ylim = c(0,7),space = 2)

#zad110####

mov %>% 
  group_by(year) %>% 
  summarise(mean.rating=mean(rating)) %>% 
  head(5)

#zad111####

mov %>% 
  group_by(year,genre) %>% 
  summarise(mean.rating=mean(rating),n())

#zad112####

mov %>% 
  mutate(profit=(gross-budget)/1E6) %>% 
  group_by(director) %>% 
  mutate(dir.n.films=n()) %>% 
  filter(dir.n.films>=5) %>% 
  summarise(mean.profit=mean(profit),n.films=mean(dir.n.films)) %>% 
  arrange(desc(mean.profit)) %>% 
  head(10)

#zad113####

#a
library(yarrr)

#b
pir <- pirates

#c
pir$sex <- factor(pir$sex)
pir$headband <- factor(pir$headband)
pir$college <- factor(pir$college)
pir$favorite.pirate <- factor(pir$favorite.pirate)
pir$sword.type <- factor(pir$sword.type)
pir$fav.pixar <- factor(pir$fav.pixar)

#zad114####

pir %>%
  select(id,sex,age) %>% 
  arrange(desc(age)) %>% 
  head(10)

#zad115####

pir %>% 
  select(id,sex,beard.length) %>% 
  arrange(desc(beard.length)) %>% 
  head(10)

#zad116####

pir %>% 
  filter(eyepatch==1) %>% 
  select(id,sex,eyepatch,parrots) %>% 
  arrange(desc(parrots)) %>% 
  head(10)

#zad117####

pir %>% 
  filter(favorite.pirate=="Blackbeard") %>% 
  select(id,sex,favorite.pirate,sword.type,tchests) %>% 
  arrange(desc(tchests)) %>% 
  head(10)

#zad118####

pir %>% 
  group_by(college) %>% 
  summarise(mean.tchests=mean(tchests),n())

#zad119####

pir %>% 
  group_by(college,sex) %>% 
  summarise(mean.tchests=mean(tchests),n())

#zad120####

pir %>% 
  group_by(sex) %>% 
  summarise(mean.age=mean(age),mean.beard=mean(beard.length),mean.grogg=mean(grogg))

#zad121####

pir %>% 
  group_by(sex) %>% 
  summarise(mean.height=mean(height))

#zad122####

pir %>% 
  group_by(headband) %>% 
  summarise(mean.tattoos=mean(tattoos))

#zad123####

pir %>% 
  group_by(sword.type) %>% 
  summarise(mean.tchests=mean(tchests))

#zad124####

pir %>% 
  group_by(headband) %>% 
  summarise(mean.sword.time=mean(sword.time))


#zad126####

vgs %>% 
  filter(platform=="PC") %>% 
  select(name,7:10) %>% 
  pivot_longer(cols = 2:5,names_to = "market",values_to = "sales") %>% 
  arrange(desc(sales)) %>% 
  head(10)

#zad127####

vgs %>% 
  filter(genre=="Platform",year==1983) %>% 
  select(name,7:9) %>% 
  pivot_longer(cols = 2:4,names_to = "sales",values_to = "value")

#zad128####

vgs %>% 
  filter(genre=="Platform",year==1983) %>% 
  select(name,7:9) %>% 
  pivot_longer(cols = 2:4,names_to = "sales",values_to = "value") %>% 
  separate(col=sales,into="market",sep="_") 

#zad129####

vgs %>% 
  filter(year%in%2005:2010) %>% 
  group_by(year,genre) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = year,values_from = n)

#zad130####

vgs %>% 
  filter(year%in%2005:2010&grepl("s",vgs$genre,ignore.case=T)) %>%  
  group_by(year,genre) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = genre,values_from = n)

#zad131####

vgs %>% 
  filter(year%in%2012:2016) %>% 
  group_by(genre,year) %>% 
  summarise(m=round(mean(global_sales),2)) %>% 
  pivot_wider(names_from = year,values_from = m) 

#zad132####

ufo <- read.csv(unzip("./data/ufo.zip", exdir = "./data"))
ufo$datetime <- mdy_hm(ufo$datetime)
ufo$date.posted <- mdy(ufo$date.posted)
ufo$country <- factor(ufo$country)
ufo$shape <- factor(ufo$shape)

ufo %>% 
  mutate(year=year(ufo$datetime)) %>% 
  filter(year%in%1941:1960) %>% 
  group_by(year) %>% 
  summarise(n=n()) 

#zad133####

ufo %>% 
  filter(year(datetime)%in%1994:2002&shape%in%c("disk","light")) %>% 
  group_by(year(datetime),shape) %>% 
  summarise(n=n())

#zad134####

ufo %>% 
  filter(year(datetime)%in%1994:2002&shape%in%c("disk","light")) %>% 
  group_by(year(datetime),shape) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = shape,values_from = n)

#zad135####

ufo %>% 
  filter(shape%in%c("disk","fireball","triangle")&state%in%c("fl","ny","tx")) %>% 
  group_by(shape,state) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = state,values_from = n)

#zad136####

ufo %>% 
  separate(col = datetime,into = c("date","time"),sep = " ") %>% 
  select(date,time,shape) %>% 
  head(20)

#zad137####

ufo %>% 
  mutate(hr=hour(datetime)) %>% 
  group_by(hr) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

#zad138####

ufo %>% 
  filter(year(datetime)>2000) %>% 
  group_by(shape) %>% 
  summarise(n=n()) %>% 
  filter(n>999) %>% 
  arrange(desc(n))

#zad139####

ufo %>% 
  mutate(century=ifelse(year(datetime)<2001,"XX w.","XXI w.")) %>% 
  group_by(century,shape) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  pivot_wider(names_from = century,values_from = n)

#zad140####

ufo %>% 
  filter(year(datetime)>1979) %>% 
  mutate("year"=year(datetime)) %>% 
  group_by(year) %>% 
  summarise(n=n()) %>% 
  plot(type="b",xlab="rok",ylab="liczba obserwacji ufo")


#zad141####

library(yarrr)
pir <- pirates
pir$sex <- factor(pir$sex)
pir$headband <- factor(pir$headband)
pir$college <- factor(pir$college)
pir$favorite.pirate <- factor(pir$favorite.pirate)
pir$sword.type <- factor(pir$sword.type)
pir$fav.pixar <- factor(pir$fav.pixar)

pir %>% 
  group_by(sword.type,sex) %>%
  summarise(mean.tchests=mean(tchests))

#zad142####

pir %>% 
  group_by(sword.type,sex) %>%
  summarise(mean.tchests=mean(tchests)) %>% 
  pivot_wider(names_from = sex,values_from = mean.tchests)

#zad143####

pir %>% 
  group_by(college,sex) %>%
  summarise(mean.age=mean(age)) %>% 
  pivot_wider(names_from = sex,values_from = mean.age)

#zad144####

pir %>% 
  group_by(college,sex) %>%
  summarise(n=n(),mean.age=mean(age),mean.tchests=mean(tchests))

#zad145####

pir %>% 
  group_by(college,sex) %>%
  summarise(n=n(),mean.age=mean(age),mean.tchests=mean(tchests)) %>% 
  pivot_longer(cols = 3:5,names_to = "name",values_to = "value")

#zad146####

pir %>% 
  group_by(favorite.pirate,sex) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = sex,values_from = n)

#zad147####

pir %>% 
  group_by(sex) %>% 
  mutate(n=n()) %>% 
  group_by(favorite.pirate,sex) %>% 
  summarise(n2=n()/mean(n)*100) %>% 
  pivot_wider(names_from = sex,values_from = n2)


#zad148####

pir %>% 
  group_by(fav.pixar,sex) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = sex,values_from = n,values_fill = F)

#zad149####

boxpir <- pir %>% 
  select(beard.length,grogg,parrots,tattoos) %>% 
  pivot_longer(cols = 1:4,names_to = "zmienna",values_to = "wartość" ) %>% 
  group_by(zmienna) 
boxplot(boxpir$wartość~boxpir$zmienna,xlab="zmienna",ylab="wartość")

library(yarrr)
pir <- pirates
pir$sex <- factor(pir$sex)
pir$eyepatch <- factor(pir$eyepatch)

#zad151####

#a
pir %>% 
  ggplot(aes(x=height,y=weight))+
  geom_point()

#b
pir %>% 
  ggplot(aes(x=height,y=weight))+
  geom_point(color="orange")

#c
pir %>% 
  ggplot(aes(x=height,y=weight,col=sex))+
  geom_point()

#d
pir %>% 
  ggplot(aes(x=height,y=weight,fill=sex))+
  geom_point(shape=21)

#zad152####

#a
pir %>% 
  filter(sex!="other") %>% 
  ggplot(aes(x=height,y=weight,col=sex,shape=college))+
  geom_point()+
  theme(legend.position = "top")

#b
pir %>% 
  filter(sword.type!="cutlass") %>% 
  ggplot(aes(x=weight,y=tchests,col=sword.type))+
  geom_point(shape=15,size=2.5)

#zad153####

#a
pir %>% 
  filter(favorite.pirate!="Jack Sparrow") %>% 
  ggplot(aes(x=age,y=parrots,col=sex))+
  geom_point()+
  facet_wrap(~favorite.pirate)+
  theme(legend.position = "top")

#b
pir %>% 
  ggplot(aes(x=tattoos,y=beard.length,col=sex))+
  geom_point()+
  facet_grid(cols=vars(college),rows = vars(headband))+
  theme(legend.position = "top")

#zad154####

#a
pir %>% 
  ggplot(aes(x=sword.type,y=sword.time))+
  geom_boxplot(fill="yellow")

#b

pir %>% 
  ggplot(aes(x=sex,y=beard.length,fill=sex))+
  geom_violin()

#zad155####

#a
pir %>% 
  ggplot(aes(x=college,y=tchests))+
  geom_boxplot()

#b
pir %>% 
  ggplot(aes(x=college,y=tchests,col=sex))+
  geom_boxplot()

#zad156####

pir %>% 
  ggplot(aes(x=sex,y=grogg))+
  geom_violin(aes(fill=sex),show.legend = F)+
  geom_boxplot()

#zad157####

#a
pir %>% 
  ggplot(aes(x=sex))+
  geom_bar()

#b
pir %>% 
  ggplot(aes(x=sword.type))+
  geom_bar(fill="white",col="black")

#c
pir %>% 
  ggplot(aes(x=favorite.pirate,fill=favorite.pirate))+
  geom_bar(col="black",show.legend = F)

#zad158####

#a
pir %>% 
  ggplot(aes(x=college,fill=sex))+
  geom_bar(col="black")

#b
pir %>% 
  ggplot(aes(x=college,fill=sex))+
  geom_bar(col="black",position = "dodge")

#c
pir %>% 
  ggplot(aes(x=college,fill=sex))+
  geom_bar(col="black",position = "fill",width = 0.5)

#zad159####

pir %>% 
  group_by(sword.type) %>% 
  summarise(mean.parrots=mean(parrots)) %>% 
  ggplot(aes(x=sword.type,y=mean.parrots))+
  geom_col(fill="white",col="black",width = 0.4)

#zad160####

pir %>% 
  group_by(favorite.pirate) %>% 
  summarise(mean.age=mean(age)) %>% 
  ggplot(aes(x=mean.age,y=favorite.pirate,fill=favorite.pirate)) +
  geom_col(col="black",width = 0.7)

#zad161####

#a
pir %>% 
  ggplot(aes(x=beard.length))+
  geom_histogram()

#b
pir %>% 
  ggplot(aes(x=beard.length))+
  geom_histogram(fill="orange",col="white",boundary=0,binwidth = 2)

#c
pir %>% 
  filter(sex=="male") %>% 
  ggplot(aes(x=beard.length))+
  geom_histogram(fill="orange",col="white",boundary=0,binwidth = 2)

#zad162####

pir %>% 
  ggplot(aes(x=grogg))+
  geom_histogram(fill="white",col="black",boundary=0,binwidth = 1)

#zad163####

#a
pir %>% 
  ggplot(aes(x=grogg)) +
  geom_density()

#b
pir %>% 
  ggplot(aes(x=grogg,fill=sex))+
  geom_density(alpha=0.5)

#zad164####

pir %>% 
  ggplot(aes(x=tchests))+
  geom_histogram(aes(y=after_stat(density)),boundary=0)+
  facet_grid(sword.type~.)

#zad165####

pir %>% 
  ggplot(aes(x=age))+
  geom_histogram(aes(y=after_stat(density)),boundary=10,binwidth = 2,col="black",fill="white")+
  facet_grid(sex~.)+
  geom_density(col="red")

#zad166####

pir %>% 
  ggplot(aes(x=headband,fill=sex))+
  geom_bar(position = "dodge")+
  ggtitle("Piraci z brodą i bez")

#zad167####

pir %>% 
  filter(sex=="female") %>% 
  ggplot(aes(x=height,y=weight,size=tchests,fill=eyepatch))+
  geom_point(pch=21)+
  xlab("wzrost")+
  ylab("waga")+
  theme_bw()+
  ggtitle("Wzrost,waga i liczba skrzynek piratów płci żeńskiej")

#zad168####

pir %>% 
  ggplot(aes(x=sword.type,y=tattoos,fill=sword.type))+
  geom_boxplot()+
  scale_fill_manual(values=c("yellow","salmon","grey","lightblue"))

#zad169####

pir %>% 
  filter(sex!="other") %>% 
  ggplot(aes(x=grogg,fill=sex)) +
  geom_density(alpha=0.5) +
  facet_grid(eyepatch~.,labeller="label_both")+
  scale_fill_manual(values = c("pink","lightgreen"))+
  theme(legend.position = "bottom")+
  xlab("liczba jednostek wypijanego grogu")+
  ggtitle("Rozkład liczby jednostek wypijanego przez piratów grogu")

#zad171####

mov <- read.csv2(file = "./data/movies.csv")

mov$year <- factor(mov$year)
mov$genre <- factor(mov$genre)

mov %>% 
  group_by(year,genre) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = genre,values_from = n)

#zad172####

mov %>% 
  group_by(genre) %>% 
  summarise(median=median(rating)) %>% 
  ggplot(aes(x=genre,y=median)) +
  geom_col(fill="orange",col="black",width = 0.5)+
  theme_bw()

#zad173####

mov %>% 
  group_by(genre) %>% 
  ggplot(aes(x=genre,y=rating,fill=genre))+
  geom_violin(alpha=0.5)+
  theme_bw()

#zad174####

mov %>% 
  group_by(genre) %>% 
  ggplot(aes(x=rating,fill=genre))+
  geom_density(alpha=0.5)+
  theme_bw()

#zad175####

mov %>% 
  filter(year==2014) %>% 
  ggplot(aes(x=budget,y=gross,col=genre,shape=genre))+
  geom_point()

#zad176####

mov %>% 
  filter(duration<201) %>% 
  ggplot(aes(duration))+
  geom_histogram(boundary=0,binwidth = 5,col="white")+
  facet_grid(genre~.)+
  theme_bw()

#zad177####

mov %>% 
  ggplot(aes(y=year,fill=genre))+
  geom_bar(position = "fill",col="black")+
  theme_bw()

#zad178####

mov %>% 
  group_by(genre) %>% 
  ggplot(aes(x=rating,y=gross,fill=genre))+
  geom_point(col="black",pch=21)+
  facet_wrap(~genre)+
  theme_bw()

#zad179####

mov %>% 
  filter(genre%in%c("Action","Adventure")) %>% 
  group_by(year,genre) %>% 
  summarise(mean.budget=mean(budget)) %>% 
  ggplot(aes(x=year,y=mean.budget,fill=genre))+
  geom_col(position = "dodge",col="black",width = 0.6)+
  theme_bw()

#zad180####

mov %>% 
  group_by(year,genre) %>% 
  summarise(mean.budget=mean(budget)/1E6) %>% 
  ggplot(aes(x=year,y=mean.budget,col=genre))+
  geom_line(aes(group=genre),show.legend = T)+
  geom_point()+
  theme_bw()+
  theme(legend.position = "top")+
  ylab("Średni budżet filmów (mln)")+
  xlab("")

#zad181####

dm <- diamonds

dm %>% 
  ggplot(aes(x=carat,y=price))+
  geom_point(alpha=0.3)+
  theme_bw()

#zad182####

dm %>% 
  ggplot(aes(x=carat,y=price,col=clarity))+
  geom_point(alpha=0.4)+
  theme_bw()

#zad183####

dm %>% 
  ggplot(aes(x=clarity,fill=cut))+
  geom_bar(position = "dodge",col="black")+
  theme_bw()+
  theme(legend.position="top")

#zad184####

dm %>% 
  group_by(color) %>% 
  summarise(n=n(),mean.carat=mean(carat),mean.price=mean(price),price.carat=mean.price/mean.carat)

#zad185####

dm %>% 
  group_by(color,cut) %>% 
  summarise(mean.price=mean(price)) %>% 
  pivot_wider(names_from = cut,values_from = mean.price)

#zad186####

dm %>% 
  group_by(cut,color) %>% 
  ggplot(aes(x=cut,y=carat,fill=color))+
  geom_boxplot(col="black",outlier.shape = NA)+
  ylim(c(0,3))+
  theme_bw()

#zad187####

dm %>% 
  group_by(cut) %>% 
  summarise(mean.carat=mean(carat),mean.price=mean(price)) %>% 
  ggplot(aes(x=mean.carat,y=mean.price,fill=cut))+
  geom_point(pch=21,col="black",size=7)+
  theme_bw()

#zad188####

dm %>% 
  ggplot(aes(x=table,y=depth,col=cut))+
  geom_point(alpha=1)+
  theme_bw()

#zad189####

dm %>% 
  group_by(cut) %>% 
  mutate(cutted=cut(price,breaks = c(-Inf,5000,10000,15000,Inf),labels=c("<5k","[5k;10k)","[10k;15k)",">=15k"))) %>% 
  ggplot(aes(x=cutted,fill=cut))+
  geom_bar(position = "fill",col="black")+
  theme_bw()+
  ggtitle("Rozkład jakości obróbki w zależności od ceny")+
  xlab("")

#zad190####

dm %>% 
  group_by(clarity) %>% 
  ggplot(aes(x=price))+
  geom_histogram(aes(y=after_stat(density)),col="black",fill="grey")+
  facet_wrap(~clarity)+
  geom_density(col="red")+
  theme_minimal()

#zad191####

my_root <- function(x){
  x^(1/3)
}

my_root(8)

my_root(125)

my_root(200)

#zad192####

my_root2 <- function(x,k=3){
  x^(1/k)
}

my_root2(8)

my_root2(8, k = 4)

my_root2(243, 5)

#zad193####

my_skewness <- function(x){
  result <- mean((x-mean(x))^3)/sd(x)^3
  return(result)
}

my_skewness(1:10)

my_skewness(c(1, 2, 2, 5, 5, 5, 5))

set.seed(10)
my_skewness(rpois(1000, 5))

my_skewness(c(7, 7, 7))

#zad194####

my_skewness2 <- function(x){
  result <- mean((x-mean(x))^3)/sd(x)^3
  if(result==0){
    print("Standard deviation of liczba
is equal to 0")
  } else{
    return(result)}
}

my_skewness2(1:10)

my_skewness2(c(1, 2, 2, 5, 5, 5, 5))

set.seed(10)
my_skewness2(rpois(1000, 5))

my_skewness2(c(7, 7, 7))

#zad195####

numbers <- function(x){
  for(i in 1:x){
    print(c(1:i))}
}


numbers(4)

numbers(9)

numbers(20)

#zad196####

strange <- function(n,k){
  for(i in 1:n){
    if(i%%k==0){
      print("Weeez!")
    }else{
      print(i)
    }
    
  }
}

strange(10,4)

strange(15,3)

#zad197####

modulo <- function(x,d){
  if(d>0){
    res <- x %% d 
    return(res)
  }else{
    if(d==0){
      print("Nie dziel cholero przez 0!")
    }else{
      print("ta wersja funkcji działa tylko dla liczb dodatnich, dla liczb ujemnych wykup dostęp do funkcji premium")
    }
  }
}

modulo(7,-3)

#zad198####

minimax <- function(x,n=3){
  if(length(x)<n){
    print("Podaj więcej liczb")
  }else{
    print(list(
      minimalne=head(sort(x),n),
      maksymalne=head(sort(x,decreasing = T),n)
    ))
  }
}

minimax(1:100)

minimax(1:100,6)

minimax(c(2, 5, 1, 7, 3, 9), 2)

minimax(c(2, 5, 1, 7, 3, 9), 7)

#zad199####

dzieli_sie <- function(x,d=2){
  tak <- "dzieli się przez"
  nie <- "nie dzieli się przez"
  for (i in x) {
    res <- i%%d
    if(res==0){
      cat(c(i,tak,d),fill = T)
    }else{
      cat(c(i,nie,d),fill = T)
    }
  }
}

dzieli_sie(x = c(2, 5, 6))

dzieli_sie(x = c(245, 45, -17, 23561), d = 3)

#zad200####

dziwne_mnozenie <- function(x,m=3,n=15){
  nie <- "nie jest większe niż"
  tak <- "jest większe niż"
  for (i in x) {
    res <- i*m
    if(res>n){
      cat(c(i,"*",m,tak,n),fill = T)
    }else{
      cat(c(i,"*",m,nie,n),fill = T)
    }
  }
}

dziwne_mnozenie(c(2, 5, 1, 6))

dziwne_mnozenie(c(2, 5, 1, 6), m = 25, n = 100)


