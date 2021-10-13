install.packages("dplyr")
library("dplyr")

cluster <- function(ID1, ID2) {x <- data.frame(ID1, ID2) 
mini <- apply(x,1,min)
maxi <- apply(x,1,max)
x2 <- unique(data.frame(mini,maxi))
x3 <- unique(data.frame(maxi,mini))
names(x3) <- c("mini","maxi")
x4 <- rbind(x2,x3)
names(x4) <- c("ID1","ID2")
x5 <- x4
names(x5) <- c("ID1","ID3")
x6 <- inner_join(x4,x5,by = "ID1")
x7 <- x6[x6$ID2 != x6$ID3,]
x8 <- x4
names(x8) <- c("ID2","ID3")
x8$mer <- 1
x9 <- left_join(x7, x8, by = c("ID2","ID3"))
x9[is.na(x9$mer),"mer"] = 0
x10 <- tapply(x9$mer,x9$ID1,sum)
x11 <- tapply(x9$mer,x9$ID1,length)
x12 <- data.frame(x10/2,x11/2)
names(x12) <- c("exists","possible")
x12$cluster <- x12$exist/x12$possibl
print(x12$cluster)
}

setwd("/groups/yotam/dwdetzi")
load("network.RData")

cluster_g <- cluster(g$ID1, g$ID2)

save.image("cluster.RData")