library(bigchess)
library(tidyverse)
library(ff)
library(transformr)
library(gganimate)

# How to use:
### In the LOADING section change the filepaths in stream_ndata, save_ndata, and load_ndata
### if you have streamed and saved already load_ndata() **should be much faster than streaming
### if you have not streamed and saved already stream_ndata() and save_ndata() **only need to do this once
### note: if you are using new lichess data with rapid and ultrabullet this will not include all games

# helper functions ####

## LOADING
stream_ndata <- function() {
  setwd("~/")
  con <- file("lichess-db-filename.pgn","rb",encoding = "latin1") # using a stream b/c data is so big
  ndata <- read.pgn.ff(con=con,batch.size=10^6, stat.moves = F, extract.moves = F, add.tags = c("WhiteElo","BlackElo","ECO","Opening"))
  ndata <<- as_tibble(data.frame(ndata)) #superassignment
  nndata <<- ndata #just in case we mess up
}

save_ndata <- function() {
  save(ndata, file = "~/filename.RData")
}

load_ndata <- function() {
  load(file = "~/filename.RData")
  ndata <<- as_tibble(data.frame(ndata)) #superassignment
  nndata <<- ndata #just in case we mess up
}

## AREA GRAPH
area_df <- function(data, group, colnames, x, x_type, moving_avg_size) {
  oTable <- list()
  pTable <- data.frame(NA,NA,NA)
  colnames(pTable) <- colnames
  tick <- 1
  # Closure would be a nice alternative to this
  if(x_type == "Elo") { 
    selection <- function(data, upper, lower){
      return(table(data[data$MeanElo >= lower & data$MeanElo <= upper,]$ECO))
    }
  } else if(x_type == "Moves") {
    selection <- function(data, upper, lower){
      return(table(data[data$NMoves >= lower & data$NMoves <= upper,]$ECO))
    }
  } else if(x_type == "Diff") {
    selection <- function(data, upper, lower){
      return(table(data[data$EloDiff >= lower & data$EloDiff <= upper,]$ECO))
    }
  } else {
    stop("invalid x_type")
  }
  
  for(n in x) { # slow!
    lower <- n - moving_avg_size/2
    upper <- n + moving_avg_size/2
    oTable[[n]] <- selection(data, upper, lower)
    sum <- sum(oTable[[n]])
    topSum <- 0
    for(eco in group) {
      eco_sum <- sum(oTable[[n]][names(oTable[[n]])%in%eco])
      percentage <- (eco_sum/sum)*100
      pTable[tick,] <- list(n,eco[1],percentage)
      tick <- tick + 1
      topSum <- topSum + eco_sum
    }
    rest <- ((sum-topSum)/sum)*100
    pTable[tick,] <- list(n,"OTHER",rest)
    tick <- tick + 1
  }
  return(pTable)
}

## SMOOTHING
smooth_table <- function(table, span) {
  tick <- 0
  for(eco in levels(as.factor(table$ECO))) {
    tick <- tick + 1
    data <- filter(table, table$ECO==eco)
    data$Percentage <- predict(loess(data$Percentage ~ data$Elo, span = span))
    if(tick == 1) {
      smoothed <- data
    } else{
      smoothed <- rbind(smoothed, data)
    } 
  }
  return(smoothed)
}

smooth_lines <- function(table, span) { #for making lines where the divisions are instead of regions
  tick <- 0
  for(eco in rev(levels(as.factor(table$ECO)))) {
    tick <- tick + 1
    data <- filter(table, table$ECO==eco)
    data$Percentage <- predict(loess(data$Percentage ~ data$Elo, span = span))
    if(tick == 1) {
      smoothed <- data
    } else {
      data$Percentage <- tail(smoothed$Percentage,length(data$Percentage)) + data$Percentage
      smoothed <- rbind(smoothed, data)
    }
  }
  smoothed$Percentage <- smoothed$Percentage/100
  return(smoothed)
}

## ECO
make_eco <- function(vector, char){
  unlist(lapply(vector,function(x) {
    if(x<10) paste0(char,"0",x)
    else paste0(char,x)
  }))
}

# getting the data ####

load_ndata()

# removing 0 move games ####

ndata <- ndata %>% filter(!NMoves==0)

# indices ####
draw_index <- ndata$Result=="1/2-1/2"
notdraw_index <- ndata$Result!="1/2-1/2"
bullet_index <- str_detect(ndata$Event, ".*Bullet.*")
blitz_index <- str_detect(ndata$Event, ".*Blitz.*")
classical_index <- str_detect(ndata$Event, ".*Classical.*")
correspondence_index <- str_detect(ndata$Event, ".*Correspondence.*") # I love you, R

# altering the data ####
ndata$MeanElo <- (ndata$BlackElo+ndata$WhiteElo)/2 # my professor would tell me to use mutate here! Base R is fine...
ndata$EloDiff <- abs(ndata$BlackElo - ndata$WhiteElo)

# histograms ####
if(FALSE) {
## length of games between draws
hist(filter(ndata, notdraw_index)$NMoves, col="lightblue", freq = F, breaks = 77, density = 80)
hist(filter(ndata, draw_index)$NMoves, col="orange", freq = F, add = T, breaks = 60, density = 80)

## length of games between game modes
hist(filter(ndata, bullet_index)$NMoves, col="lightblue", freq = F, breaks = 77, density = 80)
hist(filter(ndata, blitz_index)$NMoves, col="red", freq = F, breaks = 77, density = 80, add = T)
hist(filter(ndata, classical_index)$NMoves, col="green", freq = F, breaks = 77, density = 80, add=T)
hist(filter(ndata, correspondence_index)$NMoves, col="purple", freq = F, breaks = 77, density = 80, add=T)
}

# calling area graph ####
DIFFseq <- 1:600 #supposed range of difference between opponents
MOVESseq <- 1:90 #supposed range of moves per game
ELOseq <- 1000:2200 #hand picked range. See README

# Loading ECOs
## general ECOs - these cover all ECOs
eco0 <- make_eco(10:39,"A") #c4
eco1 <- make_eco(c(40:44,80:99),"A") #d4 other
eco2 <- c(make_eco(45:79,"A"), make_eco(70:99,"D"), make_eco(0:99,"E"))#d4 Nf6
eco3 <- make_eco(0:69,"D") #d4 d5
eco4 <- make_eco(c(0,2:6),"B") #e4 other
eco5 <- make_eco(1,"B") #e4 d5
eco6 <- make_eco(7:9,"B") #e4 d6
eco7 <- make_eco(10:19,"B") #e4 c6
eco8 <- make_eco(20:99,"B") #e4 c5
eco9 <- make_eco(0:19,"C") #e4 e6
eco10 <- make_eco(20:99,"C") #e4 e5
eco11 <- make_eco(4:9,"A") #Nf4
eco12 <- make_eco(0:3,"A") #other

# You have to load your specified ECOs first!
group <- list(eco0, eco1, eco2, eco3, eco4, eco5, eco6, eco7, eco8, eco9, eco10, eco11, eco12)

colnames <- c("Elo", "ECO", "Percentage")

# Arguments: data, group, colnames, x, x_type, moving_avg_size
pTable2_bullet <- area_df(filter(ndata,bullet_index), group, colnames, ELOseq, "Elo", 20)
pTable2_blitz <- area_df(filter(ndata,blitz_index), group, colnames, ELOseq, "Elo", 20)
pTable2_classical <- area_df(filter(ndata,classical_index), group, colnames, ELOseq, "Elo", 20)
pTable2_correspondence <- area_df(filter(ndata,correspondence_index), group, colnames, ELOseq, "Elo", 20)

# smoothing data ####

new_pTable2_bullet <- smooth_table(pTable2_bullet, .2)

new_pTable2_blitz <- smooth_table(pTable2_blitz, .2)

new_pTable2_classical <- smooth_table(pTable2_classical, .2)

new_pTable2_correspondence <- smooth_table(pTable2_correspondence, .2)

# renaming ECOs ####

names(new_pTable2_bullet)[names(new_pTable2_bullet)=="ECO"]  <- "Opening"
new_pTable2_bullet <- new_pTable2_bullet %>% filter(Opening!="OTHER") #if you have correctly chosen ECOs that cover all possible games this is unnecessary
new_pTable2_bullet$Opening <- as.factor(new_pTable2_bullet$Opening)
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="A00"] <- "1. other (non-mainline)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="C20"] <- "1. e4 e5 (King's pawn game)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="D00"] <- "1. d4 d5 (Queen's pawn game)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="A04"] <- "1. Nf3 (Reti)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="A10"] <- "1. c4 (English)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="A40"] <- "1. d4 other (Queen's pawn non-mainline)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="A45"] <- "1. d4 Nf6 (Indian)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="B00"] <- "1. e4 other (King's pawn non-mainline)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="B01"] <- "1. e4 d5 (Scandinavian)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="B07"] <- "1. e4 d6 (Pirc)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="B20"] <- "1. e4 c5 (Sicilian)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="C00"] <- "1. e4 e6 (French)"
levels(new_pTable2_bullet$Opening)[levels(new_pTable2_bullet$Opening)=="B10"] <- "1. e4 c6 (Caro-Kann)"
#reordering the levels so the stack is in the right order
new_pTable2_bullet$Opening <- factor(new_pTable2_bullet$Opening, levels=levels(new_pTable2_bullet$Opening)[c(1,12,13,10,11,2:9)])


names(new_pTable2_blitz)[names(new_pTable2_blitz)=="ECO"]  <- "Opening"
new_pTable2_blitz <- new_pTable2_blitz %>% filter(Opening!="OTHER")
new_pTable2_blitz$Opening <- as.factor(new_pTable2_blitz$Opening)
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="A00"] <- "1. other (non-mainline)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="A04"] <- "1. Nf3 (Reti)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="A10"] <- "1. c4 (English)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="A40"] <- "1. d4 other (Queen's pawn non-mainline)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="A45"] <- "1. d4 Nf6 (Indian)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="B00"] <- "1. e4 other (King's pawn non-mainline)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="B01"] <- "1. e4 d5 (Scandinavian)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="B07"] <- "1. e4 d6 (Pirc)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="B20"] <- "1. e4 c5 (Sicilian)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="C00"] <- "1. e4 e6 (French)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="B10"] <- "1. e4 c6 (Caro-Kann)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="C20"] <- "1. e4 e5 (King's pawn game)"
levels(new_pTable2_blitz$Opening)[levels(new_pTable2_blitz$Opening)=="D00"] <- "1. d4 d5 (Queen's pawn game)"
#reordering the levels so the stack is in the right order
new_pTable2_blitz$Opening <- factor(new_pTable2_blitz$Opening, levels=levels(new_pTable2_blitz$Opening)[c(1,12,13,10,11,2:9)])

names(new_pTable2_classical)[names(new_pTable2_classical)=="ECO"]  <- "Opening"
new_pTable2_classical <- new_pTable2_classical %>% filter(Opening!="OTHER")
new_pTable2_classical$Opening <- as.factor(new_pTable2_classical$Opening)
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="A00"] <- "1. other (non-mainline)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="A04"] <- "1. Nf3 (Reti)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="A10"] <- "1. c4 (English)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="A40"] <- "1. d4 other (Queen's pawn non-mainline)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="A45"] <- "1. d4 Nf6 (Indian)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="B00"] <- "1. e4 other (King's pawn non-mainline)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="B01"] <- "1. e4 d5 (Scandinavian)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="B07"] <- "1. e4 d6 (Pirc)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="B20"] <- "1. e4 c5 (Sicilian)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="C00"] <- "1. e4 e6 (French)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="B10"] <- "1. e4 c6 (Caro-Kann)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="C20"] <- "1. e4 e5 (King's pawn game)"
levels(new_pTable2_classical$Opening)[levels(new_pTable2_classical$Opening)=="D00"] <- "1. d4 d5 (Queen's pawn game)"
#reordering the levels so the stack is in the right order
new_pTable2_classical$Opening <- factor(new_pTable2_classical$Opening, levels=levels(new_pTable2_classical$Opening)[c(1,12,13,10,11,2:9)])






# plotting ####

### BULLET ###
p_bullet <- ggplot(data=new_pTable2_bullet, aes(x=Elo,y=Percentage,fill=Opening)) +  #This is so much harder than it has to be
  geom_area(position="fill", alpha = 1, size=.5, color = "black") +
  ylab("Proportion of Games") + xlab("ELO") + ggtitle("Distribution of openings by ELO for bullet games")+
  theme(
        axis.ticks.length=unit(.25, "cm"),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(color="black", size=16, face="bold.italic"),
        legend.key.width = unit(2, "line"),
        legend.key.height = unit(2, "line"),
        legend.text = element_text(size=12),
        legend.title = element_text(color="black", size=14, face="bold.italic"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold")
        ) +
  geom_segment(mapping = aes(x = 1000, y = 0, xend = 1000, yend = 1), size=1) +
  geom_segment(mapping = aes(x = 2200, y = 0, xend = 2200, yend = 1), size=1) +
  geom_segment(mapping = aes(x = 1000, y = 0, xend = 2200, yend = 0), size=1) +
  geom_segment(mapping = aes(x = 1000, y = 1, xend = 2200, yend = 1), size=1) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = seq(from = 1000, to = 2200, by=200)) +
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))

### BLITZ ###
p_blitz <- ggplot(data=new_pTable2_blitz, aes(x=Elo,y=Percentage,fill=Opening)) +  
  geom_area(position="fill", alpha = 1, size = .5, color= "black") +
  ylab("Proportion of Games") + xlab("ELO") + ggtitle("Distribution of openings by ELO for blitz games")+
  theme(
    axis.ticks.length=unit(.25, "cm"),
    axis.text = element_text(colour="black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(color="black", size=16, face="bold.italic"),
    legend.key.width = unit(2, "line"),
    legend.key.height = unit(2, "line"),
    legend.text = element_text(size=12),
    legend.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) +
  geom_segment(mapping = aes(x = 1000, y = 0, xend = 1000, yend = 1), size=1) +
  geom_segment(mapping = aes(x = 2200, y = 0, xend = 2200, yend = 1), size=1) +
  geom_segment(mapping = aes(x = 1000, y = 0, xend = 2200, yend = 0), size=1) +
  geom_segment(mapping = aes(x = 1000, y = 1, xend = 2200, yend = 1), size=1) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = seq(from = 1000, to = 2200, by=200)) +
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))

### CLASSICAL ###
p_classical <- ggplot(data=new_pTable2_classical, aes(x=Elo,y=Percentage,fill=Opening)) +  
  geom_area(position="fill", alpha = 1, size = .5, color = "black") +
  ylab("Proportion of Games") + xlab("ELO") + ggtitle("Distribution of openings by ELO for classical games")+
  theme(
    axis.ticks.length=unit(.25, "cm"),
    axis.text = element_text(colour="black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(color="black", size=16, face="bold.italic"),
    legend.key.width = unit(2, "line"),
    legend.key.height = unit(2, "line"),
    legend.text = element_text(size=12),
    legend.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) +
  geom_segment(mapping = aes(x = 1000, y = 0, xend = 1000, yend = 1), size=1) +
  geom_segment(mapping = aes(x = 2200, y = 0, xend = 2200, yend = 1), size=1) +
  geom_segment(mapping = aes(x = 1000, y = 0, xend = 2200, yend = 0), size=1) +
  geom_segment(mapping = aes(x = 1000, y = 1, xend = 2200, yend = 1), size=1) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = seq(from = 1000, to = 2200, by=200)) +
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))

# Animation between graphs ####

### I kept having this problem where the legend keys were not spaced far enough
### apart vertically. I tried a million things to try and solve this and found
### this solution of modifying the ggplot2 GeomArea function. It really sucks
### that I have to do this...
### ref1: https://github.com/tidyverse/ggplot2/issues/2844 
### ref2: @Tung https://stackoverflow.com/questions/11366964/is-there-a-way-to-change-the-spacing-between-legend-items-in-ggplot2
### The solution is below. This modifies the geom_area function for the rest
### of the R session so make sure you are done doing the static plotting 
### above before running this.
### Solution:
# function to increase vertical spacing between legend keys
# @clauswilke
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

# register new key drawing function, 
# the effect is global & persistent throughout the R session
GeomArea$draw_key = draw_key_polygon3

### Now that we have changed the ggplot2 function we can continue

#format into one data frame
new_pTable2_blitz$type <- "blitz"
new_pTable2_bullet$type <- "bullet"
new_pTable2_classical$type <- "classical"
long_pTable <- bind_rows(new_pTable2_bullet, new_pTable2_blitz, new_pTable2_classical)
long_pTable$type <- factor(long_pTable$type, levels=c("bullet","blitz","classical"))

#plotting
p_long <- ggplot(data=long_pTable, aes(x=Elo,y=Percentage,fill=Opening)) +  #This is so much harder than it has to be
  geom_area(position="fill", alpha = 1, size=.5, color = "black") +
  ylab("Proportion of Games") + xlab("ELO") +
  theme(
    axis.ticks.length=unit(.5, "cm"),
    axis.text = element_text(colour="black", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(color="black", size=22, face="bold.italic"),
    legend.key.width = unit(4, "line"),
    legend.key.height = unit(4, "line"),
    legend.key = element_rect(color = NA, fill = NA),
    legend.text = element_text(size=18),
    legend.title = element_text(color="black", size=20, face="bold.italic"),
    axis.title.x = element_text(color="black", size=18, face="bold"),
    axis.title.y = element_text(color="black", size=18, face="bold")
  ) +
  geom_segment(mapping = aes(x = 1000, y = 0, xend = 1000, yend = 1), size=1) +
  geom_segment(mapping = aes(x = 2200, y = 0, xend = 2200, yend = 1), size=1) +
  geom_segment(mapping = aes(x = 1000, y = 0, xend = 2200, yend = 0), size=1) +
  geom_segment(mapping = aes(x = 1000, y = 1, xend = 2200, yend = 1), size=1) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(breaks = seq(from = 1000, to = 2200, by=200)) +
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))

anim <- p_long +
  transition_states(type,
                    transition_length = 2,
                    state_length = 6) +
  ease_aes('cubic-in-out') +
  ggtitle("Distribution of openings by ELO for {closest_state} games") #cool

animate(
  anim,
  nframes = 200,
  fps = 20,
  width = 1500,
  height = 1000,
  renderer = gifski_renderer()
)


# custom ECO codes ####

if(FALSE) {
## Specific ECOs - these do not cover all games and should be modified
  eco0 <- make_eco(56:79,"A")
  eco1 <- make_eco(0:59,"E")
  eco2 <- c(make_eco(60:99,"E"),make_eco(70:99,"D"))
  eco3 <- make_eco(47:49,"A")
  eco4 <- make_eco(6:69,"D")
  eco5 <- make_eco(2:4,"D")
  eco6 <- make_eco(60:99,"C")
  eco7 <- make_eco(50:59,"C")
  eco8 <- make_eco(42:43,"C")
  eco9 <- make_eco(30:39,"C")
  eco10 <- make_eco(25:29,"C")
  eco11 <- make_eco(0:19,"C")
  eco12 <- make_eco(20:99,"B")
  eco13 <- make_eco(10:19,"B")
  eco14 <- make_eco(7:9,"B")
  eco15 <- make_eco(1,"B")
  eco16 <- make_eco(80:99,"A")
  eco17 <- make_eco(10:39,"A")
  eco18 <- make_eco(4:9,"A")
}
### Some ECOs for common openings
#'  
#'1 d4 Nf6 c4 e6                                      E0-59
#'2 d4 Nf6 c4 g6                                      E60-99 D70-99
#'3 d4 Nf6 Nf3                                        A47-A49
#'4 d4 d5 c4                                          D06 - D69
#'5 d4 d5 Nf3                                        D02-04
#'6 e4 e5 2. Nf3 Nc6 3. Bb5 (Ruy)                   C60-99
#'7 e4 e5 2. Nf3 Nc6 3. Bc4                       C50-59
#'8 petrov                                           C42-43
#'9 kings gambit                                     C30-39
#'10 Vienna Game                                      C25-C29
#'11 French                                            C00-19
#'12 Sicilian                                         B20-99
#'13 Caro-Kann                                       B10-19
#'14 Pirc                                             B07-09
#'15 Scand                                            B01
#'16 Dutch                                             A80-99
#'17 1. d4 Nf6 2. c4 c5 (benoni)                    A56-79
#'18 1 c4                                              A10-39
#'19 Reti                                              A04-A09


