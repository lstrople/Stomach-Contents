library(ggplot2)

Index.df <- read.csv("C:/Users/lstrople/OneDrive - Norwegian University of Life Sciences/Mathias & Leah/Excel files/Values_from_RInSP.csv")

#E index
p0<- ggplot(Index.df, aes(x=Lake, y=E_value,  fill=Season))
p0 <- p0 + geom_bar(stat="identity", position=position_dodge())
p0 <- p0 + theme_bw(base_size = 9)
p0 <- p0 + scale_fill_manual(values=c("#cfe0c3", "#005478"))
p0 <- p0 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) #change inner plot size
p0 <- p0 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent"),plot.background = element_rect(fill = "transparent", color = NA), axis.text=element_text(size=9),legend.text = element_text(color = "black", size = 7),legend.title = element_text(size = 9), legend.direction="horizontal", legend.position="bottom")
p0 <- p0 + labs (x="Lake", y="E-index", fill="Season")
p0


#############################################################
#you can use something like this to make the stacked bar plot
##############################################################
p0<- ggplot(safo, aes(x=, y=,  fill=))
p0 <- p0 + geom_bar(position="fill", stat="identity")
p0 <- p0 + theme_bw(base_size = 9)
p0 <- p0 + scale_fill_manual(values=c("#cfe0c3", "#005478"))
p0 <- p0 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) #change inner plot size
p0 <- p0 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent"),plot.background = element_rect(fill = "transparent", color = NA), axis.text=element_text(size=9),legend.text = element_text(color = "black", size = 7),legend.title = element_text(size = 9), legend.direction="horizontal", legend.position="bottom")
p0 <- p0 + labs (x="", y="", fill="")
p0