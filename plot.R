
define.curves <- function(mu,sd,N, captions, linetype,color.index) {
  result <- data.frame(mu = mu, sd = sd, n = N, caption = captions, lt = linetype, ci = color.index)
  
  result
}

cb.palette <- function(color.index) {
  #This palette came from the following url http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
  this.cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  this.cbPalette[((color.index-1) %% length(this.cbPalette))+1]
}

pdf.plot <- function(curves,interval,sample.idx,expected.idx) {
  my.sd.interval <- 4
  my.point.count <- 200
  
  x.min <- min(curves$mu - my.sd.interval*curves$sd)
  x.max <- max(curves$mu + my.sd.interval*curves$sd)
  
  x <- seq(x.min,x.max,length.out = my.point.count)
  
  plot.data <- NULL
  ID.names <- NULL
  for( i in 1:nrow(curves)) {
    cdata <-data.frame(x,
                       curves$n[i] * dnorm(x, mean = curves$mu[i],sd = curves$sd[i]),
                       paste("Line",i,sep=""),
                       Hcondition = "ignore"
                      ) 
    if (i == sample.idx) {
      cdata$Hcondition = "retain"
      for (j in 1:length(interval)) {
        if (interval[j] < curves$mu[expected.idx]) {
          cdata$Hcondition[cdata$x < interval[j]] = "reject"
        } else {
          cdata$Hcondition[cdata$x > interval[j]] = "reject"
        }
      }
    } else {
      cdata$Hcondition = "ignore"
    }
    plot.data <- rbind(plot.data,cdata )
    
    ID.names <- c(ID.names,paste("Line",i,sep=""))
  }
  names(plot.data) <- c("x","y","ID", "Hcondition")
  
  linetypelegend <- as.vector(curves$lt)
  names(linetypelegend) <- ID.names
  
  colortypelegend <- cb.palette(curves$ci)
  names(colortypelegend) <- ID.names
  
  fillcolorlegend <- cb.palette(c(4,5))
  names(fillcolorlegend) <- c("retain","reject")
  
  captions <-as.character( curves$caption)
  names(captions) <- ID.names
  
  fill.captions <- c("Retain H0","Reject H0")
  
  reject.fill.data <- subset(plot.data,Hcondition == "reject") 
  retain.fill.data <- subset(plot.data,Hcondition == "retain")
  
  my.plot <- ggplot(data = plot.data,aes(x = x,y = y,group = ID)) 
  
  my.plot <- my.plot + geom_ribbon(data = reject.fill.data, mapping=aes(ymin = 0, ymax = y),fill = cb.palette(5))
  
  my.plot <- my.plot + geom_ribbon(data = retain.fill.data, mapping=aes(ymin = 0, ymax = y),fill = cb.palette(4))
  
  
  my.plot <- my.plot + geom_line(data = plot.data, aes(linetype=ID,colour=ID)) 
  
  my.plot <- my.plot + scale_linetype_manual(values = linetypelegend, labels = captions ) +
             scale_color_manual(values = colortypelegend, labels = captions)
  
  #my.plot <- my.plot + scale_color_identity (values = fillcolorlegend, labels = fill.captions)
  
  my.plot <- my.plot + theme(legend.position = "top")
  
  my.plot
}

H1area.plot <- function(existing.pdf.plot,curves,curve.idx,intervals) {
  my.sd.interval <- 4
  my.point.count <- 200
  
  x.min <- min(curves$mu - my.sd.interval*curves$sd)
  x.max <- max(curves$mu + my.sd.interval*curves$sd)
  x <- seq(x.min,x.max,length.out = my.point.count)    
  
  curve.mu <- curves$mu[curve.idx]
  curve.sd <- curves$sd[curve.idx]
  curve.n <- curves$n[curve.idx]
  y <- curve.n * dnorm(x, mean = curve.mu,sd = curve.sd)
  curve.data <- data.frame(x = x,y = y)
  area.data <- NULL
  for (i in 1:length(intervals)) {
    if (intervals[i] < curve.mu) {
      area.data <-rbind(area.data, subset(curve.data,x<=intervals[i]))
    } else {
      area.data <-rbind(area.data, subset(curve.data,x>=intervals[i]))
    }
  }
  
  area.data$ID <- paste("Line",i,sep="")
  
  my.plot <- existing.pdf.plot 
  
  my.plot <- my.plot + geom_ribbon(data = area.data)
  
  my.plot
}

mean.plot <- function(existing.pdf.plot, mu,linetype, colouridx) {
  my.plot <- existing.pdf.plot 
  
  for ( i in 1:length(mu) ) {
    
    my.plot <- my.plot + geom_vline(xintercept = mu[i], colour = cb.palette(colouridx[i]), linetype = linetype[i])
    #my.plot <- my.plot + geom_text(data = NULL, x=mu[i], y = 0,label = "mu", parse = TRUE)
  }
  
  my.plot
}