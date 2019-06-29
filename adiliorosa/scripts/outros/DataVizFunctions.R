require(ggplot2)

plot.p.value <- function(mpvalue, vlabel, filename) 
{
  grf <- ggplot() +
    geom_line(data=mpvalue[mpvalue$config==1,], aes(x = participation, y = p.value, color = vlabel[1])) + 
    geom_point(data=mpvalue[mpvalue$config==1,], aes(x = participation, y = p.value, color = vlabel[1]))
  
  if (length(mpvalue$config[mpvalue$config==2]) > 0) 
  {
    grf <- grf + geom_line(data=mpvalue[mpvalue$config==2,], aes(x = participation, y = p.value, color = vlabel[2])) +
      geom_point(data=mpvalue[mpvalue$config==2,], aes(x = participation, y = p.value, color = vlabel[2]))
  }
  
  if (length(mpvalue$config[mpvalue$config==3]) > 0) 
  {
    grf <- grf + geom_line(data=mpvalue[mpvalue$config==3,], aes(x = participation, y = p.value, color = vlabel[3])) +
      geom_point(data=mpvalue[mpvalue$config==3,], aes(x = participation, y = p.value, color = vlabel[3]))
  }
  
  if (length(mpvalue$config[mpvalue$config==4]) > 0) {
    grf <- grf + geom_line(data=mpvalue[mpvalue$config==4,], aes(x = participation, y = p.value, color = vlabel[4])) 
  }
  
  grf <- grf + 
    scale_color_manual(values=c("orange", "darkblue", "darkgreen", "darkred")) +
    labs(color="Configuration") +    
    theme_bw() +  
    theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom") +
    scale_x_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format()) +
    scale_y_continuous(breaks = seq(0, 1, 0.1))
  
  ggsave(filename, width = 8, height = 5)  
}

plot.boxplot.clo <- function(mpvalue, filename) 
{
  grf <- ggplot(mpvalue, aes(participation, closeness)) + geom_boxplot(aes(group = cut_width(participation, 0.1)), color="darkblue") +
    theme_bw() +  
    theme(panel.grid.minor = element_blank()) + theme(legend.position = "bottom") +
    scale_x_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format())
  
  ggsave(filename, width = 8, height = 5)  
}

plot.boxplot.degree <- function(mpvalue, filename, filenameh) 
{
  grf <- ggplot(mpvalue, aes(participation, degree)) + geom_boxplot(aes(group = cut_width(participation, 0.1)), color="darkblue") +
    theme_bw() +  
    theme(panel.grid.minor = element_blank()) + theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format())
  
  ggsave(filename, width = 8, height = 5)  
  
  t <- mpvalue[mpvalue$participation==0 | mpvalue$participation==1,]
  
  t$participation = paste(t$participation*100, "%")
  
  ggplot(t, aes(x=degree, fill = participation)) +
    geom_histogram(binwidth = 1) +
    scale_fill_manual(values=c("darkblue", "darkgreen", "darkred")) +
    theme_bw() +  
    theme(panel.grid.minor = element_blank()) + theme(legend.position = "none") +
    theme(strip.background = element_rect(fill="orange")) +
    facet_wrap(~participation, ncol = 1)
  
  ggsave(filenameh, width = 8, height = 5)  
}