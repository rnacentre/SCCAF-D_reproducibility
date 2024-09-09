
cairo_pdf(file = 'T2D_results_test.pdf',width = 12,height = 6,family = 'Arial')
ggplot(music1)+geom_bar(aes(x=CT,y=observed_values,fill=Disease),stat = "identity",position = "dodge",width = 0.7)+scale_fill_manual(values = cols,#modify legend title text
                                                                                                                                     name="Dataset")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),axis.title.x = element_text(family='Arial',size=16,margin = margin(0.5,1,0,1,'cm')),legend.text = element_text(family = 'Arial',size=16),legend.title = element_text(family='Arial',size=16),axis.title.y = element_text(family = 'Arial',size=16),axis.text.y = element_text(family = 'Arial',size=16),
                                                                                                                                                                      strip.text.x = element_text(family = 'Arial',size=16,colour = 'black'),#调整分页标题
                                                                                                                                                                      axis.text.x = element_text(family='Arial',size=16,angle = 90,hjust = 1,colour = 'black',vjust = 0.5),strip.background = element_rect(colour = 'black',fill = 'grey90'))

dev.off()
cols <- c("#FFB307" , "#FF5F1F")

###
cairo_pdf(file = 'T2D_scatter1.pdf',width = 17,height = 6,family = 'Arial')
p3=ggplot(music1, aes(hba1c, observed_values)) + 
  geom_smooth(method = 'lm', se = FALSE, col = 'black', lwd = 0.25) +
  geom_point(aes(fill = CT, color = Disease, stroke = D, shape = Disease), size = 2, alpha = 0.7)+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),text = element_text(family = 'Arial',color = 'black'),axis.text.x = element_text(colour = 'black',size = 16),axis.title.y = element_text(colour = 'black',size = 16),axis.text.y = element_text(colour = 'black',size = 16),legend.text = element_text(family = 'Arial',size=16),legend.title = element_text(family='Arial',size=16),,axis.title.x = element_text(family = 'Arial',size=16),strip.text.x = element_text(family = 'Arial',size=16,colour = 'black'))+
  scale_colour_manual(values = c('grey', "black")) + 
  scale_shape_manual(values = c(21, 25)) +
  scale_fill_manual(values = c("#7C6FAF", "#AA7816", "#00976E", "#E2167F", "#75AB43", "#D5580D", "#E5AD00")) +
  facet_wrap(~ CT, nrow = 1) +
  guides(fill = guide_legend(override.aes = list(shape = 21)))

p3
dev.off()

###
library(ggpubr)
cairo_pdf(filename = 'T2D_cell_proportion.pdf',width = 6,height = 7,family = 'Arial')
cols=c("#7C6FAF", "#AA7816", "#00976E", "#E2167F", "#75AB43", "#D5580D", "#E5AD00")
p1=ggplot(mydata) + 
  geom_bar(aes(x =fib, y=x, fill = CT),stat = "identity",width = 0.5,size = 0.5,colour = '#222222')+
  theme_bw() +
  labs(x='Disease',y = 'Cell proportion')+
  scale_fill_manual(values = cols)+theme_classic()+
  theme(axis.title.x = element_text(size=16,colour = 'black'),
        axis.text.x = element_text(size=16,colour = 'black'),axis.title.y = element_text(size=16,colour = 'black'),axis.text.y = element_text(size=16,colour = 'black'),
        legend.title = element_text(size=16,colour = 'black'),legend.text = element_text(size=16,colour = 'black'))+scale_y_continuous(limits = c(0, NA))

p1
dev.off()


mydata <- aggregate(music1$observed_values,by=list(fib=music1$Disease,CT=music1$CT),mean)


ggviolin(music1, "CT", "observed_values", fill = "Disease",
         add = "boxplot", add.params = list(fill = "white"))
###
cairo_pdf(file = 'T2D2_8-31.pdf',width = 8,height = 6,family = 'Arial')
p2=ggviolin(music1, "CT", "observed_values", fil = "Disease",trim = TRUE,
            palette = c("#00AFBB", "#E7B800"), add = "median_iqr")+theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),axis.title.x = element_text(family='Arial',size=16,margin = margin(0.5,1,0,1,'cm')),legend.text = element_text(family = 'Arial',size=16),legend.title = element_text(family='Arial',size=16),axis.title.y = element_text(family = 'Arial',size=16),axis.text.y = element_text(family = 'Arial',size=16),
                                                                         strip.text.x = element_text(family = 'Arial',size=16,colour = 'black'),#调整分页标题
                                                                         axis.text.x = element_text(family='Arial',size=16,angle = 90,hjust = 1,colour = 'black',vjust = 0.5),strip.background = element_rect(colour = 'black',fill = 'grey90'))+ylim(c(0,NA))
p2
dev.off()
