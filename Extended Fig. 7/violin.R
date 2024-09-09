#violin_plot_liver
options(repr.plot.width=7,repr.plot.height=7)
library(ggpubr)
my_comparisons <- list(c('1','0'),c('2','0'),c('3','0'),c('4','0'))
###

#####pdf
cairo_pdf(filename = 'liver_cDC.pdf',width = 7,height = 6,family = 'Arial')
ggviolin(fib[fib$CT=='cDC',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),add=c('jitter'),ylim=c(0,0.018),
         xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.009,0.011,0.013,0.015),
                                                 aes(label = after_stat(p.signif)))+
  theme(text=element_text(family='Arial'),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 20,family = 'Arial'),axis.title.y = element_text(size=20,family = 'Arial'),axis.text.x = element_text(size = 12,family='Arial'),axis.text.y = element_text(size = 12,family='Arial'),legend.text = element_text(size=12,family = 'Arial'),
        legend.title = element_text(size=12,family = 'Arial'))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('cDC')
dev.off()

cairo_pdf(filename = 'liver_T.pdf',width = 7,height = 6,family = 'Arial')
p8=ggviolin(fib[fib$CT=='T',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),trim = TRUE,
            add=c('jitter'),ylim=c(0,0.08),
            xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.02,0.03,0.07,0.06),
                                                    aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('T')
dev.off()



####
cairo_pdf(filename = 'liver_Plasma.pdf',width = 7,height = 6,family = 'Arial')
p7=ggviolin(fib[fib$CT=='Plasma',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),trim = TRUE,
            add=c('jitter'),ylim=c(0,0.015),
            xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.005,0.007,0.009,0.011),
                                                    aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('Plasma')
dev.off()


###
cairo_pdf(filename = 'liver_pDC.pdf',width = 7,height = 6,family = 'Arial')
p6=ggviolin(fib[fib$CT=='pDC',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),trim = TRUE,
            add=c('jitter'),ylim=c(0,0.025),
            xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.005,0.01,0.015,0.02),
                                                    aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('pDC')
dev.off()
###
cairo_pdf(filename = 'liver_HSC.pdf',width = 7,height = 6,family = 'Arial')
p5=ggviolin(fib[fib$CT=='HSC',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),trim = TRUE,
            add=c('jitter'),ylim=c(0,0.07),
            xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.01,0.02,0.03,0.05),
                                                    aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('HSC')
dev.off()
###
cairo_pdf(filename = 'liver_Hepatocyte.pdf',width = 7,height = 6,family = 'Arial')
p4=ggviolin(fib[fib$CT=='Hepatocyte',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),trim = TRUE,
            add=c('jitter'),ylim=c(0,1.5),
            xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(1.0,1.1,1.2,1.3),
                                                    aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('Hepatocyte')
dev.off()
###
cairo_pdf(filename = 'liver_fibroblast.pdf',width = 7,height = 6,family = 'Arial')
p3=ggviolin(fib[fib$CT=='fibroblast',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),trim = TRUE,
            add=c('jitter'),ylim=c(0,0.035),
            xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.01,0.015,0.02,0.025),
                                                    aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('Fibroblast')
dev.off()
###
cairo_pdf(filename = 'liver_endothelial.pdf',width = 7,height = 6,family = 'Arial')
p2=ggviolin(fib[fib$CT=='endothelial',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),trim = TRUE,
            add=c('jitter'),ylim=c(0,0.2),
            xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.09,0.12,0.15,0.18),
                                                    aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('Endothelial')
dev.off()
###
cairo_pdf(filename = 'liver_Cholangiocyte.pdf',width = 7,height = 6,family = 'Arial')
p1=ggviolin(fib[fib$CT=='Cholangiocyte',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),trim = TRUE,
            add=c('jitter'),ylim=c(0,0.25),
            xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.06,0.1,0.14,0.19),
                                                    aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('Cholangiocyte')
dev.off()

##
cairo_pdf(filename = 'liver_pDC.pdf',width = 7,height = 6,family = 'Arial')
ggviolin(fib[fib$CT=='pDC',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),
         add=c('jitter'),ylim=c(0,0.02),
         xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.01,0.013,0.016,0.019),
                                                 aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('pDC')
dev.off()

###
cairo_pdf(filename = 'liver_Cholangiocyte.pdf',width = 7,height = 6,family = 'Arial')
ggviolin(fib[fib$CT=='Cholangiocyte',],x="fib",y="observed_values",fill="fib",palette =cols,add.params = list(fill="white"),
         add=c('jitter'),ylim=c(0,0.4),
         xlab = '',ylab = '')+stat_compare_means(comparisons = my_comparisons,label.y = c(0.15,0.2,0.25,0.3),
                                                 aes(label = after_stat(p.signif)))+
  theme(text=element_text(size=30),plot.margin = margin(3,15,0,0),axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size=30),axis.text.x = element_text(size = 30,colour = 'black'),axis.text.y = element_text(size = 30,colour = 'black'),legend.text = element_text(size=30),
        legend.title = element_text(size=30))+labs(fill='Fibrosis stage')+ylab('Cell proportion')+
  ggtitle('Cholangiocyte')
dev.off()

####
cairo_pdf(filename = 'liver_all.pdf',width = 32,height = 12,family = 'Arial')
p1+p2+p3+p4+p5+p6+p7+p8+plot_layout(nrow=2)
dev.off()
