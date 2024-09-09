hlca=readRDS('./hlca_ipf_bulk_all.rds')

cairo_pdf(filename = '../hlca_ipf_results_test1.pdf',width = 8,height = 10,family = 'Arial')
ggboxplot(hlca, "CT", "observed_values",fill = 'group',title = 'Optimised Reference 1',xlab = 'Cell types',ylab = 'Observed values',
          palette = c("#DD6091", "#006091"),orientation = "horizontal",outlier.shape = NA,order = rev(levels(hlca$CT)))+facet_grid(.~study)+theme(axis.title.x = element_text(family = 'Arial',size=15,colour = 'black'),axis.text.x = element_text(family = 'Arial',size=15,colour = 'black'),
                                                                                                                                                  axis.title.y = element_text(family = 'Arial',size = 15,colour = 'black'),axis.text.y = element_text(size=15,family = 'Arial',colour = 'black'),
                                                                                                                                                  strip.text.x = element_text(family = 'Arial',size=15,colour = 'black'),legend.text = element_text(size = 15,family = 'Arial',colour = 'black'),legend.title =element_text(size = 15,family = 'Arial',colour = 'black'),legend.position = 'right')

dev.off()
###
new=readRDS('./new_reference_ipf_bulk_all.rds')
cairo_pdf(filename = '../new_ipf_results_test1.pdf',width = 8,height = 10,family = 'Arial')
ggboxplot(new, "CT", "observed_values",fill = 'group',title = 'Optimised Reference 2',xlab = 'Cell types',ylab = 'Observed values',
          palette = c("#DD6091", "#006091"),orientation = "horizontal",outlier.shape = NA,order = rev(levels(new$CT)))+facet_grid(.~study)+theme(axis.title.x = element_text(family = 'Arial',size=15,colour = 'black'),axis.text.x = element_text(family = 'Arial',size=15,colour = 'black'),
                                                                                                                                                 axis.title.y = element_text(family = 'Arial',size = 15,colour = 'black'),axis.text.y = element_text(size=15,family = 'Arial',colour = 'black'),
                                                                                                                                                 strip.text.x = element_text(family = 'Arial',size=15,colour = 'black'),legend.text = element_text(size = 15,family = 'Arial',colour = 'black'),legend.title =element_text(size = 15,family = 'Arial',colour = 'black'),legend.position = 'right')

dev.off()


new$CT=factor(new$CT,levels = c('AT1','AT2','Aberrant_basaloid','Basal','Ciliated','Club','Mucous','B','T','NK','DC','Mast','Macrophage','Monocyte','SMC','fibroblast','Pericyte','Mesothelial','Vessel'))

hlca$CT=factor(hlca$CT,levels = c('AT1','AT2','Basal','Secretory','Multiciliated','B','T','NK','DC','Mast','Macrophages','Monocytes','SMC','Fibroblasts','EC'))

study=c('GSE124685'='McDonough',
        'GSE134692'='Sivakumar',
        'GSE150910'='Furusawa')
McDonough:GSE124685

Sivakumar:GSE134692
Furusawa:GSE150910

hlca$study= unname(study[hlca$study])
new$study= unname(study[new$study])
