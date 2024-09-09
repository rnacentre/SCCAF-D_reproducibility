cairo_pdf(filename = 'heatmap_all.pdf',width = 26,height = 6,family = 'Arial')
p1 <- Heatmap(t(as.matrix(dat_dwls)),col=col_fun,name = 'Pearson',cluster_rows = F,cluster_columns = F,column_split = c(rep('a',1),rep('b',4)),column_gap = unit(8, "mm"),border = TRUE,
              column_title = 'DWLS',row_names_side = 'left',row_names_gp = gpar(fontsize=20),
              column_names_gp =gpar(fontsize=20),rect_gp = gpar(col='white',lwd=1),heatmap_legend_param = list(labels_gp = gpar(fontsize = 20),title_gp = gpar(fontsize = 20)),column_title_gp = gpar(fontsize = 20),width = ncol(t(as.matrix(dat_dwls)))*unit(1.15, "cm"), 
              height = nrow(t(as.matrix(dat_dwls)))*unit(1, "cm"))


p3 <- Heatmap(t(as.matrix(dat_fardeep)),col=col_fun,name = 'Pearson',cluster_rows = F,cluster_columns = F,column_gap = unit(5, "mm"),border = TRUE,column_title = 'FARDEEP',row_names_side = 'left',row_names_gp = gpar(fontsize=20),
              column_names_gp =gpar(fontsize=20),rect_gp = gpar(col='white',lwd=1),heatmap_legend_param = list(labels_gp = gpar(fontsize = 20),title_gp = gpar(fontsize = 20)),column_title_gp = gpar(fontsize = 20),width = ncol(t(as.matrix(dat_fardeep)))*unit(1, "cm"), 
              height = nrow(t(as.matrix(dat_fardeep)))*unit(1, "cm"))

p2 <- Heatmap(t(as.matrix(dat_epidish)),col=col_fun,name = 'Pearson',cluster_rows = F,cluster_columns = F,column_gap = unit(5, "mm"),border = TRUE,column_title = 'EpiDISH',row_names_side = 'left',row_names_gp = gpar(fontsize=20),
              column_names_gp =gpar(fontsize=20),rect_gp = gpar(col='white',lwd=1),heatmap_legend_param = list(labels_gp = gpar(fontsize = 20),title_gp = gpar(fontsize = 20)),column_title_gp = gpar(fontsize = 20),width = ncol(t(as.matrix(dat_epidish)))*unit(1, "cm"), 
              height = nrow(t(as.matrix(dat_epidish)))*unit(1, "cm"))

p4 <- Heatmap(t(as.matrix(dat_music)),col=col_fun,name = 'Pearson',cluster_rows = F,cluster_columns = F,column_gap = unit(5, "mm"),border = TRUE,column_title = 'MuSiC',row_names_side = 'left',row_names_gp = gpar(fontsize=20),
              column_names_gp =gpar(fontsize=20),rect_gp = gpar(col='white',lwd=1),heatmap_legend_param = list(labels_gp = gpar(fontsize = 20),title_gp = gpar(fontsize = 20)),column_title_gp = gpar(fontsize = 20),width = ncol(t(as.matrix(dat_music)))*unit(1, "cm"), 
              height = nrow(t(as.matrix(dat_music)))*unit(1, "cm"))

p5 <- Heatmap(t(as.matrix(dat_nnls)),col=col_fun,name = 'Pearson',cluster_rows = F,cluster_columns = F,column_gap = unit(5, "mm"),border = TRUE,column_title = 'NNLS',row_names_side = 'left',row_names_gp = gpar(fontsize=20),
              column_names_gp =gpar(fontsize=20),rect_gp = gpar(col='white',lwd=1),heatmap_legend_param = list(labels_gp = gpar(fontsize = 20),title_gp = gpar(fontsize = 20)),column_title_gp = gpar(fontsize = 20),width = ncol(t(as.matrix(dat_nnls)))*unit(1, "cm"), 
              height = nrow(t(as.matrix(dat_nnls)))*unit(1, "cm"))

p6 <- Heatmap(t(as.matrix(dat_rlr)),col=col_fun,name = 'Pearson',cluster_rows = F,cluster_columns = F,column_gap = unit(5, "mm"),border = TRUE,column_title = 'RLR',row_names_side = 'left',row_names_gp = gpar(fontsize=20),
              column_names_gp =gpar(fontsize=20),rect_gp = gpar(col='white',lwd=1),heatmap_legend_param = list(labels_gp = gpar(fontsize = 20),title_gp = gpar(fontsize = 20)),column_title_gp = gpar(fontsize = 20),width = ncol(t(as.matrix(dat_rlr)))*unit(1, "cm"), 
              height = nrow(t(as.matrix(dat_rlr)))*unit(1, "cm"))

ht_list_pancreas = p1+p2+p3+p4+p5+p6
draw(ht_list, ht_gap = unit(2, "cm"))

dev.off()