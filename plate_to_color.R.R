
T <- read.table("table_parts/01_2022-02-11-raw-plate.tab", header=TRUE)
heatmap(as.matrix(T), Colv = NA, Rowv = NA, revC=TRUE, 
        labRow=c("A:standatd","B:HIV-p24","C:Brisavirus","D:Vientovirus","E:Coronavirus 229E","F:Coronavirus HKU1","G:Norovirus","H:GFP"),
        labCol=c("R1-D1","R1-D2","R1-D3","R1-D4","R2-D1","R2-D2","R2-D3","R2-D4","R3-D1","R3-D2","R3-D3","R3-D4"), 
        main = "Healthy 832, saliva, IgA")

