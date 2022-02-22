library(scales)
library(tidyverse)
library(purrr)

source("modELISA_lib.R")
tableFile <- "file-to-info.csv"
if(! file.exists(tableFile)) stop('Error -- tableFile file not found.')
file_table <- read.csv(tableFile,stringsAsFactors=FALSE)

#c_row <- file_table[2,]
#tablepath=paste0("table_parts/",sprintf("%02d",c_row$Plate_in_file),"_",tools::file_path_sans_ext(c_row$File_name),".tab")
#raw_table <- read.table(tablepath)

#sample_cols <- c("row_A_antigen","row_B_antigen","row_C_antigen",
#                 "row_D_antigen","row_E_antigen","row_F_antigen",
#                 "row_G_antigen","row_H_antigen")
#standar_row <- min(which(c_row[,sample_cols]=="p24-standard"))
#control_row <- min(which(c_row[,sample_cols]=="p24-standard"))

#table <-reblank_table(raw_table,standar_row)
#get_control_threshold(r_table,standar_row)



#standar_table <- standarize_table(raw_table,standar_row)
#standar_table$antibody <- as.list(c_row[,sample_cols])
#standar_table$sample <- c_row$Sample_in_plate



#standar_table

kk <- apply(file_table,1, function(x){
  plate <- as.numeric(x["Plate_in_file"])
  filename <- x["File_name"] 
  tablepath <- paste0("table_parts/",sprintf("%02d",plate),"_",tools::file_path_sans_ext(filename),".tab")
  if(! file.exists(tablepath)) stop(paste0('Error -- tableFile ' , tablepath , ' file not found.'))
  print(tablepath)
  raw_table <- read.table(tablepath,header = TRUE)
  sample_cols <- c(x["row_A_sample"],x["row_B_sample"],x["row_C_sample"],
                    x["row_D_sample"],x["row_E_sample"],x["row_F_sample"],
                    x["row_G_sample"],x["row_H_sample"])  
  antigen_cols <- c(x["row_A_antigen"],x["row_B_antigen"],x["row_C_antigen"],
                   x["row_D_antigen"],x["row_E_antigen"],x["row_F_antigen"],
                   x["row_G_antigen"],x["row_H_antigen"])
#  browser(skipCalls=100)
  standar_row <- min(which(antigen_cols=="p24-standard"))
  standar_table <- reblank_table(raw_table,standar_row)
  control_threshold <- get_control_threshold(standar_table,standar_row,standar_file=x["p24.file"])
  standar_table$antigen <- unname(as.list(antigen_cols))
  standar_table$sample <- unname(as.list(sample_cols))
  standar_table$control_threshold <- rep(control_threshold,8)
  return(standar_table)
})
c_data <-reduce(kk,rbind) 
c_data <- c_data %>% mutate_all(simplify) %>% mutate_if(is.character, str_trim)
#c_data %>% mutate_if(is.character, str_trim)
c_data

kk2 <- apply(c_data,1, function(x){
  logdilutions <- log10(rep(c(1/300,1/900,1/2700,1/8100),3))
  measurments <- as.numeric(c(x["X1"],x["X2"],x["X3"],x["X4"],
                              x["X5"],x["X6"],x["X7"],x["X8"],
                              x["X9"],x["X10"],x["X11"],x["X12"]))
  model <- lm(measurments ~ logdilutions)
  threshold <- as.numeric(x['control_threshold'])
  return((threshold-model$coefficients[1])/model$coefficients[2])
})
kk2

#kk3 <- kk2
c_data$heat <- kk2
order_ant <- c('HIV-p24','GFP','Brisavirus','Vientovirus','Coronavirus 229E','Coronavirus HKU1','Norovirus')
c_data$antigen <- factor(c_data$antigen,levels=rev(order_ant))
c_data <- c_data %>%
  filter(!is.na(antigen))

#kk4 <- spread(kk3, antigen, heat, fill = 0, convert = TRUE)
My_Theme = theme(
  axis.title.x = element_text(size = 10),
  axis.text.x = element_text(size = 5),
  axis.title.y = element_text(size = 10),
  axis.text.y = element_text(size = 5))

png(height = 7, width = 7,units = 'in', res=300, file = 'heatmap.png')
#forcats::fct_rev(forcats::fct_inorder(sample)
ggplot(c_data, aes(forcats::fct_inorder(sample),antigen, fill= heat)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="black") +
#  scale_fill_gradient(low="red", high="blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Sample") +
  My_Theme +
  labs(fill='Dilution \nat threshold') +
  coord_equal()
dev.off()


sample_info_file <- "sample-to-info.csv"
sample_info <- read.csv(sample_info_file,stringsAsFactors=FALSE)
sample_info <- sample_info %>% mutate_if(is.character, str_trim)
kk4 <- c_data %>% mutate(SAMPLE_ID=sample,sample=NULL)
kk5 <- left_join(kk4,sample_info, by="SAMPLE_ID")

write_csv(kk5,"all_dilutions_at_threshold.csv")


### A general heatmap for all samples. One plot for IgA another for IgG 
###Other plots that need edits based on group categories: 

IgA <- kk5[kk5$IG=='IgA',] %>%
        arrange(GROUP) %>%
  mutate(heat=ifelse(heat<0,0,heat)) %>%
  mutate(heat=ifelse(heat>100,100,heat))
IgG <- kk5[kk5$IG=='IgG',] %>%
  arrange(GROUP) %>%
  mutate(heat=ifelse(heat<0,0,heat)) %>%
  mutate(heat=ifelse(heat>100,100,heat))

png(height = 4.5, width = 10,units = 'in', res=300, file = 'heatmap_IgA-V2.png')
#forcats::fct_rev(forcats::fct_inorder(sample)
ggplot(IgA, aes(forcats::fct_inorder(SAMPLE_ID),antigen, fill= heat)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="black") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Sample") +
  labs(fill='Dilution \nat threshold',
       title='IgA') +
  My_Theme +
  coord_equal()
dev.off()

png(height = 4.5, width = 10,units = 'in', res=300, file = 'heatmap_IgG-V2.png')
#forcats::fct_rev(forcats::fct_inorder(sample)
ggplot(IgG, aes(forcats::fct_inorder(SAMPLE_ID),antigen, fill= heat)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="black") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Sample") +
  labs(fill='Dilution \nat threshold',
       title='IgG') +
  My_Theme +
  coord_equal()
dev.off()