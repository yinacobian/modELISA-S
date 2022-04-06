library(tidyverse)
library(readr)


sample_to_info <- read_csv("sample-to-info.csv")
tableFile <- "file-to-info.csv"
if(! file.exists(tableFile)) stop('Error -- tableFile file not found.')
file_table <- read.csv(tableFile,stringsAsFactors=FALSE)
j_table <- left_join(file_table,sample_to_info,by =c("row_E_sample" = "SAMPLE_ID"))

kk <- apply(j_table,1, function(x){
  plate <- as.numeric(x["Plate_in_file"])
  filename <- x["File_name"] 
  tablepath <- paste0("table_parts/",sprintf("%02d",plate),"_",tools::file_path_sans_ext(filename),".tab")
  main_t <- paste0(x["row_E_sample"],' group: ',x["GROUP"])
  if(! file.exists(tablepath)) stop(paste0('Error -- tableFile ' , tablepath , ' file not found.'))
  raw_table <- read.table(tablepath,header = TRUE)
  rownames(raw_table) <- c("A:standard","B:HIV,HIV,HIV+","C:HIV,BRISA,HIV+","D:Blank",
                 "E:BRISA, BRISA, RV+","F:BRISA,HIV,RV+","G:BRISA, BRISA, RV+","BRISA,HIV,RV+")
  colnames(raw_table) <- c("competitor 1 ug","competitor 1/2 ug","competitor 1/4 ug","competitor 1/8 ug","competitor 1/16 ug","competitor 1/32 ug","competitor 1/64 ug","competitor 1/128 ug","competitor 1/256 ug","competitor 1/512 ug", "competitor 1/1024 ug","no competitor")
  raw_table %>% 
    rownames_to_column(var="antigen") %>%
    pivot_longer(`competitor 1 ug`:`no competitor`,names_to = 'dilutions') %>%
    mutate(patient=main_t)
})

all_data <- Reduce(rbind,kk) %>%
  mutate(antigen=factor(antigen,rev(c("A:standard","B:HIV,HIV,HIV+","C:HIV,BRISA,HIV+","D:Blank",
                                       "E:BRISA, BRISA, RV+","F:BRISA,HIV,RV+","G:BRISA, BRISA, RV+","BRISA,HIV,RV+")))) %>% 
  mutate(dilutions=factor(dilutions, c("competitor 1 ug","competitor 1/2 ug","competitor 1/4 ug","competitor 1/8 ug","competitor 1/16 ug","competitor 1/32 ug","competitor 1/64 ug","competitor 1/128 ug","competitor 1/256 ug","competitor 1/512 ug", "competitor 1/1024 ug","no competitor")))
                  

IgG_plot <- all_data %>%
  ggplot( aes(dilutions, antigen,fill= value)) + 
  geom_tile(size=1) +
  scale_fill_gradient(low = "bisque", high = "darkorange3") +
  geom_text(aes(label=round(value,2))) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_equal() +
  facet_wrap(~ patient)
ggsave('competition_plot.pdf',plot = IgG_plot, width = 15.2, height = 13.9, units = 'in', scale = 1.5)

# standard curve plots

standard_plot <- all_data %>%
  filter(grepl('standard',antigen)) %>%
  ggplot(aes(x=dilutions, y=value)) + 
  geom_point(size=3) +
  geom_smooth() +
  facet_wrap(~ patient) +
  ggpubr::rotate_x_text()
standard_plot
ggsave('standard_plot.pdf',plot = standard_plot, width = 15.2, height = 13.9, units = 'in', scale = 1.5)

#plots for HIV competition experiment

B_all_data <- all_data %>%
  filter(grepl('B:HIV,HIV,HIV+',antigen))
C_all_data <- all_data %>%
  filter(grepl('C:HIV,BRISA,HIV+',antigen))

HIVcurve <- rbind(C_all_data, B_all_data)

HIV_plot <- ggplot(data=HIVcurve) + 
  geom_point(aes(y=value, x=dilutions, color=antigen, group=antigen), size=3) +
  geom_line(aes(y=value, x=dilutions, color=antigen, group=antigen)) +
  facet_wrap(~ patient) +
   theme_minimal() +
  ggpubr::rotate_x_text() 
HIV_plot

ggsave('HIV_plot.pdf',plot = HIV_plot, width = 15.2, height = 13.9, units = 'in', scale = 1.5)

HIV_plot2 <- ggplot(data=HIVcurve) + 
  geom_point(aes(y=value, x=dilutions, color=antigen, group=antigen), size=3) +
  geom_smooth(method="lm",aes(y=value, x=dilutions, color=antigen, group=antigen)) +
  facet_wrap(~ patient) +
  theme_minimal() +
  ylab ("OD450") +
  xlab ("Competitor dilutions") +
  ggpubr::rotate_x_text() 
HIV_plot2
ggsave('HIV_plot2.png',plot = HIV_plot2, width = 15.2, height = 13.9, units = 'in', scale = 1.5)
ggsave('HIV_plot2.pdf',plot = HIV_plot2, width = 15.2, height = 13.9, units = 'in', scale = 1.5)



#plots for redondovirus competition experiment

E_all_data <- all_data %>%
  filter(grepl('E:BRISA, BRISA, RV+',antigen))
F_all_data <- all_data %>%
  filter(grepl('F:BRISA,HIV,RV+',antigen))

BRISAcurve <- rbind(E_all_data, F_all_data)

BRISA_plot <- ggplot(data=BRISAcurve) + 
  geom_point(aes(y=value, x=dilutions, color=antigen, group=antigen), size=3) +
  geom_line(aes(y=value, x=dilutions, color=antigen, group=antigen)) +
  facet_wrap(~ patient) +
  theme_minimal() +
  ggpubr::rotate_x_text() 
BRISA_plot

BRISA_plot2 <- ggplot(data=BRISAcurve) + 
  geom_point(aes(y=value, x=dilutions, color=antigen, group=antigen), size=3) +
  geom_smooth(method="lm",aes(y=value, x=dilutions, color=antigen, group=antigen)) +
  facet_wrap(~ patient) +
  theme_minimal() +
  ylab ("OD450") +
  xlab ("Competitor dilutions") +
  ggpubr::rotate_x_text() 
BRISA_plot2
ggsave('HIV_plot2.png',plot = HIV_plot2, width = 15.2, height = 13.9, units = 'in', scale = 1.5)
ggsave('HIV_plot2.pdf',plot = HIV_plot2, width = 15.2, height = 13.9, units = 'in', scale = 1.5)

#NICE EXAMPLE
one <- HIVcurve %>%
  filter(grepl('RV-01 group: RV-NEGATIVE',patient))

ONE_plot2 <- ggplot(data=one) + 
  geom_point(aes(y=value, x=dilutions, color=antigen, group=antigen), size=3) +
  geom_smooth(method="lm",aes(y=value, x=dilutions, color=antigen, group=antigen)) +
  facet_wrap(~ patient) +
  theme_minimal() +
  ylab ("OD450") +
  xlab ("Competitor dilutions") +
  ggpubr::rotate_x_text() 
ONE_plot2