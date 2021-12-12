cwd<-getwd() #this is the project directory

source(file=file.path(cwd,"lib","organoid_funcs.R"),local=TRUE)
input_file<-file.path(cwd,"test","test.xlsx")

wb<-import_wb(input_file )
feature='counts'
metric='ratio'
maz<-TRUE
pat="Test"
subdata<-subset_measurements(wb,feature='counts')
norm_counts<-normalize_data(subdata,metric="proportion")
plot(plot_curves(norm_counts,feature=feature,patient=pat,error_bars = F,normalized = F,metric=metric))
plot(plot_boxes(subdata,feature=feature,patient=pat))
