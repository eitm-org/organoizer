library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)

#Import all the sheets in a workbook into a consolidated table (tibble)
import_wb <- function(input_file) {
  # The consolidated table
  wb <- tibble()
  
  # Create a data frame of sheet names in the workbook with columns for day and wether it's dead or live
  sheets <- excel_sheets(input_file)
  mdata_d <-
    matrix(ncol = 2, unlist(strsplit(sheets, '_')), byrow = TRUE)
  mdata <-
    data.frame(
      sheet = sheets,
      Day = as.integer(gsub('^D', '', mdata_d[, 1])),
      LD = mdata_d[, 2],
      stringsAsFactors = FALSE
    )
  
  # Iterate across all worksheets in the workbook and add them to the consolidated table
  # For every sheet, two columns are added: one for the day and one for the Live/dead status
  for (i in seq(from = 1, to = nrow(mdata))) {
    tws <-
      read_excel(input_file, sheet = as.character(mdata[i, "sheet"]), col_types = 'text')
    names(tws) <-
      gsub(" ?(?:Live|Dead) ?", '', names(tws), perl = TRUE) #Some column names are different in the _Dead and _Alive sheets. This removes references to Live and Dead
    tws$Day <- mdata[i, "Day"]
    tws$LD <- mdata[i, "LD"]
    wb <- rbind(wb, tws)
  }
  names(wb)<-gsub("^- ","",names(wb))
  return(wb)
}

check_input<-function(input_file) {
  # The consolidated table
  wb <- tibble()
  msg<-""
  # Create a data frame of sheet names in the workbook with columns for day and wether it's dead or live
  sheets <- excel_sheets(input_file)
  mdata_d <-
    matrix(ncol = 2, unlist(strsplit(sheets, '_')), byrow = TRUE)
  mdata <-
    data.frame(
      sheet = sheets,
      Day = as.integer(gsub('^D', '', mdata_d[, 1])),
      LD = mdata_d[, 2],
      nvars<-0,
      stringsAsFactors = FALSE
    )
  
  # Iterate across all worksheets in the workbook and add them to the consolidated table
  # For every sheet, two columns are added: one for the day and one for the Live/dead status
  for (i in seq(from = 1, to = nrow(mdata))) {
    tws <-
      read_excel(input_file, sheet = as.character(mdata[i, "sheet"]))
    nvars<-length(names(tws))
    names(tws) <-
      gsub("^(?:Organoid )?(?:Live|Dead) - ", '', names(tws), perl = TRUE) #Some column names are different in the _Dead and _Alive sheets. This removes references to Live and Dead
    tws$Day <- mdata[i, "Day"]
    tws$LD <- mdata[i, "LD"]
    wb <- rbind(wb, names(tws))
    mdata[i,"nvars"]<-nvars+2
  }
  if(dim(wb)[1] != nrow(mdata)){
    return(paste("Expecting ",nrow(mdata),"sheets but imported ",dim(wb)[1]))
  }

  return(mdata)
}

subset_measurements <-   function(measurements,
                               feature = 'counts',
                               missing_as_zero = TRUE) {
  sumfun <-
    paste0('mean(utils::type.convert(`', feature, '`,na.strings="", as.is=TRUE),na.rm=TRUE)') #The summarization function (mean of the feature)
  vname <-
    paste0('mean_', make.names(feature, allow_ = TRUE)) #The name of the summarized variable
  
  #This is a bit of a helper step: data are imported as characters
  #But concentration is a number. This converts it to the appropriate type
  #TODO: Concentration is a grouping variable, so it needs to be modified early on.
  #There might be a better place for this call.
  measurements<-measurements %>% mutate(Concentration=utils::type.convert(Concentration,na.string="", as.is=TRUE)) 
  
  wd <- tibble()
  
  if (feature == 'counts') {
    #counts are handled differently, as it's not a data column.
    vname = 'counts'
    wd <-
      measurements %>% group_by(Day, LD, Compound, Concentration, `Cell Type`, Row, Column) %>%
      summarise(counts = n())
  } else{
    wd <-
      measurements %>% group_by(Day, LD, Compound, Concentration, `Cell Type`, Row, Column) %>%
      summarise_(.dots = setNames(sumfun, vname))
  }
  
  # This reshapes the table so that for every well the values for dead and live are in columns
  dwd <-
    dcast(wd,
          Day + Compound + Concentration + `Cell Type` + Row + Column ~ LD ,
          value.var = vname)
  
  #TODO: Should cases where Dead=0 set to 0 or leave NA?
  #In some cases, it makes sense to set missing data to 0 (e.g. counts, area). In some others, it would be incorrect (e.g. roundness)
  if (missing_as_zero) {
    dwd[is.na(dwd$Dead), "Dead"] <- 0
    dwd[is.na(dwd$Live), "Live"] <- 0
  }
  return(dwd)
}

normalize_data <- function(dwd, metric = 'proportion') {
  dwd$plive <- NA
  if (metric != 'proportion') {
    dwd$plive <- dwd$Live / dwd$Dead
  } else{
    dwd$plive <- dwd$Live / (dwd$Live + dwd$Dead)
    
  }
  
  #Normalizes the proportion/ratios by the corresponding value at day 0
  ndwd <- dwd %>%
    group_by(`Cell Type`, Compound, Concentration, Day) %>%
    summarise(
      N_wells = sum(!is.na(plive)),
      live_mean = mean(plive, na.rm = TRUE),
      live_sd = sd(plive, na.rm = TRUE)
    ) %>%
    mutate(
      nlive = live_mean / first(live_mean, order_by = 'Day'),
      nlive_sd = live_sd / first(live_mean, order_by = 'Day')
    )
  
  return(ndwd)
}

make_treatments <- function(measurements) {

  treatments <-
    measurements %>% ungroup() %>% select(Compound, Concentration)  %>% distinct()
  
  
  #Separate controls and response curves. Controls alpha sorted come first,
  #then response cureves sorted alpha on compond, then increasing concentration
  k<-unlist(treatments %>% group_by(Compound) %>%summarise(n=n()) %>% filter(n==1) %>%select(Compound))
  concs <-
    treatments %>% filter(! Compound %in% k) %>% arrange(Compound, Concentration) #Compounds with multiple concs
  ctrls <-
    treatments %>% filter(Compound %in% k) %>% arrange(Compound)#Controls.

  #Move "Media" to the top of the list
  media <- ctrls %>% filter(tolower(Compound) == "media" | tolower(Compound) == "control")
  if (media %>% count() == 1) {
    ctrls <-
      ctrls %>% filter(!(tolower(Compound) == "media" | tolower(Compound) == "control")) %>% rbind(media, .)
  }
  
  #Merge the controls and curves
  treatments <- rbind(ctrls, concs)
  
  #Treatment is combination of compound and concentration
  treatments <-
    treatments %>% mutate(Treatment = paste0(Compound, ifelse(
      is.na(Concentration), '', paste0("_", Concentration)
    )))
  #TODO: Concentration should be rounded before paste
  
  measurements <-
    measurements %>% mutate(Treatment = paste0(Compound, ifelse(
      is.na(Concentration), '', paste0("_", Concentration)
    )))
  
  measurements$Treatment <-
    ordered(measurements$Treatment, levels = treatments$Treatment)
  measurements$Compound <-
    ordered(measurements$Compound, levels = unlist(treatments %>% select(Compound) %>% distinct()))
  
  return(measurements)
}

get_concentration<-function(x){
  if(is.factor(x$Compound)){
    x$Compound<-as.character(x$Compound)
  }
  tc<-strsplit(x$Compound,split='[ _]',perl=TRUE)
  tct<-data.frame(t(sapply(tc, function(x) c(Compound=x[1],OConcentration=x[2]))),stringsAsFactors = FALSE)
  tct$OCompound<-x$Compound
  tct$Concentration<-as.numeric(tct$OConcentration)
  no_conc<-is.na(x$Concentration)
  keep_compound<-is.na(tct$Concentration) & !is.na(tct$OConcentration) #Cases where Concentration is not a number, the original compound name should be kept
  x$Compound<-tct$Compound
  x[no_conc,"Concentration"]<-tct[no_conc,"OConcentration"]
  x[keep_compound,"Compound"]<-tct[keep_compound,"OCompound"]
  return(x)
}

make_aes <- function(measurements) {
  treatments <-
    measurements %>% ungroup() %>% select(Compound, Concentration)  %>% distinct()
  nmedia <-
    unlist(treatments %>% filter(tolower(Compound) == "media" | tolower(Compound) == "control" ) %>% count())
  if (nmedia  == 1) {
    treatments <- treatments %>% filter(!(tolower(Compound) == "media" | tolower(Compound) == "control"))
  }
  
  ncats <-
    as.integer(treatments %>% select(Compound) %>% distinct() %>% count())
  dark_cols <- few_pal(palette = 'Dark')(ncats)
  light_cols <- few_pal(palette = 'Light')(ncats)
  
  cat_tbl <-
    data.frame(
      treatments %>% group_by(Compound) %>% summarise(n = n()),
      from = light_cols,
      to = dark_cols
    )
  cat_tbl$shape <-
    as.numeric(row.names(cat_tbl)) - 1 #if there are >20 categories, it gets messy
  
  cols <-
    unlist(apply(cat_tbl, 1, function(x)
      colorRampPalette(c(
        ifelse(as.numeric(x[2]) == 1, x[4], x[3]), x[4]
      ))(as.numeric(x[2]))))
  
  shapes <-
    unlist(apply(cat_tbl, 1, function(x)
      rep(as.numeric(x[5]), as.numeric(x[2]))))
  
  if (nmedia == 1) {
    cols <- c('#000000', cols)
    shapes <- c(32, shapes)
  }
  return(list(cols = cols, shapes = shapes))
}

plot_curves <-
  function(measurements,
           feature,
           patient = NULL,
           metric = NULL,
           normalized = TRUE,
           error_bars = TRUE,
           freescale = FALSE,
           i_aes=NULL) {
    y_label <- switch(metric,
                      proportion = "Proportion Live/Total",
                      ratio = "Ratio Live/Dead")
    
    measurements <- make_treatments(measurements)
    
    if (normalized) {
      measurements <- measurements %>% rename(val = nlive, dev = nlive_sd)
      y_label <- paste("Normalized", y_label)
    } else{
      measurements <- measurements %>% rename(val = live_mean, dev = live_sd)
    }
    measurements <- measurements %>% filter(abs(val) != Inf)
    pd <- position_identity()
    days <- sort(as.numeric(unique(measurements$Day)))
    
    if(is.null(i_aes)){
      cols <-
        make_aes(measurements)$cols
      shapes <-
        make_aes(measurements)$shapes
    } else{
      cols<-i_aes$cols
      shapes<-i_aes$shapes
    }
    
    pl <- ggplot(data = measurements) + aes(
      x = Day,
      y = val,
      col = Treatment,
      shape = Compound
    )
    
    if (error_bars) {
      pd <- position_dodge(0.1)
      pl <-
        pl + geom_errorbar(aes(ymin = val - dev, ymax = val + dev),
                           width = 1,
                           position = pd)
    }
    
    pl <- pl  +
      geom_line(lwd = 1) +
      geom_point(position = pd) +
      scale_x_continuous(breaks = days) +
      scale_color_manual(values = cols) +
      ylab(label = y_label) +
      labs(title = ifelse(is.null(patient), '', paste("Patient", patient)), subtitle =
             feature) +
      scale_shape_manual(values = unique(shapes), guide=FALSE) +
      guides(colour = guide_legend(override.aes = list(shape = shapes ))) +
      theme_linedraw()
    
    if (freescale) {
      pl <- pl +
        facet_grid(`Cell Type` ~ ., scales = "free_y")
    } else{
      if (error_bars) {
        maxy <- max(measurements$val + measurements$dev, na.rm = TRUE)
        miny <-
          ifelse(min(measurements$val - measurements$dev, na.rm = TRUE) > 0,
                 0,
                 min(measurements$val - measurements$dev, na.rm = TRUE))
      } else{
        maxy <- max(measurements$val, na.rm = TRUE)
        miny <-
          ifelse(min(measurements$val, na.rm = TRUE) > 0,
                 0,
                 min(measurements$val, na.rm = TRUE))
      }
      if(miny<0){
        miny<-0
      }else {
        miny <- floor(miny)
      }
      maxy <- ifelse(maxy < 2, round(maxy, 1), ceiling(maxy))
      scale_fac <- ifelse(maxy < 2, .2, round(maxy / 5, 1))
      pl <- pl +
        facet_grid(`Cell Type` ~ .) +
        scale_y_continuous(breaks = seq(miny, maxy, by = scale_fac)) +
        expand_limits(y = miny)
    }
    
    
  }

plot_boxes <- function(cmeasurements, feature, patient) {
  mcd <-
    melt(
      cmeasurements,
      variable.name = "Class",
      id.vars = c(
        "Day",
        "Compound",
        "Concentration",
        "Cell Type",
        "Row",
        "Column"
      )
    )
  
  mcd <- make_treatments(mcd)
  mcd$Day <- as.factor(mcd$Day)
  mcd$Class <- relevel(mcd$Class, ref = "Live")
  pl <- ggplot(data = mcd) + aes(x = Day, y = value, fill = Class) +
    geom_boxplot()  +
    scale_fill_few() +
    facet_grid(facets = `Cell Type` ~ Treatment, scales = 'free_y') +
    ylab(label = feature) +
    labs(title = ifelse(is.null(patient), '', paste("Patient", patient))) +
    theme_linedraw()
}


format_for_dl <- function(measurements, patient, feature, metric) {
  on <- names(measurements)
  feature_name <- make.names(feature, allow_ = TRUE)
  if (is.null(patient)) {
    patient_name = ''
  } else{
    patient_name <- make.names(patient, allow_ = TRUE)
    if (substr(patient_name, 1, 1) == 'X' &&
        substr(patient, 1, 1) != 'X') {
      patient_name <- substr(patient_name, 2, nchar(patient_name))
    }
  }
  
  file_name = paste0(patient_name, '_', feature_name)
  file_name <- gsub('\\.+', '_', file_name, perl = TRUE)
  file_name <- gsub('^_|_$', '', file_name, perl = TRUE)
  
  mod_names <-
    c(
      paste('Mean', metric, feature),
      paste('SD', metric, feature),
      paste('Mean Normalized', metric, feature),
      paste('SD Normalized', metric, feature)
    )
  on <- on[1:(length(on) - 4)]
  names(measurements) <- c(on, mod_names)
  return(list(name = file_name, payload = measurements))
}
