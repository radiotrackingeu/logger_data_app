read_logger_data <- function(filepath,lines_to_skip=0,mid_freq=150100){
  data<-read.csv2(filepath, skip=lines_to_skip,stringsAsFactors = FALSE, dec = ".")
  data$Duration<-as.numeric(data$duration)
  data$HighLevel<-as.numeric(data$strength)
  data$RelFreq1<-as.numeric(data$freq)/1000
  #data<-na.omit(data)
  data$timestamp<-as.POSIXct(data$time, "%Y-%m-%d %H:%M:%S", tz="UTC")
  data$RelFreq1<-data$RelFreq1+mid_freq
  #data<-na.omit(data)
  return(data)
}

filter_data_length <- function(data,pulse_length){
  return(subset(data, (data$Duration>(pulse_length[1])) & (data$Duration<(pulse_length[2]))))
}

filter_data_freq <- function(data,freq,freq_error,mid_freq,freq_labels = NULL){
  freq_sorted<-NULL
  for(i in freq){
    tmp<-subset(data, (data$RelFreq1>(i-freq_error)) & (data$RelFreq1<(i+freq_error)))
    if(nrow(tmp)>0){
      if(is.null(freq_labels)){
        tmp$freq_tag<-paste0(as.character((i+mid_freq)/1000),"MHz")
      }
      else
      {
        tmp$freq_tag<-freq_labels[which(i==freq)[1]]
      }
      freq_sorted<-rbind.data.frame(freq_sorted, tmp)
    }
  }
  data<-freq_sorted
  
  freq_name<-paste0(as.character((freq)/1000),"MHz")
  #one_inv<-subset(data, (RelFreq1> low_freq) & (RelFreq1< high_freq) & (Duration > low_pulse_len) & (Duration < high_pulse_len) )
  
  #data$timestamp<-as.POSIXct(paste(data$Date, data$, "%Y-%m-%d %H:%M:%S", tz="UTC")
  return(data)
}

plot_time_signal <- function(data, mid_freq = 150100,active){
  
  require(ggplot2)
  #qplot(data$timestamp, data$HighLevel, xlab = "Time", ylab = "Signal Strength", col = data$freq_tag, size=I(0.8)) + labs(colour = "Frequencies") 
  p<-ggplot(data) + geom_point(aes(timestamp, HighLevel), size=I(0.8)) + labs(x="Time", y = "Signal Strength") 
  if(active){
    p + facet_wrap(~ data$freq_tag)
  }
  else{
    p
  }
}

hist_signalplot_time_signal <- function(data){
  ggplot(data) + geom_histogram(aes(RelFreq1),bins=200)
}