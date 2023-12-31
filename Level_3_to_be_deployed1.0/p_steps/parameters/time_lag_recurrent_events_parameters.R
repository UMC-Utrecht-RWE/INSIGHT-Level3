#list of chronic and recurrent events analysis
recurrent_events_list<-c("Depression", 
                         "Elective termination",
                         "Gestational diabetes",
                         "Multifoetal pregnancy",
                         "Preeclampsia", 
                         "Spontaneous abortion",
                         "Termination of pregnancy for foetal anomaly", 
                         "Breast cancer")
#time lag for recurrent events analysis
time_lag<-data.table(condition=c("Depression", 
                                 "Elective termination",
                                 "Gestational diabetes",
                                 "Multifoetal pregnancy",
                                 "Preeclampsia", 
                                 "Spontaneous abortion",
                                 "Termination of pregnancy for foetal anomaly", 
                                 "Breast cancer"),
                     time_lag=c(3*30, 
                                8*7, 
                                23*7, 
                                23*7, 
                                8*7, 
                                8*7, 
                                8*7,
                                5*365), 
                     time_remove=c(3*30, 
                                   8*7, 
                                   23*7, 
                                   23*7, 
                                   8*7, 
                                   8*7, 
                                   8*7,
                                   0))
