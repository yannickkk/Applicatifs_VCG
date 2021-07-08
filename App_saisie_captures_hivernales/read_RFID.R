
# rm(list = ls())
# close(trovan)
#trovan<-serialConnection("get_rfid", port = "COM4", mode ="9600,N,8,1", newline = 1, translation = "cr", handshake = "xonxoff")
trovan<-serialConnection("get_rfid", port = "ttyUSB0", mode ="9600,n,8,1", newline = 1, translation = "cr", handshake = "xonxoff")
#ttySERIAL0
open(trovan)
wait(1)
inf<- "NA";
# # while (inf != "IOK"){inf<-write.serialConnection(trovan,"I")}
# #wait(1)
while (inf != "DONE"){inf<-write.serialConnection(trovan,"N")} ######  Dump the contents of all Lots in reader memory. The ID code is sent with the most significant 2 hexadecimal digits first and the least significant 2 hexadecimal digits last.
wait(2.5)
res<-read.serialConnection(trovan) ####resultat lecture
#res<-"0100079A81DC180719041610425100000100079A33C51807190416120747000001000799B791180719041612121700000100079A1F5A18071904161215990000010007869C0E18071904160220620000NOK"
beg_last_read<-gregexpr("010007", res)[[1]][length(gregexpr("010007", res)[[1]])]
rfid<-substr(res,beg_last_read +2,beg_last_read +11) #####tag rfid a lier avec la table animal
# lot<-substr(res,beg_last_read,beg_last_read+1)
# year<-paste0("20",substr(res,beg_last_read+12,beg_last_read+13))
# month<-substr(res,beg_last_read+14,beg_last_read+15)
# day<-substr(res,beg_last_read+16,beg_last_read+17)
# hour<-as.numeric(substr(res,beg_last_read+20,beg_last_read+21)) ###extraction remise a l'heure
# if(nchar(hour) == 1){hour<- paste0("0",hour,"")}
# minute<-as.numeric(substr(res,beg_last_read+22,beg_last_read+23))
# if(nchar(minute) == 1){minute<- paste0("0",minute,"")}
# ###extraction remise a l'heure
# second<-substr(res,beg_last_read+24,beg_last_read+25)
# if(nchar(second) == 1){second<- paste0("0",second,"")}
# time<-paste0(hour,":",minute,":",second)
# date<-paste0(year,"-",month,"-",day)
#system(paste0("sudo date --set \'",date," ",time,"\'")) #####remise à l'heure du raspberry
#rm(res)
close(trovan)

######clear memory take between 30 and 40 seconds
# library(serial)
# library(audio)
# rm(list = ls())
# close(trovan)
# trovan<-serialConnection("get_rfid", port = "COM4", mode ="9600,N,8,1", newline = 1, translation = "cr", handshake = "xonxoff")
# wait(1)
# open(trovan)
# wait(1)
# inf<-write.serialConnection(trovan,"C")
# wait(40)
# res<-read.serialConnection(trovan)
# if (res == "COK") {cat("mémoire effacée")}
# close(trovan)