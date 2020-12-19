library(dplyr)
library(stringr)
library(tidyr)

maindata = read.csv("cleandata.csv")


newdata = maindata  %>%  filter ( maindata$A_PFREL !=0 &  
                                  maindata$PEIO1COW !=0 & 
                                  maindata$CLWK !=0 &
                                  maindata$CLWK !=4 &
                                  maindata$CLWK !=5 &
                                  maindata$PEAFEVER >0 &
                                  maindata$ANN_VAL >=0 & #first encountered problems with how much it was filtering 
                                  maindata$A_UNCOV > 0 & #got rid of a large chunk of data
                                  maindata$SRVS_VAL !=0 & #dropped to like 30 observations but that makes sense because not a ton of people get survivors income so we should actually keep the zero values
                                  maindata$DEPPRIV !=0 &
                                  maindata$DEPGRP !=0 &
                                  maindata$HIPAID !=0 &
                                  maindata$DIR !=0 &
                                  maindata$DEPMRK !=0 &
                                  maindata$DEPMRKS !=0 &
                                  maindata$DEPMRKUN !=0 &
                                  maindata$DEPNONM !=0 &
                                  maindata$CAID !=0 &
                                  maindata$PCHIP !=0 &
                                  maindata$CHAMPVA !=0 &
                                  maindata$VACARE !=0 &
                                  maindata$PNSN_VAL !=0 &
                                  maindata$RINT_VAL1 !=0 &
                                  maindata$RINT_VAL2 !=0 &
                                  maindata$SS_VAL !=0 &
                                  maindata$SRVS_VAL !=0)

 print(newdata)
 #COLUMN FILTERING
 newdata$A_MARITL[newdata$A_MARITL == 2 | newdata$A_MARITL == 3 ] = 1
 newdata$A_MARITL[newdata$A_MARITL == 4 | newdata$A_MARITL == 5 | newdata$A_MARITL == 6 | newdata$A_MARITL == 7 ] = 0
 
 newdata$PEAFEVER[newdata$PEAFEVER == 2] = 0 #1=YES, 0=NO
 
 newdata$PEIO1COW[newdata$PEIO1COW == 2] = 0 #1=yes, 0= no, previously zeros filtered
 
 newdata$CLWK[newdata$CLWK == 1 | newdata$CLWK == 3] = 0 #0=private/selfemploy
 newdata$CLWK[newdata$CLWK == 2] = 1 #1=gov
 
 newdata$A_UNCOV[newdata$A_UNCOV == 2] = 0 #1=armed force 0=civilian
 
 newdata$COV[newdata$COV == 2] = 0 #0=no 1=yes
 
 newdata$NOW_PUB[newdata$NOW_PUB ==2] = 0 #0=no 1=yes
 
 newdata$DEPPRIV[newdata$DEPPRIV ==2] = 0 #0=no 1=yes
 
 newdata$DEPGRP[newdata$DEPGRP ==2] = 0 #0=no 1=yes
 
 newdata$GRP[newdata$GRP ==2] = 0 #0=no 1=yes
 
 newdata$HIPAID[newdata$HIPAID == 1] = 0 #0=employer paid all premiums
 newdata$HIPAID[newdata$HIPAID == 2 | newdata$HIPAID == 3] = 1 #1= employer did not pay all premiums
 
 newdata$DIR[newdata$DIR == 2] = 0 #1=yes by direct purchase  0=not by direct
 
 newdata$DEPMRK[newdata$DEPMRK == 2] = 0
 
 newdata$DEPMRKS[newdata$DEPMRKS == 2] = 0
 
 newdata$DEPMRKUN[newdata$DEPMRKUN == 2] = 0
 
 newdata$DEPNONM[newdata$DEPNONM == 2] = 0
 
 newdata$CAID[newdata$CAID == 2] = 0
 
 newdata$PCHIP[newdata$PCHIP == 2] = 0
 
 newdata$CHAMPVA[newdata$CHAMPVA == 2] = 0
 
 newdata$VACARE[newdata$VACARE == 2] = 0
 
 newdata$HEA[newdata$HEA == 1 | newdata$HEA == 2] = 0 
 newdata$HEA[newdata$HEA == 3 | newdata$HEA == 4 | newdata$HEA == 5 ] = 1 #bad healtb
 
 print(newdata)
 
 ret_tot = newdata$PNSN_VAL + newdata$RINT_VAL1 + newdata$RINT_VAL2 + newdata$SS_VAL + newdata$SRVS_VAL
 print(ret_tot)
 logret = log(ret_tot)
 print(logret)
 model1 = lm(logret ~ newdata$COV, maindata)
 summary(model1)

