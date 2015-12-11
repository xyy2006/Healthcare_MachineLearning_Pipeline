#!/bin/bash
# Content: merge the two tables to one big data, which contains the needed columns for response variable, knowledge based predictors and data driven predictors.
# Author: Yang Yang
# Date:"Tue Jul  1 08:57:26 2014"
# Usage: manually
#######################################################################################

source("~/head.r")	# R header, copy it to you, put under home directory.
setwd("/home/310166442/HCUP_data")
#------------the example here use the NY HCUP inpatient databases.--------------#
a<-load("NY_SID_2009_CORE_DTE_allDISPUNIFORM.Rdata")	
b<-load("NY_SID_2009_FULL_joined.Rdata")
str(NY_SID_2009_FULL_joined)
str(NY_SID_2009_CORE_DTE_allDISPUNIFORM)


# # ----------inclusion critria-------------------#
# LOS >=2

# > head6(NY_SID_2009_CORE_DTE_allDISPUNIFORM)
        # KEYCSV VisitLink DaysToEvent LOS Year DISPUNIFORM
# 1 3.620091e+13        NA          NA  41 2009           2
# 2 3.620091e+13        NA          NA   3 2009           6
# 3 3.620091e+13        NA          NA   8 2009           1
# 4 3.620091e+13        NA          NA  29 2009           1
# 5 3.620091e+13        NA          NA   8 2009           1
# 6 3.620091e+13        NA          NA   7 2009           1
# > head10(NY_SID_2009_FULL_joined)
       # PKEYCSV AGE AHOUR AMONTH ANESTH ASOURCE PointOfOriginUB04 ATYPE AWEEKEND
# 1 3.620091e+13  40  1300     NA     40       1                 0     1        0
# 2 3.620091e+13  45   700     NA      0       1                 0     1        1
# 3 3.620091e+13  45   700     NA     20       4                 0     3        0
# 4 3.620091e+13  52  1500     NA     40       5                 0     2        0
# 5 3.620091e+13  52  2200     NA     NA       1                 0     1        0
# 6 3.620091e+13  55  1200     NA      0       4                 0     1        0
  # DISPUNIFORM
# 1           2
# 2           6
# 3           1
# 4           1
# 5           1
# 6           1

NY_SID_2009_HFdata <- merge(NY_SID_2009_CORE_DTE_allDISPUNIFORM,NY_SID_2009_FULL_joined,by="KEYCSV",sort=F)
NY_SID_2009_HFdata %>>% head
NY_SID_2009_HFdata %>>% dim
 # 68288   119
# NY_SID_2009_HFdata %|>% (x ~ x$AGE) %>>% range
# [1]  18 107
# NY_SID_2009_HFdata %|>% (x ~ names(x)) %:>% grep("dispuniform",.,perl=T,ig=T,value=T)
# [1] "DISPUNIFORM.x" "DISPUNIFORM.y"
# > identical(NY_SID_2009_HFdata[,6],NY_SID_2009_HFdata[,16])
# [1] TRUE

#-----------------#
# remove those transferred from/to another hospital.
NY_SID_2009_HFdata_refined <- subset(NY_SID_2009_HFdata,subset = (DISPUNIFORM.x != 2 & DISPUNIFORM.x != 7) )
# > NY_SID_2009_HFdata_refined$DISPUNIFORM.x %>>% table

   # -8     1     5     6    20
    # 3 30396 12908 18468  2805
# > NY_SID_2009_HFdata$DISPUNIFORM.x %>>% table

   # -8     1     2     5     6     7    20
    # 3 30396  2533 12908 18468  1175  2805
# > NY_SID_2009_HFdata_refined %>>% nrow
# [1] 64580

#############################################
###
## some extra data cleansing, 
NY_SID_2009_HFdata_refined <-  mutate(NY_SID_2009_HFdata_refined,N_CM = rowSums(NY_SID_2009_HFdata_refined[grep("CM_",colnames(NY_SID_2009_HFdata_refined),perl=T)]) )	# so we have the count of the CMs for each record.
#---#
# NY_SID_2009_HFdata_refined_copy <- NY_SID_2009_HFdata_refined	# for debugging usage. can be easily restored from here.
NY_SID_2009_HFdata_refined$DX1 <- as.factor(NY_SID_2009_HFdata_refined$DX1)
###
##
#
N_DXn <- 15	# change depending on specific state and year.
#
##
###


###########################################################################
# the merging process for translating DX1~15 to CC labels (not CCS labels)#
###########################################################################
#=========================================================================#
ICD_crosstalk_CC_table <- fread("2009_CondCat_ICD9-CM_Crswlk.txt")
ICD_crosstalk_CC_table_reduced <- ICD_crosstalk_CC_table[,list(CC,`ICD-9-CM`)]
setkey(ICD_crosstalk_CC_table_reduced,"ICD-9-CM")

for (DX_variable in sprintf("DX%d",1:N_DXn)) {
# NY_SID_2009_HFdata_refined <- data.table(NY_SID_2009_HFdata_refined,key = c("DX1","DX2","DX3"))
NY_SID_2009_HFdata_refined <- data.table(NY_SID_2009_HFdata_refined,key = DX_variable)
#
NY_SID_2009_HFdata_refined <- ICD_crosstalk_CC_table_reduced[NY_SID_2009_HFdata_refined]
}	
# data_input_dt2 <- data.table( merge.data.frame(NY_SID_2009_HFdata_refined,ICD_crosstalk_CC_table_reduced,by.x="DX2",by.y="ICD-9-CM",all.x=T) )
# data_input_dt4 <- merge(NY_SID_2009_HFdata_refined,ICD_crosstalk_CC_table_reduced)

#---the product---#
# > NY_SID_2009_HFdata_refined[,(1:6),with=F]
       # ICD-9-CM CC ICD-9-CM.14 CC.14 ICD-9-CM.13 CC.13
    # 1:       NA NA          NA    NA          NA    NA
    # 2:       NA NA          NA    NA          NA    NA
    # 3:       NA NA          NA    NA          NA    NA
    # 4:       NA NA          NA    NA          NA    NA
    # 5:       NA NA          NA    NA          NA    NA
   # ---
# 64576:    V8801 NA       V4576   179        V453   179
# 64577:    V8801 NA       V4579   179        2724    24
# 64578:    V8801 NA       V4582   179       V1251   184
# 64579:    V8801 NA       V4589   179       25000    19
# 64580:    V8801 NA        V462   179       V1083   184

# colnames(NY_SID_2009_HFdata_refined)[match(c("ICD-9-CM","CC"),colnames(NY_SID_2009_HFdata_refined) )] <- c("ICD-9-CM.15","CC.15")	# bad, will lead to copy of the table, so inefficient.

setnames(NY_SID_2009_HFdata_refined,c("ICD-9-CM","CC"), sprintf(c("ICD-9-CM.%d","CC.%d"),N_DXn ) )	# since last X[Y] will always produce the names without suffix.
##############################################
###
##
# less categories when using CC instead of CCS.
# > table(data_input$DXCCS3) %>>% length
# [1] 213
# > table(data_input$DXCCS4) %>>% length
# [1] 217
# > table(data_input$DXCCS5) %>>% length
# [1] 215
# > table(data_input$DXCCS6) %>>% length
# [1] 217
# > table(data_input$CC3) %>>% length
# [1] 0
# > table(data_input$CC.3) %>>% length
# [1] 156
# > table(data_input$CC.4) %>>% length
# [1] 158
# > table(data_input$CC.5) %>>% length
# [1] 158
# > table(data_input$CC.6) %>>% length
# [1] 154
##
###
###############################################
###########################################################################
#=========================================================================#
###########################################################################


#=========================================================================#
# check the data missing level, too much missing will lead to drop of that variables.
# > NY_SID_2009_HFdata_refined[,sapply(.SD,function(x) sum(is.na(x)))]                                       [9/4673]
      # ICD-9-CM.15             CC.15       ICD-9-CM.14             CC.14
            # 48624             48985             44256             44625
      # ICD-9-CM.13             CC.13       ICD-9-CM.12             CC.12
            # 38200             38660             33369             33833
      # ICD-9-CM.11             CC.11       ICD-9-CM.10             CC.10
            # 28000             28574             22545             23149
       # ICD-9-CM.9              CC.9        ICD-9-CM.8              CC.8
            # 15888             16503             10863             11433
       # ICD-9-CM.7              CC.7        ICD-9-CM.6              CC.6
             # 6637              7223              3568              4115
       # ICD-9-CM.5              CC.5        ICD-9-CM.4              CC.4
             # 1596              2119               577               989
       # ICD-9-CM.3              CC.3        ICD-9-CM.2              CC.2
              # 213               535               105               524
       # ICD-9-CM.1              CC.1            KEYCSV       VisitLink.x
                # 0                 0                 0              1170
    # DaysToEvent.x             LOS.x              Year     DISPUNIFORM.x
             # 1170                 0                 0                 0
          # PKEYCSV               AGE             AHOUR            AMONTH
                # 0                 0                 0              1139
           # ANESTH           ASOURCE PointOfOriginUB04             ATYPE
            # 15818             48007                 0                69
         # AWEEKEND     DISPUNIFORM.y              DQTR               DRG
                # 0                 0              1139                 0
            # DRG24          DSHOSPID            DXCCS1            DXCCS2
                # 0                 0                 0               105
           # DXCCS3            DXCCS4            DXCCS5            DXCCS6
              # 213               577              1596              3568
           # DXCCS7            DXCCS8            DXCCS9           DXCCS10
             # 6637             10863             15888             22545
          # DXCCS11           DXCCS12           DXCCS13           DXCCS14
            # 28000             33369             38200             44256
          # DXCCS15     DaysToEvent.y            FEMALE           HCUP_ED
            # 48624              1170                 0                 0
          # HCUP_OS            HOSPST          Homeless             LOS.y
                # 0                 0                 0                 0
              # MDC             MDC24          MDNUM1_R          MDNUM2_R
                # 0                 0                13             25858
        # MEDINCSTQ               NDX               NPR              PAY1
             # 4173                 0                 0                 0
             # PAY2        PL_UR_CAT4               PR1            PRCCS1
             # 9761              1198             25845             25845
           # PRCCS2            PRCCS3            PRCCS4            PRCCS5
            # 41116             49467             55007             58280
           # PRCCS6            PRCCS7            PRCCS8            PRCCS9
            # 60344             61683             62619             63210
          # PRCCS10           PRCCS11           PRCCS12           PRCCS13
            # 63596             63871             64055             64187
          # PRCCS14           PRCCS15              RACE            TOTCHG
            # 64317             64386               223                 0
      # VisitLink.y       ZIPINC_QRTL            BMONTH             BYEAR
             # 1170              4173              1139              1139
              # ZIP           CM_AIDS        CM_ALCOHOL        CM_ANEMDEF
               # 35                 0                 0                 0
          # CM_ARTH        CM_BLDLOSS            CM_CHF       CM_CHRNLUNG
                # 0                 0                 0                 0
          # CM_COAG        CM_DEPRESS             CM_DM           CM_DMCX
                # 0                 0                 0                 0
          # CM_DRUG          CM_HTN_C        CM_HYPOTHY          CM_LIVER
                # 0                 0                 0                 0
        # CM_LYMPH          CM_LYTES           CM_METS          CM_NEURO
                # 0                 0                 0                 0
         # CM_OBESE           CM_PARA       CM_PERIVASC          CM_PSYCH
                # 0                 0                 0                 0
      # CM_PULMCIRC       CM_RENLFAIL          CM_TUMOR          CM_ULCER
                # 0                 0                 0                 0
         # CM_VALVE       CM_WGHTLOSS              N_CM
                # 0                 0                 0
#--------------check the DXCCSn and PRCCSn content-------------------------#
# lapply(NY_SID_2009_HFdata_refined[,c(sprintf("DXCCS%d",1:N_DXn)), with=F ], function(x) names(table(x)) ) %>>% unlist %>>% table
# # same effect as NY_SID_2009_HFdata_refined[,list(DXCCS1,DXCCS2),with=T],
# # however we need to use string here.
  # 1  10 100 101 102 103 104 105 106 107 108 109  11 110 111 112 113 114 115 116
 # 14  14  14  14  14  14  14  14  14  14  15  13  14  14  14  12  14  14  14  11
# 117 118 119  12 120 121 122 123 124 125 126 127 128 129  13 130 131 132 133 134
 # 14  14  14  12  14  14  14  14   3  14  14  14  14  13  14  14  14  14  14  14
# 135 136 137 138 139  14 140 141 142 143 144 145 146 147 148 149  15 151 152 153
 # 14  14  14  14  14  14  14  14   3  14  12  14  14  14  10  14  14  14  14  14
# 154 155 156 157 158 159  16 160 161 162 163 164 165 166 167 168 169  17 170 171
 # 14  14  14  14  14  14  12  14  14  14  14  14  14  14  14  10   2   8  13  10
# 172 173 175 176  18  19 195 197 198 199   2  20 200 201 202 203 204 205 206 207
  # 9  12  14   6  13  14   5  14  14  14  14   5  14  14  14  14  14  14  14  14
# 208 209  21 210 211 212 213 214 215 216 217  22 224 225 226 227 228 229  23 230
 # 12  14   7  14  14  14  14  11  14   9  14  14   1  12  13   3   6  14  14  13
# 231 232 233 234 235 236 237 238 239  24 240 241 242 243 244 245 246 247 248 249
 # 13  12  10  13  14  14  14  14  14  14   9   4  14   5  14  14  14  14  13  14
 # 25 250 251 252 253 254 255 256 257 258 259  26  27  28  29   3  30  31  32  33
 # 13  14  14  14  14   1  14  13  14   2  14  13  14  11  14  14   7   7  14  14
 # 34  35  36  37  38  39   4  40  41  42  43  44  45  46  47  48  49   5  50  51
  # 3   7  13  12  14  14  14  10  14  13  13  14   6  13  14  14  14  14  14  14
 # 52  53  54  55  57  58  59   6  60  61  62  63  64 650 651 652 653 654 655 656
 # 14  14  14  14   9  14  14  14  14  14  14  14  14  13  14  10  14  14   6   2
# 657 658 659 660 661 662 663 670   7  76  77  78  79   8  80  81  82  83  84  85
 # 14  13  14  14  14  11  14  14  14   4   3  13  14  14  12  14  14  14  14  14
 # 86  87  88  89   9  90  91  92  93  94  95  96  97  98  99
 # 14  14  14  14  13  14  14  10  14  14  14  14  14  14  15
# #------#
# lapply(NY_SID_2009_HFdata_refined[,c(sprintf("PRCCS%d",1:N_DXn)), with=F ], function(x) names(table(x)) ) %>>% unlist %>>% table
  # 1  10 100 101 102 103 105 108 109  11 110 111 112 113 114 115 116 117 118 119
  # 2   3  10   7   7   4   2  15   6   7   9  12   4   1   1   2   3   5   4   2
 # 12 120 124 125 128 129 130 131 132 142 143 145 146 147 148  15 153 154 155 156
  # 4   2   1   1   2   1   4   2  10   5   1   2   3   2   2   3   4   1  11   4
# 157 159 160 161 162 163 164 165 166 167 168 169  17 170 171 172 173 174 176 177
  # 7   5  11   5   3   8   2   5   3   1  10  14   1   6   8   4  12  14   1  15
# 178 179  18 180 181 182 183 184 185 186 187 188 189  19 190 191 192 193 194 195
 # 14  15   1  13   1   3  11   2  12   3   1  10  13   2  13  15  15  15  11  13
# 196 197 198 199   2  20 200 201 202 203 204 205 206 207 208 209  21 210 211 212
 # 14  15  15  15   3   1   5  14  14  15  15  15   9   8  13  13   1  10   4  14
# 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229  23 231  26
 # 15  12  15  15  15  13   8   1  12  15  15   8  15  15  15  15   5   1  15   6
 # 27  28  29   3  31  32  33  34  35  36  37  38  39   4  40  41  42  43  44  45
 # 10   2  10   1   8   5   4   8  14   3  15   4  15   8  12  14  10  10   8   7
 # 47  48  49   5  50  51  52  53  54  55  56  57  58  59  60  61  62  63  65  67
 # 15  14  15   7  15   2   3   1  15   3   1   5  15   6   5  15  15  15  11   5
 # 69   7  70  71  72  73  75  76  77  78  79   8  81  82  83  84  85  86  87  88
  # 5   2  14  14   2   8   3  13  10   6   1   6   2   5   9   3   4   3   4  13
 # 89   9  90  91  92  93  94  95  96  97  98  99
  # 1   3   5   7   5  14   2  14   6   5  10   5
#=========================================================================#


###########################################################################
#reformat the DX secondary value to become binary factors for each row,
#any we will have new variables there to use.
###########################################################################
#=========================================================================#
DX_secondary_items <- NY_SID_2009_HFdata_refined[,sprintf("DXCCS%d",2:N_DXn),with=F] %>>% unlist %>>% unique %>>% na.omit %>>% sort	# from 2 ~ N_DXn, all are treated equally as DX secondary.
names(DX_secondary_items)   <- sprintf("DX_secondary_value%d",DX_secondary_items)
#-----insert the newly created variables-----------#
NY_SID_2009_HFdata_refined[,names(DX_secondary_items):=0]	# 0 is the default values, await insert true values.
# eachRow_values <- apply(NY_SID_2009_HFdata_refined[,sprintf("DXCCS%d",2:N_DXn),with=F],1,unique)	# get the DX_secondary_items for each row. For debug usage to check whether the NY_SID_2009_HFdata_refined has changed or not.
#---a sub func to insert true values---#
insertValues <- function(data = NY_SID_2009_HFdata_refined,index = 1){ # data itself changed for every index, and so happened on real backend data - NY_SID_2009_HFdata_refined here.
	# x is the string Name of a column, e.g., DXCCS2.
	# browser()
	# > data[1] %>>% class
	# [1] "data.table" "data.frame"

	row_values <- data[index,sprintf("DXCCS%d",2:N_DXn),with=F] %>>% unlist %>>% unique %>>% na.omit
	if (length(row_values) == 0) return()	# exit the function now.
	#
	data[index,sprintf("DX_secondary_value%d",row_values):=1]	# assign new value.
	return()
}
#-----execute in serial only-----
# temp <- foreach (index = 1:nrow(NY_SID_2009_HFdata_refined),.verbose=T ) %do% { # prohibit paralle here, because NY_SID_2009_HFdata_refined should be local copy to modify!
	# insertValues(NY_SID_2009_HFdata_refined,index=index)
# }
#---#
# mclapply(1:nrow(NY_SID_2009_HFdata_refined), insertValues, data = NY_SID_2009_HFdata_refined,mc.cores=detectCores())	# even fork, each slave has its own copied "NY_SID_2009_HFdata_refined", changes occured at each slave will not be saved and reflected on master local copy of "NY_SID_2009_HFdata_refined".

#---#
# so sapply is most practical way here, do it serially but avoiding for loops.
sapply(1:nrow(NY_SID_2009_HFdata_refined), insertValues, data = NY_SID_2009_HFdata_refined)	# if instead for one billion record size, we will consider do we want the speed or memory-efficient?
#=========================================================================#
###########################################################################
###########################################################################


###########################################################################
# do the similar thing on PRCCSn, remember there is missing values in PRCCS1
# so we need to assign missing to no procedures.
###########################################################################
#=========================================================================#
# ---1st, we will want to assign 0 to NA in PRCCS1, since 0 is out of 1~213, can represent "No procedures done"---#
NY_SID_2009_HFdata_refined[,(.SD),.SDcols="PRCCS1"] [,is.na(.SD)] %:>% NY_SID_2009_HFdata_refined[c(.),"PRCCS1":=0,with=F]	# assign 0, which represents "No procedures done"
NY_SID_2009_HFdata_refined[,(.SD),.SDcols="PRCCS1"] [,sum(is.na(.SD))]
# 0
###
##
#
N_PRn <- 15	# change depending on specific state and year.
#
##
###
PR_secondary_items <- NY_SID_2009_HFdata_refined[,sprintf("PRCCS%d",2:N_PRn),with=F] %>>% unlist %>>% unique %>>% na.omit %>>% sort	# from 2 ~ N_PRn, all are treated equally as DX secondary.
names(PR_secondary_items)   <- sprintf("PR_secondary_value%d",PR_secondary_items)
#-----insert the newly created variables-----------#
NY_SID_2009_HFdata_refined[,names(PR_secondary_items):=0]	# 0 is the default values, await insert true values.
eachRow_values <- apply(NY_SID_2009_HFdata_refined[,sprintf("PRCCS%d",2:N_PRn),with=F],1,unique)	# get the DX_secondary_items for each row. For debug usage to check whether the NY_SID_2009_HFdata_refined has changed or not.

#---a sub func to insert true values---#
insertValues_PRversion <- function(data = NY_SID_2009_HFdata_refined,index = 1){ # data itself changed for every index, and so happened on real backend data - NY_SID_2009_HFdata_refined here.
	# x is the string Name of a column, e.g., DXCCS2.
	# browser()
	# > data[1] %>>% class
	# [1] "data.table" "data.frame"

	row_values <- data[index,sprintf("PRCCS%d",2:N_PRn),with=F] %>>% unlist %>>% unique %>>% na.omit
	if (length(row_values) == 0) return()	# exit the function now.
	#
	data[index,sprintf("PR_secondary_value%d",row_values):=1]	# assign new value.
	return()
}
#---#
# so sapply is most practical way here, do it serially but avoiding for loops.
sapply(1:nrow(NY_SID_2009_HFdata_refined), insertValues_PRversion, data = NY_SID_2009_HFdata_refined)	# if instead for one billion record size, we will consider do we want the speed or memory-efficient?

#=========================================================================#
###########################################################################
###########################################################################


###########################################################################
# do the similar thing on CC.n, almost the same as DXCCSn, since they are just
# different cateorization systems on ICD-9-CM.
###########################################################################
#=========================================================================#
# ---1st, we will want to assign 0 to NA in PRCCS1, since 0 is out of 1~213, can represent "No procedures done"---#
###
##
#
N_CCn <- 15	# change depending on specific state and year.
#
##
###
CC_secondary_items <- NY_SID_2009_HFdata_refined[,sprintf("CC.%d",2:N_CCn),with=F] %>>% unlist %>>% unique %>>% na.omit %>>% sort	# from 2 ~ N_CCn, all are treated equally as DX secondary.
names(CC_secondary_items)   <- sprintf("CC_secondary_value%d",CC_secondary_items)
#-----insert the newly created variables-----------#
NY_SID_2009_HFdata_refined[,names(CC_secondary_items):=0]	# 0 is the default values, await insert true values.
eachRow_values <- apply(NY_SID_2009_HFdata_refined[,sprintf("CC.%d",2:N_CCn),with=F],1,unique)	# get the DX_secondary_items for each row. For debug usage to check whether the NY_SID_2009_HFdata_refined has changed or not.

#---a sub func to insert true values---#
insertValues_CCversion <- function(data = NY_SID_2009_HFdata_refined,index = 1){ # data itself changed for every index, and so happened on real backend data - NY_SID_2009_HFdata_refined here.
	# x is the string Name of a column, e.g., DXCCS2.
	# browser()
	# > data[1] %>>% class
	# [1] "data.table" "data.frame"

	row_values <- data[index,sprintf("CC.%d",2:N_CCn),with=F] %>>% unlist %>>% unique %>>% na.omit
	if (length(row_values) == 0) return()	# exit the function now.
	#
	data[index,sprintf("CC_secondary_value%d",row_values):=1]	# assign new value.
	return()
}
#---#
# so sapply is most practical way here, do it serially but avoiding for loops.
sapply(1:nrow(NY_SID_2009_HFdata_refined), insertValues_CCversion, data = NY_SID_2009_HFdata_refined)	# if instead for one billion record size, we will consider do we want the speed or memory-efficient?

#=========================================================================#
###########################################################################
###########################################################################


#--------------------#
###
##
# now we have NY_SID_2009_HFdata_refined with every factors reformated,
# then rename to make them more straightforward at the first galance by someone else.
DXCCSn_names_old <- grep("DX_secondary",names(NY_SID_2009_HFdata_refined),perl=T,value=T)
DXCCSn_names_new <- sub("DX","DXCCS",DXCCSn_names_old,perl=T)
#
PRCCSn_names_old <- grep("PR_secondary",names(NY_SID_2009_HFdata_refined),perl=T,value=T)
PRCCSn_names_new <- sub("PR","PRCCS",PRCCSn_names_old,perl=T)
#
DXCCnoSn_names_old <- grep("CC_secondary",names(NY_SID_2009_HFdata_refined),perl=T,value=T)
DXCCnoSn_names_new <- sub("CC","DXCCnoS",DXCCnoSn_names_old,perl=T)
#
setnames(NY_SID_2009_HFdata_refined,DXCCSn_names_old,DXCCSn_names_new)
setnames(NY_SID_2009_HFdata_refined,PRCCSn_names_old,PRCCSn_names_new)
setnames(NY_SID_2009_HFdata_refined,DXCCnoSn_names_old,DXCCnoSn_names_new)

#--------generate DIED variable for survival data analysis purpose----#
# > NY_SID_2009_HFdata_refined %:>% grep("UNI",names(.),value=T)
# [1] "DISPUNIFORM.x" "DISPUNIFORM.y"
setkeyv(NY_SID_2009_HFdata_refined,"DISPUNIFORM.x")
if (! any(grepl("died",colnames(NY_SID_2009_HFdata_refined),perl=T,ignore=T)) ){
	NY_SID_2009_HFdata_refined[,died:=0]
	setkeyv(NY_SID_2009_HFdata_refined,"DISPUNIFORM.x")
	NY_SID_2009_HFdata_refined[DISPUNIFORM.x==20,died:=1]	# DISPUNIFORM ==20 corresponding to died in hospital.
}
#---and export the names to a list for excel usage later---#
write.table(names(NY_SID_2009_HFdata_refined),file="variableNames_all.txt",quote=F,row.names=F,col.names=F)




save_Rdata("NY_SID_2009_HFdata_refined")