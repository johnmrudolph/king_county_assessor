library(data.table)
library(sqldf)
library(stringr)
library(ggplot2)

#import resbldg and lookup files
ResBldg = fread('EXTR_ResBldg.csv')
AptComplex = fread('EXTR_AptComplex.csv')
CondoComplex = fread('EXTR_CondoComplex.csv')
LookUp = fread('EXTR_LookUp.csv')

#filter for zip codes
#need to extract zip from apt address - will drop some
AptComplex$ZipCode = str_sub(AptComplex$Address,-5,-1)
SeattleZip = c(
               #Seattle 
               98101, 98102, 98103, 98104, 98105, 98106, 98107, 
               98108, 98109, 98112, 98115, 98116, 98117, 98118, 
               98119, 98121, 98122, 98125, 98126, 98133, 98134, 
               98136, 98144, 98146, 98154, 98164, 98174, 98177, 
               98178, 98195, 98199,
               #Burien
               98146, 98148, 98166, 98168,
               #Lake Forrest Park
               98155,
               #Tukwilla
               98138, 98168, 98178, 98188,
               #Shoreline
               98133, 98155, 98160, 98177,
               #SeaTac
               98148, 98158, 98168, 98188, 98198
               )

#subset building data by zip            
ResBldg_filt = ResBldg[ZipCode %in% SeattleZip]
CondoComplex_filt = CondoComplex[ZipCode %in% SeattleZip]
AptComplex_filt = AptComplex[ZipCode %in% SeattleZip]
#subset LookUp to get fuel types
LookUp_filt = LookUp[as.numeric(LUType) == 84]

#clear up space
rm(LookUp, ResBldg, AptComplex, CondoComplex)

#combine apt and condo datasets
MultiComplex = as.data.table(sqldf('
  select "CONDO" as Type, Major, ComplexDescr, NbrBldgs, NbrStories, NbrUnits,
  AvgUnitSize, YrBuilt, EffYr, AptConversion, ZipCode
  from CondoComplex_filt
  union
  select "APT" as Type, Major, ComplexDescr, NbrBldgs, NbrStories, NbrUnits,
  AvgUnitSize, YrBuilt, EffYr, "N" as AptConversion, ZipCode
  from AptComplex_filt                    
'))

#clear up space
rm(AptComplex_filt, CondoComplex_filt)

#get singlefamily stats by year
ResBldg_Count = as.data.table(sqldf(
  'select YrBuilt, Avg(SqFtTotLiving) as AvgSqFt, Count(Address) as ResUnits
  from ResBldg_filt
  where YrBuilt >= 1980 and YrBuilt <= 2016
  group by YrBuilt'
  ))

#get multifamily stats by year
MultiComplex_Count = as.data.table(sqldf(
  'select YrBuilt, Sum(NbrUnits) as MultiUnits, 
  Avg(AvgUnitSize) as AvgUnitSize, Count(Major) as MultiCount
  from MultiComplex
  where YrBuilt >= 1980 and YrBuilt <= 2016
  group by YrBuilt'
))

#join multifamily stats by year
TotalUnits_Count = as.data.table(sqldf(
   'select l.*, r.*, l.MultiUnits + r.ResUnits as TotalUnits
   from MultiComplex_Count l left join
   ResBldg_Count r on
   l.YrBuilt = r.YrBuilt'
))

#create variable for multi to single family ratio
TotalUnits_Count$MultiRatio = as.numeric(
  TotalUnits_Count$MultiUnits/TotalUnits_Count$ResUnits
)

#create variable for average size
TotalUnits_Count$WtdAvgSqFt = as.numeric(
  TotalUnits_Count$MultiUnits/TotalUnits_Count$TotalUnits*
  as.numeric(TotalUnits_Count$AvgUnitSize) + 
  TotalUnits_Count$ResUnits/TotalUnits_Count$TotalUnits*
  TotalUnits_Count$AvgSqFt
)

#graph multi to single family ratio
ggplot(data=TotalUnits_Count, aes(x=YrBuilt, y=MultiRatio, group=1)) +
  geom_point() + geom_smooth(span = 0.3) +
  scale_x_discrete(breaks = seq(1980, 2016, 5))

#graph weighted average square footage
ggplot(data=TotalUnits_Count, aes(x=YrBuilt, y=WtdAvgSqFt, group=1)) +
  geom_point() + geom_smooth(span = 0.3) +
  scale_x_discrete(breaks = seq(1980, 2016, 5))

#graph multi vs single family total units
ggplot() + 
  geom_smooth(data=TotalUnits_Count, aes(x=YrBuilt, y=MultiUnits, group=1), 
              color = "red", span=0.3) +
  geom_smooth(data=TotalUnits_Count, aes(x=YrBuilt, y=ResUnits, group=1), 
              color = "blue", span=0.3) +
  scale_x_discrete(breaks = seq(1980, 2016, 5))