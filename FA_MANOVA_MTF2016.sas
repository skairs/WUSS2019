*****
* Factor analysis and MANOVA in MTF 2016
* S.N. Kairs, 2019
* WUSS 2019, ePoster & Paper
***** ;

/* Edit libname statement to include data directory path and remove < > characters */
libname mydata "<Data Directory Path>" access=readonly;
proc contents data=mydata.da36799p1;
run;

data fa_data;
  set mydata.da36799p1;
  keep V7102 V7107 V7215 V7216 V7221 V7222 V7223 V7226 V7231 V7232 V7233 
 V7253 V7331 V7334 v501 v508 v509 v7202 v1070; 
run;

/* recoding and parceling */
data fa_data_parcels;
  set fa_data;
  * recode for college prep = 1, no = 0;
  if v7222 > 1 then
     v7222=0;
  *recode to move "don't know" to missing (.);
  if v7215 > 6 then
     v7215=.;
  if v7216 > 6 then
     v7216=.;
  *parent education 7215 + 7216;
  parent_ed=v7215 + v7216;
  *remedial school;
  rem_school=v7232 + v7233;
  *population density <- for later MANOVA
    per codebook, additive combination of v508 and v509 yield 
    0 = lowest density, 1 = moderate density, 2 = highest density;
  * using '+' to drop NA values, assuming NA = 0 is misleading;
  pop_density=v508 + v509;
  label parent_ed="Parental education" rem_school="Remedial schooling" 
    pop_density="Population density";
  drop v508 v509 v7215 v7216 v7232 v7233;
run;

title " Descriptive statistics for numeric variables & pop_density";

proc means data=fa_data_parcels n nmiss std min mean median max;
  var V7102 V7107 V7221 V7222 V7223 V7226 V7231 V7253 V7331 V7334 parent_ed
    rem_school pop_density;
    *demographics removed except pop_density;
run;

title;
proc univariate data=fa_data_parcels;
  var V7102 V7107 V7221 V7222 V7223 V7226 V7231 V7253 V7331 V7334 parent_ed 
    rem_school;
  histogram / normal kernel;
run;

proc freq data=fa_data_parcels;
  tables v501 v7202 v1070 pop_density;
run;

/* Missing data check */
Data check;
  set fa_data_parcels;
  array chckmiss{*} V7102 V7107 V7221 V7222 V7223 V7226 V7231 V7253 V7331 
    V7334  parent_ed rem_school v7202 v1070 pop_density;
  missdata=0;

  do i=1 to dim(chckmiss);
    if chckmiss{i}=. then missdata=missdata + 1;
  end;

  if missdata > 0 then 
     anymiss=1;
  else
     anymiss=0;
run;

/*Check for missing data differences by grade*/
proc freq data=check;
  tables anymiss missdata;
run;

proc freq data=check;
  tables anymiss*(v501 v7202 v1070) / chisq;
run;

proc ttest data=check;
  class v501;
  var missdata;
run;

/* Using settings from best FA from exploratory analysis */
Title " Factor Analysis: PCA varimax, parcels";
proc factor data=fa_data_parcels rotate=varimax reorder scree 
    nfactors=3 out=fa_scored;
  var V7102 V7107 V7221 V7222 V7223 V7226 V7231 V7253 V7331 V7334 parent_ed 
    rem_school;
run;

Data manova_data;
  set fa_scored;
  if factor1 ne .;
  label factor1="achievement" factor2="environment" factor3="at-risk 
    behavior";
run;

/*Are the DVs correlated? */
proc corr data=manova_data;
  var factor1 - factor3;
run;

/* MANOVA factors1-3 by gender, race and pop density*/
proc glm data=manova_data plots=all;
  class v7202 v1070 pop_density;
  model factor1 - factor3=v7202 v1070 pop_density;
  CONTRAST 'High-density vs rest' pop_density 1 1 -2;
  Manova h=_all_ / printe printh;
  lsmeans v7202 v1070 pop_density;
  means v7202 v1070 pop_density/ alpha=0.0024 cldiff tukey;
run;
