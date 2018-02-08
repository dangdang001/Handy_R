libname ulcer "Z:\Donglei\20161003-Marginal ulcers following bariatric surgery\data";
%let dir=Z:\Donglei\20161003-Marginal ulcers following bariatric surgery;
libname sparcs "Z:\Yaqi\20151021-11 surgeries and readmission search\Sparcs with correct admssion date format";
%include "Z:\Donglei\Sample sas codes\FrequencyTable.sas";
%include "&dir.\code\0.format.sas";
option validvarname=any;


*20161206 define time to last follow up(for non-mu) or time to event(for mu);


*for MU followup;
proc sql;
create table mupatient as 
select a.*
from ulcer.all as a, ulcer.des_define3 as b
where compress(a.newupid)=compress(b.newupid) and b.mu=1  and a.first~=1 and a.ulcer=1;
quit;*4044;

*for non-MU followup;
proc sql;
create table nonmupatient as 
select a.*
from ulcer.all as a, ulcer.des_define3 as b
where compress(a.newupid)=compress(b.newupid) and b.mu=0 and a.first~=1 and a.ulcer=0;
quit;*579;


proc sort data=mupatient;
by newupid day;run;
proc sort data=nonmupatient;
by newupid day;run;

data mupatient_first;
set mupatient;
by newupid;
if first.newupid;
run;*2424 mu patients' first mu follow-up;

data nonmupatient_last;
set nonmupatient;
by newupid;
if last.newupid;
run;*546 non-mu paitents with follow-up;


proc sql;
create table time_to_event1 as
select a.*, b.day-a.day as first_mu
from ulcer.des_define3 as a left join mupatient_first as b
on compress(a.newupid)=compress(b.newupid);
quit;

proc sql;
create table time_to_event2 as
select a.*, b.day-a.day as last_follow
from time_to_event1 as a left join nonmupatient_last as b
on compress(a.newupid)=compress(b.newupid);
quit;


data ulcer.des_time_mu;
set time_to_event2;
format disday date9.;
disday = input(compress(put(disdt,10.)),ANYDTDTE10.);
if last_follow~=. then time_mu=last_follow;
else if first_mu~=. then time_mu=first_mu;
else if first_mu=. and last_follow=. then time_mu=disday-day;
run;



data ulcer.des_time_mu1;
format severe yn.;
label severe="Severe Complication";
set ulcer.des_time_mu;
if payer~=5;
if PulmonaryEmb=1 or reoperHemorrhage=1 or Anastomotic=1 or Abscess=1 or Dehiscence=1 or  Ventilation=1 or Tracheostomy=1 or 
RespFailure=1 or RespArrest=1 or MI=1 or CardiacArrest=1 or
RenalFailC=1 or Shock=1 or hemorrhage=1 then severe=1;else severe=0;
label length_of_stay="Length of Stay"
disyear="Year";
disch_char=PUT(disch,30.);
length_of_stay=disday-day;
disyear=year(disday);
run;*35075;

data check;
set ulcer.des_time_mu1;
if first_mu=. and last_follow=. ;
run;*32107 out of 35075;

data check1;
set ulcer.des_time_mu1;
if first_mu=. and last_follow~=. ;
run;*544 out of 35075;

data check2;
set ulcer.des_time_mu1;
if first_mu~=. and last_follow=. ;
run;*2424 out of 35075;


proc means data=ulcer.des_time_mu1;
var time_mu last_follow first_mu;
run;


data ulcer.des_time_mu2;
format race_new race_mu.;
format payer_new payer_muc.;
set ulcer.des_time_mu1;
label race_new="Race/ethnicity"
payer_new="Insurance";
if race2=1 then race_new=1;else race_new=2;
if payer=1 or payer=2 then payer_new=1;
else if payer=3 then payer_new=2;
else payer_new=3;
run;

%include "Z:\Donglei\Sample sas codes\descriptive.sas";

%let char=age_group gender race_new region1 payer_new;
%let comb=any_cb CHF VALVE  PULMCIRC PERIVASC
HTN_C    PARA     NEURO    CHRNLUNG Diabetes  HYPOTHY  RENLFAIL LIVER    ULCER
AIDS     LYMPH    METS     TUMOR    ARTH
COAG     OBESE    WGHTLOSS LYTES    BLDLOSS
ANEMDEF  ALCOHOL  DRUG     PSYCH    DEPRESS tobacco;

%let comp=any_cp severe PulmonaryEmb reoperHemorrhage Anastomotic Abscess Dehiscence  Ventilation Tracheostomy 
Pneumonia RespFailure RespArrest PulmonaryEdema  Collapsed  MI CardiacArrest  CardiacComp
RenalFailC Shock bacterialdisease hypertnC atherosclerosis phlebitis enteritis intestinal 
systemicinflamm nervous vascular digestive surgicalerror hemorrhage liverC;

*T1: chisq table for characteristics;
%categorical_all(outcome = mu, data = ulcer.des_time_mu2, outdata = ulcer.des_out, 
                 cov_list = &char. &comb. &comp. , total_pct=1, colpct = 1,exact=1);

data aa;
set ulcer.des_out;
if level='1Yes' or level='1';
event=input(scan('1Yes'n,1,'('),8.);
run;

proc sql;
select variable
into: var_select separated by " "
from aa
having event>=10 and variable~="OBESE";
quit;

%put &var_select.;
%let char=mu time_mu newupid age gender race_new payer_new length_of_stay last_follow first_mu;

data out;
retain &char. &var_select.;
set ulcer.des_time_mu2;
keep &char. &var_select.;
FORMAT _ALL_ ; 
run;

proc export data=out
   outfile="&dir.\temp\time_to_mu.csv"
   dbms=csv
   replace;
run;



*20161229: researching the follow-up records based on whole sparc datasets;

/*ulcer.any_clean is any follow-up records for bypass records(not included)*/

*now for MU patients, their time to first MU were already defined;
*next need to define the last follow-up records for Non-MU patients;


proc sort data=ulcer.any_clean;
by newupid descending day;
run;

data ulcer.any_clean_last;
set ulcer.any_clean;
by newupid;
if first.newupid;
run;


proc sql;
create table ulcer.nonmu_follow as 
select a.*, (c.day-a.day) as diffdate_nonmu
from ulcer.des_time_mu3 as a left join ulcer.any_clean_last as c
on compress(c.newupid)=compress(a.newupid);
quit;

data ulcer.des_time_mu4;
set ulcer.nonmu_follow;
if first_mu=. then last_follow_new=diffdate_nonmu;
run;

data ulcer.des_time_mu_final;
set ulcer.des_time_mu4;
if first_mu~=. then time_mu=first_mu;
else if last_follow_new~=. then time_mu=last_follow_new;
else time_mu=disday-day;
run;

proc freq data=ulcer.des_time_mu_final;
table time_mu;
run;

data check;
set ulcer.des_time_mu_final;
if first_mu=. and last_follow_new=. ;
run;*5304 out of 35075;

data check1;
set ulcer.des_time_mu_final;
if first_mu=. and last_follow_new~=. ;
run;*27347 out of 35075;


data aa;
set ulcer.des_out;
if level='1Yes' or level='1';
event=input(scan('1Yes'n,1,'('),8.);
run;

proc sql;
select variable
into: var_select separated by " "
from aa
having event>=10 and variable~="OBESE";
quit;

%put &var_select.;
%let char=mu time_mu newupid age gender race_new payer_new length_of_stay last_follow first_mu;

data out;
retain &char. &var_select.;
set ulcer.des_time_mu_final;
keep &char. &var_select.;
FORMAT _ALL_ ; 
run;

proc export data=out
   outfile="&dir.\temp\time_to_mu.csv"
   dbms=csv
   replace;
run;
