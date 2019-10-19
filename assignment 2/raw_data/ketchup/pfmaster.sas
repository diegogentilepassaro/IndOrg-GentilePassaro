**  pfmaster.sas (taken in part from g_reg.sas (02/20/90));


libname d '/acn1/edk/ketchup';
libname library '/acn1/edk/formats';
filename wklyprc '/acn1/edk/programs/wklyprc_gen.sas';
filename algfin '/acn1/edk/programs/prcalg_fin.sas';
filename dips '/acn1/edk/programs/dips.sas';
filename dstep '/acn1/edk/programs/dstep.sas';
filename finish '/acn1/edk/programs/finish.sas';
filename scu '/acn1/edk/programs/scu.sas';
filename scoupval '/acn1/edk/programs/scoupval.sas';
filename inclsid '/acn1/edk/programs/include_sid.sas';
option ls=80 ; 

**  &ino= hp-file 
    &in = reduced version of hp-file with variables needed for algorithm 
    &price = price output file with daily price of date/sid combination
             when there is purchase
    &dprice = price output file with daily price of date/sid combination
              everyday
    &wprice = price output file with weekly price of week/sid combination
              everyweek
   ;

** Set macro variables;

%let mkt = 1;
%let ino=d.hp&mkt;
%let in = chp&mkt ;
%let price=cprice&mkt;
%let sid = chain;
%let dprice=cdprice;
%let wprice=cwprice&mkt;
%let wp = wp ;
%let maxdeal = 3;
%let pid = upc;


** Extract and create only useful variables
   Include values of &pid with Market Share ge 1%;

data newhp;
   set &ino(rename=(scval=scv mcval=mcv));
   if (&pid=13000001250 or &pid=27000382580 or &pid=24000004100 or
      &pid=13000001280 or &pid=75450035950 or &pid=13000001070 or
      &pid=13000001210 or &pid=75450035890 or &pid=41130110720 or
      &pid=13000001240 or &pid=70253286280 or &pid=75450035970 or
      &pid=75450035990 or &pid=41130110640 or &pid=24000007220)
      and not(sid=32 or sid=33 or sid=34 or sid=35);
   scval=0; mcval=0;
   if (scunits ne 0) then do;
      scval=scv/scunits;
   end;
   if (mcunits ne 0) then do;
      mcval=mcv/mcunits;
   end;
   keep id &pid sid date chain market units price adtype
        endd frontd ind otherd mcunits mcval scunits scval;

proc sort data=newhp;
   by &pid &sid date price;


*******************************************
*****  Beginning of process to apply  *****
*****  the price algorithm to 'price' *****
*******************************************;

** Programs involved are wklyprc_gen.sas,
   dips.sas, dstep.sas, finish.sas, and
   prcalg_fin.sas;
** The algorithm will first be applied to data which
   may be done by chain.  Which chains to do is to be
   determined beforehand by the programmer;

%let num=1;

data newhpa;
   set newhp;
   if chain=4 or chain=5;

proc sort data=newhpa;
   by &pid &sid date price;

data &in;
   set newhpa(keep=&sid &pid date price units);
   retain sumunits;
   by &pid &sid date price;
   if first.date or first.price then sumunits=0;
   sumunits=sumunits+units;
   if last.price then do;
      drop units;
      rename sumunits=units;
      output;
   end;

** Use wklyprc_gen.sas to generate the weekly prices;

%include wklyprc ;

** The programs dips.sas, dstep.sas, and finish.sas are
   used to perform the actual price algorithm.  Three
   iterations of dips.sas and dstep.sas should be
   sufficient.  Only one iteration of finish.sas is
   necessary;

%include dips ;
%include dips ;
%include dips ;
%include dstep ;
%include dstep ;
%include dstep ;
%include finish ;
 
** Use prcalg_fin.sas to create a daily price file,
   i.e. &dprice, which will have the original 'price'
   variable(using daily modes as the values) and a
   smoothed price variable 'reg';

%include algfin ;



***
*** Now run the price algorithm for those chains which
*** must be done by SID;
***;

%let sid=sid;
%let num=2;

data newhpb;
   set newhp;
   if (chain ne 4) and (chain ne 5);

proc sort data=newhpb;
   by &pid sid date price;

data &in;
   set newhpb(keep=&sid &pid date price units);
   retain sumunits;
   by &pid &sid date price;
   if first.date or first.price then sumunits=0;
   sumunits=sumunits+units;
   if last.price then do;
      drop units;
      rename sumunits=units;
      output;
   end;

%include wklyprc;

%include dips;
%include dips;
%include dips;
%include dstep;
%include dstep;
%include dstep;
%include finish;

%include algfin;

** Put the variable 'chain' back in the datset;

data &dprice.2;
   set &dprice.2;
   length chain 4;
   chain=put(sid, chain.);


******************************************
*****  Beginning of process to deal  *****
*****  with advertising and coupons  *****
******************************************;


** Create indicators, per observation, as to whether
   an ad was in effect, for line and major types.
   Also an indicator for whether a display was made.
   Note that this is done by sid and NOT by chain;

data ads;
   set newhp;
   length major   4
          line    4
          display 4;
   major=(adtype='M');
   line=(adtype='L');
   display=(endd='Y' or frontd='Y' or ind='Y' or otherd='Y');
   keep &pid chain sid date major line display;
 
** Using the summarizing algorithm, create a daily value 
   for the variables in the file work.ads.  Then create
   an indicator which is 1 if the daily value is greater than
   zero;

proc sort data=ads;
   by &pid chain sid date;

data ads2;
   set ads;
   retain smajor sline sdisplay;
   by &pid chain sid date;
   if first.date then do;
      smajor=0; sline=0; sdisplay=0;
   end;
   smajor=smajor+major;
   sline=sline+line;
   sdisplay=sdisplay+display;
   if last.date then do;
      drop major line display;
      output;
   end;

data ads2;
   set ads2;
   length major   4
          line    4
          display 4;
   if smajor gt 0 then major=1;
   else if smajor=0 then major=0;
   if sline gt 0 then line=1;
   else if sline=0 then line=0;
   if sdisplay gt 0 then display=1;
   else if sdisplay=0 then display=0;
   keep &pid chain sid date major line display;



** Use the summarizing algorithom on the infile
   to sum the mcunits and scunits for each day;

proc sort data=newhp;
   by &pid chain sid date ;

data untmpa;
   set newhp;
   retain sumunits summc sumsc;
   by &pid chain sid date ;
   if first.date then do;
      sumunits=0; summc=0; sumsc=0;
   end;
   sumunits=sumunits+units;
   summc=summc+mcunits;
   sumsc=sumsc+scunits;
   if last.date then do;
      drop units mcunits scunits;
      rename sumunits=units
             summc=mcunits
             sumsc=scunits;
      output;
   end;
   
** Use 'proc means' to find the average manufacturer's
   coupon value, where the mean is taken using all
   nonzero values for a date, within a SID, within the
   chain, within the &pid;

data settmp;
   set newhp;
   if mcval ne 0;
   keep &pid chain sid date mcval;

proc sort data=settmp;
   by &pid chain sid date;

proc means data=settmp noprint;
   by &pid chain sid date;
   var mcval;
   output out=untmpb(drop=_type_ _freq_) mean=tamcv;

data untmp;
   merge untmpa untmpb;
   by &pid chain sid date;
   amcv=round(tamcv,.01);
   if amcv=. then amcv=0;

   
** Create indicators for store and manufacturer coupons, but retain
   the scunits and mcunits;

data untmp;
   set untmp;
   length scind 4
          mcind 4;
   if scunits gt 0 then scind=1;
   else if scunits=0 then scind=0;
   if mcunits gt 0 then mcind=1;
   else if mcunits=0 then mcind=0;
   keep &pid chain sid date units amcv mcunits mcind scunits scind;


** Use the program scu.sas to extend the coupon indicator
   for all days of a week in which there was a store
   coupon presented;
** Use the program scoupval.sas to create weekly values for
   scval if one of the dates for that week in the HP file has at
   least one nonzero value of scval;
** Note that, again, this will have to be done in two steps.
   First the values to be done by chain, then those to be
   done by sid;

%let num=1;
%let sid=chain;
%let sub=a;

data untmp1;
   set untmp;
   if chain=4 or chain=5;

%include scu;
%include scoupval;

** Now run the same two programs for sid data;

%let num=2;
%let sid=sid;
%let sub=b;

data untmp2;
   set untmp;
   if (chain ne 4) and (chain ne 5);

%include scu;
%include scoupval;


*********************************************
***** The final step is to merge all of *****
***** the resulting datasets into one   *****
***** daily price file.                 *****
*********************************************;


** Chain-files to be merged are &dprice1, scutmp1, 
   and fscval1.  Sid files to be merged are &dprice2,
   scutmp2, and fscval2.  The first three must be merged
   by &pid/chain/date.  The second three must be merged by
   &pid/sid/date; 

proc sort data=&dprice.1(drop=units);
   by &pid chain date;

proc sort data=scutmp1(drop=week wday);
   by &pid chain date;

proc sort data=fscval1(drop=week);
   by &pid chain date;

data &dprice.1;
   merge &dprice.1 scutmp1 fscval1;
   by &pid chain date;
   if scind=. then scind=0;
   if scval=. then scval=0;


** Use include_sid.sas here to add sid's.  The output file is
   called 'fincsid', and contains &pid chain sid week date
   dpr reg scind and scval;

%include inclsid;


** Merge together the same files as above for the
   sid data.  Note, there is no need to use the
   program 'include_sid.sas';

proc sort data=&dprice.2(drop=units);
   by &pid sid date;

proc sort data=scutmp2(drop=week wday);
   by &pid sid date;

proc sort data=fscval2(drop=week);
   by &pid sid date;

data &dprice.2;
   merge &dprice.2 scutmp2 fscval2;
   by &pid sid date;
   if scind=. then scind=0;
   if scval=. then scval=0;

***** Stack the files fincsid and &dprice2;

data fincsid;
   set fincsid &dprice.2;

** Finally, fincsid untmp and ads2 should be sorted by
   &pid/chain/sid/date and merged together;
 
proc sort data=fincsid;
   by &pid chain sid date;

proc sort data=untmp(drop=scind);
   by &pid chain sid date;

proc sort data=ads2;
   by &pid chain sid date;


data d.pf&mkt;
   merge fincsid untmp ads2 ;
   by &pid chain sid date;
   if mcind=. then mcind=0;
   if amcv=. then amcv=0;
   if units=. then units=0;
   if mcunits=. then mcunits=0;
   if scunits=. then scunits=0;
   if major=. then major=0;
   if line=. then line=0;
   if display=. then display=0;
   keep &pid chain sid date week dpr reg units scind scval scunits
        mcind amcv mcunits major line display;



**********************************************
******** Further Adjustments *****************
**********************************************;

** In the original HP file, sometimes the scunits
   (or mcunits) are nonzero, but the scval(or mcval)
   is zero.  If this is the case in our final dataset,
   then let the scval(or amcv) be equal to -999. Also, 
   there may be a nonzero value of scind, but the scval
   is again zero;

data d.pf&mkt;
   set d.pf&mkt;
   if scunits gt 0 and scval=0 then scval=-999;
   if mcunits gt 0 and amcv=0 then amcv=-999;
   if scind=1 and scval=0 then scval=-999;

