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

%let mkt = 2;
%let ino=d.hp&mkt;
%let in = chp&mkt ;
%let price=cprice&mkt;
%let sid = sid;
%let dprice=cdprice;
%let wprice=cwprice&mkt;
%let wp = wp ;
%let maxdeal = 3;
%let pid = upc;
%let sub=z;


** Extract and create only useful variables
   Include values of &pid with Market Share ge 1%;

data newhp&sub;
   set &ino(rename=(scval=scv mcval=mcv));
      if (&pid=27000382580 or &pid=70038307070 or &pid=13000001250 or
          &pid=70038307050 or &pid=36800709150 or &pid=24000004100 or
          &pid=13000001280 or &pid=36800704500 or &pid=70038307090 or
          &pid=13000001240 or &pid=36800703200 or &pid=13000001070 or
          &pid=36800702160 or &pid=36800704200 or &pid=27000382440 or
          &pid=70038307080 or &pid=36800709350 or &pid=23000950550 or
          &pid=24000007220)
          and (date ne .) and (units ne .)
          and not(sid=32 or sid=33 or sid=34 or sid=35);
   scval=0; mcval=0;
   if (scunits ne 0) then do;
      scval=scv/scunits;
   end;
   if (mcunits ne 0) then do;
      mcval=mcv/mcunits;
   end;
   keep id sid &pid date chain market units price adtype
        endd frontd ind otherd mcunits mcval scunits scval;

proc sort data=newhp&sub;
   by &pid &sid date price;


*******************************************
*****  Beginning of process to apply  *****
*****  the price algorithm to 'price' *****
*******************************************;

** Programs involved are wklyprc_gen.sas,
   dips.sas, dstep.sas, finish.sas, and
   prcalg_fin.sas;

data &in;
   set newhp&sub(keep=  &sid &pid date price units);
   retain sumunits;
   by &pid &sid date price;
   if first.date or first.price then sumunits=0;
   sumunits=sumunits+units;
   if last.price then do;
      drop units;
      rename sumunits=units;
      output;
   end;

%let num=0;

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


******************************************
*****  Beginning of process to deal  *****
*****  with advertising and coupons  *****
******************************************;


** Create indicators, per observation, as to whether
   an ad was in effect, for line and major types.
   Also an indicator for whether a display was made;

data ads;
   set newhp&sub;
   length major   4
          line    4
          display 4;
   major=(adtype='M');
   line=(adtype='L');
   display=(endd='Y' or frontd='Y' or ind='Y' or otherd='Y');
   keep &pid &sid sid date major line display;
 
** Using the summarizing algorithm, create a daily value 
   for the variables in the file work.ads.  Then create
   an indicator which is 1 if the daily value is greater than
   zero;

proc sort data=ads;
   by &pid &sid sid date;

data ads2;
   set ads;
   retain smajor sline sdisplay;
   by &pid &sid sid date;
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
   keep &pid &sid sid date major line display;



** Use the summarizing algorithom on the infile
   to sum the mcunits and scunits for each day;

proc sort data=newhp&sub;
   by &pid &sid sid date ;

data untmpa;
   set newhp&sub;
   retain sumunits summc sumsc;
   by &pid &sid sid date ;
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
   set newhp&sub;
   if mcval ne 0;
   keep &pid &sid sid date mcval;

proc sort data=settmp;
   by &pid &sid sid date;

proc means data=settmp noprint;
   by &pid &sid sid date;
   var mcval;
   output out=untmpb(drop=_type_ _freq_) mean=tamcv;

data untmp;
   merge untmpa untmpb;
   by &pid &sid sid date;
   amcv=round(tamcv,.01);
   if amcv=. then amcv=0;

   
** Create indicators for store and manufacturer coupons, but retain
   the scunits and mcunits;

data untmp&num;
   set untmp;
   length scind 4
          mcind 4;
   if scunits gt 0 then scind=1;
   else if scunits=0 then scind=0;
   if mcunits gt 0 then mcind=1;
   else if mcunits=0 then mcind=0;
   keep &pid &sid sid date units amcv mcunits mcind scunits scind;


** Use the program scu.sas to extend the coupon indicator
   for all days of a week in which there was a store
   coupon presented;

%let sub=z;
%include scu;


** Use the program scoupval.sas to create matching weekly
   values for the dates which have a '1' in the store
   coupon indicator variable created by scu.sas;

%include scoupval;


*********************************************
***** The final step is to merge all of *****
***** the resulting datasets into one   *****
***** daily price file.                 *****
*********************************************;


** Files to be merged are &dprice, scutmp, fscval,
   untmp, and ads2.  The first three are to be sorted
   by &pid/&sid/date, and then merged.  The last two
   are to be sorted by &pid/&sid/sid/date and then merged
   with the first merged file after it is run through
   include_sid.sas to expand it for all SIDs;

proc sort data=&dprice.0(drop=units);
   by &pid &sid date;

proc sort data=scutmp0(drop=week wday);
   by &pid &sid date;

proc sort data=fscval0(drop=week);
   by &pid &sid date;

data &dprice.0;
   merge &dprice.0 scutmp0 fscval0;
   by &pid &sid date;
   if scind=. then scind=0;
   if scval=. then scval=0;


** Use include_sid.sas here. The output file is
   called 'fincsid', and contains &pid &sid sid week date
   dpr reg units scind and scval;

*%include inclsid;


** Finally, fincsid untmp and ads2 should be sorted by
   &pid/&sid/sid/date and merged together;
 
proc sort data=&dprice.0;
   by &pid &sid sid date;

proc sort data=untmp&num(drop=scind);
   by &pid &sid sid date;

proc sort data=ads2;
   by &pid &sid sid date;


data d.pf&mkt;
   merge &dprice.0 untmp&num ads2 ;
   by &pid &sid sid date;
   if mcind=. then mcind=0;
   if amcv=. then amcv=0;
   if units=. then units=0;
   if mcunits=. then mcunits=0;
   if scunits=. then scunits=0;
   if major=. then major=0;
   if line=. then line=0;
   if display=. then display=0;
   keep &pid &sid sid date week dpr reg units scind scval scunits
        mcind amcv mcunits major line display;

*****************************************
****** Further Adjustments **************
*****************************************;

** At times, in the original HP-file, there were
   units recorded as having been purchased on store
   and/or manufacturer coupons, but zero is given as
   the value of the coupon.  If such is the case, we
   make scval or amcv equal to -999.  Also, the variable
   'chain' needs to be put back into the dataset;

data d.pf&mkt;
   length chain 4;
   set d.pf&mkt;
   chain=put(sid, chain.);
   if scunits gt 0 and scval=0 then scval=-999;
   if mcunits gt 0 and amcv=0 then amcv=-999;
   if scind=1 and scval=0 then scval=-999;


