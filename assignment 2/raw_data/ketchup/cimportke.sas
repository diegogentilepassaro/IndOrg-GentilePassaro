/****************************************************************

                        CIMPORT.SAS

Date:   2/28/97
Author: A Ainslie

This program will import the SAS files from the transport files
forthis category only.

The file provided in this directory is a SAS transport file.
The procedure is to do as follows:

1. Make sure that you are happy with the pathnames set up
below. For example, on Unix machines the forward slash is
appropriate; on Dos/Windows/Win95/Win NT, a back slash is
appropriate.

2. Run the file using Sas. Make sure you have at least
200 MB free when you do this as there will be some large working
files as well as some large Sas files output.

****************************************************************/

libname s1 'f:\aftproot\acnpanel\sas_files\erim\ketchup';
filename t1 'f:\aftproot\acnpanel\sas_files\erim\ketchup/erim_ke';

proc cimport library=s1 infile=t1;
run;
