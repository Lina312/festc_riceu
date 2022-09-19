# FEST-C V1.4 RICEU 
# VERSION: 20220701
# BASE MODEL: FEST-C V1.4 EPIC (APP & SU)
# MODIFICATIONS: 
(1) HONO & NO & N2O EMISSIONS FROM NITRIFICATION; NO EMISSIONS FROM DENITRIFICATION;
(2) FIXED HUSC & AUTO P BUGS (ONLY IN APPLICATION PROGRAM)
(3) DO NOT PRINT SOME OUTFILES DEFINED BY FEST-C: KWTNA; KWNCM; KWTNS; KWNCS
# USAGE: 
IDN == 2 FOR RICEU DENITRIFICATION SUBPROGRAM 
To activate RICEU modified N schemes, IDN should be 2 in the EPICCONT.DAT file.
# NEW N VARS IN DAILY & ANNUAL OUTPUTS:
(1)NINX: emissions of NO and HONO from nitrification process (without considering canopy reduction)
(2)NINO: NO emissions from nitrification process (without considering canopy reduction)
(3)NIN2: N2O emissions from nitrification process
(4)HONO: HONO emissions
(5)DENO: NO emissions from denitrification process
(6)DEN2: N2O emissions from denitrification process
(7)TONO: Total NO emissions
(8)TON2: Total N2O emissions
(9)TONX: Total NO & HONO emissions 
ALL OF THE ABOVE N VARS WERE INCLUDED IN MONTHLY OUTPUTS, BUT THIS VERSION DOES NOT PRINT MONTHLY RESULTS.  
 