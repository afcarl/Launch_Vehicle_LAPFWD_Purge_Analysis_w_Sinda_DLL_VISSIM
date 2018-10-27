      PROGRAM SINDA
      CHARACTER*6 H
      COMMON /TITLE/H(20)
      COMMON /TEMP/T(     91)
      COMMON /CAP/C(     32)
      COMMON /SOURCE/Q(     86)
      COMMON /COND/G(     124)
      COMMON /KONST/K(     71)
      COMMON /ARRAY/A(      50)
      COMMON /PC1/LSQ1(     970)
      COMMON /PC2/LSQ2(     156)
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR
      COMMON /XSPACE/NDIM,NTH,X(   10000)
      COMMON /FIXCON/
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG
      DIMENSION XK(     71),NX(   10000),IA(      50)
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))
      LOGICAL ASCI,FLUD,GNRL,CHAR
      COMMON /MODNAME/MODNAME
      CHARACTER *50 MODNAME
      COMMON /IMODNAME/MODNSTRT,MODNEND,IFILESYS
      MODNAME='sinda'                                                                               
      MODNSTRT=  1
      MODNEND=  5
      IFILESYS=  1
      NIN=5
      LDAT=2
      LDIC=4
      OPEN(LDAT,FILE='sinda.TP2',STATUS='UNKNOWN',FORM='UNFORMATTED')                               
      OPEN(LDIC,FILE='sinda.TP4',STATUS='UNKNOWN',FORM='UNFORMATTED')                               
      ASCI=.FALSE.
      CHAR=.FALSE.
      FLUD=.FALSE.
      GNRL=.FALSE.
      T(1)=0.
      C(1)=0.
      Q(1)=0.
      G(1)=0.
      LSQ1(1)=0
      LSQ2(1)=0
      K(1)=0
      A(1)=0.
      X(1)=0.
      NOUT=   6
      OPEN(NOUT ,FILE='sinda.OUT',STATUS='UNKNOWN',FORM='FORMATTED')                                
      CALL INPUT
      CALL EXECT
      CLOSE(LDAT)
      CLOSE(LDIC)
      CLOSE(NIN)
      CLOSE(NOUT)
      END
      SUBROUTINE EXECT 
      CHARACTER*6 H
      COMMON /TITLE/H(20)
      COMMON /TEMP/T(     91)
      COMMON /CAP/C(     32)
      COMMON /SOURCE/Q(     86)
      COMMON /COND/G(     124)
      COMMON /KONST/K(     71)
      COMMON /ARRAY/A(      50)
      COMMON /PC1/LSQ1(     970)
      COMMON /PC2/LSQ2(     156)
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR
      COMMON /XSPACE/NDIM,NTH,X(   10000)
      COMMON /FIXCON/
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG
      DIMENSION XK(     71),NX(   10000),IA(      50)
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))
      LOGICAL ASCI,FLUD,GNRL,CHAR
       open(88,file='fwdlap_afc_c_88.out',access='sequential',                  
     +  status='unknown')                                                       
        XK(13)= 0                                                       
      CALL SNDSNR                                                       
      RETURN
      END
      SUBROUTINE VARBL1
      CHARACTER*6 H
      COMMON /TITLE/H(20)
      COMMON /TEMP/T(     91)
      COMMON /CAP/C(     32)
      COMMON /SOURCE/Q(     86)
      COMMON /COND/G(     124)
      COMMON /KONST/K(     71)
      COMMON /ARRAY/A(      50)
      COMMON /PC1/LSQ1(     970)
      COMMON /PC2/LSQ2(     156)
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR
      COMMON /XSPACE/NDIM,NTH,X(   10000)
      COMMON /FIXCON/
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG
      DIMENSION XK(     71),NX(   10000),IA(      50)
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))
      LOGICAL ASCI,FLUD,GNRL,CHAR
       T(88)=XK(14)                                                     
       T(89)=XK(15)                                                     
       G(69)=XK(2)* 14.4                                                
       G(70)=XK(2)* 14.4                                                
       G(71)=XK(2)* 14.4                                                
       G(72)=XK(2)* 14.4                                                
       G(66)=XK(3)* 14.4                                                
       G(51)=XK(4)* 1.47 * 14.7 * 0.23                                  
       G(52)=XK(5)* 1.47 * 14.7 * 1.0                                   
       G(53)=XK(8)* 1.00 * 34.9 * 1.0                                   
       G(54)=XK(7)* 1.47 * 14.3 * 1.0                                   
       G(55)=XK(7)* 1.47 * 13.1 * 1.0                                   
       G(56)=XK(7)* 1.47 * 8.52 * 1.0                                   
       G(57)=XK(7)* 1.47 * 98.0 * 0.30                                  
       G(58)=XK(4)* 1.47 * 98.0 * 0.70                                  
       G(59)=XK(4)* 1.47 * 17.0 * 0.64                                  
       G(60)=XK(5)* 1.47 * 30.7 * 1.0                                   
       G(61)=XK(4)* 1.47 * 30.7 * 0.64                                  
       G(62)=XK(5)* 1.47 * 0.80 * 1.0                                   
       G(63)=XK(6)* 1.00 * 14.7 * 0.77                                  
       G(64)=XK(6)* 1.00 * 17.0 * 0.36                                  
       G(65)=XK(6)* 1.00 * 30.7 * 0.36                                  
       T(90)=XK(17)                                                     
       G(67)=XK(16)* 14.4                                               
       G(68)=XK(18)* 152.9                                              
      RETURN
      END
      SUBROUTINE VARBL2
      CHARACTER*6 H
      COMMON /TITLE/H(20)
      COMMON /TEMP/T(     91)
      COMMON /CAP/C(     32)
      COMMON /SOURCE/Q(     86)
      COMMON /COND/G(     124)
      COMMON /KONST/K(     71)
      COMMON /ARRAY/A(      50)
      COMMON /PC1/LSQ1(     970)
      COMMON /PC2/LSQ2(     156)
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR
      COMMON /XSPACE/NDIM,NTH,X(   10000)
      COMMON /FIXCON/
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG
      DIMENSION XK(     71),NX(   10000),IA(      50)
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))
      LOGICAL ASCI,FLUD,GNRL,CHAR
      RETURN
      END
      SUBROUTINE OUTCAL
      CHARACTER*6 H
      COMMON /TITLE/H(20)
      COMMON /TEMP/T(     91)
      COMMON /CAP/C(     32)
      COMMON /SOURCE/Q(     86)
      COMMON /COND/G(     124)
      COMMON /KONST/K(     71)
      COMMON /ARRAY/A(      50)
      COMMON /PC1/LSQ1(     970)
      COMMON /PC2/LSQ2(     156)
      COMMON /DIMENS/NND,NNC,NNT,NGL,NGT,NGE,NCC,NUC,NCT,NAT,LENA
     +,NSQ1,NSQ2,NPC,NPT,NSQ3,NVL,NPM,NTE,NSQ4,NCS,LCS
      COMMON /POINTN/LNODE,LCOND,LCONS,LARRY,IVB,LPRES,LTUBE,LCHAR
      COMMON /TAPE/NIN,NOUT,LDAT,LDIC,ASCI,FLUD,GNRL,CHAR
      COMMON /XSPACE/NDIM,NTH,X(   10000)
      COMMON /FIXCON/
     +TIMEN ,DTIMEU,TIMEND,CSGFAC,NLOOP ,DTMPCA,ITROUT,DTIMEH,
     +DAMPA ,DAMPD ,ATMPCA,BACKUP,TIMEO ,TIMEM ,DTMPCC,ATMPCC,
     +CSGMIN,OUTPUT,ARLXCA,LOOPCT,DTIMEL,DTIMEI,CSGMAX,CSGRAL,
     +CSGRCL,DRLXCA,DRLXCC,NLINE ,NPAGE ,ARLXCC,LSPCS ,ENGBAL,
     +BALENG,ATSLIM,NCSGMN,NDTMPC,NARLXC,NATMPC,ITEST ,JTEST ,
     +KTEST ,LTEST ,MTEST ,RTEST ,STEST ,TTEST ,UTEST ,VTEST ,
     +LAXFAC,SIGMA ,TMPZRO,NDRLXC,TDERV ,NTDERV,BENODE,EBNODE,
     +NODEEB,EXTLIM,NCOLUM,PRLXCA,PRLXCC,NEGSIV,GRVCON,PZERO ,
     +NCSGMX,NTEST ,ATEST ,BTEST ,CTEST ,DTEST ,ETEST ,FTEST ,
     +GTEST ,HTEST ,OTEST ,PTEST ,QTEST ,WTEST ,XTEST ,YTEST ,
     +ZTEST ,NTROSS,ISNUNC,NLINPP,LOTEMP,ERRMAX,ERRMIN,SENGIN,
     +DBLPRC,MPCNTL,IPCNT1,IPCNT2,ATSLM1,NLOOP1,JDIFQ ,KMAX  ,
     +FRACHG,EPS   ,PRSABS,PRSREL,FLOABS,FLOREL,FLOMAX,PRANGE,
     +ISOLVE,NPASS ,DEFLIM,ICHECK,GRAV  ,GC1   ,GC2   ,USRFLO,
     +PMPTOL,DEBUGF,NOFERR,GC3   ,SPARE1,SPARE2,SPARE3,SPARE4,
     +SPARE5,SPARE6,SPARE7,NNGSPM,NCONVG
      DIMENSION XK(     71),NX(   10000),IA(      50)
      EQUIVALENCE (K(1),XK(1)),(X(1),NX(1)),(A(1),IA(1))
      LOGICAL ASCI,FLUD,GNRL,CHAR
      CALL TPRINT                                                       
       XK(9)= -999.0                                                    
       XK(10)=  999.0                                                   
       XK(11)= -999.0                                                   
       XK(12)=  999.0                                                   
       if (T(1).gt.XK(9)) then                                          
         XK(9)= T(1)                                                    
       end if                                                           
       if (T(33).gt.XK(9)) then                                         
         XK(9)= T(33)                                                   
       end if                                                           
       if (T(2).gt.XK(9)) then                                          
         XK(9)= T(2)                                                    
       end if                                                           
       if (T(34).gt.XK(9)) then                                         
         XK(9)= T(34)                                                   
       end if                                                           
       if (T(3).gt.XK(9)) then                                          
         XK(9)= T(3)                                                    
       end if                                                           
       if (T(35).gt.XK(9)) then                                         
         XK(9)= T(35)                                                   
       end if                                                           
       if (T(4).gt.XK(9)) then                                          
         XK(9)= T(4)                                                    
       end if                                                           
       if (T(36).gt.XK(9)) then                                         
         XK(9)= T(36)                                                   
       end if                                                           
       if (T(5).gt.XK(9)) then                                          
         XK(9)= T(5)                                                    
       end if                                                           
       if (T(37).gt.XK(9)) then                                         
         XK(9)= T(37)                                                   
       end if                                                           
       if (T(6).gt.XK(9)) then                                          
         XK(9)= T(6)                                                    
       end if                                                           
       if (T(38).gt.XK(9)) then                                         
         XK(9)= T(38)                                                   
       end if                                                           
       if (T(7).gt.XK(9)) then                                          
         XK(9)= T(7)                                                    
       end if                                                           
       if (T(39).gt.XK(9)) then                                         
         XK(9)= T(39)                                                   
       end if                                                           
       if (T(8).gt.XK(9)) then                                          
         XK(9)= T(8)                                                    
       end if                                                           
       if (T(40).gt.XK(9)) then                                         
         XK(9)= T(40)                                                   
       end if                                                           
       if (T(9).gt.XK(9)) then                                          
         XK(9)= T(9)                                                    
       end if                                                           
       if (T(41).gt.XK(9)) then                                         
         XK(9)= T(41)                                                   
       end if                                                           
       if (T(42).gt.XK(9)) then                                         
         XK(9)= T(42)                                                   
       end if                                                           
       if (T(43).gt.XK(9)) then                                         
         XK(9)= T(43)                                                   
       end if                                                           
       if (T(44).gt.XK(9)) then                                         
         XK(9)= T(44)                                                   
       end if                                                           
       if (T(11).gt.XK(9)) then                                         
         XK(9)= T(11)                                                   
       end if                                                           
       if (T(46).gt.XK(9)) then                                         
         XK(9)= T(46)                                                   
       end if                                                           
       if (T(47).gt.XK(9)) then                                         
         XK(9)= T(47)                                                   
       end if                                                           
       if (T(13).gt.XK(9)) then                                         
         XK(9)= T(13)                                                   
       end if                                                           
       if (T(48).gt.XK(9)) then                                         
         XK(9)= T(48)                                                   
       end if                                                           
       if (T(53).gt.XK(9)) then                                         
         XK(9)= T(53)                                                   
       end if                                                           
       if (T(54).gt.XK(9)) then                                         
         XK(9)= T(54)                                                   
       end if                                                           
       if (T(55).gt.XK(9)) then                                         
         XK(9)= T(55)                                                   
       end if                                                           
       if (T(56).gt.XK(9)) then                                         
         XK(9)= T(56)                                                   
       end if                                                           
       if (T(57).gt.XK(9)) then                                         
         XK(9)= T(57)                                                   
       end if                                                           
       if (T(58).gt.XK(9)) then                                         
         XK(9)= T(58)                                                   
       end if                                                           
       if (T(59).gt.XK(9)) then                                         
         XK(9)= T(59)                                                   
       end if                                                           
       if (T(60).gt.XK(9)) then                                         
         XK(9)= T(60)                                                   
       end if                                                           
       if (T(61).gt.XK(9)) then                                         
         XK(9)= T(61)                                                   
       end if                                                           
       if (T(62).gt.XK(9)) then                                         
         XK(9)= T(62)                                                   
       end if                                                           
       if (T(63).gt.XK(9)) then                                         
         XK(9)= T(63)                                                   
       end if                                                           
       if (T(64).gt.XK(9)) then                                         
         XK(9)= T(64)                                                   
       end if                                                           
       if (T(65).gt.XK(9)) then                                         
         XK(9)= T(65)                                                   
       end if                                                           
       if (T(66).gt.XK(9)) then                                         
         XK(9)= T(66)                                                   
       end if                                                           
       if (T(10).gt.XK(9)) then                                         
         XK(9)= T(10)                                                   
       end if                                                           
       if (T(49).gt.XK(9)) then                                         
         XK(9)= T(49)                                                   
       end if                                                           
       if (T(14).gt.XK(9)) then                                         
         XK(9)= T(14)                                                   
       end if                                                           
       if (T(50).gt.XK(9)) then                                         
         XK(9)= T(50)                                                   
       end if                                                           
       if (T(51).gt.XK(9)) then                                         
         XK(9)= T(51)                                                   
       end if                                                           
       if (T(52).gt.XK(9)) then                                         
         XK(9)= T(52)                                                   
       end if                                                           
       if (T(45).gt.XK(9)) then                                         
         XK(9)= T(45)                                                   
       end if                                                           
       if (T(1).lt.XK(10)) then                                         
         XK(10)= T(1)                                                   
       end if                                                           
       if (T(33).lt.XK(10)) then                                        
         XK(10)= T(33)                                                  
       end if                                                           
       if (T(2).lt.XK(10)) then                                         
         XK(10)= T(2)                                                   
       end if                                                           
       if (T(34).lt.XK(10)) then                                        
         XK(10)= T(34)                                                  
       end if                                                           
       if (T(3).lt.XK(10)) then                                         
         XK(10)= T(3)                                                   
       end if                                                           
       if (T(35).lt.XK(10)) then                                        
         XK(10)= T(35)                                                  
       end if                                                           
       if (T(4).lt.XK(10)) then                                         
         XK(10)= T(4)                                                   
       end if                                                           
       if (T(36).lt.XK(10)) then                                        
         XK(10)= T(36)                                                  
       end if                                                           
       if (T(5).lt.XK(10)) then                                         
         XK(10)= T(5)                                                   
       end if                                                           
       if (T(37).lt.XK(10)) then                                        
         XK(10)= T(37)                                                  
       end if                                                           
       if (T(6).lt.XK(10)) then                                         
         XK(10)= T(6)                                                   
       end if                                                           
       if (T(38).lt.XK(10)) then                                        
         XK(10)= T(38)                                                  
       end if                                                           
       if (T(7).lt.XK(10)) then                                         
         XK(10)= T(7)                                                   
       end if                                                           
       if (T(39).lt.XK(10)) then                                        
         XK(10)= T(39)                                                  
       end if                                                           
       if (T(8).lt.XK(10)) then                                         
         XK(10)= T(8)                                                   
       end if                                                           
       if (T(40).lt.XK(10)) then                                        
         XK(10)= T(40)                                                  
       end if                                                           
       if (T(9).lt.XK(10)) then                                         
         XK(10)= T(9)                                                   
       end if                                                           
       if (T(41).lt.XK(10)) then                                        
         XK(10)= T(41)                                                  
       end if                                                           
       if (T(42).lt.XK(10)) then                                        
         XK(10)= T(42)                                                  
       end if                                                           
       if (T(43).lt.XK(10)) then                                        
         XK(10)= T(43)                                                  
       end if                                                           
       if (T(44).lt.XK(10)) then                                        
         XK(10)= T(44)                                                  
       end if                                                           
       if (T(11).lt.XK(10)) then                                        
         XK(10)= T(11)                                                  
       end if                                                           
       if (T(46).lt.XK(10)) then                                        
         XK(10)= T(46)                                                  
       end if                                                           
       if (T(47).lt.XK(10)) then                                        
         XK(10)= T(47)                                                  
       end if                                                           
       if (T(13).lt.XK(10)) then                                        
         XK(10)= T(13)                                                  
       end if                                                           
       if (T(48).lt.XK(10)) then                                        
         XK(10)= T(48)                                                  
       end if                                                           
       if (T(53).lt.XK(10)) then                                        
         XK(10)= T(53)                                                  
       end if                                                           
       if (T(54).lt.XK(10)) then                                        
         XK(10)= T(54)                                                  
       end if                                                           
       if (T(55).lt.XK(10)) then                                        
         XK(10)= T(55)                                                  
       end if                                                           
       if (T(56).lt.XK(10)) then                                        
         XK(10)= T(56)                                                  
       end if                                                           
       if (T(57).lt.XK(10)) then                                        
         XK(10)= T(57)                                                  
       end if                                                           
       if (T(58).lt.XK(10)) then                                        
         XK(10)= T(58)                                                  
       end if                                                           
       if (T(59).lt.XK(10)) then                                        
         XK(10)= T(59)                                                  
       end if                                                           
       if (T(60).lt.XK(10)) then                                        
         XK(10)= T(60)                                                  
       end if                                                           
       if (T(61).lt.XK(10)) then                                        
         XK(10)= T(61)                                                  
       end if                                                           
       if (T(62).lt.XK(10)) then                                        
         XK(10)= T(62)                                                  
       end if                                                           
       if (T(63).lt.XK(10)) then                                        
         XK(10)= T(63)                                                  
       end if                                                           
       if (T(64).lt.XK(10)) then                                        
         XK(10)= T(64)                                                  
       end if                                                           
       if (T(65).lt.XK(10)) then                                        
         XK(10)= T(65)                                                  
       end if                                                           
       if (T(66).lt.XK(10)) then                                        
         XK(10)= T(66)                                                  
       end if                                                           
       if (T(10).lt.XK(10)) then                                        
         XK(10)= T(10)                                                  
       end if                                                           
       if (T(49).lt.XK(10)) then                                        
         XK(10)= T(49)                                                  
       end if                                                           
       if (T(14).lt.XK(10)) then                                        
         XK(10)= T(14)                                                  
       end if                                                           
       if (T(50).lt.XK(10)) then                                        
         XK(10)= T(50)                                                  
       end if                                                           
       if (T(51).lt.XK(10)) then                                        
         XK(10)= T(51)                                                  
       end if                                                           
       if (T(52).lt.XK(10)) then                                        
         XK(10)= T(52)                                                  
       end if                                                           
       if (T(45).lt.XK(10)) then                                        
         XK(10)= T(45)                                                  
       end if                                                           
       if (T(43).gt.XK(11)) then                                        
         XK(11)= T(43)                                                  
       end if                                                           
       if (T(46).gt.XK(11)) then                                        
         XK(11)= T(46)                                                  
       end if                                                           
       if (T(12).gt.XK(11)) then                                        
         XK(11)= T(12)                                                  
       end if                                                           
       if (T(47).gt.XK(11)) then                                        
         XK(11)= T(47)                                                  
       end if                                                           
       if (T(43).lt.XK(12)) then                                        
         XK(12)= T(43)                                                  
       end if                                                           
       if (T(46).lt.XK(12)) then                                        
         XK(12)= T(46)                                                  
       end if                                                           
       if (T(12).lt.XK(12)) then                                        
         XK(12)= T(12)                                                  
       end if                                                           
       if (T(47).lt.XK(12)) then                                        
         XK(12)= T(47)                                                  
       end if                                                           
       XK(19)= XK(2)* 14.4 * (T(15)- T(88))                             
       XK(19)= XK(19)+ G(66)* (T(15)- T(88))                            
       XK(19)= XK(19)+ G(67)* (T(15)- T(90))                            
       XK(13)= XK(13)+ 1.0                                              
       if (XK(13).gt.3.0) then                                          
         write(88,991) T(15),T(67),T(68),T(69),XK(9),XK(10),XK(11),XK(12
     +),XK(19)
       endif                                                            
  991 format(1x,9(E14.7,1x))                                                    
      RETURN
      END
      SUBROUTINE VARBLF
      RETURN
      END
