      *-----------------------------------------------------------------
      * GENERATED APPLICATION: MAIN TRANSACTION
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NSA01.
       AUTHOR.                 NONE
           INSTALLATION.       NONE
           DATE WRITTEN.       11/03/2007.
           DATE COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      *-----------------------------------------------------------------
      * APPLICATION NAME    : NSA01
      * APPLICATION TYPE    : MAIN
      * MODIFICATION DATE   : 11/03/2007
      * MODIFICATION TIME   : 15:25:56
      * GENERATION SYSTEM   : MVSCICS
      * GENERATION DATE     : 11/03/2007
      * GENERATION TIME     : 15:26:57
      * GENERATION OPTIONS  :
      *                       ANSISQL(NO)
      *                       CICSDBCS(NO)
      *                       COMMLVL(4)
      *                       CONTABLE(ELAS0420)
      *                       DATA(31)
      *                       DEBUGTRACE(NO)
      *                       ENDCOMMAREA(NO)
      *                       FOLD(NO)
      *                       GENRET(NO)
      *                       INEDIT(ALL)
      *                       INITADDWS(YES)
      *                       INITRECD(YES)
      *                       LINEINFO(NO)
      *                       MATH(COBOL)
      *                       NUMOVFL(YES)
      *                       PREPFILE(YES)
      *                       PRINTDEST(EZEP)
      *                       SPZERO(NO)
      *                       SYNCDXFR(YES)
      *                       SYSCODES(NO)
      *                       TARGNLS(ENU)
      *                       TRACE()
      *                       TRANSID(NSA0,)
      *                       TWAOFF(0)
      *                       USERID(CSO.DVPGRP)
      *                       VALIDMIX(YES)
      *                       WORKDB(AUX)
      *
      * PROLOGUE:
      *
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
      * RTS APPLICATION PROFILE BLOCK
       01  EZEAPP-PROFILE SYNCHRONIZED.
           05  FILLER                  PIC X(8)
                                       VALUE "ELARHAPP".
           05  EZEAPP-APPL-NAME        PIC X(8)
                                       VALUE "NSA01".
           05  EZEAPP-PGM-VERSION.
             10  EZEAPP-GEN-DATE       PIC X(8)
                                       VALUE "20070311".
             10  EZEAPP-GEN-TIME       PIC X(8)
                                       VALUE "15265793".
           05  EZEAPP-RTS-PTR          USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-GEN-VERSION      PIC X(16)
                                       VALUE "040405".
           05  EZEAPP-COB-SYS          PIC X(8)
                                       VALUE "MVSCICS".
           05  EZEAPP-CALLER-PROFILE   USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-EZE-WORDS-PTR    USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-CURS-BLK-PTR     USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-DLI-SCAN-PTR     USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-MSP-IDENT-PTR    USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-SPA-LEN          PIC S9(9) COMP
                                       VALUE +0.
           05  EZEAPP-MAX-MSG-LEN      PIC S9(9) COMP
                                       VALUE +0.
           05  EZEAPP-WSR-PTR          USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-DB-IO-LEN        PIC S9(9) COMP
                                       VALUE +65535.
           05  EZEAPP-PARM-VAL-PTR     USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-1ST-MAP-PTR      USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-PSBNAME          PIC X(8)
                                       VALUE SPACES.
           05  EZEAPP-PCB-CNT          PIC S9(4) COMP
                                       VALUE +0.
           05  EZEAPP-MS-PCB-NO        PIC S9(4) COMP
                                       VALUE ZERO.
           05  EZEAPP-WK-PCB-NO        PIC S9(4) COMP
                                       VALUE +0.
           05  EZEAPP-ERRDEST          PIC X(8)
                                       VALUE SPACES.
           05  EZEAPP-LOG-ID           PIC X(1)
                                       VALUE LOW-VALUES.
           05  EZEAPP-MSP-PROGRAM      PIC X(8)
                                       VALUE "NSG01".
           05  EZEAPP-MAP-GROUP        PIC X(8)
                                       VALUE "NSG01".
           05  EZEAPP-HELP-MAP-GROUP   PIC X(8)
                                       VALUE SPACES.
           05  EZEAPP-HELP-PF-KEY      PIC X(2)
                                       VALUE "01".
           05  EZEAPP-BYPASS-PF-KEYS.
             10  FILLER                PIC X(10)
                                       VALUE SPACES.
           05  FILLER REDEFINES EZEAPP-BYPASS-PF-KEYS.
             10  EZEAPP-BYPASS-PF-KEY  PIC X(2)
                                       OCCURS 5 TIMES.
           05  EZEAPP-MSG-FILE-ID      PIC X(4)
                                       VALUE "NESG".
           05  EZEAPP-MS-DB-TYPE       PIC X(1)
                                       VALUE "5".
           05  EZEAPP-WK-DB-TYPE       PIC X(1)
                                       VALUE "3".
           05  EZEAPP-ADF-SPA          PIC X(1)
                                       VALUE "N".
           05  EZEAPP-APPL-TYPE        PIC X(1)
                                       VALUE "1".
           05  EZEAPP-EXECMODE         PIC X(1)
                                       VALUE "2".
           05  EZEAPP-SCAN-IO-PCB      PIC X(1)
                                       VALUE "N".
           05  EZEAPP-PF1-12-IS-PF13-24
                                        PIC X(1) VALUE "N".
           05  EZEAPP-NLS-CODE         PIC X(3)
                                       VALUE "ENU".
           05  EZEAPP-CURRENCY-SYMBOL  PIC X(1)
                                       VALUE X"68".
           05  EZEAPP-DECIMAL-SYMBOL   PIC X(1)
                                       VALUE ".".
           05  EZEAPP-NUM-SEP-SYMBOL   PIC X(1)
                                       VALUE ",".
           05  EZEAPP-MATH             PIC X(5)
                                       VALUE "COBOL".
           05  EZEAPP-SYSTEM-RTN-CODES PIC X(1)
                                       VALUE "N".
           05  EZEAPP-ENTRY-FUNCTION   PIC X(2)
                                       VALUE LOW-VALUES.
           05  EZEAPP-MS-RTB-ADDRESS   USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-TBK-STACK-SIZE   PIC S9(9) COMP
                                       VALUE +44.
           05  FILLER                  PIC X(8)
                                       VALUE LOW-VALUES.
           05  EZEAPP-FAST-PATH-SW     PIC X(1)
                                       VALUE "N".
           05  EZEAPP-RECOVERY-SW      PIC X(1)
                                       VALUE "N".
           05  FILLER                  PIC X(1)
                                       VALUE LOW-VALUES.
           05  EZEAPP-EZEDESTP-CHANGED PIC X(1)
                                       VALUE "N".
           05  EZEAPP-LINK-TYPE        PIC X(1)
                                       VALUE "4".
           05  EZEAPP-PARM-FORM        PIC X(1)
                                       VALUE "1".
           05  EZEAPP-CURS-BLK-CNT     PIC S9(4) COMP
                                       VALUE +0.
           05  EZEAPP-TWA-LENGTH       PIC S9(9) COMP
                                       VALUE +0.
           05  EZEAPP-TWA-ADDRESS      USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-TWA-USER-LENGTH  PIC S9(9) COMP
                                       VALUE 0.
           05  EZEAPP-MAX-SSA-LENGTH   PIC S9(9) COMP
                                       VALUE +0.
           05  EZEAPP-LTB-ARRAY-ADDRESS USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-ENTRY-COMMAREA-PTR
                                       USAGE IS POINTER
                                       VALUE NULLS.
           05  FILLER                  PIC X(1)
                                       VALUE SPACES.
           05  EZEAPP-NEED-ENDB        PIC X(1)
                                       VALUE "N".
           05  EZEAPP-BAD-RESP         PIC X(1)
                                       VALUE "N".
           05  FILLER                  PIC X(1)
                                       VALUE SPACES.
           05  EZEAPP-SYNC-XFERS-SW    PIC X(1)
                                       VALUE "N".
           05  EZEAPP-SYNC-DXFRS-SW    PIC X(1)
                                       VALUE "Y".
           05  EZEAPP-STATIC-CALLS     PIC X(1)
                                       VALUE "N".
           05  EZEAPP-INEDIT-UNP-SW    PIC X(1)
                                       VALUE "N".
           05  EZEAPP-MAX-DB-IOAREA    PIC S9(9) COMP
                                       VALUE +32767.
           05  EZEAPP-LAST-MAPBUF-PTR  USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-FIRST-MAPBUF-PTR USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-ROWS-USED-PTR    USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-MAPG-MOD-PTR     USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-HELPG-MOD-PTR    USAGE IS POINTER
                                       VALUE NULL.
           05  EZEAPP-CURRENT-RSCT-IDX PIC S9(9) COMP
                                       VALUE +0.
           05  EZEAPP-CURRENT-HELP-MAP PIC X(8)
                                       VALUE SPACES.
           05  EZEAPP-EZEDESTP.
             10  EZEDESTP              PIC X(65)
                                       VALUE SPACES.
           05  EZEAPP-OPEN-NEW-DESTP   PIC X(1)
                                       VALUE "N".
           05  EZEAPP-EZEDESTP-DIFF    PIC X(1)
                                       VALUE "N".
           05  EZEAPP-USES-SQL         PIC X(1)
                                       VALUE "N".
           05  EZEAPP-XFER-MAP         PIC X(8)
                                       VALUE LOW-VALUES.
           05  FILLER                  PIC X(4)
                                       VALUE LOW-VALUES.
           05  EZEAPP-USE-CURRENCY     PIC X(1)
                                       VALUE "Y".
           05  FILLER                  PIC X(16)
                                       VALUE LOW-VALUES.
      * RTS ERROR HANDLING REQUEST BLOCK
           COPY ELAEHERR.
      * RTS MEMORY MANAGEMENT REQUEST BLOCK
           COPY ELARHMEM.
       01  EZECICS-TMP-2BYTE-COMP   PIC S9(4) COMP   VALUE ZERO.
      * RTS MNEMONICS
           COPY ELARHMNE.
      * RTS REQUEST BLOCK
           COPY ELARHRRB.
      * DISPLAY SERVICES REQUEST BLOCK
           COPY ELARHFMR.
      * EZE SPECIAL FUNCTION WORDS
       01  EZEWORDS.
           05  EZEEZE-ID               PIC X(8)
                                       VALUE "ELARHEZE".
           05  EZEWORDS-LL             PIC S9(9) COMP
                                       VALUE +334.
           05  EZEWORDS-I.
             10  EZEAID                PIC X(2)
                                       VALUE SPACES.
               88  EZEAID-ENTER        VALUE "  ".
               88  EZEAID-CLEAR        VALUE "CL".
               88  EZEAID-PAKEY        VALUE "P1" "P2" "P3".
               88  EZEAID-PA1          VALUE "P1".
               88  EZEAID-PA2          VALUE "P2".
               88  EZEAID-PA3          VALUE "P3".
             10  EZEAID-BYPASS-SW      PIC X(1)
                                       VALUE SPACES.
               88  EZEAID-BYPASS       VALUE "Y".
               88  EZEAID-NO-BYPASS    VALUE "N".
             10  EZEAID-HELP-SW        PIC X(1)
                                       VALUE SPACES.
               88  EZEAID-HELP         VALUE "Y".
               88  EZEAID-NO-HELP      VALUE "N".
             10  EZEAPP                PIC X(8)
                                       VALUE SPACES.
             10  EZECNVCM              PIC 9(1)
                                       VALUE 0.
               88  EZECNVCM-NOCOMMIT   VALUE 0.
               88  EZECNVCM-COMMIT     VALUE 1.
             10  EZEDLTRM REDEFINES EZECNVCM
                                       PIC 9(1).
             10  EZEREPLY              PIC 9(1)
                                       VALUE ZEROES.
               88  EZEREPLY-TERMINATE  VALUE 0.
               88  EZEREPLY-CONTINUE   VALUE 1.
             10  FILLER                PIC X(4)
                                       VALUE LOW-VALUES.
             10  EZELOC                PIC X(8)
                                       VALUE SPACES.
             10  EZEDLCER              PIC X(2)
                                       VALUE "00".
             10  EZEDLCON              PIC X(2)
                                       VALUE "00".
             10  FILLER                PIC 9(7)
                                       VALUE ZEROES.
             10  EZEFEC                PIC 9(1)
                                       VALUE ZEROES.
               88  EZEFEC-TERMINATE    VALUE 0.
               88  EZEFEC-CONTINUE     VALUE 1.
             10  EZEDLERR              PIC 9(1)
                                       VALUE ZEROES.
               88  EZEDLERR-TERMINATE  VALUE 0.
               88  EZEDLERR-CONTINUE   VALUE 1.
             10  EZESQISL              PIC 9(1)
                                       VALUE ZEROES.
             10  EZEMNO                PIC S9(4) COMP
                                       VALUE ZEROES.
               88  EZEMNO-ERROR        VALUES 1 THRU 9998
                                              -9999 THRU -1.
               88  EZEMNO-NO-ERROR     VALUE 0.
               88  EZEMNO-RE-CONVERSE  VALUE 9999.
             10  EZEMNO-MSG-FILE-SW    PIC X(1)
                                       VALUE "N".
               88  EZEMNO-APP-MSG-FILE VALUE "Y".
               88  EZEMNO-SYS-MSG-FILE VALUE "N".
             10  EZEMSG                PIC X(78).
               88  EZEMSG-SPACES       VALUE SPACES.
             10  EZEOVER               PIC 9(1)
                                       VALUE ZEROES.
               88  EZEOVER-DEFAULT     VALUE 0.
               88  EZEOVER-TERMINATE   VALUE 1.
               88  EZEOVER-CONTINUE    VALUE 2.
             10  EZEOVERS              PIC 9(1)
                                       VALUE ZEROES.
             10  EZERCODE              PIC S9(9) COMP
                                       VALUE ZEROES.
             10  EZERT2                PIC X(2)
                                       VALUE SPACES.
             10  EZERT8.
               15  EZERT8FS.
                 20  EZERT8FH          PIC 9(1).
                 20  EZERT8FL          PIC 9(1).
               15  EZERT8VS.
                 20  EZERT8VR          PIC 9(2).
                 20  EZERT8VF          PIC 9(1).
                 20  EZERT8VB          PIC 9(3).
             10  EZERT8-CICS REDEFINES EZERT8.
               15  EZERT8-RESP         PIC 9(4).
               15  EZERT8-RESP2        PIC 9(4).
             10  EZESEGM               PIC 9(1)
                                       VALUE 1.
               88  EZESEGM-NONSEGMENT  VALUE 0.
               88  EZESEGM-SEGMENTED   VALUE 1.
               88  EZESEGM-DEFINED     VALUE 1.
             10  EZECONVT              PIC X(8)
                                       VALUE SPACES.
             10  EZETST                PIC S9(4) COMP
                                       VALUE ZEROES.
             10  EZETST2               PIC S9(4) COMP
                                       VALUE ZEROES.
             10  EZESQLCA.
               15  EZESQNAM            PIC X(8)
                                       VALUE SPACES.
               15  EZESQABC            PIC S9(9) COMP
                                       VALUE ZEROES.
               15  EZESQCOD            PIC S9(9) COMP
                                       VALUE ZEROES.
               15  EZESQRRL            PIC S9(4) COMP
                                       VALUE ZEROES.
               15  EZESQRRM.
                 20 EZESQRET PIC X OCCURS 70 TIMES INDEXED BY EZESQSUB.
               15  EZESQRRP.
                 20  EZESQRPP          PIC X(3)
                                       VALUE SPACES.
                 20  EZESQRVM          PIC X(5)
                                       VALUE SPACES.
               15  EZESQRD1            PIC S9(9) COMP
                                       VALUE ZEROES.
               15  EZESQRD2            PIC S9(9) COMP
                                       VALUE ZEROES.
               15  EZESQRD3            PIC S9(9) COMP
                                       VALUE ZEROES.
               15  EZESQRD4            PIC S9(9) COMP
                                       VALUE ZEROES.
               15  EZESQRD5            PIC S9(9) COMP
                                       VALUE ZEROES.
               15  EZESQRD6            PIC S9(9) COMP
                                       VALUE ZEROES.
               15  FILLER              PIC X(1)
                                       VALUE SPACES.
               15  EZESQWN1            PIC X(1)
                                       VALUE SPACES.
               15  FILLER              PIC X(4)
                                       VALUE SPACES.
               15  EZESQWN6            PIC X(1)
                                       VALUE SPACES.
               15  FILLER              PIC X(9)
                                       VALUE SPACES.
             10  EZEDL-PCB-INFO.
               15  EZEDLDBD            PIC X(8)
                                       VALUE SPACES.
               15  EZEDLLEV            PIC 9(2)
                                       VALUE ZEROES.
               15  EZEDLSTC            PIC X(2)
                                       VALUE SPACES.
               15  EZEDLPRO            PIC X(4)
                                       VALUE SPACES.
               15  FILLER              PIC X(4)
                                       VALUE SPACES.
               15  EZEDLSEG            PIC X(8)
                                       VALUE SPACES.
               15  EZEDLKYL            PIC S9(4) COMP
                                       VALUE 1.
               15  EZEDLSSG            PIC S9(4) COMP
                                       VALUE ZEROES.
           05  EZEMNO-LOOKED-UP-SW     PIC X(1)
                                       VALUE "N".
               88  EZEMNO-LOOKED-UP    VALUE "Y".
               88  EZEMNO-NOT-LOOKED-UP
                                       VALUE "N".
           05  EZESYS                  PIC X(8)
                                       VALUE "MVSCICS".
              88  EZESYS-IMSVS         VALUE "IMSVS".
              88  EZESYS-IMSBMP        VALUE "IMSBMP".
              88  EZESYS-MVSBATCH      VALUE "MVSBATCH".
              88  EZESYS-MVSCICS       VALUE "MVSCICS".
              88  EZESYS-OS2CICS       VALUE "OS2CICS".
              88  EZESYS-TSO           VALUE "TSO".
              88  EZESYS-VSECICS       VALUE "VSECICS".
              88  EZESYS-VSEBATCH      VALUE "VSEBATCH".
              88  EZESYS-OS400         VALUE "OS400".
              88  EZESYS-OS2GUI        VALUE "OS2GUI".
              88  EZESYS-OS2           VALUE "OS2".
              88  EZESYS-AIX           VALUE "AIX".
              88  EZESYS-WINGUI        VALUE "WINGUI".
              88  EZESYS-AIXCICS       VALUE "AIXCICS".
              88  EZESYS-VMCMS         VALUE "VMCMS".
              88  EZESYS-VMBATCH       VALUE "VMBATCH".
              88  EZESYS-HP            VALUE "HP".
              88  EZESYS-ITF           VALUE "ITF".
              88  EZESYS-NTCICS        VALUE "NTCICS".
              88  EZESYS-WINNT         VALUE "WINNT".
           05  FILLER                  PIC X(2)
                                       VALUE LOW-VALUES.
           05  EZEDLKEY.
             10  EZEDLKYC              PIC X(1).
      * FIXED WORK FIELDS
           COPY ELARHWRK.
      * RTS CHA TO NUM REQUEST BLOCK
           COPY ELASHNUM.
       01  EZETBK-MIN-ENTRIES          PIC S9(9) COMP
                                       VALUE +44.
       01  EZETBK-UNUSED-ENTRIES       PIC S9(9) COMP.
      * RTS SEGMENTATION STORAGE REQUEST BLOCK
           COPY ELATHSSM.
       01  EZEMAP-IO-AREA              PIC X(488).
       01  EZECTL-FMC-FIELDS.
           05  EZECTL-RETURN-CODE      PIC S9(4) COMP.
           05  EZECTL-INDEX            PIC S9(4) COMP.
           05  EZECTL-SSM-STATUS-N     PIC S9(4) COMP.
           05  EZECTL-SSM-STATUS-TEXT  PIC X(32)
               VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456".
           05  EZECTL-SSM-STATUS-TABLE REDEFINES
                                       EZECTL-SSM-STATUS-TEXT.
             10  EZECTL-SSM-STATUS-ENTRY PIC X OCCURS 32 TIMES
                                        INDEXED BY EZECTL-SSM-SUB.
           05  EZECTL-OUTBOUND-MAP     PIC X(8).
           05  EZECTL-INBOUND-MAP      PIC X(8).
           05  EZECTL-MAP-AREA-LEN     PIC S9(9) COMP.
       01  EZECTL-CONTROL-FIELDS.
           05  EZECTL-IN-EZETERMINATE-FLAG
                                       PIC X(1)

                                       VALUE "N".
             88  EZECTL-IN-EZETERMINATE
                                       VALUE "Y".
             88  EZECTL-NOT-IN-EZETERMINATE
                                       VALUE "N".
       01  EZECTL-FUNCTION-RETURN-CODE.
           05  EZECTL-FUNCTION-RC-BIN-2
                                       PIC S9(4) COMP.
           05  EZECTL-FUNCTION-RC-BIN-4
                                       PIC S9(9) COMP.
           05  EZECTL-FUNCTION-RC-NUM-8
                                       PIC 9(8).
      * DATE / TIME WORK FIELDS
           COPY ELARDTWK.
       01  EZEDATEINTEGER              PIC S9(7).
      *-----------------------------------------------------------------
      * WORKING STORAGE RECORD LOGON_CHK1
      *-----------------------------------------------------------------
       01  EZEWS-EZER-4-GP.
           02  EZEWS-ID                PIC X(8)
                                       VALUE "ELAASGWS".
           02  EZEWS-EZER-4-LL         PIC S9(8) COMP
                                       VALUE +67.
           02  FILLER                  PIC X(2) VALUE SPACES.
           02  FILLER                  PIC X(18)
                                       VALUE "LOGON_CHK1".
      *-----------------------------------------------------------------
      * RECORD NAME         : LOGON_CHK1
      * FILE ORGANIZATION   : WORKSTOR
      * MODIFICATION DATE   : 07/03/2007
      * MODIFICATION TIME   : 08:19:12
      * RECORD PROLOGUE     :
      *
      *-----------------------------------------------------------------
      * LOGON_CHK1 WAS RENAMED TO EZER-4
           02  EZER-4.
      * OPNAME WAS RENAMED TO EZEI-97
             05  EZEI-97               PIC X(20).
      * OPPSWD WAS RENAMED TO EZEI-98
             05  EZEI-98               PIC X(8).
      * NPPSWD WAS RENAMED TO EZEI-99
             05  EZEI-99               PIC X(8).
      * LOGON_STATUS WAS RENAMED TO EZEI-100
             05  EZEI-100              PIC S9.
      * USRID WAS RENAMED TO EZEI-101
             05  EZEI-101              PIC S9(4) COMP.
           02  EZE-INIT-EZER-4 REDEFINES EZER-4.
             05  EZE-INIT-1            PIC A(20).
             05  EZE-INIT-2            PIC A(8).
             05  EZE-INIT-3            PIC A(8).
             05  EZE-INIT-4            PIC 9.
             05  EZE-INIT-5            PIC S9(4) COMP.
           02  FILLER                  PIC X(4)
                                       VALUE "*END".
      *-----------------------------------------------------------------
      * WORKING STORAGE RECORD RASHA_REC
      *-----------------------------------------------------------------
       01  EZEWS-EZER-5-GP.
           02  EZEWS-ID                PIC X(8)
                                       VALUE "ELAASGWS".
           02  EZEWS-EZER-5-LL         PIC S9(8) COMP
                                       VALUE +327581.
           02  FILLER                  PIC X(2) VALUE SPACES.
           02  FILLER                  PIC X(18)
                                       VALUE "RASHA_REC".
      *-----------------------------------------------------------------
      * RECORD NAME         : RASHA_REC
      * FILE ORGANIZATION   : WORKSTOR
      * MODIFICATION DATE   : 06/03/2007
      * MODIFICATION TIME   : 10:42:58
      * RECORD PROLOGUE     :
      *
      *-----------------------------------------------------------------
      * RASHA_REC WAS RENAMED TO EZER-5
           02  EZER-5.
      * KEEP-WHERE WAS RENAMED TO EZEI-102
             05  EZEI-102              PIC X(400).
             05  EZE-REDEF-1 REDEFINES EZEI-102.
      * WH-CHOICE WAS RENAMED TO EZEI-103
              06  EZEI-103             PIC S9(2).
      * WH-CNAME11 WAS RENAMED TO EZEI-104
              06  EZEI-104             PIC X(25).
      * WH-CNAME12 WAS RENAMED TO EZEI-105
              06  EZEI-105             PIC X(25).
      * WH-CNAME21 WAS RENAMED TO EZEI-106
              06  EZEI-106             PIC X(25).
      * WH-CNAME22 WAS RENAMED TO EZEI-107
              06  EZEI-107             PIC X(25).
      * WH-FTHCNAME1 WAS RENAMED TO EZEI-108
              06  EZEI-108             PIC X(20).
      * WH-FTHCNAME2 WAS RENAMED TO EZEI-109
              06  EZEI-109             PIC X(20).
      * WH-MTHCNAME1 WAS RENAMED TO EZEI-110
              06  EZEI-110             PIC X(20).
      * WH-MTHCNAME2 WAS RENAMED TO EZEI-111
              06  EZEI-111             PIC X(20).
      * WH-SPOUSE-CNAME1 WAS RENAMED TO EZEI-112
              06  EZEI-112             PIC X(25).
      * WH-SPOUSE-CNAME2 WAS RENAMED TO EZEI-113
              06  EZEI-113             PIC X(25).
      * WH-BRTHDT1 WAS RENAMED TO EZEI-114
              06  EZEI-114             PIC X(10).
      * WH-BRTHDT2 WAS RENAMED TO EZEI-115
              06  EZEI-115             PIC X(10).
      * WH-CSONUM WAS RENAMED TO EZEI-116
              06  EZEI-116             PIC S9(9) COMP.
      * WH-IDNUM WAS RENAMED TO EZEI-117
              06  EZEI-117             PIC S9(15) COMP-3.
      * WH-NICKNAME1 WAS RENAMED TO EZEI-118
              06  EZEI-118             PIC X(25).
      * WH-NICKNAME2 WAS RENAMED TO EZEI-119
              06  EZEI-119             PIC X(25).
      * WH-GOV WAS RENAMED TO EZEI-120
              06  EZEI-120             PIC S9(4) COMP.
      * WH-CIVIL WAS RENAMED TO EZEI-121
              06  EZEI-121             PIC S9(4) COMP.
      * WH-REGDT1 WAS RENAMED TO EZEI-122
              06  EZEI-122             PIC X(10).
      * WH-REGDT2 WAS RENAMED TO EZEI-123
              06  EZEI-123             PIC X(10).
      * WH-REGNUM WAS RENAMED TO EZEI-124
              06  EZEI-124             PIC S9(9) COMP.
      * WH-FILLER WAS RENAMED TO EZEI-125
              06  EZEI-125             PIC X(58).
      * REC60 WAS RENAMED TO EZEI-126
             05  EZEI-126              PIC X(60).
             05  EZE-REDEF-2 REDEFINES EZEI-126.
      * REC10 WAS RENAMED TO EZEI-127
              06  EZEI-127             PIC X(10).
      * REC50 WAS RENAMED TO EZEI-128
              06  EZEI-128             PIC X(50).
      * FIDNUMO WAS RENAMED TO EZEI-129
             05  EZEI-129              PIC X(20).
             05  EZE-REDEF-3 REDEFINES EZEI-129.
      * FIDNUM1 WAS RENAMED TO EZEI-130
              06  EZEI-130             PIC X(1).
      * FIDNUMSP1 WAS RENAMED TO EZEI-131
              06  EZEI-131             PIC X(2).
      * FIDNUM2 WAS RENAMED TO EZEI-132
              06  EZEI-132             PIC X(6).
      * FIDNUMSP2 WAS RENAMED TO EZEI-133
              06  EZEI-133             PIC X(2).
      * FIDNUM3 WAS RENAMED TO EZEI-134
              06  EZEI-134             PIC X(2).
      * FIDNUMSP3 WAS RENAMED TO EZEI-135
              06  EZEI-135             PIC X(2).
      * FIDNUM4 WAS RENAMED TO EZEI-136
              06  EZEI-136             PIC X(5).
      * FULNAMMR WAS RENAMED TO EZEI-137
             05  EZEI-137              PIC X(60).
             05  EZE-REDEF-4 REDEFINES EZEI-137.
      * FULNAM1R WAS RENAMED TO EZEI-138
              06  EZEI-138             PIC X(15).
      * FULNAM2R WAS RENAMED TO EZEI-139
              06  EZEI-139             PIC X(15).
      * FULNAM3R WAS RENAMED TO EZEI-140
              06  EZEI-140             PIC X(30).
      * CHAROF2 WAS RENAMED TO EZEI-141
             05  EZEI-141              PIC X(2).
             05  EZE-REDEF-5 REDEFINES EZEI-141.
      * CHAROFLAM WAS RENAMED TO EZEI-142
              06  EZEI-142             PIC X(1).
      * CHAROFANY WAS RENAMED TO EZEI-143
              06  EZEI-143             PIC X(1).
      * W-TXT-GP WAS RENAMED TO EZEI-144
             05  EZEI-144              PIC X(10).
             05  EZE-REDEF-6 REDEFINES EZEI-144.
      * W-TXT-ROW WAS RENAMED TO EZEI-145
           06  EZEI-145             PIC X(1) OCCURS 10 INDEXED EZEIDX1.
      * WTXT1 WAS RENAMED TO EZEI-146
             05  EZEI-146              PIC S9(4) COMP.
      * WTXT2 WAS RENAMED TO EZEI-147
             05  EZEI-147              PIC S9(4) COMP.
      * W-SQLCOD WAS RENAMED TO EZEI-768
             05  EZEI-768              PIC S9(10).
      * W-TABLE WAS RENAMED TO EZEI-148
             05  EZEI-148              PIC X(30).
      * TERM_ID WAS RENAMED TO EZEI-767
             05  EZEI-767              PIC X(4).
             05  EZE-REDEF-7 REDEFINES EZEI-767.
      * TERM1 WAS RENAMED TO EZEI-766
              06  EZEI-766             PIC S9(2).
      * TERM2 WAS RENAMED TO EZEI-765
              06  EZEI-765             PIC X(2).
      * W-NUMCHK WAS RENAMED TO EZEI-764
             05  EZEI-764              PIC X(15).
             05  EZE-REDEF-8 REDEFINES EZEI-764.
      * W-NUMCHK-ROW WAS RENAMED TO EZEI-149
           06  EZEI-149             PIC X(1) OCCURS 15 INDEXED EZEIDX2.
      * W-NUMCHK20 WAS RENAMED TO EZEI-150
             05  EZEI-150              PIC X(20).
             05  EZE-REDEF-9 REDEFINES EZEI-150.
      * W-NUMCHK-ROW20 WAS RENAMED TO EZEI-151
           06  EZEI-151             PIC X(1) OCCURS 20 INDEXED EZEIDX3.
      * W-NUMCHK33 WAS RENAMED TO EZEI-763
             05  EZEI-763              PIC X(33).
             05  EZE-REDEF-10 REDEFINES EZEI-763.
      * W-NUMCHK-ROW33 WAS RENAMED TO EZEI-152
           06  EZEI-152             PIC X(1) OCCURS 33 INDEXED EZEIDX4.
      * W-NUMCHK40 WAS RENAMED TO EZEI-153
             05  EZEI-153              PIC X(40).
             05  EZE-REDEF-11 REDEFINES EZEI-153.
      * W-NUMCHK-ROW40 WAS RENAMED TO EZEI-154
           06  EZEI-154             PIC X(1) OCCURS 40 INDEXED EZEIDX5.
      * W-NUMCHK50 WAS RENAMED TO EZEI-762
             05  EZEI-762              PIC X(50).
             05  EZE-REDEF-12 REDEFINES EZEI-762.
      * W-NUMCHK-ROW50 WAS RENAMED TO EZEI-155
           06  EZEI-155             PIC X(1) OCCURS 50 INDEXED EZEIDX6.
      * FUNCFLG WAS RENAMED TO EZEI-761
             05  EZEI-761              PIC X(1).
      * TFCN WAS RENAMED TO EZEI-156
             05  EZEI-156              PIC X(9).
             05  EZE-REDEF-13 REDEFINES EZEI-156.
      * TFCN1 WAS RENAMED TO EZEI-157
              06  EZEI-157             PIC X(2).
      * TFCN2 WAS RENAMED TO EZEI-158
              06  EZEI-158             PIC X(7).
      * PLAN_NAME WAS RENAMED TO EZEI-760
             05  EZEI-760              PIC X(30).
      * LEVEL WAS RENAMED TO EZEI-759
             05  EZEI-759              PIC X(2).
      * USR0 WAS RENAMED TO EZEI-758
             05  EZEI-758              PIC X(8).
             05  EZE-REDEF-14 REDEFINES EZEI-758.
      * USR2 WAS RENAMED TO EZEI-757
              06  EZEI-757             PIC X(2).
      * USR3 WAS RENAMED TO EZEI-756
              06  EZEI-756             PIC X(2).
      * USR10 WAS RENAMED TO EZEI-755
              06  EZEI-755             PIC X(4).
      * USR1 WAS RENAMED TO EZEI-754
             05  EZEI-754              PIC X(8).
      * NET1 WAS RENAMED TO EZEI-753
             05  EZEI-753              PIC S9(4).
             05  EZE-REDEF-15 REDEFINES EZEI-753.
      * NET2 WAS RENAMED TO EZEI-752
              06  EZEI-752             PIC S9(2).
      * NET3 WAS RENAMED TO EZEI-751
              06  EZEI-751             PIC S9(2).
      * NET11 WAS RENAMED TO EZEI-750
             05  EZEI-750              PIC X(4).
             05  EZE-REDEF-16 REDEFINES EZEI-750.
      * NET22 WAS RENAMED TO EZEI-749
              06  EZEI-749             PIC S9(2).
      * NET33 WAS RENAMED TO EZEI-748
              06  EZEI-748             PIC X(2).
      * FLAG33 WAS RENAMED TO EZEI-747
             05  EZEI-747              PIC S9.
      * TITLE WAS RENAMED TO EZEI-746
             05  EZEI-746              PIC X(25).
      * NATCODEA WAS RENAMED TO EZEI-745
             05  EZEI-745              PIC S9(9).
      * NATCODEE WAS RENAMED TO EZEI-744
             05  EZEI-744              PIC S9(9).
      * NUM-SW WAS RENAMED TO EZEI-743
             05  EZEI-743              PIC S9.
      * NUM-SUB WAS RENAMED TO EZEI-742
             05  EZEI-742              PIC S9(4) COMP.
      * NUM-LEN WAS RENAMED TO EZEI-741
             05  EZEI-741              PIC S9(9) COMP.
      * CON_NAME WAS RENAMED TO EZEI-740
             05  EZEI-740              PIC X(60).
      * CON_MOTHER WAS RENAMED TO EZEI-739
             05  EZEI-739              PIC X(45).
      * FFULLNAME WAS RENAMED TO EZEI-738
             05  EZEI-738              PIC X(45).
      * FAM_NUM WAS RENAMED TO EZEI-737
             05  EZEI-737              PIC S9(9).
      * FAMRETURN WAS RENAMED TO EZEI-736
             05  EZEI-736              PIC X(10).
             05  EZE-REDEF-17 REDEFINES EZEI-736.
      * FAMRETFIL1 WAS RENAMED TO EZEI-735
              06  EZEI-735             PIC X(2).
      * FAMRETOPID WAS RENAMED TO EZEI-734
              06  EZEI-734             PIC X(5).
      * FAMRETFIL2 WAS RENAMED TO EZEI-733
              06  EZEI-733             PIC X(3).
      * FULNAM WAS RENAMED TO EZEI-732
             05  EZEI-732              PIC X(45).
             05  EZE-REDEF-18 REDEFINES EZEI-732.
      * FNAM1 WAS RENAMED TO EZEI-731
              06  EZEI-731             PIC X(15).
      * FNAM2 WAS RENAMED TO EZEI-730
              06  EZEI-730             PIC X(15).
      * FNAM3 WAS RENAMED TO EZEI-729
              06  EZEI-729             PIC X(15).
      * IDREC9 WAS RENAMED TO EZEI-159
             05  EZEI-159              PIC S9(9).
             05  EZE-REDEF-19 REDEFINES EZEI-159.
      * BRTHCEN1 WAS RENAMED TO EZEI-160
              06  EZEI-160             PIC S9.
      * BRTHYY1 WAS RENAMED TO EZEI-161
              06  EZEI-161             PIC S9(2).
      * BRTHMM1 WAS RENAMED TO EZEI-162
              06  EZEI-162             PIC S9(2).
      * BRTHDD1 WAS RENAMED TO EZEI-163
              06  EZEI-163             PIC S9(2).
      * BRTHGOV1 WAS RENAMED TO EZEI-164
              06  EZEI-164             PIC S9(2).
      * IDSER4 WAS RENAMED TO EZEI-165
             05  EZEI-165              PIC S9(4).
             05  EZE-REDEF-20 REDEFINES EZEI-165.
      * BRTHSER1 WAS RENAMED TO EZEI-166
              06  EZEI-166             PIC S9(3).
      * BRTHSER2 WAS RENAMED TO EZEI-167
              06  EZEI-167             PIC S9.
      * IDREC91 WAS RENAMED TO EZEI-168
             05  EZEI-168              PIC S9(9).
             05  EZE-REDEF-21 REDEFINES EZEI-168.
      * BRTHCEN11 WAS RENAMED TO EZEI-169
              06  EZEI-169             PIC S9.
      * BRTHYY11 WAS RENAMED TO EZEI-170
              06  EZEI-170             PIC S9(2).
      * BRTHMM11 WAS RENAMED TO EZEI-171
              06  EZEI-171             PIC S9(2).
      * BRTHDD11 WAS RENAMED TO EZEI-172
              06  EZEI-172             PIC S9(2).
      * BRTHGOV11 WAS RENAMED TO EZEI-173
              06  EZEI-173             PIC S9(2).
      * IDRECD WAS RENAMED TO EZEI-728
             05  EZEI-728              PIC S9(14).
             05  EZE-REDEF-22 REDEFINES EZEI-728.
      * BRTHCEN WAS RENAMED TO EZEI-727
              06  EZEI-727             PIC S9.
      * BRTHYY WAS RENAMED TO EZEI-726
              06  EZEI-726             PIC S9(2).
      * BRTHMM WAS RENAMED TO EZEI-725
              06  EZEI-725             PIC S9(2).
      * BRTHDD WAS RENAMED TO EZEI-724
              06  EZEI-724             PIC S9(2).
      * BRTHGOV WAS RENAMED TO EZEI-723
              06  EZEI-723             PIC S9(2).
      * BRTHSER WAS RENAMED TO EZEI-722
              06  EZEI-722             PIC S9(4).
      * BRTHCHK WAS RENAMED TO EZEI-721
              06  EZEI-721             PIC X(1).
      * CHKR14 WAS RENAMED TO EZEI-720
             05  EZEI-720              PIC S9(14).
             05  EZE-REDEF-23 REDEFINES EZEI-720.
      * CHKR13 WAS RENAMED TO EZEI-719
              06  EZEI-719             PIC S9(13).
      * CHKR1 WAS RENAMED TO EZEI-718
              06  EZEI-718             PIC S9.
      * CHKNUM WAS RENAMED TO EZEI-717
             05  EZEI-717              PIC S9(10).
             05  EZE-REDEF-24 REDEFINES EZEI-717.
      * CHKNUM9 WAS RENAMED TO EZEI-716
              06  EZEI-716             PIC S9(9).
      * CHKNUM1 WAS RENAMED TO EZEI-715
              06  EZEI-715             PIC S9.
      * ADDNO WAS RENAMED TO EZEI-714
             05  EZEI-714              PIC S9(10).
      * YEAR2000 WAS RENAMED TO EZEI-713
             05  EZEI-713              PIC S9(4).
      * APPLNUM1 WAS RENAMED TO EZEI-712
             05  EZEI-712              PIC S9(10).
      * FORMNUM1 WAS RENAMED TO EZEI-711
             05  EZEI-711              PIC S9(9).
      * STRPNTR WAS RENAMED TO EZEI-710
             05  EZEI-710              PIC S9(4) COMP.
      * ENDPNTR WAS RENAMED TO EZEI-709
             05  EZEI-709              PIC S9(4) COMP.
      * STRPNTR1 WAS RENAMED TO EZEI-708
             05  EZEI-708              PIC S9(4) COMP.
      * ENDPNTR1 WAS RENAMED TO EZEI-707
             05  EZEI-707              PIC S9(4) COMP.
      * WCOUNT WAS RENAMED TO EZEI-706
             05  EZEI-706              PIC S9(9) COMP.
      * WWCOUNT WAS RENAMED TO EZEI-705
             05  EZEI-705              PIC S9(9).
      * WWWCOUNT WAS RENAMED TO EZEI-704
             05  EZEI-704              PIC X(9).
      * LGRGWIND WAS RENAMED TO EZEI-703
             05  EZEI-703              PIC S9(4) COMP.
      * END-PROC WAS RENAMED TO EZEI-702
             05  EZEI-702              PIC S9.
      * END-PROC0 WAS RENAMED TO EZEI-701
             05  EZEI-701              PIC S9.
      * END-LOOP WAS RENAMED TO EZEI-700
             05  EZEI-700              PIC S9.
      * END-LOAP WAS RENAMED TO EZEI-699
             05  EZEI-699              PIC S9.
      * FLAG WAS RENAMED TO EZEI-698
             05  EZEI-698              PIC X(1).
      * CORFLAG WAS RENAMED TO EZEI-697
             05  EZEI-697              PIC X(1).
      * CORFLAG2 WAS RENAMED TO EZEI-696
             05  EZEI-696              PIC S9(2).
      * F0 WAS RENAMED TO EZEI-174
             05  EZEI-174              PIC S9.
      * F1 WAS RENAMED TO EZEI-695
             05  EZEI-695              PIC S9.
      * F11 WAS RENAMED TO EZEI-175
             05  EZEI-175              PIC S9.
      * F2 WAS RENAMED TO EZEI-694
             05  EZEI-694              PIC S9.
      * F3 WAS RENAMED TO EZEI-693
             05  EZEI-693              PIC S9.
      * MFNAME1 WAS RENAMED TO EZEI-692
             05  EZEI-692              PIC X(15).
      * MSNAME1 WAS RENAMED TO EZEI-691
             05  EZEI-691              PIC X(15).
      * MTNAME1 WAS RENAMED TO EZEI-690
             05  EZEI-690              PIC X(15).
      * RECSTATR WAS RENAMED TO EZEI-689
             05  EZEI-689              PIC X(1).
      * WNATC WAS RENAMED TO EZEI-688
             05  EZEI-688              PIC S9(5).
      * WDATE1 WAS RENAMED TO EZEI-687
             05  EZEI-687              PIC X(10).
             05  EZE-REDEF-25 REDEFINES EZEI-687.
      * MM1 WAS RENAMED TO EZEI-686
              06  EZEI-686             PIC X(2).
      * S1 WAS RENAMED TO EZEI-685
              06  EZEI-685             PIC X(1).
      * DD1 WAS RENAMED TO EZEI-684
              06  EZEI-684             PIC X(2).
      * S2 WAS RENAMED TO EZEI-683
              06  EZEI-683             PIC X(1).
      * YY1 WAS RENAMED TO EZEI-682
              06  EZEI-682             PIC X(4).
      * HEALTH WAS RENAMED TO EZEI-681
             05  EZEI-681              PIC X(20).
      * NATIONALITY WAS RENAMED TO EZEI-680
             05  EZEI-680              PIC X(20).
      * NATIONALITY1 WAS RENAMED TO EZEI-679
             05  EZEI-679              PIC S9(9).
      * SERIAL10 WAS RENAMED TO EZEI-678
             05  EZEI-678              PIC S9(10).
             05  EZE-REDEF-26 REDEFINES EZEI-678.
      * SERIALA9 WAS RENAMED TO EZEI-677
              06  EZEI-677             PIC S9(9).
      * SERIALA1 WAS RENAMED TO EZEI-676
              06  EZEI-676             PIC X(1).
      * CSONUM9 WAS RENAMED TO EZEI-675
             05  EZEI-675              PIC S9(9).
      * IDNUM14 WAS RENAMED TO EZEI-674
             05  EZEI-674              PIC S9(14).
      * IDNUM14_CHR WAS RENAMED TO EZEI-673
             05  EZEI-673              PIC X(14).
      * IDNUM14CHR WAS RENAMED TO EZEI-672
             05  EZEI-672              PIC X(14).
             05  EZE-REDEF-27 REDEFINES EZEI-672.
      * IDNUM1C WAS RENAMED TO EZEI-671
              06  EZEI-671             PIC X(1).
      * IDNUM2C WAS RENAMED TO EZEI-670
              06  EZEI-670             PIC X(6).
      * IDNUM3C WAS RENAMED TO EZEI-669
              06  EZEI-669             PIC X(2).
      * IDNUM4C WAS RENAMED TO EZEI-668
              06  EZEI-668             PIC X(5).
      * INVREC WAS RENAMED TO EZEI-667
             05  EZEI-667              PIC X(84).
             05  EZE-REDEF-28 REDEFINES EZEI-667.
      * INVST WAS RENAMED TO EZEI-666
              06  EZEI-666             PIC X(1).
      * INVLEN WAS RENAMED TO EZEI-665
              06  EZEI-665             PIC S9(2).
      * INVCD WAS RENAMED TO EZEI-664
              06  EZEI-664             PIC X(1).
      * INVDATA WAS RENAMED TO EZEI-663
              06  EZEI-663             PIC X(80).
      * INVAREA WAS RENAMED TO EZEI-662
             05  EZEI-662              PIC X(84).
      * YEAR_2000 WAS RENAMED TO EZEI-661
             05  EZEI-661              PIC S9(4).
      * WNNK WAS RENAMED TO EZEI-660
             05  EZEI-660              PIC S9(9).
             05  EZE-REDEF-29 REDEFINES EZEI-660.
      * WNNKCEN WAS RENAMED TO EZEI-659
              06  EZEI-659             PIC S9.
      * WNNKDAT WAS RENAMED TO EZEI-658
              06  EZEI-658             PIC S9(6).
      * WNNKGOV WAS RENAMED TO EZEI-657
              06  EZEI-657             PIC S9(2).
      * REGNUM1 WAS RENAMED TO EZEI-656
             05  EZEI-656              PIC S9(9).
      * REGNUM2 WAS RENAMED TO EZEI-655
             05  EZEI-655              PIC S9(9).
      * WNNSER WAS RENAMED TO EZEI-654
             05  EZEI-654              PIC X(5).
             05  EZE-REDEF-30 REDEFINES EZEI-654.
      * WNNSER1 WAS RENAMED TO EZEI-653
              06  EZEI-653             PIC S9(4).
      * WNNSER2 WAS RENAMED TO EZEI-176
              06  EZEI-176             PIC X(1).
      * TDATE WAS RENAMED TO EZEI-652
             05  EZEI-652              PIC X(10).
             05  EZE-REDEF-31 REDEFINES EZEI-652.
      * TYDATE WAS RENAMED TO EZEI-651
              06  EZEI-651             PIC S9(4).
      * TTDATE WAS RENAMED TO EZEI-650
              06  EZEI-650             PIC X(6).
      * LGSYSDT WAS RENAMED TO EZEI-649
             05  EZEI-649              PIC X(6).
             05  EZE-REDEF-32 REDEFINES EZEI-649.
      * LGSYYY WAS RENAMED TO EZEI-648
              06  EZEI-648             PIC X(2).
      * LGSYMM WAS RENAMED TO EZEI-647
              06  EZEI-647             PIC X(2).
      * LGSYDD WAS RENAMED TO EZEI-646
              06  EZEI-646             PIC X(2).
      * RRDATE WAS RENAMED TO EZEI-645
             05  EZEI-645              PIC X(10).
             05  EZE-REDEF-33 REDEFINES EZEI-645.
      * RYDATE1 WAS RENAMED TO EZEI-644
              06  EZEI-644             PIC X(4).
      * SL11 WAS RENAMED TO EZEI-643
              06  EZEI-643             PIC X(1).
      * RMDATE1 WAS RENAMED TO EZEI-642
              06  EZEI-642             PIC X(2).
      * SL22 WAS RENAMED TO EZEI-641
              06  EZEI-641             PIC X(1).
      * RDDATE1 WAS RENAMED TO EZEI-640
              06  EZEI-640             PIC X(2).
      * RRDATE0 WAS RENAMED TO EZEI-639
             05  EZEI-639              PIC X(10).
             05  EZE-REDEF-34 REDEFINES EZEI-639.
      * RYDATE0 WAS RENAMED TO EZEI-638
              06  EZEI-638             PIC X(4).
      * SL110 WAS RENAMED TO EZEI-637
              06  EZEI-637             PIC X(1).
      * RMDATE0 WAS RENAMED TO EZEI-636
              06  EZEI-636             PIC X(2).
      * SL220 WAS RENAMED TO EZEI-635
              06  EZEI-635             PIC X(1).
      * RDDATE0 WAS RENAMED TO EZEI-634
              06  EZEI-634             PIC X(2).
      * MOITIME WAS RENAMED TO EZEI-633
             05  EZEI-633              PIC X(8).
             05  EZE-REDEF-35 REDEFINES EZEI-633.
      * MOIHH WAS RENAMED TO EZEI-632
              06  EZEI-632             PIC S9(2).
      * MOIDL1 WAS RENAMED TO EZEI-631
              06  EZEI-631             PIC X(1).
      * MOIMN WAS RENAMED TO EZEI-630
              06  EZEI-630             PIC S9(2).
      * MOIDL2 WAS RENAMED TO EZEI-629
              06  EZEI-629             PIC X(1).
      * MOISS WAS RENAMED TO EZEI-628
              06  EZEI-628             PIC S9(2).
      * MOITIME1 WAS RENAMED TO EZEI-627
             05  EZEI-627              PIC X(8).
             05  EZE-REDEF-36 REDEFINES EZEI-627.
      * MOIHH1 WAS RENAMED TO EZEI-626
              06  EZEI-626             PIC S9(2).
      * MOIDL11 WAS RENAMED TO EZEI-625
              06  EZEI-625             PIC X(1).
      * MOIMN1 WAS RENAMED TO EZEI-624
              06  EZEI-624             PIC S9(2).
      * MOIDL21 WAS RENAMED TO EZEI-623
              06  EZEI-623             PIC X(1).
      * MOISS1 WAS RENAMED TO EZEI-622
              06  EZEI-622             PIC S9(2).
      * MOIDATE WAS RENAMED TO EZEI-621
             05  EZEI-621              PIC X(10).
             05  EZE-REDEF-37 REDEFINES EZEI-621.
      * MOIYY WAS RENAMED TO EZEI-620
              06  EZEI-620             PIC S9(4).
      * MOID1 WAS RENAMED TO EZEI-619
              06  EZEI-619             PIC X(1).
      * MOIMM WAS RENAMED TO EZEI-618
              06  EZEI-618             PIC S9(2).
      * MOID2 WAS RENAMED TO EZEI-617
              06  EZEI-617             PIC X(1).
      * MOIDD WAS RENAMED TO EZEI-616
              06  EZEI-616             PIC S9(2).
      * MOIDATE1 WAS RENAMED TO EZEI-615
             05  EZEI-615              PIC X(10).
             05  EZE-REDEF-38 REDEFINES EZEI-615.
      * MOIYY1 WAS RENAMED TO EZEI-614
              06  EZEI-614             PIC S9(4).
      * MOID11 WAS RENAMED TO EZEI-613
              06  EZEI-613             PIC X(1).
      * MOIMM1 WAS RENAMED TO EZEI-612
              06  EZEI-612             PIC S9(2).
      * MOID21 WAS RENAMED TO EZEI-611
              06  EZEI-611             PIC X(1).
      * MOIDD1 WAS RENAMED TO EZEI-610
              06  EZEI-610             PIC S9(2).
      * CURDATE WAS RENAMED TO EZEI-609
             05  EZEI-609              PIC X(10).
             05  EZE-REDEF-39 REDEFINES EZEI-609.
      * DATEMM WAS RENAMED TO EZEI-608
              06  EZEI-608             PIC X(2).
      * SL1 WAS RENAMED TO EZEI-607
              06  EZEI-607             PIC X(1).
      * DATEDD WAS RENAMED TO EZEI-606
              06  EZEI-606             PIC X(2).
      * SL2 WAS RENAMED TO EZEI-605
              06  EZEI-605             PIC X(1).
      * DATEYY WAS RENAMED TO EZEI-604
              06  EZEI-604             PIC X(4).
      * WRKDATE WAS RENAMED TO EZEI-603
             05  EZEI-603              PIC X(10).
             05  EZE-REDEF-40 REDEFINES EZEI-603.
      * WRKDD WAS RENAMED TO EZEI-602
              06  EZEI-602             PIC X(2).
      * WRKSLSH1 WAS RENAMED TO EZEI-601
              06  EZEI-601             PIC X(1).
      * WRKMM WAS RENAMED TO EZEI-600
              06  EZEI-600             PIC X(2).
      * WRKSLSH2 WAS RENAMED TO EZEI-599
              06  EZEI-599             PIC X(1).
      * WRKYY WAS RENAMED TO EZEI-598
              06  EZEI-598             PIC X(4).
      * DNN0 WAS RENAMED TO EZEI-597
             05  EZEI-597              PIC X(14).
             05  EZE-REDEF-41 REDEFINES EZEI-597.
      * DNNKCEN0 WAS RENAMED TO EZEI-596
              06  EZEI-596             PIC X(1).
      * DNNKDAT0 WAS RENAMED TO EZEI-595
              06  EZEI-595             PIC X(6).
      * DNNKGOV0 WAS RENAMED TO EZEI-594
              06  EZEI-594             PIC X(2).
      * DNNKSER0 WAS RENAMED TO EZEI-593
              06  EZEI-593             PIC X(5).
      * RSERR WAS RENAMED TO EZEI-592
             05  EZEI-592              PIC S9(10).
             05  EZE-REDEF-42 REDEFINES EZEI-592.
      * RSER1 WAS RENAMED TO EZEI-591
              06  EZEI-591             PIC S9(9).
      * RSER2 WAS RENAMED TO EZEI-590
              06  EZEI-590             PIC S9.
      * CARDTIME WAS RENAMED TO EZEI-177
             05  EZEI-177              PIC X(8).
      * SERW WAS RENAMED TO EZEI-589
             05  EZEI-589              PIC S9(10).
      * GOV WAS RENAMED TO EZEI-588
             05  EZEI-588              PIC X(20).
      * POLICE WAS RENAMED TO EZEI-587
             05  EZEI-587              PIC X(20).
      * CIVIL WAS RENAMED TO EZEI-586
             05  EZEI-586              PIC X(20).
      * AREA WAS RENAMED TO EZEI-585
             05  EZEI-585              PIC X(20).
      * NUMFLD WAS RENAMED TO EZEI-584
             05  EZEI-584              PIC S9(10).
      * TMOFCD WAS RENAMED TO EZEI-583
             05  EZEI-583              PIC S9(4).
      * TMHOCD WAS RENAMED TO EZEI-582
             05  EZEI-582              PIC S9(5).
      * TBKNUM WAS RENAMED TO EZEI-581
             05  EZEI-581              PIC S9(4).
      * BREGDT WAS RENAMED TO EZEI-580
             05  EZEI-580              PIC X(10).
      * WREGNUM WAS RENAMED TO EZEI-579
             05  EZEI-579              PIC S9(6).
      * BFK_NATIONALITYCD WAS RENAMED TO EZEI-578
             05  EZEI-578              PIC S9(9) COMP.
      * MMHNAM WAS RENAMED TO EZEI-577
             05  EZEI-577              PIC X(60).
             05  EZE-REDEF-43 REDEFINES EZEI-577.
      * MMHNAM1 WAS RENAMED TO EZEI-576
              06  EZEI-576             PIC X(15).
      * MMHNAM2 WAS RENAMED TO EZEI-575
              06  EZEI-575             PIC X(15).
      * MMHNAM3 WAS RENAMED TO EZEI-574
              06  EZEI-574             PIC X(15).
      * MMHNAM4 WAS RENAMED TO EZEI-573
              06  EZEI-573             PIC X(15).
      * MMHNAM10 WAS RENAMED TO EZEI-572
             05  EZEI-572              PIC X(60).
      * MINHNAM WAS RENAMED TO EZEI-571
             05  EZEI-571              PIC X(25).
      * MINHNAM1 WAS RENAMED TO EZEI-570
             05  EZEI-570              PIC X(20).
      * MINHNAM20 WAS RENAMED TO EZEI-569
             05  EZEI-569              PIC X(20).
      * MINHNAM33 WAS RENAMED TO EZEI-178
             05  EZEI-178              PIC X(33).
      * MWHNAM WAS RENAMED TO EZEI-568
             05  EZEI-568              PIC X(25).
      * MAXHNAM WAS RENAMED TO EZEI-567
             05  EZEI-567              PIC X(25).
      * MAXHNAM1 WAS RENAMED TO EZEI-566
             05  EZEI-566              PIC X(20).
      * MAXHNAM20 WAS RENAMED TO EZEI-565
             05  EZEI-565              PIC X(20).
      * MAXHNAM33 WAS RENAMED TO EZEI-179
             05  EZEI-179              PIC X(33).
      * NAMEFLDH WAS RENAMED TO EZEI-564
             05  EZEI-564              PIC X(25).
             05  EZE-REDEF-44 REDEFINES EZEI-564.
      * NAMEBYTH WAS RENAMED TO EZEI-563
           06  EZEI-563             PIC X(1) OCCURS 25 INDEXED EZEIDX7.
      * NAMEFLDH20 WAS RENAMED TO EZEI-562
             05  EZEI-562              PIC X(20).
             05  EZE-REDEF-45 REDEFINES EZEI-562.
      * NAMEBYTH20 WAS RENAMED TO EZEI-561
           06  EZEI-561             PIC X(1) OCCURS 20 INDEXED EZEIDX8.
      * NAMEFLDH33 WAS RENAMED TO EZEI-560
             05  EZEI-560              PIC X(33).
             05  EZE-REDEF-46 REDEFINES EZEI-560.
      * NAMEBYTH33 WAS RENAMED TO EZEI-559
           06  EZEI-559             PIC X(1) OCCURS 33 INDEXED EZEIDX9.
      * LIKE1 WAS RENAMED TO EZEI-180
             05  EZEI-180              PIC X(22).
             05  EZE-REDEF-47 REDEFINES EZEI-180.
      * LIKE2 WAS RENAMED TO EZEI-181
              06  EZEI-181             PIC X(1).
      * LIKE3 WAS RENAMED TO EZEI-182
              06  EZEI-182             PIC X(20).
      * LIKE22 WAS RENAMED TO EZEI-183
              06  EZEI-183             PIC X(1).
      * LIKE4 WAS RENAMED TO EZEI-184
           05  EZEI-184              PIC X(1) OCCURS 19 INDEXED
            EZEIDX10.
      * I WAS RENAMED TO EZEI-558
             05  EZEI-558              PIC S9(4) COMP.
      * N WAS RENAMED TO EZEI-557
             05  EZEI-557              PIC S9(4) COMP.
      * J WAS RENAMED TO EZEI-556
             05  EZEI-556              PIC S9(4) COMP.
      * P WAS RENAMED TO EZEI-185
             05  EZEI-185              PIC S9(4) COMP.
      * Y WAS RENAMED TO EZEI-186
             05  EZEI-186              PIC S9(4) COMP.
      * YY WAS RENAMED TO EZEI-187
             05  EZEI-187              PIC S9(4) COMP.
      * Z WAS RENAMED TO EZEI-188
             05  EZEI-188              PIC S9(4) COMP.
      * ZZ WAS RENAMED TO EZEI-189
             05  EZEI-189              PIC S9(4) COMP.
      * ERRFLAG WAS RENAMED TO EZEI-555
             05  EZEI-555              PIC S9.
      * CMPDATA WAS RENAMED TO EZEI-554
             05  EZEI-554              PIC X(60).
      * CMPLEN WAS RENAMED TO EZEI-553
             05  EZEI-553              PIC S9(2).
      * CMPDATA100 WAS RENAMED TO EZEI-552
             05  EZEI-552              PIC X(100).
      * CMPLEN4 WAS RENAMED TO EZEI-551
             05  EZEI-551              PIC S9(4).
      * RESULT WAS RENAMED TO EZEI-550
             05  EZEI-550              PIC S9(9) COMP.
      * RESULT1 WAS RENAMED TO EZEI-549
             05  EZEI-549              PIC S9(9) COMP.
      * RESULT2 WAS RENAMED TO EZEI-548
             05  EZEI-548              PIC S9(9) COMP.
      * VAR1 WAS RENAMED TO EZEI-547
             05  EZEI-547              PIC S9(2).
      * VAR1N WAS RENAMED TO EZEI-190
             05  EZEI-190              PIC S9.
      * VAR1C WAS RENAMED TO EZEI-191
             05  EZEI-191              PIC X(1).
      * VAR2N WAS RENAMED TO EZEI-546
             05  EZEI-546              PIC S9(2).
      * VAR21 WAS RENAMED TO EZEI-192
             05  EZEI-192              PIC S9(2).
      * VAR211 WAS RENAMED TO EZEI-193
             05  EZEI-193              PIC S9(2).
      * VAR2C WAS RENAMED TO EZEI-194
             05  EZEI-194              PIC X(2).
      * VAR3 WAS RENAMED TO EZEI-545
             05  EZEI-545              PIC X(3).
      * VAR3N WAS RENAMED TO EZEI-195
             05  EZEI-195              PIC S9(3).
      * VAR4 WAS RENAMED TO EZEI-544
             05  EZEI-544              PIC S9(4).
      * VAR4N WAS RENAMED TO EZEI-196
             05  EZEI-196              PIC S9(4).
      * VAR4B WAS RENAMED TO EZEI-197
             05  EZEI-197              PIC S9(4) COMP.
      * VAR41 WAS RENAMED TO EZEI-198
             05  EZEI-198              PIC S9(4).
      * VAR4C WAS RENAMED TO EZEI-199
             05  EZEI-199              PIC X(4).
      * VAR5 WAS RENAMED TO EZEI-200
             05  EZEI-200              PIC S9(5).
      * VAR5N WAS RENAMED TO EZEI-201
             05  EZEI-201              PIC S9(5).
             05  EZE-REDEF-48 REDEFINES EZEI-201.
      * VAR5N1 WAS RENAMED TO EZEI-202
              06  EZEI-202             PIC S9(2).
      * VAR5N2 WAS RENAMED TO EZEI-203
              06  EZEI-203             PIC S9(3).
      * VAR6 WAS RENAMED TO EZEI-543
             05  EZEI-543              PIC S9(6).
      * VAR6C WAS RENAMED TO EZEI-204
             05  EZEI-204              PIC X(6).
      * VAR7 WAS RENAMED TO EZEI-542
             05  EZEI-542              PIC S9(6).
      * VARC7 WAS RENAMED TO EZEI-541
             05  EZEI-541              PIC X(7).
      * VARN7 WAS RENAMED TO EZEI-205
             05  EZEI-205              PIC S9(7).
      * VAR8N WAS RENAMED TO EZEI-206
             05  EZEI-206              PIC S9(8).
      * VAR9 WAS RENAMED TO EZEI-207
             05  EZEI-207              PIC S9(9).
      * VAR9N WAS RENAMED TO EZEI-208
             05  EZEI-208              PIC S9(9).
      * VAR9B WAS RENAMED TO EZEI-209
             05  EZEI-209              PIC S9(9) COMP.
      * VAR9C WAS RENAMED TO EZEI-210
             05  EZEI-210              PIC X(9).
      * VAR10N WAS RENAMED TO EZEI-211
             05  EZEI-211              PIC S9(10).
      * VAR10C WAS RENAMED TO EZEI-212
             05  EZEI-212              PIC X(10).
             05  EZE-REDEF-49 REDEFINES EZEI-212.
      * VYDATE0 WAS RENAMED TO EZEI-540
              06  EZEI-540             PIC X(4).
      * SLV1 WAS RENAMED TO EZEI-539
              06  EZEI-539             PIC X(1).
      * VMDATE0 WAS RENAMED TO EZEI-538
              06  EZEI-538             PIC X(2).
      * SLV2 WAS RENAMED TO EZEI-537
              06  EZEI-537             PIC X(1).
      * VDDATE0 WAS RENAMED TO EZEI-536
              06  EZEI-536             PIC X(2).
      * VAR10CC WAS RENAMED TO EZEI-213
             05  EZEI-213              PIC X(10).
             05  EZE-REDEF-50 REDEFINES EZEI-213.
      * VAR10CC1 WAS RENAMED TO EZEI-214
              06  EZEI-214             PIC X(4).
      * VAR10CC2 WAS RENAMED TO EZEI-215
              06  EZEI-215             PIC X(6).
      * VARMC1 WAS RENAMED TO EZEI-535
             05  EZEI-535              PIC X(1).
      * VARMC2 WAS RENAMED TO EZEI-534
             05  EZEI-534              PIC S9(2).
      * Q WAS RENAMED TO EZEI-533
             05  EZEI-533              PIC X(2).
             05  EZE-GROUP-51          OCCURS 350 INDEXED EZEIDX11.
      * CQW05 WAS RENAMED TO EZEI-532
              06  EZEI-532             PIC X(95).
              06  EZE-REDEF-52 REDEFINES EZEI-532.
      * ABNAME WAS RENAMED TO EZEI-531
               07  EZEI-531            PIC X(15).
      * AFFNAME WAS RENAMED TO EZEI-530
               07  EZEI-530            PIC X(15).
      * AFSNAME WAS RENAMED TO EZEI-529
               07  EZEI-529            PIC X(15).
      * AFTNAME WAS RENAMED TO EZEI-528
               07  EZEI-528            PIC X(15).
      * ADATE WAS RENAMED TO EZEI-527
               07  EZEI-527            PIC X(10).
      * ASLCOD WAS RENAMED TO EZEI-526
               07  EZEI-526            PIC S9(10).
      * AMNAME WAS RENAMED TO EZEI-525
               07  EZEI-525            PIC X(15).
             05  EZE-GROUP-53          OCCURS 350 INDEXED EZEIDX12.
      * CQW051 WAS RENAMED TO EZEI-524
              06  EZEI-524             PIC X(62).
              06  EZE-REDEF-54 REDEFINES EZEI-524.
      * RISSUD WAS RENAMED TO EZEI-523
               07  EZEI-523            PIC X(10).
      * RREG WAS RENAMED TO EZEI-522
               07  EZEI-522            PIC S9(4).
      * RVALC WAS RENAMED TO EZEI-521
               07  EZEI-521            PIC X(1).
      * RSLCOD WAS RENAMED TO EZEI-520
               07  EZEI-520            PIC S9(10).
      * RACQPACNUM WAS RENAMED TO EZEI-519
               07  EZEI-519            PIC S9(9).
      * RAPPLNUM WAS RENAMED TO EZEI-518
               07  EZEI-518            PIC S9(10).
      * RFCN WAS RENAMED TO EZEI-517
               07  EZEI-517            PIC X(9).
      * RPRODNUM WAS RENAMED TO EZEI-216
               07  EZEI-216            PIC S9(9).
             05  EZE-GROUP-55          OCCURS 350 INDEXED EZEIDX13.
      * CQW05-UN WAS RENAMED TO EZEI-516
              06  EZEI-516             PIC X(110).
              06  EZE-REDEF-56 REDEFINES EZEI-516.
      * ABNAMEU WAS RENAMED TO EZEI-515
               07  EZEI-515            PIC X(15).
      * AFFNAMEU WAS RENAMED TO EZEI-514
               07  EZEI-514            PIC X(15).
      * AFSNAMEU WAS RENAMED TO EZEI-513
               07  EZEI-513            PIC X(15).
      * AFTNAMEU WAS RENAMED TO EZEI-512
               07  EZEI-512            PIC X(15).
      * ADATEU WAS RENAMED TO EZEI-511
               07  EZEI-511            PIC X(10).
      * ASLCODU WAS RENAMED TO EZEI-510
               07  EZEI-510            PIC S9(10).
      * AMNAMEU WAS RENAMED TO EZEI-217
               07  EZEI-217            PIC X(15).
      * ADATSRCU WAS RENAMED TO EZEI-218
               07  EZEI-218            PIC X(1).
      * AIDNUMU WAS RENAMED TO EZEI-219
               07  EZEI-219            PIC S9(14).
             05  EZE-GROUP-57          OCCURS 100 INDEXED EZEIDX14.
      * CQW052 WAS RENAMED TO EZEI-509
              06  EZEI-509             PIC X(51).
              06  EZE-REDEF-58 REDEFINES EZEI-509.
      * TOPID WAS RENAMED TO EZEI-508
               07  EZEI-508            PIC S9(4).
      * TOPNAME WAS RENAMED TO EZEI-507
               07  EZEI-507            PIC X(20).
      * TFORM_NUMBER WAS RENAMED TO EZEI-506
               07  EZEI-506            PIC S9(9).
      * TISSUE_DATE WAS RENAMED TO EZEI-505
               07  EZEI-505            PIC X(10).
      * TTERM_ID WAS RENAMED TO EZEI-504
               07  EZEI-504            PIC X(4).
      * TCERT_TYPE WAS RENAMED TO EZEI-503
               07  EZEI-503            PIC S9(4).
             05  EZE-GROUP-59          OCCURS 100 INDEXED EZEIDX15.
      * NQA12REC WAS RENAMED TO EZEI-502
              06  EZEI-502             PIC X(93).
              06  EZE-REDEF-60 REDEFINES EZEI-502.
      * ASLCOD3 WAS RENAMED TO EZEI-501
               07  EZEI-501            PIC S9(10).
      * ABNAME3 WAS RENAMED TO EZEI-500
               07  EZEI-500            PIC X(29).
      * IDNUM3 WAS RENAMED TO EZEI-499
               07  EZEI-499            PIC S9(14).
      * REGNUM3 WAS RENAMED TO EZEI-498
               07  EZEI-498            PIC S9(6).
      * REGDATE3 WAS RENAMED TO EZEI-497
               07  EZEI-497            PIC X(10).
      * MSTATUS3 WAS RENAMED TO EZEI-496
               07  EZEI-496            PIC X(4).
      * APPL_FORM_NUM3 WAS RENAMED TO EZEI-495
               07  EZEI-495            PIC S9(10).
      * APPLTYPE3 WAS RENAMED TO EZEI-494
               07  EZEI-494            PIC X(1).
      * FAMILY_NUMBER3 WAS RENAMED TO EZEI-493
               07  EZEI-493            PIC S9(9).
             05  EZE-GROUP-61          OCCURS 100 INDEXED EZEIDX16.
      * NQA11REC WAS RENAMED TO EZEI-492
              06  EZEI-492             PIC X(59).
              06  EZE-REDEF-62 REDEFINES EZEI-492.
      * APPL_FORM_NUM2 WAS RENAMED TO EZEI-491
               07  EZEI-491            PIC S9(10).
      * APPL_TYPE2 WAS RENAMED TO EZEI-490
               07  EZEI-490            PIC X(1).
      * FAMILY_NUMBER2 WAS RENAMED TO EZEI-489
               07  EZEI-489            PIC S9(9).
      * MSTATUS2 WAS RENAMED TO EZEI-488
               07  EZEI-488            PIC X(4).
      * SNAME2 WAS RENAMED TO EZEI-487
               07  EZEI-487            PIC X(25).
      * ASLCOD2 WAS RENAMED TO EZEI-486
               07  EZEI-486            PIC S9(10).
             05  EZE-GROUP-63          OCCURS 100 INDEXED EZEIDX17.
      * NQA10REC WAS RENAMED TO EZEI-485
              06  EZEI-485             PIC X(69).
              06  EZE-REDEF-64 REDEFINES EZEI-485.
      * ABNAME1 WAS RENAMED TO EZEI-484
               07  EZEI-484            PIC X(15).
      * AFFNAME1 WAS RENAMED TO EZEI-483
               07  EZEI-483            PIC X(30).
      * ADATE1 WAS RENAMED TO EZEI-482
               07  EZEI-482            PIC X(10).
      * ASLCOD1 WAS RENAMED TO EZEI-481
               07  EZEI-481            PIC S9(10).
      * MSTATUS1 WAS RENAMED TO EZEI-480
               07  EZEI-480            PIC X(4).
             05  EZE-GROUP-65          OCCURS 100 INDEXED EZEIDX18.
      * FAMDISPREC WAS RENAMED TO EZEI-479
              06  EZEI-479             PIC X(160).
              06  EZE-REDEF-66 REDEFINES EZEI-479.
      * ABNAME9 WAS RENAMED TO EZEI-478
               07  EZEI-478            PIC X(15).
      * AIDNUM9 WAS RENAMED TO EZEI-477
               07  EZEI-477            PIC S9(14).
      * ADATE9 WAS RENAMED TO EZEI-476
               07  EZEI-476            PIC X(10).
      * ADATE91 WAS RENAMED TO EZEI-475
               07  EZEI-475            PIC X(10).
      * ANUM9 WAS RENAMED TO EZEI-220
               07  EZEI-220            PIC S9(6).
      * ASLCOD9 WAS RENAMED TO EZEI-474
               07  EZEI-474            PIC X(10).
      * MSTATUS9 WAS RENAMED TO EZEI-473
               07  EZEI-473            PIC X(1).
      * MSTATUS91 WAS RENAMED TO EZEI-472
               07  EZEI-472            PIC X(1).
      * AMOTHER9 WAS RENAMED TO EZEI-221
               07  EZEI-221            PIC X(15).
      * AMOTHER459 WAS RENAMED TO EZEI-222
               07  EZEI-222            PIC X(60).
      * ANOTES9 WAS RENAMED TO EZEI-223
               07  EZEI-223            PIC S9(9).
      * AFNAT WAS RENAMED TO EZEI-224
               07  EZEI-224            PIC S9(9).
             05  EZE-GROUP-67          OCCURS 100 INDEXED EZEIDX19.
      * FAMDISPREC9A WAS RENAMED TO EZEI-471
              06  EZEI-471             PIC X(146).
              06  EZE-REDEF-68 REDEFINES EZEI-471.
      * ABNAME9A WAS RENAMED TO EZEI-470
               07  EZEI-470            PIC X(15).
      * AIDNUM9A WAS RENAMED TO EZEI-469
               07  EZEI-469            PIC S9(14).
      * ADATE9A WAS RENAMED TO EZEI-468
               07  EZEI-468            PIC X(10).
      * ANUM9A WAS RENAMED TO EZEI-225
               07  EZEI-225            PIC S9(9).
      * ASLCOD9A WAS RENAMED TO EZEI-467
               07  EZEI-467            PIC X(10).
      * MSTATUS9A WAS RENAMED TO EZEI-466
               07  EZEI-466            PIC X(4).
      * AMOTHER9A WAS RENAMED TO EZEI-226
               07  EZEI-226            PIC X(15).
      * AMOTHER459A WAS RENAMED TO EZEI-227
               07  EZEI-227            PIC X(60).
      * ANOTES9A WAS RENAMED TO EZEI-228
               07  EZEI-228            PIC S9(9).
             05  EZE-GROUP-69          OCCURS 20 INDEXED EZEIDX20.
      * SLOCALOFFICE WAS RENAMED TO EZEI-465
              06  EZEI-465             PIC X(50).
              06  EZE-REDEF-70 REDEFINES EZEI-465.
      * SLOCALDESC WAS RENAMED TO EZEI-464
               07  EZEI-464            PIC X(20).
      * SSDATE WAS RENAMED TO EZEI-463
               07  EZEI-463            PIC X(10).
      * SEDATE WAS RENAMED TO EZEI-462
               07  EZEI-462            PIC X(10).
      * SSTATUS WAS RENAMED TO EZEI-461
               07  EZEI-461            PIC X(1).
      * SACQCODE WAS RENAMED TO EZEI-460
               07  EZEI-460            PIC S9(9).
      * LINENO WAS RENAMED TO EZEI-459
             05  EZEI-459              PIC S9.
             05  EZE-GROUP-71          OCCURS 180 INDEXED EZEIDX21.
      * ACQLIST0 WAS RENAMED TO EZEI-458
              06  EZEI-458             PIC X(84).
              06  EZE-REDEF-72 REDEFINES EZEI-458.
      * ANAME WAS RENAMED TO EZEI-457
               07  EZEI-457            PIC X(15).
      * AONAMES WAS RENAMED TO EZEI-456
               07  EZEI-456            PIC X(40).
      * ADATEOFBIRTH WAS RENAMED TO EZEI-455
               07  EZEI-455            PIC X(10).
      * AAPPLNUM WAS RENAMED TO EZEI-454
               07  EZEI-454            PIC S9(10).
      * ASEQNUM WAS RENAMED TO EZEI-453
               07  EZEI-453            PIC S9(9).
             05  EZE-GROUP-73          OCCURS 180 INDEXED EZEIDX22.
      * ACQLIST21 WAS RENAMED TO EZEI-452
              06  EZEI-452             PIC X(117).
              06  EZE-REDEF-74 REDEFINES EZEI-452.
      * ANAME2 WAS RENAMED TO EZEI-451
               07  EZEI-451            PIC X(40).
      * AREASON2 WAS RENAMED TO EZEI-450
               07  EZEI-450            PIC X(40).
      * ADATEOFBIRTH2 WAS RENAMED TO EZEI-449
               07  EZEI-449            PIC X(14).
      * AAPPLNUM2 WAS RENAMED TO EZEI-448
               07  EZEI-448            PIC S9(10).
      * ASEQNUM2 WAS RENAMED TO EZEI-447
               07  EZEI-447            PIC S9(4).
      * ACSONUM2 WAS RENAMED TO EZEI-446
               07  EZEI-446            PIC S9(9).
             05  EZE-GROUP-75          OCCURS 180 INDEXED EZEIDX23.
      * ACQLIST01 WAS RENAMED TO EZEI-445
              06  EZEI-445             PIC X(98).
              06  EZE-REDEF-76 REDEFINES EZEI-445.
      * ANAME1 WAS RENAMED TO EZEI-444
               07  EZEI-444            PIC X(15).
      * AONAMES1 WAS RENAMED TO EZEI-443
               07  EZEI-443            PIC X(40).
      * ADATEOFBIRTH1 WAS RENAMED TO EZEI-442
               07  EZEI-442            PIC X(10).
      * AAPPLNUM1 WAS RENAMED TO EZEI-441
               07  EZEI-441            PIC S9(10).
      * ASEQNUM1 WAS RENAMED TO EZEI-440
               07  EZEI-440            PIC S9(9).
      * ASTAT1 WAS RENAMED TO EZEI-439
               07  EZEI-439            PIC X(1).
      * AAPPL_RSN1 WAS RENAMED TO EZEI-438
               07  EZEI-438            PIC S9(4).
      * ACSONUM1 WAS RENAMED TO EZEI-229
               07  EZEI-229            PIC S9(9).
             05  EZE-GROUP-77          OCCURS 100 INDEXED EZEIDX24.
      * ACQLISTO WAS RENAMED TO EZEI-230
              06  EZEI-230             PIC X(44).
              06  EZE-REDEF-78 REDEFINES EZEI-230.
      * ACQNUM WAS RENAMED TO EZEI-231
               07  EZEI-231            PIC S9(10).
      * LOCATION WAS RENAMED TO EZEI-232
               07  EZEI-232            PIC S9(9).
      * DATEOFAPPL WAS RENAMED TO EZEI-233
               07  EZEI-233            PIC X(10).
      * NUMOFAPPL1 WAS RENAMED TO EZEI-234
               07  EZEI-234            PIC S9(4).
      * DTOFAPPL1 WAS RENAMED TO EZEI-235
               07  EZEI-235            PIC X(10).
      * ACQ_STAT1 WAS RENAMED TO EZEI-236
               07  EZEI-236            PIC X(1).
      * USR30 WAS RENAMED TO EZEI-237
             05  EZEI-237              PIC S9(2).
      * LINENO1 WAS RENAMED TO EZEI-437
             05  EZEI-437              PIC S9.
             05  EZE-GROUP-79          OCCURS 600 INDEXED EZEIDX25.
      * OPERSEC_RES WAS RENAMED TO EZEI-436
              06  EZEI-436             PIC X(39).
              06  EZE-REDEF-80 REDEFINES EZEI-436.
      * OPERID0 WAS RENAMED TO EZEI-435
               07  EZEI-435            PIC X(6).
      * OPERNAME0 WAS RENAMED TO EZEI-434
               07  EZEI-434            PIC X(20).
      * SEDATE0 WAS RENAMED TO EZEI-433
               07  EZEI-433            PIC X(10).
      * ACTIVE0 WAS RENAMED TO EZEI-432
               07  EZEI-432            PIC X(3).
      * FIRSTGOV1 WAS RENAMED TO EZEI-431
             05  EZEI-431              PIC X(3).
      * FIRSTGOV2 WAS RENAMED TO EZEI-430
             05  EZEI-430              PIC S9(4).
      * TITRGG2 WAS RENAMED TO EZEI-429
             05  EZEI-429              PIC X(2).
      * IDNUM-REC WAS RENAMED TO EZEI-428
             05  EZEI-428              PIC S9(14).
             05  EZE-REDEF-81 REDEFINES EZEI-428.
      * CEN-REC WAS RENAMED TO EZEI-427
              06  EZEI-427             PIC S9.
      * YY-REC WAS RENAMED TO EZEI-426
              06  EZEI-426             PIC S9(2).
      * MM-REC WAS RENAMED TO EZEI-425
              06  EZEI-425             PIC S9(2).
      * DD-REC WAS RENAMED TO EZEI-424
              06  EZEI-424             PIC S9(2).
      * GOV-REC WAS RENAMED TO EZEI-423
              06  EZEI-423             PIC S9(2).
      * SER1-REC WAS RENAMED TO EZEI-422
              06  EZEI-422             PIC S9(3).
      * SER2-REC WAS RENAMED TO EZEI-421
              06  EZEI-421             PIC S9.
      * CHK-REC WAS RENAMED TO EZEI-420
              06  EZEI-420             PIC S9.
      * BRTHREC WAS RENAMED TO EZEI-419
             05  EZEI-419              PIC X(10).
             05  EZE-REDEF-82 REDEFINES EZEI-419.
      * RECCEN WAS RENAMED TO EZEI-418
              06  EZEI-418             PIC X(2).
      * RECYY WAS RENAMED TO EZEI-417
              06  EZEI-417             PIC X(2).
      * RECSL1 WAS RENAMED TO EZEI-416
              06  EZEI-416             PIC X(1).
      * RECMM WAS RENAMED TO EZEI-415
              06  EZEI-415             PIC X(2).
      * RECSL2 WAS RENAMED TO EZEI-414
              06  EZEI-414             PIC X(1).
      * RECDD WAS RENAMED TO EZEI-413
              06  EZEI-413             PIC X(2).
      * TMPFCN WAS RENAMED TO EZEI-412
             05  EZEI-412              PIC X(9).
      * TMPAPPLNUM WAS RENAMED TO EZEI-411
             05  EZEI-411              PIC S9(10).
      * TMPACQ WAS RENAMED TO EZEI-410
             05  EZEI-410              PIC S9(9).
      * NOPID WAS RENAMED TO EZEI-409
             05  EZEI-409              PIC S9(5).
      * REGIONAL_CENTER1 WAS RENAMED TO EZEI-408
             05  EZEI-408              PIC S9(4).
      * END-PROC-1 WAS RENAMED TO EZEI-407
             05  EZEI-407              PIC S9.
      * TIME8 WAS RENAMED TO EZEI-406
             05  EZEI-406              PIC X(8).
             05  EZE-REDEF-83 REDEFINES EZEI-406.
      * HR WAS RENAMED TO EZEI-405
              06  EZEI-405             PIC S9(2).
      * SEM1 WAS RENAMED TO EZEI-404
              06  EZEI-404             PIC X(1).
      * MN WAS RENAMED TO EZEI-403
              06  EZEI-403             PIC S9(2).
      * SEM2 WAS RENAMED TO EZEI-402
              06  EZEI-402             PIC X(1).
      * SC WAS RENAMED TO EZEI-401
              06  EZEI-401             PIC S9(2).
      * CHKS WAS RENAMED TO EZEI-400
           05  EZEI-400              PIC S9 OCCURS 20 INDEXED EZEIDX26.
      * VAR22 WAS RENAMED TO EZEI-399
             05  EZEI-399              PIC S9(4).
             05  EZE-REDEF-84 REDEFINES EZEI-399.
      * VAR221 WAS RENAMED TO EZEI-398
              06  EZEI-398             PIC S9(2).
      * VAR222 WAS RENAMED TO EZEI-397
              06  EZEI-397             PIC S9(2).
      * QUALDEGREE WAS RENAMED TO EZEI-238
             05  EZEI-238              PIC X(68).
      * MASG WAS RENAMED TO EZEI-239
             05  EZEI-239              PIC X(40).
             05  EZE-REDEF-85 REDEFINES EZEI-239.
      * REG_CEN WAS RENAMED TO EZEI-240
              06  EZEI-240             PIC X(16).
      * ORG_MSG WAS RENAMED TO EZEI-241
              06  EZEI-241             PIC X(24).
      * MASG1 WAS RENAMED TO EZEI-242
             05  EZEI-242              PIC X(40).
             05  EZE-REDEF-86 REDEFINES EZEI-242.
      * ORG_MSG1 WAS RENAMED TO EZEI-243
              06  EZEI-243             PIC X(19).
      * USERID WAS RENAMED TO EZEI-244
              06  EZEI-244             PIC X(5).
      * REG_CEN1 WAS RENAMED TO EZEI-245
              06  EZEI-245             PIC X(16).
      * DNUM WAS RENAMED TO EZEI-246
             05  EZEI-246              PIC X(15).
      * MMNAME WAS RENAMED TO EZEI-247
             05  EZEI-247              PIC X(60).
             05  EZE-REDEF-87 REDEFINES EZEI-247.
      * MMNAME1 WAS RENAMED TO EZEI-248
              06  EZEI-248             PIC X(15).
      * MMNAME2 WAS RENAMED TO EZEI-249
              06  EZEI-249             PIC X(45).
      * CHK_NAME WAS RENAMED TO EZEI-250
             05  EZEI-250              PIC X(45).
      * WFJOBC WAS RENAMED TO EZEI-251
             05  EZEI-251              PIC S9(9).
      * WMJOBT WAS RENAMED TO EZEI-252
             05  EZEI-252              PIC X(25).
      * WMJOBC WAS RENAMED TO EZEI-253
             05  EZEI-253              PIC S9(9).
      * JKL WAS RENAMED TO EZEI-396
             05  EZEI-396              PIC S9(4) COMP.
      * APPL_FORM_NUM WAS RENAMED TO EZEI-395
             05  EZEI-395              PIC S9(10).
      * APPL_TYPE WAS RENAMED TO EZEI-394
             05  EZEI-394              PIC X(1).
      * FAM_LAST_SERIAL WAS RENAMED TO EZEI-393
             05  EZEI-393              PIC S9(9) COMP.
             05  EZE-GROUP-88          OCCURS 400 INDEXED EZEIDX27.
      * LOCKUP_REC WAS RENAMED TO EZEI-254
              06  EZEI-254             PIC X(51).
              06  EZE-REDEF-89 REDEFINES EZEI-254.
      * ASR WAS RENAMED TO EZEI-255
               07  EZEI-255            PIC X(1).
      * ADESC WAS RENAMED TO EZEI-256
               07  EZEI-256            PIC X(44).
      * ACODE WAS RENAMED TO EZEI-257
               07  EZEI-257            PIC S9(6).
      * I2 WAS RENAMED TO EZEI-258
             05  EZEI-258              PIC S9(4) COMP.
      * N2 WAS RENAMED TO EZEI-259
             05  EZEI-259              PIC S9(4) COMP.
      * STRPNTR2 WAS RENAMED TO EZEI-260
             05  EZEI-260              PIC S9(4) COMP.
      * ENDPNTR2 WAS RENAMED TO EZEI-261
             05  EZEI-261              PIC S9(4) COMP.
      * NTT WAS RENAMED TO EZEI-392
             05  EZEI-392              PIC X(4).
             05  EZE-REDEF-90 REDEFINES EZEI-392.
      * MNTT WAS RENAMED TO EZEI-391
              06  EZEI-391             PIC X(2).
      * DNTT WAS RENAMED TO EZEI-390
              06  EZEI-390             PIC X(2).
      * ADAT1 WAS RENAMED TO EZEI-262
             05  EZEI-262              PIC X(10).
             05  EZE-REDEF-91 REDEFINES EZEI-262.
      * ADATY1 WAS RENAMED TO EZEI-263
              06  EZEI-263             PIC X(4).
      * ADATMD1 WAS RENAMED TO EZEI-264
              06  EZEI-264             PIC X(1).
      * ADATM1 WAS RENAMED TO EZEI-265
              06  EZEI-265             PIC X(2).
      * ADATYM1 WAS RENAMED TO EZEI-266
              06  EZEI-266             PIC X(1).
      * ADATD1 WAS RENAMED TO EZEI-267
              06  EZEI-267             PIC X(2).
      * FST1 WAS RENAMED TO EZEI-268
             05  EZEI-268              PIC X(60).
      * FST2 WAS RENAMED TO EZEI-269
             05  EZEI-269              PIC S9(4) COMP.
      * CHKX1 WAS RENAMED TO EZEI-270
             05  EZEI-270              PIC X(10).
      * CHKY1 WAS RENAMED TO EZEI-271
             05  EZEI-271              PIC S9(4) COMP.
      * CHKZ1 WAS RENAMED TO EZEI-272
             05  EZEI-272              PIC S9(4) COMP.
      * VAR20 WAS RENAMED TO EZEI-273
             05  EZEI-273              PIC X(20).
             05  EZE-REDEF-92 REDEFINES EZEI-273.
      * IDVAR1 WAS RENAMED TO EZEI-274
              06  EZEI-274             PIC X(1).
      * IDVAR2 WAS RENAMED TO EZEI-275
              06  EZEI-275             PIC X(6).
      * IDVAR3 WAS RENAMED TO EZEI-276
              06  EZEI-276             PIC X(2).
      * IDVAR4 WAS RENAMED TO EZEI-277
              06  EZEI-277             PIC X(5).
      * IDVAR5 WAS RENAMED TO EZEI-278
              06  EZEI-278             PIC X(6).
      * S-NAME WAS RENAMED TO EZEI-279
             05  EZEI-279              PIC X(30).
             05  EZE-REDEF-93 REDEFINES EZEI-279.
      * T-NAME WAS RENAMED TO EZEI-280
           06  EZEI-280             PIC X(1) OCCURS 30 INDEXED EZEIDX28.
      * L WAS RENAMED TO EZEI-281
             05  EZEI-281              PIC S9(2).
      * M WAS RENAMED TO EZEI-282
             05  EZEI-282              PIC S9(2).
      * K WAS RENAMED TO EZEI-283
             05  EZEI-283              PIC S9(2).
      * S-NAME1 WAS RENAMED TO EZEI-284
             05  EZEI-284              PIC X(15).
             05  EZE-REDEF-94 REDEFINES EZEI-284.
      * T-NAME1 WAS RENAMED TO EZEI-285
           06  EZEI-285             PIC X(1) OCCURS 15 INDEXED EZEIDX29.
      * PDA_NAME WAS RENAMED TO EZEI-286
             05  EZEI-286              PIC X(15).
      * PERS_NAME WAS RENAMED TO EZEI-287
             05  EZEI-287              PIC X(15).
      * VAR33ONE WAS RENAMED TO EZEI-288
             05  EZEI-288              PIC X(33).
             05  EZE-REDEF-95 REDEFINES EZEI-288.
      * VARONE33 WAS RENAMED TO EZEI-289
           06  EZEI-289             PIC X(1) OCCURS 33 INDEXED EZEIDX30.
      * VAR33ONEA WAS RENAMED TO EZEI-290
             05  EZEI-290              PIC X(33).
             05  EZE-REDEF-96 REDEFINES EZEI-290.
      * VARONE33A WAS RENAMED TO EZEI-291
           06  EZEI-291             PIC X(1) OCCURS 33 INDEXED EZEIDX31.
      * VAR20ONE WAS RENAMED TO EZEI-292
             05  EZEI-292              PIC X(20).
             05  EZE-REDEF-97 REDEFINES EZEI-292.
      * VARONE20 WAS RENAMED TO EZEI-293
           06  EZEI-293             PIC X(1) OCCURS 20 INDEXED EZEIDX32.
      * VAR20ONEA WAS RENAMED TO EZEI-294
             05  EZEI-294              PIC X(20).
             05  EZE-REDEF-98 REDEFINES EZEI-294.
      * VARONE20A WAS RENAMED TO EZEI-295
           06  EZEI-295             PIC X(1) OCCURS 20 INDEXED EZEIDX33.
      * ID15 WAS RENAMED TO EZEI-296
             05  EZEI-296              PIC X(4500).
             05  EZE-REDEF-99 REDEFINES EZEI-296.
      * ID15A WAS RENAMED TO EZEI-297
           06  EZEI-297             PIC X(15) OCCURS 300 INDEXED
            EZEIDX34.
      * KFK_CIVIL_OFFICECD WAS RENAMED TO EZEI-298
             05  EZEI-298              PIC S9(4) COMP.
      * KTYP1 WAS RENAMED TO EZEI-299
             05  EZEI-299              PIC X(1).
      * KFK_GOVERNORATECD WAS RENAMED TO EZEI-300
             05  EZEI-300              PIC S9(4) COMP.
      * KNUM9 WAS RENAMED TO EZEI-301
             05  EZEI-301              PIC S9(9) COMP.
      * WNAME0 WAS RENAMED TO EZEI-302
             05  EZEI-302              PIC X(30).
      * W-NUM6 WAS RENAMED TO EZEI-303
             05  EZEI-303              PIC X(7).
             05  EZE-GROUP-100         OCCURS 100 INDEXED EZEIDX35.
      * F0B1_DISP WAS RENAMED TO EZEI-304
              06  EZEI-304             PIC X(72).
              06  EZE-REDEF-101 REDEFINES EZEI-304.
      * ABOOK WAS RENAMED TO EZEI-305
               07  EZEI-305            PIC X(4).
      * ASTARTREC WAS RENAMED TO EZEI-306
               07  EZEI-306            PIC X(6).
      * AENDREC WAS RENAMED TO EZEI-307
               07  EZEI-307            PIC X(6).
      * ASTARTDATE WAS RENAMED TO EZEI-308
               07  EZEI-308            PIC X(10).
      * AENDDATE WAS RENAMED TO EZEI-309
               07  EZEI-309            PIC X(10).
      * ASTORE WAS RENAMED TO EZEI-310
               07  EZEI-310            PIC X(2).
      * AAIN WAS RENAMED TO EZEI-311
               07  EZEI-311            PIC X(3).
      * ABOOK1 WAS RENAMED TO EZEI-312
               07  EZEI-312            PIC X(6).
      * ANAHIA WAS RENAMED TO EZEI-313
               07  EZEI-313            PIC X(25).
      * TOTALREC WAS RENAMED TO EZEI-314
             05  EZEI-314              PIC S9(4).
             05  EZE-GROUP-102         OCCURS 100 INDEXED EZEIDX36.
      * LOCAL_REC WAS RENAMED TO EZEI-315
              06  EZEI-315             PIC X(29).
              06  EZE-REDEF-103 REDEFINES EZEI-315.
      * LREGCOD WAS RENAMED TO EZEI-316
               07  EZEI-316            PIC S9(4).
      * LLOCCOD WAS RENAMED TO EZEI-317
               07  EZEI-317            PIC S9(4).
      * LLOCDESC WAS RENAMED TO EZEI-318
               07  EZEI-318            PIC X(20).
      * LSTATUS WAS RENAMED TO EZEI-319
               07  EZEI-319            PIC X(1).
             05  EZE-GROUP-104         OCCURS 300 INDEXED EZEIDX37.
      * EMMPLOYEE_REC WAS RENAMED TO EZEI-320
              06  EZEI-320             PIC X(157).
              06  EZE-REDEF-105 REDEFINES EZEI-320.
      * CEMP_NUM WAS RENAMED TO EZEI-321
               07  EZEI-321            PIC S9(9).
      * CFIRST_NAME WAS RENAMED TO EZEI-322
               07  EZEI-322            PIC X(12).
      * CSUR_NAME WAS RENAMED TO EZEI-323
               07  EZEI-323            PIC X(30).
      * CSTART_TIME WAS RENAMED TO EZEI-324
               07  EZEI-324            PIC X(8).
      * CEND_TIME WAS RENAMED TO EZEI-325
               07  EZEI-325            PIC X(8).
      * CEMP_WORK_STAT WAS RENAMED TO EZEI-326
               07  EZEI-326            PIC S9(4).
      * CESN WAS RENAMED TO EZEI-327
               07  EZEI-327            PIC S9(9).
      * CTOTAL_HOURS WAS RENAMED TO EZEI-328
               07  EZEI-328            PIC X(8).
      * RTOTAL_HOURS WAS RENAMED TO EZEI-329
               07  EZEI-329            PIC X(9).
      * RTOTAL_HOUR WAS RENAMED TO EZEI-330
               07  EZEI-330            PIC S9(9) COMP.
      * RTOTAL_MIN WAS RENAMED TO EZEI-331
               07  EZEI-331            PIC S9(9) COMP.
      * RTOTAL_SEC WAS RENAMED TO EZEI-332
               07  EZEI-332            PIC S9(9) COMP.
      * REMP-DEP WAS RENAMED TO EZEI-333
               07  EZEI-333            PIC X(20).
      * CSERIAL WAS RENAMED TO EZEI-334
               07  EZEI-334            PIC S9(9).
      * PDATE0 WAS RENAMED TO EZEI-335
               07  EZEI-335            PIC X(10).
      * PSERIAL WAS RENAMED TO EZEI-336
               07  EZEI-336            PIC S9(9).
      * TIMER1 WAS RENAMED TO EZEI-337
             05  EZEI-337              PIC X(8).
             05  EZE-REDEF-106 REDEFINES EZEI-337.
      * TIMERH1 WAS RENAMED TO EZEI-338
              06  EZEI-338             PIC S9(2).
      * TIMERSA1 WAS RENAMED TO EZEI-339
              06  EZEI-339             PIC X(1).
      * TIMERM1 WAS RENAMED TO EZEI-340
              06  EZEI-340             PIC S9(2).
      * TIMERSB1 WAS RENAMED TO EZEI-341
              06  EZEI-341             PIC X(1).
      * TIMERS1 WAS RENAMED TO EZEI-342
              06  EZEI-342             PIC S9(2).
      * TIMER2 WAS RENAMED TO EZEI-343
             05  EZEI-343              PIC X(8).
             05  EZE-REDEF-107 REDEFINES EZEI-343.
      * TIMERH2 WAS RENAMED TO EZEI-344
              06  EZEI-344             PIC S9(2).
      * TIMERSA2 WAS RENAMED TO EZEI-345
              06  EZEI-345             PIC X(1).
      * TIMERM2 WAS RENAMED TO EZEI-346
              06  EZEI-346             PIC S9(2).
      * TIMERSB2 WAS RENAMED TO EZEI-347
              06  EZEI-347             PIC X(1).
      * TIMERS2 WAS RENAMED TO EZEI-348
              06  EZEI-348             PIC S9(2).
      * TIMER3 WAS RENAMED TO EZEI-349
             05  EZEI-349              PIC X(8).
             05  EZE-REDEF-108 REDEFINES EZEI-349.
      * TIMERH3 WAS RENAMED TO EZEI-350
              06  EZEI-350             PIC S9(2).
      * TIMERSA3 WAS RENAMED TO EZEI-351
              06  EZEI-351             PIC X(1).
      * TIMERM3 WAS RENAMED TO EZEI-352
              06  EZEI-352             PIC S9(2).
      * TIMERSB3 WAS RENAMED TO EZEI-353
              06  EZEI-353             PIC X(1).
      * TIMERS3 WAS RENAMED TO EZEI-354
              06  EZEI-354             PIC S9(2).
      * COUNTERA WAS RENAMED TO EZEI-355
           05  EZEI-355              PIC S9(2) OCCURS 20 INDEXED
            EZEIDX38.
      * COUNTERB WAS RENAMED TO EZEI-356
           05  EZEI-356              PIC S9(3) OCCURS 20 INDEXED
            EZEIDX39.
      * COUNTERC WAS RENAMED TO EZEI-357
           05  EZEI-357              PIC S9(2) OCCURS 20 INDEXED
            EZEIDX40.
      * COUNT_REP1 WAS RENAMED TO EZEI-358
           05  EZEI-358              PIC S9(3) OCCURS 20 INDEXED
            EZEIDX41.
      * DATECOUNTER WAS RENAMED TO EZEI-359
             05  EZEI-359              PIC X(9).
             05  EZE-REDEF-109 REDEFINES EZEI-359.
      * HHCOUNTER WAS RENAMED TO EZEI-360
              06  EZEI-360             PIC S9(3).
      * DOT1 WAS RENAMED TO EZEI-361
              06  EZEI-361             PIC X(1).
      * MMCOUNTER WAS RENAMED TO EZEI-362
              06  EZEI-362             PIC S9(2).
      * DOT2 WAS RENAMED TO EZEI-363
              06  EZEI-363             PIC X(1).
      * SSCOUNTER WAS RENAMED TO EZEI-364
              06  EZEI-364             PIC S9(2).
             05  EZE-GROUP-110         OCCURS 5 INDEXED EZEIDX42.
      * HOLIDAY_REC WAS RENAMED TO EZEI-365
              06  EZEI-365             PIC X(26).
              06  EZE-REDEF-111 REDEFINES EZEI-365.
      * MONTH00 WAS RENAMED TO EZEI-366
               07  EZEI-366            PIC S9(4).
      * DATE00 WAS RENAMED TO EZEI-367
               07  EZEI-367            PIC X(10).
      * DAY00 WAS RENAMED TO EZEI-368
               07  EZEI-368            PIC X(12).
             05  EZE-GROUP-112         OCCURS 100 INDEXED EZEIDX43.
      * EMP_HOL_REC WAS RENAMED TO EZEI-369
              06  EZEI-369             PIC X(70).
              06  EZE-REDEF-113 REDEFINES EZEI-369.
      * SHIFT_DATE WAS RENAMED TO EZEI-370
               07  EZEI-370            PIC X(10).
      * SEMP_NUM WAS RENAMED TO EZEI-371
               07  EZEI-371            PIC S9(9).
      * SFIRST_NAME WAS RENAMED TO EZEI-372
               07  EZEI-372            PIC X(12).
      * SSUR_NAME WAS RENAMED TO EZEI-373
               07  EZEI-373            PIC X(30).
      * SESN WAS RENAMED TO EZEI-374
               07  EZEI-374            PIC S9(9).
      * DI1 WAS RENAMED TO EZEI-375
             05  EZEI-375              PIC S9(5).
      * DI2 WAS RENAMED TO EZEI-376
             05  EZEI-376              PIC S9(3).
      * DI3 WAS RENAMED TO EZEI-377
             05  EZEI-377              PIC S9(4).
      * RRTOTAL_HOUR WAS RENAMED TO EZEI-378
           05  EZEI-378              PIC S9(9) OCCURS 20 INDEXED
            EZEIDX44.
      * RRTOTAL_MIN WAS RENAMED TO EZEI-379
           05  EZEI-379              PIC S9(9) OCCURS 20 INDEXED
            EZEIDX45.
      * RRTOTAL_SEC WAS RENAMED TO EZEI-380
           05  EZEI-380              PIC S9(9) OCCURS 20 INDEXED
            EZEIDX46.
      * DAYSVAR WAS RENAMED TO EZEI-381
             05  EZEI-381              PIC S9(9) COMP.
      * GOVCD WAS RENAMED TO EZEI-382
             05  EZEI-382              PIC S9(4) COMP.
      * POLCD WAS RENAMED TO EZEI-383
             05  EZEI-383              PIC S9(4) COMP.
      * AREACD WAS RENAMED TO EZEI-384
             05  EZEI-384              PIC S9(9) COMP.
      * PERM_DESCR WAS RENAMED TO EZEI-385
             05  EZEI-385              PIC X(200).
             05  EZE-REDEF-114 REDEFINES EZEI-385.
      * PERM_DESCR1 WAS RENAMED TO EZEI-386
              06  EZEI-386             PIC X(60).
      * PERM_DESCR2 WAS RENAMED TO EZEI-387
              06  EZEI-387             PIC X(60).
      * PERM_DESCR3 WAS RENAMED TO EZEI-388
              06  EZEI-388             PIC X(60).
      * PERM_DESCR4 WAS RENAMED TO EZEI-389
              06  EZEI-389             PIC X(20).
           02  EZE-INIT-EZER-5 REDEFINES EZER-5.
             05  EZE-INIT-6.
              06  EZE-INIT-7           PIC 9(2).
              06  EZE-INIT-8           PIC A(25).
              06  EZE-INIT-9           PIC A(25).
              06  EZE-INIT-10          PIC A(25).
              06  EZE-INIT-11          PIC A(25).
              06  EZE-INIT-12          PIC A(20).
              06  EZE-INIT-13          PIC A(20).
              06  EZE-INIT-14          PIC A(20).
              06  EZE-INIT-15          PIC A(20).
              06  EZE-INIT-16          PIC A(25).
              06  EZE-INIT-17          PIC A(25).
              06  EZE-INIT-18          PIC A(10).
              06  EZE-INIT-19          PIC A(10).
              06  EZE-INIT-20          PIC S9(9) COMP.
              06  EZE-INIT-21          PIC S9(15) COMP-3.
              06  EZE-INIT-22          PIC A(25).
              06  EZE-INIT-23          PIC A(25).
              06  EZE-INIT-24          PIC S9(4) COMP.
              06  EZE-INIT-25          PIC S9(4) COMP.
              06  EZE-INIT-26          PIC A(10).
              06  EZE-INIT-27          PIC A(10).
              06  EZE-INIT-28          PIC S9(9) COMP.
              06  EZE-INIT-29          PIC A(58).
             05  EZE-INIT-30.
              06  EZE-INIT-31          PIC A(10).
              06  EZE-INIT-32          PIC A(50).
             05  EZE-INIT-33.
              06  EZE-INIT-34          PIC A(1).
              06  EZE-INIT-35          PIC A(2).
              06  EZE-INIT-36          PIC A(6).
              06  EZE-INIT-37          PIC A(2).
              06  EZE-INIT-38          PIC A(2).
              06  EZE-INIT-39          PIC A(2).
              06  EZE-INIT-40          PIC A(5).
             05  EZE-INIT-41.
              06  EZE-INIT-42          PIC A(15).
              06  EZE-INIT-43          PIC A(15).
              06  EZE-INIT-44          PIC A(30).
             05  EZE-INIT-45.
              06  EZE-INIT-46          PIC A(1).
              06  EZE-INIT-47          PIC A(1).
             05  EZE-INIT-48.
              06  EZE-INIT-49          PIC A(1) OCCURS 10.
             05  EZE-INIT-50           PIC S9(4) COMP.
             05  EZE-INIT-51           PIC S9(4) COMP.
             05  EZE-INIT-52           PIC 9(10).
             05  EZE-INIT-53           PIC A(30).
             05  EZE-INIT-54.
              06  EZE-INIT-55          PIC 9(2).
              06  EZE-INIT-56          PIC A(2).
             05  EZE-INIT-57.
              06  EZE-INIT-58          PIC A(1) OCCURS 15.
             05  EZE-INIT-59.
              06  EZE-INIT-60          PIC A(1) OCCURS 20.
             05  EZE-INIT-61.
              06  EZE-INIT-62          PIC A(1) OCCURS 33.
             05  EZE-INIT-63.
              06  EZE-INIT-64          PIC A(1) OCCURS 40.
             05  EZE-INIT-65.
              06  EZE-INIT-66          PIC A(1) OCCURS 50.
             05  EZE-INIT-67           PIC A(1).
             05  EZE-INIT-68.
              06  EZE-INIT-69          PIC A(2).
              06  EZE-INIT-70          PIC A(7).
             05  EZE-INIT-71           PIC A(30).
             05  EZE-INIT-72           PIC A(2).
             05  EZE-INIT-73.
              06  EZE-INIT-74          PIC A(2).
              06  EZE-INIT-75          PIC A(2).
              06  EZE-INIT-76          PIC A(4).
             05  EZE-INIT-77           PIC A(8).
             05  EZE-INIT-78.
              06  EZE-INIT-79          PIC 9(2).
              06  EZE-INIT-80          PIC 9(2).
             05  EZE-INIT-81.
              06  EZE-INIT-82          PIC 9(2).
              06  EZE-INIT-83          PIC A(2).
             05  EZE-INIT-84           PIC 9.
             05  EZE-INIT-85           PIC A(25).
             05  EZE-INIT-86           PIC 9(9).
             05  EZE-INIT-87           PIC 9(9).
             05  EZE-INIT-88           PIC 9.
             05  EZE-INIT-89           PIC S9(4) COMP.
             05  EZE-INIT-90           PIC S9(9) COMP.
             05  EZE-INIT-91           PIC A(60).
             05  EZE-INIT-92           PIC A(45).
             05  EZE-INIT-93           PIC A(45).
             05  EZE-INIT-94           PIC 9(9).
             05  EZE-INIT-95.
              06  EZE-INIT-96          PIC A(2).
              06  EZE-INIT-97          PIC A(5).
              06  EZE-INIT-98          PIC A(3).
             05  EZE-INIT-99.
              06  EZE-INIT-100         PIC A(15).
              06  EZE-INIT-101         PIC A(15).
              06  EZE-INIT-102         PIC A(15).
             05  EZE-INIT-103.
              06  EZE-INIT-104         PIC 9.
              06  EZE-INIT-105         PIC 9(2).
              06  EZE-INIT-106         PIC 9(2).
              06  EZE-INIT-107         PIC 9(2).
              06  EZE-INIT-108         PIC 9(2).
             05  EZE-INIT-109.
              06  EZE-INIT-110         PIC 9(3).
              06  EZE-INIT-111         PIC 9.
             05  EZE-INIT-112.
              06  EZE-INIT-113         PIC 9.
              06  EZE-INIT-114         PIC 9(2).
              06  EZE-INIT-115         PIC 9(2).
              06  EZE-INIT-116         PIC 9(2).
              06  EZE-INIT-117         PIC 9(2).
             05  EZE-INIT-118.
              06  EZE-INIT-119         PIC 9.
              06  EZE-INIT-120         PIC 9(2).
              06  EZE-INIT-121         PIC 9(2).
              06  EZE-INIT-122         PIC 9(2).
              06  EZE-INIT-123         PIC 9(2).
              06  EZE-INIT-124         PIC 9(4).
              06  EZE-INIT-125         PIC A(1).
             05  EZE-INIT-126.
              06  EZE-INIT-127         PIC 9(13).
              06  EZE-INIT-128         PIC 9.
             05  EZE-INIT-129.
              06  EZE-INIT-130         PIC 9(9).
              06  EZE-INIT-131         PIC 9.
             05  EZE-INIT-132          PIC 9(10).
             05  EZE-INIT-133          PIC 9(4).
             05  EZE-INIT-134          PIC 9(10).
             05  EZE-INIT-135          PIC 9(9).
             05  EZE-INIT-136          PIC S9(4) COMP.
             05  EZE-INIT-137          PIC S9(4) COMP.
             05  EZE-INIT-138          PIC S9(4) COMP.
             05  EZE-INIT-139          PIC S9(4) COMP.
             05  EZE-INIT-140          PIC S9(9) COMP.
             05  EZE-INIT-141          PIC 9(9).
             05  EZE-INIT-142          PIC A(9).
             05  EZE-INIT-143          PIC S9(4) COMP.
             05  EZE-INIT-144          PIC 9.
             05  EZE-INIT-145          PIC 9.
             05  EZE-INIT-146          PIC 9.
             05  EZE-INIT-147          PIC 9.
             05  EZE-INIT-148          PIC A(1).
             05  EZE-INIT-149          PIC A(1).
             05  EZE-INIT-150          PIC 9(2).
             05  EZE-INIT-151          PIC 9.
             05  EZE-INIT-152          PIC 9.
             05  EZE-INIT-153          PIC 9.
             05  EZE-INIT-154          PIC 9.
             05  EZE-INIT-155          PIC 9.
             05  EZE-INIT-156          PIC A(15).
             05  EZE-INIT-157          PIC A(15).
             05  EZE-INIT-158          PIC A(15).
             05  EZE-INIT-159          PIC A(1).
             05  EZE-INIT-160          PIC 9(5).
             05  EZE-INIT-161.
              06  EZE-INIT-162         PIC A(2).
              06  EZE-INIT-163         PIC A(1).
              06  EZE-INIT-164         PIC A(2).
              06  EZE-INIT-165         PIC A(1).
              06  EZE-INIT-166         PIC A(4).
             05  EZE-INIT-167          PIC A(20).
             05  EZE-INIT-168          PIC A(20).
             05  EZE-INIT-169          PIC 9(9).
             05  EZE-INIT-170.
              06  EZE-INIT-171         PIC 9(9).
              06  EZE-INIT-172         PIC A(1).
             05  EZE-INIT-173          PIC 9(9).
             05  EZE-INIT-174          PIC 9(14).
             05  EZE-INIT-175          PIC A(14).
             05  EZE-INIT-176.
              06  EZE-INIT-177         PIC A(1).
              06  EZE-INIT-178         PIC A(6).
              06  EZE-INIT-179         PIC A(2).
              06  EZE-INIT-180         PIC A(5).
             05  EZE-INIT-181.
              06  EZE-INIT-182         PIC A(1).
              06  EZE-INIT-183         PIC 9(2).
              06  EZE-INIT-184         PIC A(1).
              06  EZE-INIT-185         PIC A(80).
             05  EZE-INIT-186          PIC A(84).
             05  EZE-INIT-187          PIC 9(4).
             05  EZE-INIT-188.
              06  EZE-INIT-189         PIC 9.
              06  EZE-INIT-190         PIC 9(6).
              06  EZE-INIT-191         PIC 9(2).
             05  EZE-INIT-192          PIC 9(9).
             05  EZE-INIT-193          PIC 9(9).
             05  EZE-INIT-194.
              06  EZE-INIT-195         PIC 9(4).
              06  EZE-INIT-196         PIC A(1).
             05  EZE-INIT-197.
              06  EZE-INIT-198         PIC 9(4).
              06  EZE-INIT-199         PIC A(6).
             05  EZE-INIT-200.
              06  EZE-INIT-201         PIC A(2).
              06  EZE-INIT-202         PIC A(2).
              06  EZE-INIT-203         PIC A(2).
             05  EZE-INIT-204.
              06  EZE-INIT-205         PIC A(4).
              06  EZE-INIT-206         PIC A(1).
              06  EZE-INIT-207         PIC A(2).
              06  EZE-INIT-208         PIC A(1).
              06  EZE-INIT-209         PIC A(2).
             05  EZE-INIT-210.
              06  EZE-INIT-211         PIC A(4).
              06  EZE-INIT-212         PIC A(1).
              06  EZE-INIT-213         PIC A(2).
              06  EZE-INIT-214         PIC A(1).
              06  EZE-INIT-215         PIC A(2).
             05  EZE-INIT-216.
              06  EZE-INIT-217         PIC 9(2).
              06  EZE-INIT-218         PIC A(1).
              06  EZE-INIT-219         PIC 9(2).
              06  EZE-INIT-220         PIC A(1).
              06  EZE-INIT-221         PIC 9(2).
             05  EZE-INIT-222.
              06  EZE-INIT-223         PIC 9(2).
              06  EZE-INIT-224         PIC A(1).
              06  EZE-INIT-225         PIC 9(2).
              06  EZE-INIT-226         PIC A(1).
              06  EZE-INIT-227         PIC 9(2).
             05  EZE-INIT-228.
              06  EZE-INIT-229         PIC 9(4).
              06  EZE-INIT-230         PIC A(1).
              06  EZE-INIT-231         PIC 9(2).
              06  EZE-INIT-232         PIC A(1).
              06  EZE-INIT-233         PIC 9(2).
             05  EZE-INIT-234.
              06  EZE-INIT-235         PIC 9(4).
              06  EZE-INIT-236         PIC A(1).
              06  EZE-INIT-237         PIC 9(2).
              06  EZE-INIT-238         PIC A(1).
              06  EZE-INIT-239         PIC 9(2).
             05  EZE-INIT-240.
              06  EZE-INIT-241         PIC A(2).
              06  EZE-INIT-242         PIC A(1).
              06  EZE-INIT-243         PIC A(2).
              06  EZE-INIT-244         PIC A(1).
              06  EZE-INIT-245         PIC A(4).
             05  EZE-INIT-246.
              06  EZE-INIT-247         PIC A(2).
              06  EZE-INIT-248         PIC A(1).
              06  EZE-INIT-249         PIC A(2).
              06  EZE-INIT-250         PIC A(1).
              06  EZE-INIT-251         PIC A(4).
             05  EZE-INIT-252.
              06  EZE-INIT-253         PIC A(1).
              06  EZE-INIT-254         PIC A(6).
              06  EZE-INIT-255         PIC A(2).
              06  EZE-INIT-256         PIC A(5).
             05  EZE-INIT-257.
              06  EZE-INIT-258         PIC 9(9).
              06  EZE-INIT-259         PIC 9.
             05  EZE-INIT-260          PIC A(8).
             05  EZE-INIT-261          PIC 9(10).
             05  EZE-INIT-262          PIC A(20).
             05  EZE-INIT-263          PIC A(20).
             05  EZE-INIT-264          PIC A(20).
             05  EZE-INIT-265          PIC A(20).
             05  EZE-INIT-266          PIC 9(10).
             05  EZE-INIT-267          PIC 9(4).
             05  EZE-INIT-268          PIC 9(5).
             05  EZE-INIT-269          PIC 9(4).
             05  EZE-INIT-270          PIC A(10).
             05  EZE-INIT-271          PIC 9(6).
             05  EZE-INIT-272          PIC S9(9) COMP.
             05  EZE-INIT-273.
              06  EZE-INIT-274         PIC A(15).
              06  EZE-INIT-275         PIC A(15).
              06  EZE-INIT-276         PIC A(15).
              06  EZE-INIT-277         PIC A(15).
             05  EZE-INIT-278          PIC A(60).
             05  EZE-INIT-279          PIC A(25).
             05  EZE-INIT-280          PIC A(20).
             05  EZE-INIT-281          PIC A(20).
             05  EZE-INIT-282          PIC A(33).
             05  EZE-INIT-283          PIC A(25).
             05  EZE-INIT-284          PIC A(25).
             05  EZE-INIT-285          PIC A(20).
             05  EZE-INIT-286          PIC A(20).
             05  EZE-INIT-287          PIC A(33).
             05  EZE-INIT-288.
              06  EZE-INIT-289         PIC A(1) OCCURS 25.
             05  EZE-INIT-290.
              06  EZE-INIT-291         PIC A(1) OCCURS 20.
             05  EZE-INIT-292.
              06  EZE-INIT-293         PIC A(1) OCCURS 33.
             05  EZE-INIT-294.
              06  EZE-INIT-295         PIC A(1).
              06  EZE-INIT-296         PIC A(20).
              06  EZE-INIT-297         PIC A(1).
             05  EZE-INIT-298          PIC A(1) OCCURS 19.
             05  EZE-INIT-299          PIC S9(4) COMP.
             05  EZE-INIT-300          PIC S9(4) COMP.
             05  EZE-INIT-301          PIC S9(4) COMP.
             05  EZE-INIT-302          PIC S9(4) COMP.
             05  EZE-INIT-303          PIC S9(4) COMP.
             05  EZE-INIT-304          PIC S9(4) COMP.
             05  EZE-INIT-305          PIC S9(4) COMP.
             05  EZE-INIT-306          PIC S9(4) COMP.
             05  EZE-INIT-307          PIC 9.
             05  EZE-INIT-308          PIC A(60).
             05  EZE-INIT-309          PIC 9(2).
             05  EZE-INIT-310          PIC A(100).
             05  EZE-INIT-311          PIC 9(4).
             05  EZE-INIT-312          PIC S9(9) COMP.
             05  EZE-INIT-313          PIC S9(9) COMP.
             05  EZE-INIT-314          PIC S9(9) COMP.
             05  EZE-INIT-315          PIC 9(2).
             05  EZE-INIT-316          PIC 9.
             05  EZE-INIT-317          PIC A(1).
             05  EZE-INIT-318          PIC 9(2).
             05  EZE-INIT-319          PIC 9(2).
             05  EZE-INIT-320          PIC 9(2).
             05  EZE-INIT-321          PIC A(2).
             05  EZE-INIT-322          PIC A(3).
             05  EZE-INIT-323          PIC 9(3).
             05  EZE-INIT-324          PIC 9(4).
             05  EZE-INIT-325          PIC 9(4).
             05  EZE-INIT-326          PIC S9(4) COMP.
             05  EZE-INIT-327          PIC 9(4).
             05  EZE-INIT-328          PIC A(4).
             05  EZE-INIT-329          PIC 9(5).
             05  EZE-INIT-330.
              06  EZE-INIT-331         PIC 9(2).
              06  EZE-INIT-332         PIC 9(3).
             05  EZE-INIT-333          PIC 9(6).
             05  EZE-INIT-334          PIC A(6).
             05  EZE-INIT-335          PIC 9(6).
             05  EZE-INIT-336          PIC A(7).
             05  EZE-INIT-337          PIC 9(7).
             05  EZE-INIT-338          PIC 9(8).
             05  EZE-INIT-339          PIC 9(9).
             05  EZE-INIT-340          PIC 9(9).
             05  EZE-INIT-341          PIC S9(9) COMP.
             05  EZE-INIT-342          PIC A(9).
             05  EZE-INIT-343          PIC 9(10).
             05  EZE-INIT-344.
              06  EZE-INIT-345         PIC A(4).
              06  EZE-INIT-346         PIC A(1).
              06  EZE-INIT-347         PIC A(2).
              06  EZE-INIT-348         PIC A(1).
              06  EZE-INIT-349         PIC A(2).
             05  EZE-INIT-350.
              06  EZE-INIT-351         PIC A(4).
              06  EZE-INIT-352         PIC A(6).
             05  EZE-INIT-353          PIC A(1).
             05  EZE-INIT-354          PIC 9(2).
             05  EZE-INIT-355          PIC A(2).
             05  EZE-INIT-356          OCCURS 350.
               07  EZE-INIT-357        PIC A(15).
               07  EZE-INIT-358        PIC A(15).
               07  EZE-INIT-359        PIC A(15).
               07  EZE-INIT-360        PIC A(15).
               07  EZE-INIT-361        PIC A(10).
               07  EZE-INIT-362        PIC 9(10).
               07  EZE-INIT-363        PIC A(15).
             05  EZE-INIT-364          OCCURS 350.
               07  EZE-INIT-365        PIC A(10).
               07  EZE-INIT-366        PIC 9(4).
               07  EZE-INIT-367        PIC A(1).
               07  EZE-INIT-368        PIC 9(10).
               07  EZE-INIT-369        PIC 9(9).
               07  EZE-INIT-370        PIC 9(10).
               07  EZE-INIT-371        PIC A(9).
               07  EZE-INIT-372        PIC 9(9).
             05  EZE-INIT-373          OCCURS 350.
               07  EZE-INIT-374        PIC A(15).
               07  EZE-INIT-375        PIC A(15).
               07  EZE-INIT-376        PIC A(15).
               07  EZE-INIT-377        PIC A(15).
               07  EZE-INIT-378        PIC A(10).
               07  EZE-INIT-379        PIC 9(10).
               07  EZE-INIT-380        PIC A(15).
               07  EZE-INIT-381        PIC A(1).
               07  EZE-INIT-382        PIC 9(14).
             05  EZE-INIT-383          OCCURS 100.
               07  EZE-INIT-384        PIC 9(4).
               07  EZE-INIT-385        PIC A(20).
               07  EZE-INIT-386        PIC 9(9).
               07  EZE-INIT-387        PIC A(10).
               07  EZE-INIT-388        PIC A(4).
               07  EZE-INIT-389        PIC 9(4).
             05  EZE-INIT-390          OCCURS 100.
               07  EZE-INIT-391        PIC 9(10).
               07  EZE-INIT-392        PIC A(29).
               07  EZE-INIT-393        PIC 9(14).
               07  EZE-INIT-394        PIC 9(6).
               07  EZE-INIT-395        PIC A(10).
               07  EZE-INIT-396        PIC A(4).
               07  EZE-INIT-397        PIC 9(10).
               07  EZE-INIT-398        PIC A(1).
               07  EZE-INIT-399        PIC 9(9).
             05  EZE-INIT-400          OCCURS 100.
               07  EZE-INIT-401        PIC 9(10).
               07  EZE-INIT-402        PIC A(1).
               07  EZE-INIT-403        PIC 9(9).
               07  EZE-INIT-404        PIC A(4).
               07  EZE-INIT-405        PIC A(25).
               07  EZE-INIT-406        PIC 9(10).
             05  EZE-INIT-407          OCCURS 100.
               07  EZE-INIT-408        PIC A(15).
               07  EZE-INIT-409        PIC A(30).
               07  EZE-INIT-410        PIC A(10).
               07  EZE-INIT-411        PIC 9(10).
               07  EZE-INIT-412        PIC A(4).
             05  EZE-INIT-413          OCCURS 100.
               07  EZE-INIT-414        PIC A(15).
               07  EZE-INIT-415        PIC 9(14).
               07  EZE-INIT-416        PIC A(10).
               07  EZE-INIT-417        PIC A(10).
               07  EZE-INIT-418        PIC 9(6).
               07  EZE-INIT-419        PIC A(10).
               07  EZE-INIT-420        PIC A(1).
               07  EZE-INIT-421        PIC A(1).
               07  EZE-INIT-422        PIC A(15).
               07  EZE-INIT-423        PIC A(60).
               07  EZE-INIT-424        PIC 9(9).
               07  EZE-INIT-425        PIC 9(9).
             05  EZE-INIT-426          OCCURS 100.
               07  EZE-INIT-427        PIC A(15).
               07  EZE-INIT-428        PIC 9(14).
               07  EZE-INIT-429        PIC A(10).
               07  EZE-INIT-430        PIC 9(9).
               07  EZE-INIT-431        PIC A(10).
               07  EZE-INIT-432        PIC A(4).
               07  EZE-INIT-433        PIC A(15).
               07  EZE-INIT-434        PIC A(60).
               07  EZE-INIT-435        PIC 9(9).
             05  EZE-INIT-436          OCCURS 20.
               07  EZE-INIT-437        PIC A(20).
               07  EZE-INIT-438        PIC A(10).
               07  EZE-INIT-439        PIC A(10).
               07  EZE-INIT-440        PIC A(1).
               07  EZE-INIT-441        PIC 9(9).
             05  EZE-INIT-442          PIC 9.
             05  EZE-INIT-443          OCCURS 180.
               07  EZE-INIT-444        PIC A(15).
               07  EZE-INIT-445        PIC A(40).
               07  EZE-INIT-446        PIC A(10).
               07  EZE-INIT-447        PIC 9(10).
               07  EZE-INIT-448        PIC 9(9).
             05  EZE-INIT-449          OCCURS 180.
               07  EZE-INIT-450        PIC A(40).
               07  EZE-INIT-451        PIC A(40).
               07  EZE-INIT-452        PIC A(14).
               07  EZE-INIT-453        PIC 9(10).
               07  EZE-INIT-454        PIC 9(4).
               07  EZE-INIT-455        PIC 9(9).
             05  EZE-INIT-456          OCCURS 180.
               07  EZE-INIT-457        PIC A(15).
               07  EZE-INIT-458        PIC A(40).
               07  EZE-INIT-459        PIC A(10).
               07  EZE-INIT-460        PIC 9(10).
               07  EZE-INIT-461        PIC 9(9).
               07  EZE-INIT-462        PIC A(1).
               07  EZE-INIT-463        PIC 9(4).
               07  EZE-INIT-464        PIC 9(9).
             05  EZE-INIT-465          OCCURS 100.
               07  EZE-INIT-466        PIC 9(10).
               07  EZE-INIT-467        PIC 9(9).
               07  EZE-INIT-468        PIC A(10).
               07  EZE-INIT-469        PIC 9(4).
               07  EZE-INIT-470        PIC A(10).
               07  EZE-INIT-471        PIC A(1).
             05  EZE-INIT-472          PIC 9(2).
             05  EZE-INIT-473          PIC 9.
             05  EZE-INIT-474          OCCURS 600.
               07  EZE-INIT-475        PIC A(6).
               07  EZE-INIT-476        PIC A(20).
               07  EZE-INIT-477        PIC A(10).
               07  EZE-INIT-478        PIC A(3).
             05  EZE-INIT-479          PIC A(3).
             05  EZE-INIT-480          PIC 9(4).
             05  EZE-INIT-481          PIC A(2).
             05  EZE-INIT-482.
              06  EZE-INIT-483         PIC 9.
              06  EZE-INIT-484         PIC 9(2).
              06  EZE-INIT-485         PIC 9(2).
              06  EZE-INIT-486         PIC 9(2).
              06  EZE-INIT-487         PIC 9(2).
              06  EZE-INIT-488         PIC 9(3).
              06  EZE-INIT-489         PIC 9.
              06  EZE-INIT-490         PIC 9.
             05  EZE-INIT-491.
              06  EZE-INIT-492         PIC A(2).
              06  EZE-INIT-493         PIC A(2).
              06  EZE-INIT-494         PIC A(1).
              06  EZE-INIT-495         PIC A(2).
              06  EZE-INIT-496         PIC A(1).
              06  EZE-INIT-497         PIC A(2).
             05  EZE-INIT-498          PIC A(9).
             05  EZE-INIT-499          PIC 9(10).
             05  EZE-INIT-500          PIC 9(9).
             05  EZE-INIT-501          PIC 9(5).
             05  EZE-INIT-502          PIC 9(4).
             05  EZE-INIT-503          PIC 9.
             05  EZE-INIT-504.
              06  EZE-INIT-505         PIC 9(2).
              06  EZE-INIT-506         PIC A(1).
              06  EZE-INIT-507         PIC 9(2).
              06  EZE-INIT-508         PIC A(1).
              06  EZE-INIT-509         PIC 9(2).
             05  EZE-INIT-510          PIC 9 OCCURS 20.
             05  EZE-INIT-511.
              06  EZE-INIT-512         PIC 9(2).
              06  EZE-INIT-513         PIC 9(2).
             05  EZE-INIT-514          PIC A(68).
             05  EZE-INIT-515.
              06  EZE-INIT-516         PIC A(16).
              06  EZE-INIT-517         PIC A(24).
             05  EZE-INIT-518.
              06  EZE-INIT-519         PIC A(19).
              06  EZE-INIT-520         PIC A(5).
              06  EZE-INIT-521         PIC A(16).
             05  EZE-INIT-522          PIC A(15).
             05  EZE-INIT-523.
              06  EZE-INIT-524         PIC A(15).
              06  EZE-INIT-525         PIC A(45).
             05  EZE-INIT-526          PIC A(45).
             05  EZE-INIT-527          PIC 9(9).
             05  EZE-INIT-528          PIC A(25).
             05  EZE-INIT-529          PIC 9(9).
             05  EZE-INIT-530          PIC S9(4) COMP.
             05  EZE-INIT-531          PIC 9(10).
             05  EZE-INIT-532          PIC A(1).
             05  EZE-INIT-533          PIC S9(9) COMP.
             05  EZE-INIT-534          OCCURS 400.
               07  EZE-INIT-535        PIC A(1).
               07  EZE-INIT-536        PIC A(44).
               07  EZE-INIT-537        PIC 9(6).
             05  EZE-INIT-538          PIC S9(4) COMP.
             05  EZE-INIT-539          PIC S9(4) COMP.
             05  EZE-INIT-540          PIC S9(4) COMP.
             05  EZE-INIT-541          PIC S9(4) COMP.
             05  EZE-INIT-542.
              06  EZE-INIT-543         PIC A(2).
              06  EZE-INIT-544         PIC A(2).
             05  EZE-INIT-545.
              06  EZE-INIT-546         PIC A(4).
              06  EZE-INIT-547         PIC A(1).
              06  EZE-INIT-548         PIC A(2).
              06  EZE-INIT-549         PIC A(1).
              06  EZE-INIT-550         PIC A(2).
             05  EZE-INIT-551          PIC A(60).
             05  EZE-INIT-552          PIC S9(4) COMP.
             05  EZE-INIT-553          PIC A(10).
             05  EZE-INIT-554          PIC S9(4) COMP.
             05  EZE-INIT-555          PIC S9(4) COMP.
             05  EZE-INIT-556.
              06  EZE-INIT-557         PIC A(1).
              06  EZE-INIT-558         PIC A(6).
              06  EZE-INIT-559         PIC A(2).
              06  EZE-INIT-560         PIC A(5).
              06  EZE-INIT-561         PIC A(6).
             05  EZE-INIT-562.
              06  EZE-INIT-563         PIC A(1) OCCURS 30.
             05  EZE-INIT-564          PIC 9(2).
             05  EZE-INIT-565          PIC 9(2).
             05  EZE-INIT-566          PIC 9(2).
             05  EZE-INIT-567.
              06  EZE-INIT-568         PIC A(1) OCCURS 15.
             05  EZE-INIT-569          PIC A(15).
             05  EZE-INIT-570          PIC A(15).
             05  EZE-INIT-571.
              06  EZE-INIT-572         PIC A(1) OCCURS 33.
             05  EZE-INIT-573.
              06  EZE-INIT-574         PIC A(1) OCCURS 33.
             05  EZE-INIT-575.
              06  EZE-INIT-576         PIC A(1) OCCURS 20.
             05  EZE-INIT-577.
              06  EZE-INIT-578         PIC A(1) OCCURS 20.
             05  EZE-INIT-579.
              06  EZE-INIT-580         PIC A(15) OCCURS 300.
             05  EZE-INIT-581          PIC S9(4) COMP.
             05  EZE-INIT-582          PIC A(1).
             05  EZE-INIT-583          PIC S9(4) COMP.
             05  EZE-INIT-584          PIC S9(9) COMP.
             05  EZE-INIT-585          PIC A(30).
             05  EZE-INIT-586          PIC A(7).
             05  EZE-INIT-587          OCCURS 100.
               07  EZE-INIT-588        PIC A(4).
               07  EZE-INIT-589        PIC A(6).
               07  EZE-INIT-590        PIC A(6).
               07  EZE-INIT-591        PIC A(10).
               07  EZE-INIT-592        PIC A(10).
               07  EZE-INIT-593        PIC A(2).
               07  EZE-INIT-594        PIC A(3).
               07  EZE-INIT-595        PIC A(6).
               07  EZE-INIT-596        PIC A(25).
             05  EZE-INIT-597          PIC 9(4).
             05  EZE-INIT-598          OCCURS 100.
               07  EZE-INIT-599        PIC 9(4).
               07  EZE-INIT-600        PIC 9(4).
               07  EZE-INIT-601        PIC A(20).
               07  EZE-INIT-602        PIC A(1).
             05  EZE-INIT-603          OCCURS 300.
               07  EZE-INIT-604        PIC 9(9).
               07  EZE-INIT-605        PIC A(12).
               07  EZE-INIT-606        PIC A(30).
               07  EZE-INIT-607        PIC A(8).
               07  EZE-INIT-608        PIC A(8).
               07  EZE-INIT-609        PIC 9(4).
               07  EZE-INIT-610        PIC 9(9).
               07  EZE-INIT-611        PIC A(8).
               07  EZE-INIT-612        PIC A(9).
               07  EZE-INIT-613        PIC S9(9) COMP.
               07  EZE-INIT-614        PIC S9(9) COMP.
               07  EZE-INIT-615        PIC S9(9) COMP.
               07  EZE-INIT-616        PIC A(20).
               07  EZE-INIT-617        PIC 9(9).
               07  EZE-INIT-618        PIC A(10).
               07  EZE-INIT-619        PIC 9(9).
             05  EZE-INIT-620.
              06  EZE-INIT-621         PIC 9(2).
              06  EZE-INIT-622         PIC A(1).
              06  EZE-INIT-623         PIC 9(2).
              06  EZE-INIT-624         PIC A(1).
              06  EZE-INIT-625         PIC 9(2).
             05  EZE-INIT-626.
              06  EZE-INIT-627         PIC 9(2).
              06  EZE-INIT-628         PIC A(1).
              06  EZE-INIT-629         PIC 9(2).
              06  EZE-INIT-630         PIC A(1).
              06  EZE-INIT-631         PIC 9(2).
             05  EZE-INIT-632.
              06  EZE-INIT-633         PIC 9(2).
              06  EZE-INIT-634         PIC A(1).
              06  EZE-INIT-635         PIC 9(2).
              06  EZE-INIT-636         PIC A(1).
              06  EZE-INIT-637         PIC 9(2).
             05  EZE-INIT-638          PIC 9(2) OCCURS 20.
             05  EZE-INIT-639          PIC 9(3) OCCURS 20.
             05  EZE-INIT-640          PIC 9(2) OCCURS 20.
             05  EZE-INIT-641          PIC 9(3) OCCURS 20.
             05  EZE-INIT-642.
              06  EZE-INIT-643         PIC 9(3).
              06  EZE-INIT-644         PIC A(1).
              06  EZE-INIT-645         PIC 9(2).
              06  EZE-INIT-646         PIC A(1).
              06  EZE-INIT-647         PIC 9(2).
             05  EZE-INIT-648          OCCURS 5.
               07  EZE-INIT-649        PIC 9(4).
               07  EZE-INIT-650        PIC A(10).
               07  EZE-INIT-651        PIC A(12).
             05  EZE-INIT-652          OCCURS 100.
               07  EZE-INIT-653        PIC A(10).
               07  EZE-INIT-654        PIC 9(9).
               07  EZE-INIT-655        PIC A(12).
               07  EZE-INIT-656        PIC A(30).
               07  EZE-INIT-657        PIC 9(9).
             05  EZE-INIT-658          PIC 9(5).
             05  EZE-INIT-659          PIC 9(3).
             05  EZE-INIT-660          PIC 9(4).
             05  EZE-INIT-661          PIC 9(9) OCCURS 20.
             05  EZE-INIT-662          PIC 9(9) OCCURS 20.
             05  EZE-INIT-663          PIC 9(9) OCCURS 20.
             05  EZE-INIT-664          PIC S9(9) COMP.
             05  EZE-INIT-665          PIC S9(4) COMP.
             05  EZE-INIT-666          PIC S9(4) COMP.
             05  EZE-INIT-667          PIC S9(9) COMP.
             05  EZE-INIT-668.
              06  EZE-INIT-669         PIC A(60).
              06  EZE-INIT-670         PIC A(60).
              06  EZE-INIT-671         PIC A(60).
              06  EZE-INIT-672         PIC A(20).
           02  FILLER                  PIC X(4)
                                       VALUE "*END".
      *-----------------------------------------------------------------
      * WORKING STORAGE RECORD RCGW99
      *-----------------------------------------------------------------
       01  EZEWS-EZER-1-GP.
           02  EZEWS-ID                PIC X(8)
                                       VALUE "ELAASGWS".
           02  EZEWS-EZER-1-LL         PIC S9(8) COMP
                                       VALUE +28.
           02  FILLER                  PIC X(2) VALUE SPACES.
           02  FILLER                  PIC X(18)
                                       VALUE "RCGW99".
      *-----------------------------------------------------------------
      * RECORD NAME         : RCGW99
      * FILE ORGANIZATION   : WORKSTOR
      * MODIFICATION DATE   : 07/03/2007
      * MODIFICATION TIME   : 08:20:34
      * RECORD PROLOGUE     :
      *
      *-----------------------------------------------------------------
      * RCGW99 WAS RENAMED TO EZER-1
           02  EZER-1.
           03  FILLER                  PIC X(4)
                                       VALUE "*END".
      *-----------------------------------------------------------------
      * WORKING STORAGE RECORD SAM_REC
      *-----------------------------------------------------------------
       01  EZEWS-EZER-2-GP.
           02  EZEWS-ID                PIC X(8)
                                       VALUE "ELAASGWS".
           02  EZEWS-EZER-2-LL         PIC S9(8) COMP
                                       VALUE +379.
           02  FILLER                  PIC X(2) VALUE SPACES.
           02  FILLER                  PIC X(18)
                                       VALUE "SAM_REC".
      *-----------------------------------------------------------------
      * RECORD NAME         : SAM_REC
      * FILE ORGANIZATION   : WORKSTOR
      * MODIFICATION DATE   : 07/03/2007
      * MODIFICATION TIME   : 08:20:51
      * RECORD PROLOGUE     :
      *
      *-----------------------------------------------------------------
      * SAM_REC WAS RENAMED TO EZER-2
           02  EZER-2.
      * SAM-FILLER WAS RENAMED TO EZEI-3
             05  EZEI-3                PIC X(349).
             05  EZE-REDEF-115 REDEFINES EZEI-3.
      * SAM-FILLER1 WAS RENAMED TO EZEI-4
              06  EZEI-4               PIC X(10).
      * SAM-USER-ID WAS RENAMED TO EZEI-5
              06  EZEI-5               PIC S9(4).
      * SAM-FILLER2 WAS RENAMED TO EZEI-6
              06  EZEI-6               PIC X(335).
              06  EZE-REDEF-116 REDEFINES EZEI-6.
      * IDNUM14A WAS RENAMED TO EZEI-7
               07  EZEI-7              PIC S9(14).
      * SAM-FILLER3 WAS RENAMED TO EZEI-8
               07  EZEI-8              PIC X(321).
      * USER-ID WAS RENAMED TO EZEI-9
             05  EZEI-9                PIC S9(4) COMP.
           02  EZE-INIT-EZER-2 REDEFINES EZER-2.
             05  EZE-INIT-673.
              06  EZE-INIT-674         PIC A(10).
              06  EZE-INIT-675         PIC 9(4).
              06  EZE-INIT-676.
               07  EZE-INIT-677        PIC 9(14).
               07  EZE-INIT-678        PIC A(321).
             05  EZE-INIT-679          PIC S9(4) COMP.
           02  FILLER                  PIC X(4)
                                       VALUE "*END".
      *-----------------------------------------------------------------
      * WORKING STORAGE RECORD W-COM-AREA
      *-----------------------------------------------------------------
       01  EZEWS-EZER-3-GP.
           02  EZEWS-ID                PIC X(8)
                                       VALUE "ELAASGWS".
           02  EZEWS-EZER-3-LL         PIC S9(8) COMP
                                       VALUE +808.
           02  FILLER                  PIC X(2) VALUE SPACES.
           02  FILLER                  PIC X(18)
                                       VALUE "W-COM-AREA".
      *-----------------------------------------------------------------
      * RECORD NAME         : W-COM-AREA
      * FILE ORGANIZATION   : WORKSTOR
      * MODIFICATION DATE   : 06/03/2007
      * MODIFICATION TIME   : 10:42:58
      * RECORD PROLOGUE     :
      *
      *-----------------------------------------------------------------
      * W-COM-AREA WAS RENAMED TO EZER-3
           02  EZER-3.
      * CSONUM WAS RENAMED TO EZEI-96
             05  EZEI-96               PIC S9(9) COMP.
      * NAME WAS RENAMED TO EZEI-95
             05  EZEI-95               PIC X(15).
      * FTHFNAME WAS RENAMED TO EZEI-94
             05  EZEI-94               PIC X(15).
      * FTHSNAME WAS RENAMED TO EZEI-93
             05  EZEI-93               PIC X(15).
      * FTHTNAME WAS RENAMED TO EZEI-92
             05  EZEI-92               PIC X(15).
      * BKNUM WAS RENAMED TO EZEI-91
             05  EZEI-91               PIC S9(4) COMP.
      * REGNUM WAS RENAMED TO EZEI-90
             05  EZEI-90               PIC S9(9) COMP.
      * REGDT WAS RENAMED TO EZEI-89
             05  EZEI-89               PIC X(10).
      * REGTYP WAS RENAMED TO EZEI-88
             05  EZEI-88               PIC X(1).
      * IDNUMSTAT WAS RENAMED TO EZEI-87
             05  EZEI-87               PIC X(1).
      * IDNUM WAS RENAMED TO EZEI-86
             05  EZEI-86               PIC S9(15) COMP-3.
      * BRTHDT WAS RENAMED TO EZEI-85
             05  EZEI-85               PIC X(10).
      * BRTHTM WAS RENAMED TO EZEI-84
             05  EZEI-84               PIC X(8).
      * SEX WAS RENAMED TO EZEI-83
             05  EZEI-83               PIC X(1).
      * RELG WAS RENAMED TO EZEI-82
             05  EZEI-82               PIC X(1).
      * RECSTATT WAS RENAMED TO EZEI-81
             05  EZEI-81               PIC X(1).
      * DATSRC WAS RENAMED TO EZEI-80
             05  EZEI-80               PIC X(1).
      * NATTYP WAS RENAMED TO EZEI-79
             05  EZEI-79               PIC X(1).
      * CURMARSTAT WAS RENAMED TO EZEI-78
             05  EZEI-78               PIC X(1).
      * CNAME1 WAS RENAMED TO EZEI-77
             05  EZEI-77               PIC X(25).
      * CNAME2 WAS RENAMED TO EZEI-76
             05  EZEI-76               PIC X(25).
      * FTHCNAME WAS RENAMED TO EZEI-75
             05  EZEI-75               PIC X(20).
      * FK_CIVIL_OFFICECD WAS RENAMED TO EZEI-74
             05  EZEI-74               PIC S9(4) COMP.
      * FK_AREACD WAS RENAMED TO EZEI-73
             05  EZEI-73               PIC S9(9) COMP.
      * FK_POLICE_STATICD WAS RENAMED TO EZEI-72
             05  EZEI-72               PIC S9(4) COMP.
      * FK_GOVERNORATECD WAS RENAMED TO EZEI-71
             05  EZEI-71               PIC S9(4) COMP.
      * FK_HEALTH_OFFICCD WAS RENAMED TO EZEI-70
             05  EZEI-70               PIC S9(9) COMP.
      * FK_PERSONCSONUM WAS RENAMED TO EZEI-69
             05  EZEI-69               PIC S9(9) COMP.
      * FK0PERSONCSONUM WAS RENAMED TO EZEI-68
             05  EZEI-68               PIC S9(9) COMP.
      * SWITCH1 WAS RENAMED TO EZEI-67
             05  EZEI-67               PIC S9(9) COMP.
      * SWITCH2 WAS RENAMED TO EZEI-66
             05  EZEI-66               PIC S9(9) COMP.
      * SWITCH3 WAS RENAMED TO EZEI-65
             05  EZEI-65               PIC S9(9) COMP.
      * SWITCH4 WAS RENAMED TO EZEI-64
             05  EZEI-64               PIC S9(9) COMP.
      * SWITCH5 WAS RENAMED TO EZEI-63
             05  EZEI-63               PIC S9(9) COMP.
      * SWITCH6 WAS RENAMED TO EZEI-62
             05  EZEI-62               PIC X(363).
             05  EZE-REDEF-117 REDEFINES EZEI-62.
      * WDATE WAS RENAMED TO EZEI-61
              06  EZEI-61              PIC X(10).
              06  EZE-REDEF-118 REDEFINES EZEI-61.
      * DD2 WAS RENAMED TO EZEI-60
               07  EZEI-60             PIC X(2).
      * SLASH1 WAS RENAMED TO EZEI-59
               07  EZEI-59             PIC X(1).
      * MM2 WAS RENAMED TO EZEI-58
               07  EZEI-58             PIC X(2).
      * SLASH2 WAS RENAMED TO EZEI-57
               07  EZEI-57             PIC X(1).
      * YY2 WAS RENAMED TO EZEI-56
               07  EZEI-56             PIC X(4).
      * USER-ID WAS RENAMED TO EZEI-55
              06  EZEI-55              PIC S9(4) COMP.
      * TITRGG0 WAS RENAMED TO EZEI-54
              06  EZEI-54              PIC X(88).
              06  EZE-REDEF-119 REDEFINES EZEI-54.
      * TITRGG WAS RENAMED TO EZEI-53
               07  EZEI-53             PIC X(30).
      * VAR10 WAS RENAMED TO EZEI-52
               07  EZEI-52             PIC X(10).
      * VARIABLE48 WAS RENAMED TO EZEI-51
               07  EZEI-51             PIC X(48).
      * TITRGG4 WAS RENAMED TO EZEI-50
              06  EZEI-50              PIC X(8).
              06  EZE-REDEF-120 REDEFINES EZEI-50.
      * TITRGG41 WAS RENAMED TO EZEI-49
               07  EZEI-49             PIC X(4).
      * TITRGG44 WAS RENAMED TO EZEI-48
               07  EZEI-48             PIC X(4).
      * TRAN_CODE WAS RENAMED TO EZEI-47
              06  EZEI-47              PIC S9(6).
      * TESTY WAS RENAMED TO EZEI-46
              06  EZEI-46              PIC S9.
      * WYEAR WAS RENAMED TO EZEI-45
              06  EZEI-45              PIC S9(4).
      * WMONTH WAS RENAMED TO EZEI-44
              06  EZEI-44              PIC S9(2).
      * WDAY WAS RENAMED TO EZEI-43
              06  EZEI-43              PIC S9(2).
      * F_CSONUM WAS RENAMED TO EZEI-42
              06  EZEI-42              PIC S9(9) COMP.
      * M_CSONUM WAS RENAMED TO EZEI-41
              06  EZEI-41              PIC S9(9) COMP.
      * APPL_FORM_NUM WAS RENAMED TO EZEI-395
              06  EZEI-395             PIC S9(10).
      * APPL_TYPE WAS RENAMED TO EZEI-394
              06  EZEI-394             PIC X(1).
      * FAM_LAST_SERIAL WAS RENAMED TO EZEI-393
              06  EZEI-393             PIC S9(9) COMP.
      * rr-flag
      * TEST_COUNT WAS RENAMED TO EZEI-40
              06  EZEI-40              PIC S9.
      * FFULLNAME WAS RENAMED TO EZEI-738
              06  EZEI-738             PIC X(45).
      * MFULLNAME WAS RENAMED TO EZEI-39
              06  EZEI-39              PIC X(45).
      * FFNAME WAS RENAMED TO EZEI-38
              06  EZEI-38              PIC X(15).
      * MFNAME WAS RENAMED TO EZEI-37
              06  EZEI-37              PIC X(15).
      * MFNAMEA WAS RENAMED TO EZEI-36
              06  EZEI-36              PIC X(15).
      * FULLNAME WAS RENAMED TO EZEI-35
              06  EZEI-35              PIC X(45).
      * F_APPL_FORM_NUM WAS RENAMED TO EZEI-34
              06  EZEI-34              PIC S9(10).
      * M_APPL_FORM_NUM WAS RENAMED TO EZEI-33
              06  EZEI-33              PIC S9(10).
      * F_APPL_TYPE WAS RENAMED TO EZEI-32
              06  EZEI-32              PIC X(1).
      * M_APPL_TYPE WAS RENAMED TO EZEI-31
              06  EZEI-31              PIC X(1).
      * WXYZ WAS RENAMED TO EZEI-30
              06  EZEI-30              PIC S9(4).
      * VAR44 WAS RENAMED TO EZEI-29
              06  EZEI-29              PIC S9(9).
      * INQFLAG WAS RENAMED TO EZEI-28
              06  EZEI-28              PIC S9.
      * KEEP WAS RENAMED TO EZEI-27
             05  EZEI-27               PIC X(150).
             05  EZE-REDEF-121 REDEFINES EZEI-27.
      * X-VAR1 WAS RENAMED TO EZEI-26
              06  EZEI-26              PIC X(15).
      * X-VAR2 WAS RENAMED TO EZEI-25
              06  EZEI-25              PIC X(15).
      * X-VAR3 WAS RENAMED TO EZEI-24
              06  EZEI-24              PIC X(15).
      * X-VAR4 WAS RENAMED TO EZEI-23
              06  EZEI-23              PIC X(15).
      * X-VAR5 WAS RENAMED TO EZEI-22
              06  EZEI-22              PIC X(15).
      * X-VAR6 WAS RENAMED TO EZEI-21
              06  EZEI-21              PIC X(15).
      * X-VAR7 WAS RENAMED TO EZEI-20
              06  EZEI-20              PIC X(15).
      * X-VAR8 WAS RENAMED TO EZEI-19
              06  EZEI-19              PIC X(45).
      * C-SER1 WAS RENAMED TO EZEI-18
             05  EZEI-18               PIC S9(9) COMP.
      * C-SER2 WAS RENAMED TO EZEI-17
             05  EZEI-17               PIC X(1).
      * C-CSONUM WAS RENAMED TO EZEI-16
             05  EZEI-16               PIC S9(10).
      * X-VAR9 WAS RENAMED TO EZEI-15
             05  EZEI-15               PIC X(20).
      * X-VAR10 WAS RENAMED TO EZEI-14
             05  EZEI-14               PIC X(6).
           02  EZE-INIT-EZER-3 REDEFINES EZER-3.
             05  EZE-INIT-680          PIC S9(9) COMP.
             05  EZE-INIT-681          PIC A(15).
             05  EZE-INIT-682          PIC A(15).
             05  EZE-INIT-683          PIC A(15).
             05  EZE-INIT-684          PIC A(15).
             05  EZE-INIT-685          PIC S9(4) COMP.
             05  EZE-INIT-686          PIC S9(9) COMP.
             05  EZE-INIT-687          PIC A(10).
             05  EZE-INIT-688          PIC A(1).
             05  EZE-INIT-689          PIC A(1).
             05  EZE-INIT-690          PIC S9(15) COMP-3.
             05  EZE-INIT-691          PIC A(10).
             05  EZE-INIT-692          PIC A(8).
             05  EZE-INIT-693          PIC A(1).
             05  EZE-INIT-694          PIC A(1).
             05  EZE-INIT-695          PIC A(1).
             05  EZE-INIT-696          PIC A(1).
             05  EZE-INIT-697          PIC A(1).
             05  EZE-INIT-698          PIC A(1).
             05  EZE-INIT-699          PIC A(25).
             05  EZE-INIT-700          PIC A(25).
             05  EZE-INIT-701          PIC A(20).
             05  EZE-INIT-702          PIC S9(4) COMP.
             05  EZE-INIT-703          PIC S9(9) COMP.
             05  EZE-INIT-704          PIC S9(4) COMP.
             05  EZE-INIT-705          PIC S9(4) COMP.
             05  EZE-INIT-706          PIC S9(9) COMP.
             05  EZE-INIT-707          PIC S9(9) COMP.
             05  EZE-INIT-708          PIC S9(9) COMP.
             05  EZE-INIT-709          PIC S9(9) COMP.
             05  EZE-INIT-710          PIC S9(9) COMP.
             05  EZE-INIT-711          PIC S9(9) COMP.
             05  EZE-INIT-712          PIC S9(9) COMP.
             05  EZE-INIT-713          PIC S9(9) COMP.
             05  EZE-INIT-714.
              06  EZE-INIT-715.
               07  EZE-INIT-716        PIC A(2).
               07  EZE-INIT-717        PIC A(1).
               07  EZE-INIT-718        PIC A(2).
               07  EZE-INIT-719        PIC A(1).
               07  EZE-INIT-720        PIC A(4).
              06  EZE-INIT-721         PIC S9(4) COMP.
              06  EZE-INIT-722.
               07  EZE-INIT-723        PIC A(30).
               07  EZE-INIT-724        PIC A(10).
               07  EZE-INIT-725        PIC A(48).
              06  EZE-INIT-726.
               07  EZE-INIT-727        PIC A(4).
               07  EZE-INIT-728        PIC A(4).
              06  EZE-INIT-729         PIC 9(6).
              06  EZE-INIT-730         PIC 9.
              06  EZE-INIT-731         PIC 9(4).
              06  EZE-INIT-732         PIC 9(2).
              06  EZE-INIT-733         PIC 9(2).
              06  EZE-INIT-734         PIC S9(9) COMP.
              06  EZE-INIT-735         PIC S9(9) COMP.
              06  EZE-INIT-736         PIC 9(10).
              06  EZE-INIT-737         PIC A(1).
              06  EZE-INIT-738         PIC S9(9) COMP.
              06  EZE-INIT-739         PIC 9.
              06  EZE-INIT-740         PIC A(45).
              06  EZE-INIT-741         PIC A(45).
              06  EZE-INIT-742         PIC A(15).
              06  EZE-INIT-743         PIC A(15).
              06  EZE-INIT-744         PIC A(15).
              06  EZE-INIT-745         PIC A(45).
              06  EZE-INIT-746         PIC 9(10).
              06  EZE-INIT-747         PIC 9(10).
              06  EZE-INIT-748         PIC A(1).
              06  EZE-INIT-749         PIC A(1).
              06  EZE-INIT-750         PIC 9(4).
              06  EZE-INIT-751         PIC 9(9).
              06  EZE-INIT-752         PIC 9.
             05  EZE-INIT-753.
              06  EZE-INIT-754         PIC A(15).
              06  EZE-INIT-755         PIC A(15).
              06  EZE-INIT-756         PIC A(15).
              06  EZE-INIT-757         PIC A(15).
              06  EZE-INIT-758         PIC A(15).
              06  EZE-INIT-759         PIC A(15).
              06  EZE-INIT-760         PIC A(15).
              06  EZE-INIT-761         PIC A(45).
             05  EZE-INIT-762          PIC S9(9) COMP.
             05  EZE-INIT-763          PIC A(1).
             05  EZE-INIT-764          PIC 9(10).
             05  EZE-INIT-765          PIC A(20).
             05  EZE-INIT-766          PIC A(6).
           02  FILLER                  PIC X(4)
                                       VALUE "*END".
      *-----------------------------------------------------------------
      * APPLICATION LEVEL 77 DATA ITEMS
      *-----------------------------------------------------------------
       01  EZE77-ITEMS.
           05  EZE77-ID                PIC X(8)
                                       VALUE "ELAASGWS".
           05  EZE77-ITEMS-LL          PIC S9(8) COMP
                                       VALUE +32.
           05  EZE77-ITEMS-GROUP.
      * CHKX WAS RENAMED TO EZEI-13
             10  EZEI-13               PIC X(10).
      * CHKY WAS RENAMED TO EZEI-12
             10  EZEI-12               PIC S9(4) COMP.
      * CHKZ WAS RENAMED TO EZEI-11
             10  EZEI-11               PIC S9(4) COMP.
      * CHKX14 WAS RENAMED TO EZEI-10
             10  EZEI-10               PIC X(14).
       01  EZETBL-INDEX                PIC S9(4) COMP
                                       VALUE ZERO.

      *-----------------------------------------------------------------
      * ARRAY OF LOCAL TABLE BLOCKS
      *-----------------------------------------------------------------
       01  EZETBL-ARRAY.
           05  EZETBL-NO-OF-TABLES     PIC S9(9) COMP
                                       VALUE 1.

      *-----------------------------------------------------------------
      * TABLE CONTROL BLOCK FOR TABLE NESGENU
      *-----------------------------------------------------------------
           05  EZETBL-NESGENU-CONTROL.
             10  EZETBL-NESGENU-NAME   PIC X(8)
                                       VALUE "NESGENU".
             10  EZETBL-NESGENU-ADDRESS USAGE IS POINTER
                                       VALUE NULLS.
             10  EZETBL-NESGENU-TRMID  PIC X(8)
                                       VALUE SPACES.
             10  EZETBL-NESGENU-TASKID PIC S9(9)
                                       COMP
                                       VALUE ZEROES.
             10  EZETBL-NESGENU-ROW-LL
                                       PIC S9(9) COMP
                                       VALUE 44.
             10  EZETBL-NESGENU-KAU    PIC X(1)
                                       VALUE "N".
               88  EZETBL-NESGENU-NOT-KAU
                                       VALUE "N".
               88  EZETBL-NESGENU-IS-KAU
                                       VALUE "Y".
             10  EZETBL-NESGENU-STATUS PIC X(1)
                                       VALUE "N".
               88  EZETBL-NESGENU-NOT-REFERENCED
                                       VALUE "N".
               88  EZETBL-NESGENU-REFERENCED
                                       VALUE "R".
               88  EZETBL-NESGENU-ACCESSED
                                       VALUE "Y".
             10  EZETBL-NESGENU-RO     PIC X(1)
                                       VALUE "Y".
               88  EZETBL-NESGENU-READ-WRITE
                                       VALUE "N".
               88  EZETBL-NESGENU-READ-ONLY
                                       VALUE "Y".
             10  FILLER
                                       PIC X(1).
       01  EZELTB-ARRAY REDEFINES EZETBL-ARRAY.
           05  EZELTB-OCCURANCE        PIC S9(9) COMP.
           05  EZELTB-CONTROL          OCCURS 1 TIMES.
             10  EZELTB-NAME           PIC X(8).
             10  EZELTB-RTBPTR         USAGE IS POINTER.
             10  EZELTB-TRMID          PIC X(8).
             10  EZELTB-TASKID         PIC S9(9) COMP.
             10  EZELTB-ROW-LL         PIC S9(9) COMP.
             10  EZELTB-KAU            PIC X(1).
               88  EZELTB-NOT-KAU      VALUE "N".
               88  EZELTB-IS-KAU       VALUE "Y".
             10  EZELTB-STATUS         PIC X(1).
               88  EZELTB-NOT-REFERENCED
                                       VALUE "N".
               88  EZELTB-REFERENCED
                                       VALUE "R".
               88  EZELTB-ACCESSED
                                       VALUE "Y".
             10  EZELTB-RO             PIC X(1).
               88  EZELTB-READ-WRITE   VALUE "N".
               88  EZELTB-READ-ONLY    VALUE "Y".
             10  FILLER                PIC X(1).





      * MAP-RELATED WORKING STORAGE
           COPY ELAAHMSR.
       01  EZEMP-EZEM-1 SYNCHRONIZED.
           02  EZEM-1.
             05  EZEMAP-ID             PIC X(8)
                                       VALUE "ELASSMIP".
             05  EZEMAP-MAP-HEADER.
               10  EZEMAP-LENGTH       PIC S9(9) COMP.
               10  EZEMAP-NAME         PIC X(8)
                                       VALUE "CSM01".
               10  EZEMAP-GROUP        PIC X(6)
                                       VALUE "NSG01".
               10  EZEMAP-CURSOR-IN-NAME
                                       PIC X(32).
               10  EZEMAP-CURSOR-IN-OCC
                                       PIC S9(4) COMP.
               10  EZEMAP-CURSOR-OUT-NAME
                                       PIC X(32).
               10  EZEMAP-CURSOR-OUT-OCC
                                       PIC S9(4) COMP.
               10  EZEMAP-EZEMSG-DEF-SW
                                       PIC X(1)
                                       VALUE "Y".
                 88 EZEMAP-EZEMSG-DEFINED
                                       VALUE "Y".
                 88 EZEMAP-EZEMSG-NOT-DEFINED
                                       VALUE "N".
               10  EZEMAP-MODIFIED-SW  PIC X(1).
                 88 EZEMAP-MODIFIED    VALUE "Y".
                 88 EZEMAP-NOT-MODIFIED
                                       VALUE "N".
               10  EZEMAP-ALARM-SET-SW PIC X(1).
                 88 EZEMAP-ALARM-SET   VALUE "Y".
                 88 EZEMAP-ALARM-RESET VALUE "N".
               10 EZEMAP-SET-CLEAR-SW  PIC X(1).
                 88 EZEMAP-SET-CLEARED VALUE "Y".
                 88 EZEMAP-NOT-SET-CLEARED
                                       VALUE "N".
               10 EZEMAP-SET-PAGE-SW   PIC X(1).
                 88 EZEMAP-SET-PAGE    VALUE "Y".
                 88 EZEMAP-NOT-SET-PAGE
                                       VALUE "N".
               10 EZEMAP-EDIT-DONE-SW  PIC X(1).
                 88  EZEMAP-EDIT-DONE  VALUE "Y".
                 88  EZEMAP-EDIT-NOT-DONE
                                       VALUE "N".
               10  EZEMAP-EXT-LENGTH   PIC S9(9) COMP
                                       VALUE 488.
             05  EZEMAP-FIELD-INDEX    PIC S9(9) COMP.
             05  EZEMAP-FIELD-COUNT    PIC S9(9) COMP
                                       VALUE +8.
             05  EZEMAP-DATA.
               10  EZEMAP-DATA-EZEM-1-1.
                 15  FILLER            PIC X(29)
                                       VALUE LOW-VALUES.
                 15  FILLER            PIC X(1)
                                       VALUE X"F0".
                 15  FILLER            PIC X(1)
                                       VALUE X"00".
      * DATE WAS RENAMED TO EZEI-769
                 15  EZEI-769          PIC X(10).
                 15  FILLER            PIC X(1).
               10  EZEMAP-DATA-EZEM-1-2.
                 15  FILLER            PIC X(29)
                                       VALUE LOW-VALUES.
                 15  FILLER            PIC X(1)
                                       VALUE X"E0".
                 15  FILLER            PIC X(1)
                                       VALUE X"00".
                 15  EZEMSG            PIC X(40).
                 15  FILLER            PIC X(1).
               10  EZEMAP-DATA-EZEM-1-3.
                 15  FILLER            PIC X(29)
                                       VALUE LOW-VALUES.
                 15  FILLER            PIC X(1)
                                       VALUE X"C4".
                 15  FILLER            PIC X(1)
                                       VALUE X"00".
      * MPASSWRD WAS RENAMED TO EZEI-773
                 15  EZEI-773          PIC X(8).
                 15  FILLER            PIC X(1).
               10  EZEMAP-DATA-EZEM-1-4.
                 15  FILLER            PIC X(29)
                                       VALUE LOW-VALUES.
                 15  FILLER            PIC X(1)
                                       VALUE X"C0".
                 15  FILLER            PIC X(1)
                                       VALUE X"00".
      * MNAME WAS RENAMED TO EZEI-772
                 15  EZEI-772          PIC X(30).
                 15  FILLER            PIC X(1).
               10  EZEMAP-DATA-EZEM-1-5.
                 15  FILLER            PIC X(29)
                                       VALUE LOW-VALUES.
                 15  FILLER            PIC X(1)
                                       VALUE X"C4".
                 15  FILLER            PIC X(1)
                                       VALUE X"00".
      * NPASSWRD WAS RENAMED TO EZEI-774
                 15  EZEI-774          PIC X(8).
                 15  FILLER            PIC X(1).
               10  EZEMAP-DATA-EZEM-1-6.
                 15  FILLER            PIC X(29)
                                       VALUE LOW-VALUES.
                 15  FILLER            PIC X(1)
                                       VALUE X"C4".
                 15  FILLER            PIC X(1)
                                       VALUE X"00".
      * NPASSWRD1 WAS RENAMED TO EZEI-775
                 15  EZEI-775          PIC X(8).
                 15  FILLER            PIC X(1).
               10  EZEMAP-DATA-EZEM-1-7.
                 15  FILLER            PIC X(29)
                                       VALUE LOW-VALUES.
                 15  FILLER            PIC X(1)
                                       VALUE X"F0".
                 15  FILLER            PIC X(1)
                                       VALUE X"00".
      * TIME1 WAS RENAMED TO EZEI-770
                 15  EZEI-770          PIC X(8).
                 15  FILLER            PIC X(1).
               10  EZEMAP-DATA-EZEM-1-8.
                 15  FILLER            PIC X(29)
                                       VALUE LOW-VALUES.
                 15  FILLER            PIC X(1)
                                       VALUE X"F0".
                 15  FILLER            PIC X(1)
                                       VALUE X"00".
      * TERM_ID WAS RENAMED TO EZEI-771
                 15  EZEI-771          PIC X(8).
                 15  FILLER            PIC X(1).
       01  EZEIMAP-EZEM-1 REDEFINES EZEMP-EZEM-1
                                       SYNCHRONIZED.
           05  FILLER                  PIC X(8).
           05  EZEIMAP-MAP-HEADER      PIC X(96).
           05  EZEIMAP-FIELD-INDEX     PIC S9(9) COMP.
           05  EZEIMAP-FIELD-COUNT     PIC S9(9) COMP.
           05  EZEIMAP-ADJUNCTS.
             10  EZEI-769.
      * MAP VARIABLE ADJUNCT DEFINITIONS
           COPY ELAAHAS2.
                 15  FILLER            PIC X(10).
                 15  FILLER            PIC X(1).
             10  EZEMSG.
           COPY ELAAHAS2 SUPPRESS.
                 15  FILLER            PIC X(40).
                 15  FILLER            PIC X(1).
             10  EZEI-773.
           COPY ELAAHAS2 SUPPRESS.
                 15  FILLER            PIC X(8).
                 15  FILLER            PIC X(1).
             10  EZEI-772.
           COPY ELAAHAS2 SUPPRESS.
                 15  FILLER            PIC X(30).
                 15  FILLER            PIC X(1).
             10  EZEI-774.
           COPY ELAAHAS2 SUPPRESS.
                 15  FILLER            PIC X(8).
                 15  FILLER            PIC X(1).
             10  EZEI-775.
           COPY ELAAHAS2 SUPPRESS.
                 15  FILLER            PIC X(8).
                 15  FILLER            PIC X(1).
             10  EZEI-770.
           COPY ELAAHAS2 SUPPRESS.
                 15  FILLER            PIC X(8).
                 15  FILLER            PIC X(1).
             10  EZEI-771.
           COPY ELAAHAS2 SUPPRESS.
                 15  FILLER            PIC X(8).
                 15  FILLER            PIC X(1).
       LINKAGE SECTION.

       01  DFHCOMMAREA.
           05  EZECOMMAREA             PIC X(32763).
           05  EZECOMMAREA-MAPPED REDEFINES EZECOMMAREA.
             10  EZECOMMAREA-SSM-STATUS
                                       PIC X.
             10  EZECOMMAREA-MAP-NAME
                                       PIC X(8).
             10  EZECOMMAREA-RESERVED-0
                                       PIC X.
             10  EZECOMMAREA-USER-AREA
                                       PIC X(32753).
           05  EZECOMMAREA-POINTERS REDEFINES EZECOMMAREA.
             10  EZECOMMAREA-PTR USAGE IS POINTER
                                   OCCURS 8190 TIMES.
             10  EZECOMMAREA-PTR-RSVD
                                       PIC X(3).

      * RTS CONTROL BLOCK
           COPY ELARHRTS
                 REPLACING
                ==SYNCHRONIZED EXTERNAL== BY ==SYNCHRONIZED==.
      * RTS NLS-DEPENDENT INSTALLATION OPTIONS CONTROL BLOCK
           COPY ELARHIOE.
      * RTS NLS-INDEPENDENT INSTALLATION OPTIONS CONTROL BLOCK
           COPY ELARHIOP.

       01  EZECTL-IO-AREA.
           05  EZECTL-IO-DATA          PIC X(32767).
      * RESOURCE CONTROL BLOCK
           COPY ELARHRSC.
      * MAPPING SERVICES PROGRAM IDENTIFICATION BLOCK
           COPY ELAAHMGC.
      *-----------------------------------------------------------------
      * TABLE NAME = NESGENU
      * MODIFICATION DATE   : 06/03/2007
      * MODIFICATION TIME   : 10:42:58
      * READ ONLY           : YES
      * TABLE PROLOGUE      :
      *
      *-----------------------------------------------------------------
       01  EZETBL-NESGENU.
         02  EZETBL-NESGENU-FOLD       PIC X(1).
         02  EZETBL-NESGENU-RESIDENT   PIC X(1).
           88  EZETBL-NESGENU-IS-RESIDENT   VALUE "R".
           88  EZETBL-NESGENU-NOT-RESIDENT  VALUES "N" "Y".
         02  EZETBL-NESGENU-SHARED     PIC X(1).
           88  EZETBL-NESGENU-IS-SHARED     VALUE "Y".
           88  EZETBL-NESGENU-NOT-SHARED    VALUE "N".
         02  EZETBL-NESGENU-TYPE       PIC X(1).
           88  EZETBL-NESGENU-UNSPECIFIED   VALUE "U".
           88  EZETBL-NESGENU-MATCH-VALID   VALUE "V".
           88  EZETBL-NESGENU-MATCH-INVALID VALUE "I".
           88  EZETBL-NESGENU-RANGE-MATCH   VALUE "R".
           88  EZETBL-NESGENU-MSG-TABLE     VALUE "M".
         02  EZETBL-NESGENU-COUNT      PIC S9(9) COMP.
         02  EZETBL-NESGENU-ROW-LENGTH
                                       PIC S9(9) COMP.
         02  EZETBL-NESGENU-MSG-TEXT-LENGTH
                                       PIC S9(9) COMP.
         02  NESGENU                   OCCURS 1 TO 4096 TIMES
                                       DEPENDING ON EZETBL-NESGENU-COUNT
                                       INDEXED BY EZETBL-NESGENU-INDEX.
      * MSGCOD WAS RENAMED TO EZEI-1
            05  EZEI-1                 PIC S9(4).
      * MSGTXT WAS RENAMED TO EZEI-2
            05  EZEI-2                 PIC X(40).

      *-----------------------------------------------------------------
      * RUN UNIT TABLE CONTROL BLOCK
      *-----------------------------------------------------------------
       01  EZERTB-CONTROL.
           05  EZERTB-PREVPTR          USAGE IS POINTER.
           05  EZERTB-RESOURCE-NAME.
             10   EZERTB-NAME-PREFIX   PIC X(1).
             10   EZERTB-NAME          PIC X(7).
           05  EZERTB-TCBPTR           USAGE IS POINTER.
           05  EZERTB-USE-COUNT        PIC 9(4) COMP.
           05  EZERTB-READ-ONLY-STORAGE
                                       PIC X(1).
           05  EZERTB-RESERVED         PIC X(19).
       01  EZETRACEBACK-TABLE.
           05  EZETRACEBACK-HEADER.
             10  EZETRACEBACK-ID       PIC X(8).
             10  EZETRACEBACK-TABLE-LL PIC S9(9) COMP.
             10  EZETRACEBACK-HANDLE   PIC X(8).
             10  EZETRACEBACK-PTR      PIC S9(9) COMP.
             10  EZETRACEBACK-MAX-ENTRIES
                                       PIC S9(9) COMP.
           05  EZETRACEBACK-ENTRIES.
             10  EZETRACEBACK-ENTRY    PIC S9(9) COMP
                                       OCCURS 1 TO 999999 TIMES
                                 DEPENDING ON EZETRACEBACK-MAX-ENTRIES.
       01  EZETBKSAVE-TABLE.
           05  EZETBKSAVE-HEADER.
             10  EZETBKSAVE-ID         PIC X(8).
             10  EZETBKSAVE-TABLE-LL   PIC S9(9) COMP.
             10  EZETBKSAVE-HANDLE     PIC X(8).
             10  EZETBKSAVE-PTR        PIC S9(9) COMP.
             10  EZETBKSAVE-MAX-ENTRIES
                                       PIC S9(9) COMP.
           05  EZETBKSAVE-ENTRIES.
             10  EZETBKSAVE-ENTRY      PIC S9(9) COMP
                                       OCCURS 1 TO 999999 TIMES
                                 DEPENDING ON EZETBKSAVE-MAX-ENTRIES.
       01  EZEWRK-LINKAGE-ITEM         PIC X(32767).
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      * MAIN PROCESS
      *-----------------------------------------------------------------
       EZEMAIN-PROCESS SECTION.
           PERFORM EZECONTROL
           GOBACK.

      *-----------------------------------------------------------------
      * BEGIN PROCESS
      *-----------------------------------------------------------------
       EZEBEGIN-PROCESSES SECTION.
           CONTINUE.

      *-----------------------------------------------------------------
      * TRACEBACK SET UP
      *-----------------------------------------------------------------
       EZETRACEBK-SETUP SECTION.
           IF EZERTS-CONVERSE
               GO TO EZETRACEBACK
           END-IF
           IF EZERTS-IN-CONTEXT-NOT-SAVED
              PERFORM EZEINIT-TRACEBACK
           END-IF.
       EZETRACEBK-SETUP-X.
           EXIT.

      *-----------------------------------------------------------------
      * MAIN PROCESS        : MAIN-NSA01
      *                     : MAIN-NSA01 RENAMED TO EZEP-3
      * MODIFICATION DATE   : 07/03/2007
      * MODIFICATION TIME   : 08:19:14
      * PROCESS OPTION      : EXECUTE
      *-----------------------------------------------------------------
       EZEP-3 SECTION.
           MOVE "MAIN-NSA01" TO EZERTS-PRC-NAME
000001* MOVE 0 TO END-LOOP;
           MOVE 0 TO EZEI-700 OF EZER-5
           CALL "ELAFXNUM" USING EZEI-700 OF EZER-5(LENGTH OF EZEI-700
            OF EZER-5:1)
000002* WHILE END-LOOP = 0;
           CONTINUE.
       EZECONDLBL-1.
           IF EZEI-700 OF EZER-5 = 0
             GO TO EZECONDLBL-2
           END-IF
           GO TO EZECONDLBL-3
           CONTINUE.
       EZECONDLBL-2.
000003*    CSA01P1();
           PERFORM EZEINCR-TRACEBACK-PTR
           MOVE 1 TO EZETRACEBACK-ENTRY(EZETRACEBACK-PTR)
           GO TO EZEP-1.
       EZELABEL-1.
           GO TO EZECONDLBL-1
           CONTINUE.
       EZECONDLBL-3.
000004* END;
           CONTINUE.
       EZE-EZEP-3-X.
           IF EZETRACEBACK-PTR > ZERO
             GO TO EZETRACEBACK
           END-IF.
      *-----------------------------------------------------------------
      * STRUCTURE LIST POST-PROCESSING
      *-----------------------------------------------------------------
       EZESTRUCTURE-FALLTHRU SECTION.
           SET EZERTS-EZECLOS TO TRUE
           GO TO EZETERMINATE.
       EZESTRUCTURE-FALLTHRU-X.
           EXIT.

      *-----------------------------------------------------------------
      * PROCESS             : ACCEPT_TIME
      *                     : ACCEPT_TIME RENAMED TO EZEP-4
      * MODIFICATION DATE   : 06/03/2007
      * MODIFICATION TIME   : 10:42:51
      * PROCESS OPTION      : EXECUTE
      * DESCRIPTION         : Prepare Time
      *-----------------------------------------------------------------
       EZEP-4 SECTION.
           MOVE "ACCEPT_TIME" TO EZERTS-PRC-NAME
000005* MOVE 'A' TO RASHA_REC.INVCD;
           MOVE "A" TO EZEI-664 OF EZER-5
000006* MOVE 08 TO RASHA_REC.INVLEN;
           MOVE 08 TO EZEI-665 OF EZER-5
           CALL "ELAFXNUM" USING EZEI-665 OF EZER-5(LENGTH OF EZEI-665
            OF EZER-5:1)
000007* MOVE EZETIM TO RASHA_REC.MOITIME;
           PERFORM EZEGET-EZETIM
           MOVE EZETIM TO EZEI-633 OF EZER-5
000008* /**********************************************************
000009* MOVE RASHA_REC.MOITIME TO RASHA_REC.INVDATA;
           MOVE EZEI-633 OF EZER-5 TO EZEI-663 OF EZER-5
000010* MOVE RASHA_REC.INVREC TO RASHA_REC.INVAREA;
           MOVE EZEI-667 OF EZER-5 TO EZEI-662 OF EZER-5
000011* CALL IDSCIVRS RASHA_REC.INVAREA (NOMAPS,NONCSP;
           MOVE "ACCEPT_TIME" TO EZERTS-PRC-NAME
           MOVE 11 TO EZERTS-PRC-NUM
           CALL "ELAASADR" USING EZEI-662 OF EZER-5
                                 EZECOMMAREA-PTR(1)
           CALL "ELAASSGN" USING CONTENT "A"
                CONTENT LENGTH OF EZECOMMAREA-PTR(1)
                REFERENCE EZECOMMAREA-PTR(1)
           MOVE HIGH-VALUES TO EZECOMMAREA(5:4)
           MOVE "IDSCIVRS" TO EZEPROGM
           SET EZERTS-DYNAMIC-LINK TO TRUE
           SET EZERTS-LINK-COMMPTR TO TRUE
           EXEC CICS LINK
             PROGRAM(EZEPROGM)
             COMMAREA(EZECOMMAREA)
             LENGTH(4)
           END-EXEC
           IF EIBRESP NOT = DFHRESP(NORMAL)
             MOVE 9031 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
                                   EZEPROGM
           END-IF
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
000012* MOVE RASHA_REC.INVAREA TO RASHA_REC.INVREC;
           MOVE EZEI-662 OF EZER-5 TO EZEI-667 OF EZER-5
000013* MOVE RASHA_REC.INVDATA TO RASHA_REC.MOITIME;
           MOVE EZEI-663 OF EZER-5 TO EZEI-633 OF EZER-5
000014* /***********************************************************
           CONTINUE.
       EZE-EZEP-4-X.
           GO TO EZETRACEBACK.
      *-----------------------------------------------------------------
      * PROCESS             : CGA99P1
      *                     : CGA99P1 RENAMED TO EZEP-2
      * MODIFICATION DATE   : 06/03/2007
      * MODIFICATION TIME   : 10:42:52
      * PROCESS OPTION      : EXECUTE
      * DESCRIPTION         : prepare  date
      *-----------------------------------------------------------------
       EZEP-2 SECTION.
           MOVE "CGA99P1" TO EZERTS-PRC-NAME
000015* MOVE 'A' TO RASHA_REC.INVCD;
           MOVE "A" TO EZEI-664 OF EZER-5
000016* MOVE 04 TO RASHA_REC.INVLEN;
           MOVE 04 TO EZEI-665 OF EZER-5
           CALL "ELAFXNUM" USING EZEI-665 OF EZER-5(LENGTH OF EZEI-665
            OF EZER-5:1)
000017* ; /* **
000018* MOVE EZEDTELC TO WDATE1;
           PERFORM EZEGET-EZEDTE
           MOVE EZEDTELC TO EZEI-687 OF EZER-5
000019* MOVE RASHA_REC.YY1 TO RASHA_REC.YEAR_2000;
           MOVE "CGA99P1" TO EZERTS-PRC-NAME
           MOVE 5 TO EZERTS-PRC-NUM
           MOVE LENGTH OF EZEI-682 OF EZER-5 TO EZERTS-NUM-IN-LEN
           MOVE LENGTH OF EZEI-661 OF EZER-5 TO EZERTS-NUM-OUT-LEN
           MOVE 0 TO EZERTS-NUM-DECIMAL-PLACES
           MOVE "N" TO EZERTS-NUM-CONDITIONAL
           MOVE "LEA" TO EZERTS-NUM-SIGN
           MOVE EZERTS-CHA-TO-NUM TO EZERTS-NUM-SVCS-NUM
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-NUM-REQUEST-BLOCK
                                 BY REFERENCE EZEI-682 OF EZER-5
                                 BY REFERENCE EZEI-661 OF EZER-5
           END-CALL
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
           MOVE SPACES TO EZERTS-NUM-SIGN
000020* RASHA_REC.YEAR_2000 = RASHA_REC.YEAR_2000 + 2000;
           COMPUTE EZEI-661 OF EZER-5 = EZEI-661 OF EZER-5 + 2000
             ON SIZE ERROR
               MOVE "CGA99P1" TO EZERTS-PRC-NAME
               MOVE 20 TO EZERTS-PRC-NUM
               PERFORM EZEOVER-ROUTINE
           END-COMPUTE
           CALL "ELAFXNUM" USING EZEI-661 OF EZER-5(LENGTH OF EZEI-661
            OF EZER-5:1)
000021* MOVE RASHA_REC.YEAR_2000 TO RASHA_REC.YY1;
           MOVE EZEI-661 OF EZER-5(1:LENGTH OF EZEI-661 OF EZER-5) TO
            EZEI-682 OF EZER-5
000022* MOVE RASHA_REC.YY1 TO RASHA_REC.INVDATA;
           MOVE EZEI-682 OF EZER-5 TO EZEI-663 OF EZER-5
000023* MOVE RASHA_REC.INVREC TO RASHA_REC.INVAREA;
           MOVE EZEI-667 OF EZER-5 TO EZEI-662 OF EZER-5
000024* CALL IDSCIVRS RASHA_REC.INVAREA (NOMAPS,NONCSP;
           MOVE "CGA99P1" TO EZERTS-PRC-NAME
           MOVE 24 TO EZERTS-PRC-NUM
           CALL "ELAASADR" USING EZEI-662 OF EZER-5
                                 EZECOMMAREA-PTR(1)
           CALL "ELAASSGN" USING CONTENT "A"
                CONTENT LENGTH OF EZECOMMAREA-PTR(1)
                REFERENCE EZECOMMAREA-PTR(1)
           MOVE HIGH-VALUES TO EZECOMMAREA(5:4)
           MOVE "IDSCIVRS" TO EZEPROGM
           SET EZERTS-DYNAMIC-LINK TO TRUE
           SET EZERTS-LINK-COMMPTR TO TRUE
           EXEC CICS LINK
             PROGRAM(EZEPROGM)
             COMMAREA(EZECOMMAREA)
             LENGTH(4)
           END-EXEC
           IF EIBRESP NOT = DFHRESP(NORMAL)
             MOVE 9031 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
                                   EZEPROGM
           END-IF
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
000025* MOVE RASHA_REC.INVAREA TO RASHA_REC.INVREC;
           MOVE EZEI-662 OF EZER-5 TO EZEI-667 OF EZER-5
000026* MOVE RASHA_REC.INVDATA TO W-COM-AREA.YY2;
           MOVE EZEI-663 OF EZER-5 TO EZEI-56 OF EZER-3
000027* ;  /* **
000028* MOVE 02 TO RASHA_REC.INVLEN;
           MOVE 02 TO EZEI-665 OF EZER-5
           CALL "ELAFXNUM" USING EZEI-665 OF EZER-5(LENGTH OF EZEI-665
            OF EZER-5:1)
000029* MOVE RASHA_REC.MM1 TO RASHA_REC.INVDATA;
           MOVE EZEI-686 OF EZER-5 TO EZEI-663 OF EZER-5
000030* MOVE RASHA_REC.INVREC TO RASHA_REC.INVAREA;
           MOVE EZEI-667 OF EZER-5 TO EZEI-662 OF EZER-5
000031* CALL IDSCIVRS RASHA_REC.INVAREA (NOMAPS,NONCSP;
           MOVE "CGA99P1" TO EZERTS-PRC-NAME
           MOVE 31 TO EZERTS-PRC-NUM
           CALL "ELAASADR" USING EZEI-662 OF EZER-5
                                 EZECOMMAREA-PTR(1)
           CALL "ELAASSGN" USING CONTENT "A"
                CONTENT LENGTH OF EZECOMMAREA-PTR(1)
                REFERENCE EZECOMMAREA-PTR(1)
           MOVE HIGH-VALUES TO EZECOMMAREA(5:4)
           MOVE "IDSCIVRS" TO EZEPROGM
           SET EZERTS-DYNAMIC-LINK TO TRUE
           SET EZERTS-LINK-COMMPTR TO TRUE
           EXEC CICS LINK
             PROGRAM(EZEPROGM)
             COMMAREA(EZECOMMAREA)
             LENGTH(4)
           END-EXEC
           IF EIBRESP NOT = DFHRESP(NORMAL)
             MOVE 9031 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
                                   EZEPROGM
           END-IF
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
000032* MOVE RASHA_REC.INVAREA TO RASHA_REC.INVREC;
           MOVE EZEI-662 OF EZER-5 TO EZEI-667 OF EZER-5
000033* MOVE RASHA_REC.INVDATA TO W-COM-AREA.MM2;
           MOVE EZEI-663 OF EZER-5 TO EZEI-58 OF EZER-3
000034* ; /* **
000035* MOVE RASHA_REC.DD1 TO RASHA_REC.INVDATA;
           MOVE EZEI-684 OF EZER-5 TO EZEI-663 OF EZER-5
000036* MOVE RASHA_REC.INVREC TO RASHA_REC.INVAREA;
           MOVE EZEI-667 OF EZER-5 TO EZEI-662 OF EZER-5
000037* CALL IDSCIVRS RASHA_REC.INVAREA (NOMAPS,NONCSP;
           MOVE "CGA99P1" TO EZERTS-PRC-NAME
           MOVE 37 TO EZERTS-PRC-NUM
           CALL "ELAASADR" USING EZEI-662 OF EZER-5
                                 EZECOMMAREA-PTR(1)
           CALL "ELAASSGN" USING CONTENT "A"
                CONTENT LENGTH OF EZECOMMAREA-PTR(1)
                REFERENCE EZECOMMAREA-PTR(1)
           MOVE HIGH-VALUES TO EZECOMMAREA(5:4)
           MOVE "IDSCIVRS" TO EZEPROGM
           SET EZERTS-DYNAMIC-LINK TO TRUE
           SET EZERTS-LINK-COMMPTR TO TRUE
           EXEC CICS LINK
             PROGRAM(EZEPROGM)
             COMMAREA(EZECOMMAREA)
             LENGTH(4)
           END-EXEC
           IF EIBRESP NOT = DFHRESP(NORMAL)
             MOVE 9031 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
                                   EZEPROGM
           END-IF
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
000038* MOVE RASHA_REC.INVAREA TO RASHA_REC.INVREC;
           MOVE EZEI-662 OF EZER-5 TO EZEI-667 OF EZER-5
000039* MOVE RASHA_REC.INVDATA TO W-COM-AREA.DD2;
           MOVE EZEI-663 OF EZER-5 TO EZEI-60 OF EZER-3
000040* ; /* **
000041* MOVE '/' TO W-COM-AREA.SLASH1;
           MOVE "/" TO EZEI-59 OF EZER-3
000042* MOVE '/' TO W-COM-AREA.SLASH2;
           MOVE "/" TO EZEI-57 OF EZER-3
           CONTINUE.
       EZE-EZEP-2-X.
           GO TO EZETRACEBACK.
      *-----------------------------------------------------------------
      * PROCESS             : CSA01P1
      *                     : CSA01P1 RENAMED TO EZEP-1
      * MODIFICATION DATE   : 07/03/2007
      * MODIFICATION TIME   : 08:17:57
      * PROCESS OPTION      : CONVERSE
      * PROCESS OBJECT      : CSM01
      * PROCESS OBJECT      : CSM01 RENAMED TO EZEM-1
      * DESCRIPTION         : converse map
      *-----------------------------------------------------------------
       EZEP-1 SECTION.
           MOVE "CSA01P1" TO EZERTS-PRC-NAME
000043* MOVE 1 TO EZECNVCM;
           MOVE 1 TO EZECNVCM
           CALL "ELAFXNUM" USING EZECNVCM(LENGTH OF EZECNVCM:1)
000044* CGA99P1();
           PERFORM EZEINCR-TRACEBACK-PTR
           MOVE 2 TO EZETRACEBACK-ENTRY(EZETRACEBACK-PTR)
           GO TO EZEP-2.
       EZELABEL-2.
000045* MOVE W-COM-AREA.WDATE TO CSM01.DATE;
           MOVE EZEI-61 OF EZER-3 TO EZEI-769 OF EZEM-1
           SET EZEHAST-OUTPUT-SELECTED
             OF EZEI-769 OF EZEIMAP-EZEM-1 TO TRUE
000046* ACCEPT_TIME();
           PERFORM EZEINCR-TRACEBACK-PTR
           MOVE 3 TO EZETRACEBACK-ENTRY(EZETRACEBACK-PTR)
           GO TO EZEP-4.
       EZELABEL-3.
000047* MOVE RASHA_REC.MOITIME TO CSM01.TIME1;
           MOVE EZEI-633 OF EZER-5 TO EZEI-770 OF EZEM-1
           SET EZEHAST-OUTPUT-SELECTED
             OF EZEI-770 OF EZEIMAP-EZEM-1 TO TRUE
000048* MOVE EZEUSR TO W-COM-AREA.TITRGG44;
           MOVE EZEUSR TO EZEI-48 OF EZER-3
000049* MOVE 'A' TO RASHA_REC.INVCD;
           MOVE "A" TO EZEI-664 OF EZER-5
000050* MOVE 4 TO RASHA_REC.INVLEN;
           MOVE 4 TO EZEI-665 OF EZER-5
           CALL "ELAFXNUM" USING EZEI-665 OF EZER-5(LENGTH OF EZEI-665
            OF EZER-5:1)
000051* MOVE W-COM-AREA.TITRGG44 TO RASHA_REC.INVDATA;
           MOVE EZEI-48 OF EZER-3 TO EZEI-663 OF EZER-5
000052* MOVE RASHA_REC.INVREC TO RASHA_REC.INVAREA;
           MOVE EZEI-667 OF EZER-5 TO EZEI-662 OF EZER-5
000053* CALL IDSCIVRS RASHA_REC.INVAREA (NOMAPS,NONCSP;
           MOVE "CSA01P1" TO EZERTS-PRC-NAME
           MOVE 53 TO EZERTS-PRC-NUM
           CALL "ELAASADR" USING EZEI-662 OF EZER-5
                                 EZECOMMAREA-PTR(1)
           CALL "ELAASSGN" USING CONTENT "A"
                CONTENT LENGTH OF EZECOMMAREA-PTR(1)
                REFERENCE EZECOMMAREA-PTR(1)
           MOVE HIGH-VALUES TO EZECOMMAREA(5:4)
           MOVE "IDSCIVRS" TO EZEPROGM
           SET EZERTS-DYNAMIC-LINK TO TRUE
           SET EZERTS-LINK-COMMPTR TO TRUE
           EXEC CICS LINK
             PROGRAM(EZEPROGM)
             COMMAREA(EZECOMMAREA)
             LENGTH(4)
           END-EXEC
           IF EIBRESP NOT = DFHRESP(NORMAL)
             MOVE 9031 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
                                   EZEPROGM
           END-IF
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
000054* MOVE RASHA_REC.INVAREA TO RASHA_REC.INVREC;
           MOVE EZEI-662 OF EZER-5 TO EZEI-667 OF EZER-5
000055* MOVE RASHA_REC.INVDATA TO W-COM-AREA.TITRGG44;
           MOVE EZEI-663 OF EZER-5 TO EZEI-48 OF EZER-3
000056* MOVE W-COM-AREA.TITRGG44 TO CSM01.TERM_ID;
           MOVE EZEI-48 OF EZER-3 TO EZEI-771 OF EZEM-1
           SET EZEHAST-OUTPUT-SELECTED
             OF EZEI-771 OF EZEIMAP-EZEM-1 TO TRUE
000057*
           PERFORM EZEINCR-TRACEBACK-PTR
           MOVE 4 TO EZETRACEBACK-ENTRY(EZETRACEBACK-PTR)
           GO TO EZECONV-EZEP-1.
       EZELABEL-4.
000058* IF EZEAID IS PF15 OR EZEAID IS PF3;
           IF (EZEAID = "15") OR (EZEAID = "03")
             GO TO EZECONDLBL-4
           END-IF
           GO TO EZECONDLBL-5
           CONTINUE.
       EZECONDLBL-4.
000059* /* IF LOGON_CHK1.LOGON_STATUS = 1;
000060* /* MOVE ' ' TO LOGON_CHK1.OPPSWD;
000061* /* MOVE ' ' TO LOGON_CHK1.NPPSWD;
000062* /* MOVE ' ' TO LOGON_CHK1.OPNAME;
000063* /* MOVE 3 TO LOGON_CHK1.LOGON_STATUS;
000064* /* MOVE W-COM-AREA.USER-ID TO LOGON_CHK1.USRID;
000065* /* CALL LGONCHK LOGON_CHK1 (NOMAPS,NONCSP;
000066* /* IF LOGON_STATUS = 1;
000067* /* MOVE 1 TO END-LOOP;
000068* /* EZERTN();
000069* /* ELSE;
000070* /* IF LOGON_STATUS = 6;
000071* /* MOVE 1 TO END-LOOP;
000072* /* EZERTN();
000073* /* END;
000074* /* END;
000075* /* ELSE;
000076* /* IF LOGON_CHK1.LOGON_STATUS = 0
000077* /* AND W-COM-AREA.USER-ID = 0;
000078* /* MOVE 1 TO END-LOOP;
000079* /* EZERTN();
000080* /* ELSE;
000081* /* IF LOGON_CHK1.LOGON_STATUS = 0
000082* /* AND W-COM-AREA.USER-ID > 0;
000083* /* MOVE ' ' TO LOGON_CHK1.OPPSWD;
000084* /* MOVE ' ' TO LOGON_CHK1.NPPSWD;
000085* /* MOVE ' ' TO LOGON_CHK1.OPNAME;
000086* /* MOVE 3 TO LOGON_CHK1.LOGON_STATUS;
000087* /* MOVE W-COM-AREA.USER-ID TO LOGON_CHK1.USRID;
000088* /* CALL LGONCHK LOGON_CHK1 (NOMAPS,NONCSP;
000089* /* IF LOGON_STATUS = 1;
000090* /* MOVE 1 TO END-LOOP;
000091* /* EZERTN();
000092* /* ELSE;
000093* /* IF LOGON_STATUS = 6;
000094* /* MOVE 1 TO END-LOOP;
000095* /* EZERTN();
000096* /* END;
000097* /* END;
000098* /* ELSE;
000099*   MOVE 1 TO END-LOOP;
           MOVE 1 TO EZEI-700 OF EZER-5
           CALL "ELAFXNUM" USING EZEI-700 OF EZER-5(LENGTH OF EZEI-700
            OF EZER-5:1)
000100*   EZERTN();
           GO TO EZE-EZEP-1-X
000101* /* END;
000102* /* END;
000103* /* END;
000104* ELSE;
           GO TO EZECONDLBL-6
           CONTINUE.
       EZECONDLBL-5.
000105*   IF EZEAID IS PF9;
           IF (EZEAID = "09")
             GO TO EZECONDLBL-7
           END-IF
           GO TO EZECONDLBL-8
           CONTINUE.
       EZECONDLBL-7.
000106*     IF CSM01.NPASSWRD NE ' ';
           IF EZEI-774 OF EZEM-1 NOT = " "
             GO TO EZECONDLBL-9
           END-IF
           GO TO EZECONDLBL-10
           CONTINUE.
       EZECONDLBL-9.
000107*       IF CSM01.NPASSWRD = CSM01.MPASSWRD;
           IF EZEI-774 OF EZEM-1 = EZEI-773 OF EZEM-1
             GO TO EZECONDLBL-11
           END-IF
           GO TO EZECONDLBL-12
           CONTINUE.
       EZECONDLBL-11.
000108*         EZEMNO = 91;
           MOVE 91 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000109*         EZERTN();
           GO TO EZE-EZEP-1-X
           CONTINUE.
       EZECONDLBL-12.
000110*       END;
000111*       IF CSM01.NPASSWRD NE CSM01.NPASSWRD1;
           IF EZEI-774 OF EZEM-1 NOT = EZEI-775 OF EZEM-1
             GO TO EZECONDLBL-13
           END-IF
           GO TO EZECONDLBL-14
           CONTINUE.
       EZECONDLBL-13.
000112*         SET CSM01.NPASSWRD1 CURSOR;
           MOVE "NPASSWRD1"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-775 OF EZEIMAP-EZEM-1 TO TRUE
000113*         EZEMNO = 92;
           MOVE 92 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000114*         EZERTN();
           GO TO EZE-EZEP-1-X
000115*       ELSE;
           GO TO EZECONDLBL-15
           CONTINUE.
       EZECONDLBL-14.
000116*         MOVE CSM01.MPASSWRD TO LOGON_CHK1.OPPSWD;
           MOVE EZEI-773 OF EZEM-1 TO EZEI-98 OF EZER-4
000117*         MOVE CSM01.NPASSWRD TO LOGON_CHK1.NPPSWD;
           MOVE EZEI-774 OF EZEM-1 TO EZEI-99 OF EZER-4
000118*         MOVE CSM01.MNAME TO LOGON_CHK1.OPNAME;
           MOVE EZEI-772 OF EZEM-1 TO EZEI-97 OF EZER-4
000119*         MOVE 0 TO LOGON_STATUS;
           MOVE 0 TO EZEI-100 OF EZER-4
           CALL "ELAFXNUM" USING EZEI-100 OF EZER-4(LENGTH OF EZEI-100
            OF EZER-4:1)
000120*         CALL LGONCHK LOGON_CHK1 (NOMAPS,NONCSP;
           MOVE "CSA01P1" TO EZERTS-PRC-NAME
           MOVE 120 TO EZERTS-PRC-NUM
           CALL "ELAASADR" USING EZER-4
                                 EZECOMMAREA-PTR(1)
           CALL "ELAASSGN" USING CONTENT "A"
                CONTENT LENGTH OF EZECOMMAREA-PTR(1)
                REFERENCE EZECOMMAREA-PTR(1)
           MOVE HIGH-VALUES TO EZECOMMAREA(5:4)
           MOVE "LGONCHK" TO EZEPROGM
           SET EZERTS-DYNAMIC-LINK TO TRUE
           SET EZERTS-LINK-COMMPTR TO TRUE
           EXEC CICS LINK
             PROGRAM(EZEPROGM)
             COMMAREA(EZECOMMAREA)
             LENGTH(4)
           END-EXEC
           IF EIBRESP NOT = DFHRESP(NORMAL)
             MOVE 9031 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
                                   EZEPROGM
           END-IF
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
000121*         IF LOGON_STATUS = 1;
           IF EZEI-100 OF EZER-4 = 1
             GO TO EZECONDLBL-16
           END-IF
           GO TO EZECONDLBL-17
           CONTINUE.
       EZECONDLBL-16.
000122*           MOVE LOGON_CHK1.USRID TO W-COM-AREA.USER-ID;
           MOVE EZEI-101 OF EZER-4 TO EZEI-55 OF EZER-3
000123*           MOVE LOGON_CHK1.USRID TO SAM_REC.USER-ID;
           MOVE EZEI-101 OF EZER-4 TO EZEI-9 OF EZER-2
000124*           MOVE LOGON_CHK1.USRID TO SAM_REC.SAM-USER-ID;
           MOVE EZEI-101 OF EZER-4 TO EZEI-5 OF EZER-2
           CALL "ELAFXNUM" USING EZEI-5 OF EZER-2(LENGTH OF EZEI-5 OF
            EZER-2:1)
000125*
000126* /*          MOVE 'A' TO RASHA_REC.INVCD;
000127* /*          MOVE 04 TO RASHA_REC.INVLEN;
000128* /*          MOVE SAM_REC.USER-ID TO RASHA_REC.VAR4;
000129* /*          MOVE RASHA_REC.VAR4 TO RASHA_REC.INVDATA;
000130* /*          MOVE RASHA_REC.INVREC TO RASHA_REC.INVAREA;
000131* /*          CALL IDSCIVRS RASHA_REC.INVAREA (NOMAPS,NONCSP;
000132* /*          MOVE RASHA_REC.INVAREA TO RASHA_REC.INVREC;
000133* /*          MOVE RASHA_REC.INVDATA TO TST.OPERATORNUM2;
000134* /*          TST_PROC();
000135*           MOVE '0' TO SAM_REC.SAM-FILLER1;
           MOVE ZEROS TO EZEI-4 OF EZER-2
000136*           MOVE '0' TO SAM_REC.SAM-FILLER2;
           MOVE ZEROS TO EZEI-6 OF EZER-2
000137*    /* COM_AREA1();
000138*           /* DXFR NSA02 RCGW99;      /* W-COM-AREA;
000139*           XFER NMIN SAM_REC;
           MOVE "CSA01P1" TO EZERTS-PRC-NAME
           MOVE 139 TO EZERTS-PRC-NUM
           MOVE "NMIN" TO EZERTS-TGT-TRANSACTION
           CALL "ELAASADR" USING
                           EZER-2
                           EZERTS-DXFR-XFER-REC-PTR
           MOVE LENGTH OF EZER-2
             TO EZERTS-DXFR-XFER-REC-LEN
           SET EZERTS-XFER TO TRUE
           SET EZERTS-XFER-MAP-PTR TO NULL
           MOVE SPACES TO EZERTS-MAP-GROUP
           MOVE SPACES TO EZERTS-HELP-MAP-GROUP
           GO TO EZETERMINATE
000140*         ELSE;
           GO TO EZECONDLBL-18
           CONTINUE.
       EZECONDLBL-17.
000141*           IF LOGON_STATUS = 2;
           IF EZEI-100 OF EZER-4 = 2
             GO TO EZECONDLBL-19
           END-IF
           GO TO EZECONDLBL-20
           CONTINUE.
       EZECONDLBL-19.
000142*             EZEMNO = 1;
           MOVE 1 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000143*             SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000144*             EZERTN();
           GO TO EZE-EZEP-1-X
000145*           ELSE;
           GO TO EZECONDLBL-21
           CONTINUE.
       EZECONDLBL-20.
000146*             IF LOGON_STATUS = 3;
           IF EZEI-100 OF EZER-4 = 3
             GO TO EZECONDLBL-22
           END-IF
           GO TO EZECONDLBL-23
           CONTINUE.
       EZECONDLBL-22.
000147*               EZEMNO = 2;
           MOVE 2 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000148*               SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000149*               EZERTN();
           GO TO EZE-EZEP-1-X
000150*             ELSE;
           GO TO EZECONDLBL-24
           CONTINUE.
       EZECONDLBL-23.
000151*               IF LOGON_STATUS = 4;
           IF EZEI-100 OF EZER-4 = 4
             GO TO EZECONDLBL-25
           END-IF
           GO TO EZECONDLBL-26
           CONTINUE.
       EZECONDLBL-25.
000152*                 EZEMNO = 322;
           MOVE 322 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000153*                 SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000154*                 EZERTN();
           GO TO EZE-EZEP-1-X
000155*               ELSE;
           GO TO EZECONDLBL-27
           CONTINUE.
       EZECONDLBL-26.
000156*                 IF LOGON_STATUS = 5;
           IF EZEI-100 OF EZER-4 = 5
             GO TO EZECONDLBL-28
           END-IF
           GO TO EZECONDLBL-29
           CONTINUE.
       EZECONDLBL-28.
000157*                   EZEMNO = 323;
           MOVE 323 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000158*                   SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000159*                   EZERTN();
           GO TO EZE-EZEP-1-X
000160*                 ELSE;
           GO TO EZECONDLBL-30
           CONTINUE.
       EZECONDLBL-29.
000161*                   IF LOGON_STATUS = 6;
           IF EZEI-100 OF EZER-4 = 6
             GO TO EZECONDLBL-31
           END-IF
           GO TO EZECONDLBL-32
           CONTINUE.
       EZECONDLBL-31.
000162*                     EZEMNO = 36;
           MOVE 36 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000163*                     SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000164*                     EZERTN();
           GO TO EZE-EZEP-1-X
000165*                   ELSE;
           GO TO EZECONDLBL-33
           CONTINUE.
       EZECONDLBL-32.
000166*                     IF LOGON_STATUS = 7;
           IF EZEI-100 OF EZER-4 = 7
             GO TO EZECONDLBL-34
           END-IF
           GO TO EZECONDLBL-35
           CONTINUE.
       EZECONDLBL-34.
000167*                       EZEMNO = 395;
           MOVE 395 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000168*                       SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000169*                       EZERTN();
           GO TO EZE-EZEP-1-X
           CONTINUE.
       EZECONDLBL-35.
000170*                     END;
           CONTINUE.
       EZECONDLBL-33.
000171*                   END;
           CONTINUE.
       EZECONDLBL-30.
000172*                 END;
           CONTINUE.
       EZECONDLBL-27.
000173*               END;
           CONTINUE.
       EZECONDLBL-24.
000174*             END;
           CONTINUE.
       EZECONDLBL-21.
000175*           END;
           CONTINUE.
       EZECONDLBL-18.
000176*         END;
           CONTINUE.
       EZECONDLBL-15.
000177*       END;
           CONTINUE.
       EZECONDLBL-10.
000178*     END;
000179*   ELSE;
           GO TO EZECONDLBL-36
           CONTINUE.
       EZECONDLBL-8.
000180*     IF EZEAID IS ENTER;
           IF EZEAID-ENTER
             GO TO EZECONDLBL-37
           END-IF
           GO TO EZECONDLBL-38
           CONTINUE.
       EZECONDLBL-37.
000181*          /* yousry  2002-06-25  mod
000182*       IF MPASSWRD = '\';
           IF EZEI-773 OF EZEM-1 = " "
             GO TO EZECONDLBL-39
           END-IF
           GO TO EZECONDLBL-40
           CONTINUE.
       EZECONDLBL-39.
000183*         MOVE '       PF9 ' TO EZEMSG;
           MOVE "       9FP " TO EZEMSG OF
            EZEM-1
           SET EZEHAST-OUTPUT-SELECTED
             OF EZEMSG OF EZEIMAP-EZEM-1 TO TRUE
000184*         SET NPASSWRD CURSOR;
           MOVE "NPASSWRD"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-774 OF EZEIMAP-EZEM-1 TO TRUE
000185*         EZERTN();
           GO TO EZE-EZEP-1-X
           CONTINUE.
       EZECONDLBL-40.
000186*       END;
000187*        /* yousry  2002-06-25  mod
000188*       MOVE CSM01.MPASSWRD TO LOGON_CHK1.OPPSWD;
           MOVE EZEI-773 OF EZEM-1 TO EZEI-98 OF EZER-4
000189*       MOVE ' ' TO LOGON_CHK1.NPPSWD;
           MOVE " " TO EZEI-99 OF EZER-4
000190*       MOVE CSM01.MNAME TO LOGON_CHK1.OPNAME;
           MOVE EZEI-772 OF EZEM-1 TO EZEI-97 OF EZER-4
000191*       MOVE 0 TO LOGON_STATUS;
           MOVE 0 TO EZEI-100 OF EZER-4
           CALL "ELAFXNUM" USING EZEI-100 OF EZER-4(LENGTH OF EZEI-100
            OF EZER-4:1)
000192*       CALL LGONCHK LOGON_CHK1 (NOMAPS,NONCSP;
           MOVE "CSA01P1" TO EZERTS-PRC-NAME
           MOVE 192 TO EZERTS-PRC-NUM
           CALL "ELAASADR" USING EZER-4
                                 EZECOMMAREA-PTR(1)
           CALL "ELAASSGN" USING CONTENT "A"
                CONTENT LENGTH OF EZECOMMAREA-PTR(1)
                REFERENCE EZECOMMAREA-PTR(1)
           MOVE HIGH-VALUES TO EZECOMMAREA(5:4)
           MOVE "LGONCHK" TO EZEPROGM
           SET EZERTS-DYNAMIC-LINK TO TRUE
           SET EZERTS-LINK-COMMPTR TO TRUE
           EXEC CICS LINK
             PROGRAM(EZEPROGM)
             COMMAREA(EZECOMMAREA)
             LENGTH(4)
           END-EXEC
           IF EIBRESP NOT = DFHRESP(NORMAL)
             MOVE 9031 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
                                   EZEPROGM
           END-IF
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
000193*       IF LOGON_STATUS = 1;
           IF EZEI-100 OF EZER-4 = 1
             GO TO EZECONDLBL-41
           END-IF
           GO TO EZECONDLBL-42
           CONTINUE.
       EZECONDLBL-41.
000194*         MOVE LOGON_CHK1.USRID TO W-COM-AREA.USER-ID;
           MOVE EZEI-101 OF EZER-4 TO EZEI-55 OF EZER-3
000195*         MOVE LOGON_CHK1.USRID TO SAM_REC.USER-ID;
           MOVE EZEI-101 OF EZER-4 TO EZEI-9 OF EZER-2
000196*         MOVE LOGON_CHK1.USRID TO SAM_REC.SAM-USER-ID;
           MOVE EZEI-101 OF EZER-4 TO EZEI-5 OF EZER-2
           CALL "ELAFXNUM" USING EZEI-5 OF EZER-2(LENGTH OF EZEI-5 OF
            EZER-2:1)
000197* /*        MOVE 'A' TO RASHA_REC.INVCD;
000198* /*        MOVE 04 TO RASHA_REC.INVLEN;
000199* /*        MOVE SAM_REC.USER-ID TO RASHA_REC.VAR4;
000200* /*        MOVE RASHA_REC.VAR4 TO RASHA_REC.INVDATA;
000201* /*        MOVE RASHA_REC.INVREC TO RASHA_REC.INVAREA;
000202* /*        CALL IDSCIVRS RASHA_REC.INVAREA (NOMAPS,NONCSP;
000203* /*        MOVE RASHA_REC.INVAREA TO RASHA_REC.INVREC;
000204* /*        MOVE RASHA_REC.INVDATA TO TST.OPERATORNUM2;
000205* /*        TST_PROC();
000206*       /* COM_AREA1();
000207*       /* DXFR NSA02 RCGW99;        /* W-COM-AREA;
000208*         MOVE '0' TO SAM_REC.SAM-FILLER1;
           MOVE ZEROS TO EZEI-4 OF EZER-2
000209*         MOVE '0' TO SAM_REC.SAM-FILLER2;
           MOVE ZEROS TO EZEI-6 OF EZER-2
000210*         XFER NMIN SAM_REC;
           MOVE "CSA01P1" TO EZERTS-PRC-NAME
           MOVE 210 TO EZERTS-PRC-NUM
           MOVE "NMIN" TO EZERTS-TGT-TRANSACTION
           CALL "ELAASADR" USING
                           EZER-2
                           EZERTS-DXFR-XFER-REC-PTR
           MOVE LENGTH OF EZER-2
             TO EZERTS-DXFR-XFER-REC-LEN
           SET EZERTS-XFER TO TRUE
           SET EZERTS-XFER-MAP-PTR TO NULL
           MOVE SPACES TO EZERTS-MAP-GROUP
           MOVE SPACES TO EZERTS-HELP-MAP-GROUP
           GO TO EZETERMINATE
000211*       ELSE;
           GO TO EZECONDLBL-43
           CONTINUE.
       EZECONDLBL-42.
000212*         IF LOGON_STATUS = 2;
           IF EZEI-100 OF EZER-4 = 2
             GO TO EZECONDLBL-44
           END-IF
           GO TO EZECONDLBL-45
           CONTINUE.
       EZECONDLBL-44.
000213*           EZEMNO = 1;
           MOVE 1 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000214*           SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000215*           EZERTN();
           GO TO EZE-EZEP-1-X
000216*         ELSE;
           GO TO EZECONDLBL-46
           CONTINUE.
       EZECONDLBL-45.
000217*           IF LOGON_STATUS = 3;
           IF EZEI-100 OF EZER-4 = 3
             GO TO EZECONDLBL-47
           END-IF
           GO TO EZECONDLBL-48
           CONTINUE.
       EZECONDLBL-47.
000218*             EZEMNO = 2;
           MOVE 2 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000219*             SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000220*             EZERTN();
           GO TO EZE-EZEP-1-X
000221*           ELSE;
           GO TO EZECONDLBL-49
           CONTINUE.
       EZECONDLBL-48.
000222*             IF LOGON_STATUS = 4;
           IF EZEI-100 OF EZER-4 = 4
             GO TO EZECONDLBL-50
           END-IF
           GO TO EZECONDLBL-51
           CONTINUE.
       EZECONDLBL-50.
000223*               EZEMNO = 322;
           MOVE 322 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000224*               SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000225*               EZERTN();
           GO TO EZE-EZEP-1-X
000226*             ELSE;
           GO TO EZECONDLBL-52
           CONTINUE.
       EZECONDLBL-51.
000227*               IF LOGON_STATUS = 5;
           IF EZEI-100 OF EZER-4 = 5
             GO TO EZECONDLBL-53
           END-IF
           GO TO EZECONDLBL-54
           CONTINUE.
       EZECONDLBL-53.
000228*                 EZEMNO = 323;
           MOVE 323 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000229*                 SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000230*                 EZERTN();
           GO TO EZE-EZEP-1-X
000231*               ELSE;
           GO TO EZECONDLBL-55
           CONTINUE.
       EZECONDLBL-54.
000232*                 IF LOGON_STATUS = 6;
           IF EZEI-100 OF EZER-4 = 6
             GO TO EZECONDLBL-56
           END-IF
           GO TO EZECONDLBL-57
           CONTINUE.
       EZECONDLBL-56.
000233*                   EZEMNO = 36;
           MOVE 36 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000234*                   SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000235*                   EZERTN();
           GO TO EZE-EZEP-1-X
000236*                 ELSE;
           GO TO EZECONDLBL-58
           CONTINUE.
       EZECONDLBL-57.
000237*                   IF LOGON_STATUS = 7;
           IF EZEI-100 OF EZER-4 = 7
             GO TO EZECONDLBL-59
           END-IF
           GO TO EZECONDLBL-60
           CONTINUE.
       EZECONDLBL-59.
000238*                     EZEMNO = 395;
           MOVE 395 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000239*                     SET CSM01.MNAME CURSOR,BRIGHT;
           MOVE "MNAME"
             TO EZEMAP-CURSOR-OUT-NAME OF EZEM-1
           MOVE 1 TO EZEMAP-CURSOR-OUT-OCC OF EZEM-1
           SET EZEHAST-NO-ASTERISK-FILL
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-USE-ATTR
               OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNMODIFIED
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-ALPHAMERIC
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-SET-UNPROTECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
           SET EZEHAST-BRIGHT-DETECT
             OF EZEI-772 OF EZEIMAP-EZEM-1 TO TRUE
000240*                     EZERTN();
           GO TO EZE-EZEP-1-X
           CONTINUE.
       EZECONDLBL-60.
000241*                   END;
           CONTINUE.
       EZECONDLBL-58.
000242*                 END;
           CONTINUE.
       EZECONDLBL-55.
000243*               END;
           CONTINUE.
       EZECONDLBL-52.
000244*             END;
           CONTINUE.
       EZECONDLBL-49.
000245*           END;
           CONTINUE.
       EZECONDLBL-46.
000246*         END;
           CONTINUE.
       EZECONDLBL-43.
000247*       END;
000248*     ELSE;
           GO TO EZECONDLBL-61
           CONTINUE.
       EZECONDLBL-38.
000249*       EZEMNO = 11;
           MOVE 11 TO EZEMNO
           SET EZEMNO-APP-MSG-FILE TO TRUE
000250*       EZERTN();
           GO TO EZE-EZEP-1-X
           CONTINUE.
       EZECONDLBL-61.
000251*     END;
           CONTINUE.
       EZECONDLBL-36.
000252*   END;
           CONTINUE.
       EZECONDLBL-6.
000253* END;
           CONTINUE.
       EZE-EZEP-1-X.
           GO TO EZETRACEBACK.
      *-----------------------------------------------------------------
      * INPUT / OUTPUT ROUTINE FOR PROCESS CSA01P1
      *-----------------------------------------------------------------
      * PROCESS OPTION      : CONVERSE
      * PROCESS OBJECT      : CSM01
      *-----------------------------------------------------------------
       EZECONV-EZEP-1 SECTION.
           MOVE "CSA01P1" TO EZERTS-PRC-NAME
           MOVE "CONVERSE" TO EZERTS-PRC-OPT
           MOVE "CSM01" TO EZERTS-PRC-OBJ
           SET EZERTS-NO-ERROR-ROUTINE TO TRUE
           SET EZERTS-CONVERSE TO TRUE
           MOVE EZERTS-FORMAT-OUTPUT TO EZEMSR-FUNCTION-ID
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZEMSR-REQUEST-BLOCK
                                 EZEMP-EZEM-1
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
           MOVE "CSM01" TO EZECTL-OUTBOUND-MAP
           PERFORM EZEINCR-TRACEBACK-PTR
           MOVE 5 TO EZETRACEBACK-ENTRY(EZETRACEBACK-PTR)
           PERFORM EZESAVE-WS
           GO TO EZETERMINATE.
       EZELABEL-5.
           IF NOT EZERTS-IN-HELP-ACTIVE
             MOVE EZERTS-REBUILD-MAP TO EZEMSR-FUNCTION-ID
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZEMSR-REQUEST-BLOCK
                                   EZEMP-EZEM-1
             IF EZERTS-TERMINATE
                GO TO EZETERMINATE
             END-IF
           END-IF
           MOVE "CSM01" TO EZECTL-OUTBOUND-MAP
           PERFORM EZEHANDLE-MSRREAD-REQUEST
           IF EZEMSR-FUNCTION-ID = EZERTS-FORMAT-INPUT
             SET EZEMAP-EDIT-NOT-DONE OF EZEM-1 TO TRUE
           END-IF
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZEMSR-REQUEST-BLOCK
                                 EZEMP-EZEM-1
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
           EVALUATE TRUE
             WHEN EZEAID-CLEAR
               MOVE EZERTS-SSM-IN-STATUS TO EZERTS-SSM-STATUS
               MOVE EZECTL-INBOUND-MAP TO EZECTL-OUTBOUND-MAP
               GO TO EZETERMINATE
             WHEN EZEAID-HELP
               PERFORM EZEHANDLE-HELP-REQUEST
             WHEN EZERTS-IN-HELP-ACTIVE
               PERFORM EZEHANDLE-RESEND-DATA-MAP
             WHEN OTHER
               IF EZEAID-NO-BYPASS
                 IF EZEMNO-ERROR OR EZEMNO-RE-CONVERSE
                   MOVE EZERTS-REBUILD-MAP TO EZEMSR-FUNCTION-ID
                   CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                         EZEMSR-REQUEST-BLOCK
                                         EZEMP-EZEM-1
                   IF EZERTS-TERMINATE
                     GO TO EZETERMINATE
                   END-IF
                   PERFORM EZEINCR-TRACEBACK-PTR
                   MOVE 5 TO
                     EZETRACEBACK-ENTRY(EZETRACEBACK-PTR)
                   PERFORM EZESAVE-WS
                   GO TO EZETERMINATE
                 ELSE
                   SET EZEMAP-NOT-SET-CLEARED OF EZEM-1
                     TO TRUE
                   SET EZERTS-EXECUTE TO TRUE
                   SET EZEMAP-EDIT-DONE OF EZEM-1
                     TO TRUE
                 END-IF
               ELSE
                 SET EZERTS-EXECUTE TO TRUE
                 SET EZEMAP-EDIT-DONE OF EZEM-1
                   TO TRUE
               END-IF
           END-EVALUATE
           SET EZESEGM-DEFINED TO TRUE
           SET EZERTS-CTL-REQUEST-NULL TO TRUE
           SET EZERTS-CONTEXT-NOT-SAVED TO TRUE
           CONTINUE.
       EZECONV-EZEP-1-X.
           GO TO EZETRACEBACK.

      *-----------------------------------------------------------------
      * SET EMPTY FOR RECORD EZER-4
      *-----------------------------------------------------------------
       EZESETEMP-EZER-4 SECTION.
           INITIALIZE EZE-INIT-EZER-4  REPLACING
                                       NUMERIC      BY ZERO
                                       ALPHABETIC   BY SPACE
                                       DBCS         BY SPACE
                                       ALPHANUMERIC BY LOW-VALUE
           CONTINUE.
       EZESETEMP-EZER-4-X.
           EXIT.

      *-----------------------------------------------------------------
      * SET EMPTY FOR RECORD EZER-5
      *-----------------------------------------------------------------
       EZESETEMP-EZER-5 SECTION.
           INITIALIZE EZE-INIT-EZER-5  REPLACING
                                       NUMERIC      BY ZERO
                                       ALPHABETIC   BY SPACE
                                       DBCS         BY SPACE
                                       ALPHANUMERIC BY LOW-VALUE
           CONTINUE.
       EZESETEMP-EZER-5-X.
           EXIT.

      *-----------------------------------------------------------------
      * SET EMPTY FOR RECORD EZER-2
      *-----------------------------------------------------------------
       EZESETEMP-EZER-2 SECTION.
           INITIALIZE EZE-INIT-EZER-2  REPLACING
                                       NUMERIC      BY ZERO
                                       ALPHABETIC   BY SPACE
                                       DBCS         BY SPACE
                                       ALPHANUMERIC BY LOW-VALUE
           CONTINUE.
       EZESETEMP-EZER-2-X.
           EXIT.

      *-----------------------------------------------------------------
      * SET EMPTY FOR RECORD EZER-3
      *-----------------------------------------------------------------
       EZESETEMP-EZER-3 SECTION.
           INITIALIZE EZE-INIT-EZER-3  REPLACING
                                       NUMERIC      BY ZERO
                                       ALPHABETIC   BY SPACE
                                       DBCS         BY SPACE
                                       ALPHANUMERIC BY LOW-VALUE
           CONTINUE.
       EZESETEMP-EZER-3-X.
           EXIT.

      *-----------------------------------------------------------------
      * TERMINATION LOGIC
      *-----------------------------------------------------------------
       EZETERMINATE SECTION.
           SET EZECTL-IN-EZETERMINATE TO TRUE.
           MOVE "EZETERMINATE" TO EZERTS-PRC-NAME
           IF ADDRESS OF EZETRACEBACK-TABLE NOT = NULL
             SET EZERTS-MEM-LOCATION TO ADDRESS OF EZETRACEBACK-TABLE
             MOVE EZETRACEBACK-HANDLE TO EZERTS-MEM-HANDLE
             PERFORM EZEFREE-TRACEBACK
             SET ADDRESS OF EZETRACEBACK-TABLE TO NULL
           END-IF
           IF NOT EZERTS-TERMINATE
             SET EZERTS-TERMINATE TO TRUE
           END-IF
           PERFORM EZERESRC-TABLE-CLEANUP
           PERFORM EZERESRC-CLEANUP
           GO TO EZERUN-PROCESSES-X.

      *-----------------------------------------------------------------
      * GET THE CURRENT DATE
      *-----------------------------------------------------------------
       EZEGET-EZEDTE SECTION.
           MOVE ZERO TO EZEDTE-VALUE
           EXEC CICS ASKTIME
             ABSTIME(EZEDTW-TIME)
           END-EXEC
           EXEC CICS FORMATTIME
             ABSTIME (EZEDTW-TIME)
             YYMMDD (EZEDTE)
           END-EXEC
           MOVE EZEIOE-SYSGREGRN-LONG TO EZEDTELC
           MOVE EZEDTE-YYYY
             TO EZEDTELC(EZEIOE-SYSGREGRL-YYYY-OFF:4)
           MOVE EZEDTE-MM TO EZEDTELC(EZEIOE-SYSGREGRL-MM-OFF:2)
           MOVE EZEDTE-DD TO EZEDTELC(EZEIOE-SYSGREGRL-DD-OFF:2)
           CONTINUE.
       EZEGET-EZEDTE-X.
           EXIT.

      *-----------------------------------------------------------------
      * GET THE CURRENT TIME
      *-----------------------------------------------------------------
       EZEGET-EZETIM SECTION.
           EXEC CICS ASKTIME
             ABSTIME (EZEDTW-TIME)
           END-EXEC
           EXEC CICS FORMATTIME
             ABSTIME (EZEDTW-TIME)
             TIMESEP (":")
             TIME (EZETIM)
           END-EXEC.
       EZEGET-EZETIM-X.
           EXIT.

      *-----------------------------------------------------------------
      * ARITHMETIC OVERFLOW ROUTINE
      *-----------------------------------------------------------------
       EZEOVER-ROUTINE SECTION.
           MOVE 1 TO EZEOVERS
           IF EZEOVER-TERMINATE
             MOVE 0009 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
             GO TO EZETERMINATE
           END-IF.
       EZEOVER-ROUTINE-X.
           EXIT.

      *-----------------------------------------------------------------
      * MAXIMUM VALUE OVERFLOW ROUTINE
      *-----------------------------------------------------------------
       EZEOVER-MAX-VALUE-ROUTINE SECTION.
           MOVE 1 TO EZEOVERS
           IF EZEOVER-DEFAULT
             OR EZEOVER-TERMINATE
             MOVE 0026 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
             GO TO EZETERMINATE
           END-IF.
       EZEOVER-MAX-VALUE-ROUTINE-X.
           EXIT.

      *-----------------------------------------------------------------
      * ISSUE ERROR MESSAGE FOR MODULE NOT FOUND
      *-----------------------------------------------------------------
       EZECALL-ERROR SECTION.
           MOVE 0031 TO EZERTS-ERROR-NUM
           CALL "ELARSVCS" USING  EZERTS-CONTROL-BLOCK
                                       EZERTS-ERROR-REQUEST-BLOCK
                                       EZEPROGM.
       EZECALL-ERROR-EXIT.
           EXIT.

      *-----------------------------------------------------------------
      * TRACEBACK
      *-----------------------------------------------------------------
       EZETRACEBACK SECTION.
           SUBTRACT +1 FROM EZETRACEBACK-PTR
           GO TO
             EZELABEL-1
             EZELABEL-2
             EZELABEL-3
             EZELABEL-4
             EZELABEL-5
               DEPENDING ON EZETRACEBACK-ENTRY(EZETRACEBACK-PTR + 1).
       EZETRACEBACK-X.
           EXIT.

      *-----------------------------------------------------------------
      * INITIALIZE TRACEBACK
      *-----------------------------------------------------------------
       EZEINIT-TRACEBACK SECTION.
           MOVE EZEAPP-TBK-STACK-SIZE TO EZERTS-MEM-BYTES
           MOVE "ELATRACE" TO EZERTS-MEM-HANDLE
           PERFORM EZEALLOC-TRACEBACK.
       EZEINIT-TRACEBACK-X.
           EXIT.

      *-----------------------------------------------------------------
      * INCREMENT TRACEBACK POINTER
      *-----------------------------------------------------------------
       EZEINCR-TRACEBACK-PTR SECTION.
           IF EZETRACEBACK-PTR < EZETRACEBACK-MAX-ENTRIES
             ADD +1 TO EZETRACEBACK-PTR
           ELSE
             SET ADDRESS OF EZETBKSAVE-TABLE
               TO ADDRESS OF EZETRACEBACK-TABLE
             MOVE "ELATSAVE" TO EZETBKSAVE-ID
             MOVE "ELATRACE" TO EZERTS-MEM-HANDLE
             COMPUTE EZERTS-MEM-BYTES = EZETRACEBACK-TABLE-LL * 2
               ON SIZE ERROR
                 MOVE EZERTS-ERROR TO EZERTS-ERROR-SVCS-NUM
                 MOVE "Y" TO EZERTS-ERR-RTN-SWITCH
                 MOVE 0166 TO EZERTS-ERROR-NUM
                 CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                       EZERTS-ERROR-REQUEST-BLOCK
                 GO TO EZETERMINATE
             END-COMPUTE
             PERFORM EZEALLOC-TRACEBACK
             MOVE EZETBKSAVE-PTR TO EZETRACEBACK-PTR
             MOVE EZETBKSAVE-ENTRIES TO EZETRACEBACK-ENTRIES
             SET EZERTS-MEM-LOCATION TO ADDRESS OF EZETBKSAVE-TABLE
             MOVE EZETBKSAVE-HANDLE TO EZERTS-MEM-HANDLE
             PERFORM EZEFREE-TRACEBACK
             SET ADDRESS OF EZETBKSAVE-TABLE TO NULL
             ADD +1 TO EZETRACEBACK-PTR
           END-IF.
       EZEINCR-TRACEBACK-PTR-X.
           EXIT.

      *-----------------------------------------------------------------
      * ALLOCATE TRACEBACK POINTER
      *-----------------------------------------------------------------
       EZEALLOC-TRACEBACK SECTION.
           MOVE EZERTS-GETMEM TO EZERTS-MEM-SVCS-NUM
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-MEM-REQUEST-BLOCK
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
           SET ADDRESS OF EZETRACEBACK-TABLE
             TO EZERTS-MEM-LOCATION
           COMPUTE EZETRACEBACK-TABLE-LL = EZERTS-MEM-BYTES - 8
           MOVE EZERTS-MEM-HANDLE TO EZETRACEBACK-HANDLE
           COMPUTE EZETRACEBACK-MAX-ENTRIES =
                  (EZERTS-MEM-BYTES - LENGTH OF EZETRACEBACK-HEADER) /
                  LENGTH OF EZETRACEBACK-ENTRY(1)
           MOVE ZERO TO EZETRACEBACK-PTR
           MOVE EZETRACEBACK-TABLE-LL TO EZERTS-SS-TRACEBACK-SIZE.
       EZEALLOC-TRACEBACK-X.
           EXIT.

      *-----------------------------------------------------------------
      * RELEASE THE TRACEBACK TABLE
      *-----------------------------------------------------------------
       EZEFREE-TRACEBACK SECTION.
           MOVE EZERTS-FREEMEM TO EZERTS-MEM-SVCS-NUM
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-MEM-REQUEST-BLOCK.
           IF EZERTS-TERMINATE AND EZECTL-NOT-IN-EZETERMINATE
              GO TO EZETERMINATE
           END-IF.
       EZEFREE-TRACEBACK-X.
           EXIT.

      *-----------------------------------------------------------------
      * ESTABLISH TABLE ADDRESSABILITY
      *-----------------------------------------------------------------
       EZETBL-GET-ADDRESS SECTION.
           SET ADDRESS OF EZERTB-CONTROL
             TO EZELTB-RTBPTR(EZETBL-INDEX)
           IF EZELTB-NOT-REFERENCED(EZETBL-INDEX)
             IF EZERTB-USE-COUNT = ZERO
               MOVE EZERTS-LOAD-TABLE TO EZERTS-SVCS-NUM
               CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                     EZERTS-REQUEST-BLOCK
                                     EZELTB-CONTROL(EZETBL-INDEX)
               IF EZERTS-TERMINATE
                 GO TO EZETERMINATE
               END-IF
             END-IF
             COMPUTE EZERTB-USE-COUNT = EZERTB-USE-COUNT + 1
             SET EZELTB-REFERENCED(EZETBL-INDEX) TO TRUE
           END-IF
           IF EZETBL-INDEX = 1
             SET ADDRESS OF EZETBL-NESGENU TO EZERTB-TCBPTR
             SET EZETBL-NESGENU-ACCESSED TO TRUE
             GO TO EZETBL-GET-ADDRESS-X
           END-IF
           CONTINUE.
       EZETBL-GET-ADDRESS-X.
           EXIT.

      *-----------------------------------------------------------------
      * SAVE WORKING STORAGE
      *-----------------------------------------------------------------
       EZESAVE-WS SECTION.
           IF EZETBK-MIN-ENTRIES <  EZETRACEBACK-MAX-ENTRIES
             COMPUTE EZETBK-UNUSED-ENTRIES
                   = EZETRACEBACK-MAX-ENTRIES
                   - EZETRACEBACK-PTR
             IF EZETBK-UNUSED-ENTRIES > EZETBK-MIN-ENTRIES
               COMPUTE EZETRACEBACK-MAX-ENTRIES
                     = EZETRACEBACK-PTR
                     + EZETBK-MIN-ENTRIES
               COMPUTE EZETRACEBACK-TABLE-LL
                     = (EZETRACEBACK-MAX-ENTRIES *
                        LENGTH OF EZETRACEBACK-ENTRY(1))
                     + LENGTH OF EZETRACEBACK-HEADER
             END-IF
           END-IF
           MOVE EZETRACEBACK-TABLE-LL TO EZERTS-SS-TRACEBACK-SIZE
           MOVE EZEAPP-MAP-GROUP TO EZERTS-MAP-GROUP
           MOVE EZEAPP-HELP-MAP-GROUP TO EZERTS-HELP-MAP-GROUP
           MOVE EZEAPP-HELP-PF-KEY TO EZERTS-HELP-PF-KEY
           MOVE EZEAPP-PF1-12-IS-PF13-24 TO EZERTS-PF-KEYS-EQUATED-SW
           MOVE EZESEGTR TO EZERTS-TGT-TRANSACTION
           MOVE EZERTS-SSM-SAVE-WS TO EZERTS-SSM-SVCS-NUM
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-SSM-REQUEST-BLOCK
                                 EZEWORDS
                                 EZETRACEBACK-TABLE
                                 EZE77-ITEMS
                                 EZEMP-EZEM-1
                                 EZEWS-EZER-4-GP
                                 EZEWS-EZER-5-GP
                                 EZEWS-EZER-1-GP
                                 EZEWS-EZER-2-GP
                                 EZEWS-EZER-3-GP
           SET EZERTS-CONTEXT-SAVED TO TRUE.
       EZESAVE-WS-X.
           EXIT.

      *-----------------------------------------------------------------
      * RESTORE WORKING STORAGE
      *-----------------------------------------------------------------
       EZERESTORE-WS SECTION.
           COMPUTE EZERTS-MEM-BYTES
                 = EZERTS-SS-TRACEBACK-SIZE + 8
           MOVE "TRACEBK1" TO EZERTS-MEM-HANDLE
           PERFORM EZEALLOC-TRACEBACK
           MOVE EZERTS-SSM-RESTORE-WS TO EZERTS-SSM-SVCS-NUM
           SET EZERTS-SSM-NO-TRUNCATE TO TRUE
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-SSM-REQUEST-BLOCK
                                 EZEWORDS
                                 EZETRACEBACK-TABLE
                                 EZE77-ITEMS
                                 EZEMP-EZEM-1
                                 EZEWS-EZER-4-GP
                                 EZEWS-EZER-5-GP
                                 EZEWS-EZER-1-GP
                                 EZEWS-EZER-2-GP
                                 EZEWS-EZER-3-GP
           MOVE "TRACEBK1" TO EZETRACEBACK-HANDLE
           PERFORM EZEREFRESH-STORAGE.
       EZERESTORE-WS-X.
           EXIT.

      *-----------------------------------------------------------------
      * SET THE READ REQUEST TYPE
      *-----------------------------------------------------------------
       EZEHANDLE-MSRREAD-REQUEST SECTION.
           EVALUATE TRUE
             WHEN EZERTS-IN-OUTPUT-MSG-SAVED
               MOVE EZERTS-READ-ELAM01 TO EZEMSR-FUNCTION-ID
             WHEN EZERTS-IN-HELP-ACTIVE
               MOVE EZERTS-READ-HELP TO EZEMSR-FUNCTION-ID
             WHEN OTHER
               MOVE EZERTS-FORMAT-INPUT TO EZEMSR-FUNCTION-ID
           END-EVALUATE.
       EZEHANDLE-MSRREAD-REQUEST-X.
           EXIT.

      *-----------------------------------------------------------------
      * SEND THE DATA MAP AGAIN FOLLOWING A HELP OR ERROR MAP
      *-----------------------------------------------------------------
       EZEHANDLE-RESEND-DATA-MAP SECTION.
           MOVE EZESEGTR TO EZERTS-TGT-TRANSACTION
           MOVE EZERTS-SSM-RESTORE-MSG TO EZERTS-SSM-SVCS-NUM
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-SSM-REQUEST-BLOCK
                                 EZEMAP-IO-AREA
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
           MOVE EZERTS-RESEND-MAP TO EZEMSR-FUNCTION-ID
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZEMSR-REQUEST-BLOCK
           IF EZERTS-TERMINATE
             GO TO  EZETERMINATE
           END-IF
           IF EZERTS-IN-OUTPUT-MSG-SAVED
             PERFORM EZESAVE-WS
           END-IF
           IF EZERTS-IN-OUTPUT-MSG-SAVED
             SET EZERTS-ERR-REDISPLAY TO TRUE
             SET EZERTS-IN-OUTPUT-MSG-NOT-SAVED TO TRUE
             SET EZERTS-OUTPUT-MSG-NOT-SAVED TO TRUE
           ELSE
             SET EZERTS-RESTORE-AFTER-HELP TO TRUE
             SET EZERTS-HELP-REDISPLAY TO TRUE
             SET EZERTS-IN-HELP-INACTIVE TO TRUE
             SET EZERTS-HELP-INACTIVE TO TRUE
           END-IF
           PERFORM EZESET-OUTBOUND-SSM
           GO TO EZETERMINATE.
       EZEHANDLE-RESEND-DATA-MAP-X.
           EXIT.

      *-----------------------------------------------------------------
      * HANDLE A HELP REQUEST
      *-----------------------------------------------------------------
       EZEHANDLE-HELP-REQUEST SECTION.
           IF EZERTS-IN-HELP-INACTIVE
             PERFORM EZESAVE-MSG-AREA
           END-IF
           MOVE EZEAPP-CURRENT-HELP-MAP TO EZECTL-OUTBOUND-MAP
           SET EZERTS-HELP-ACTIVE TO TRUE
           SET EZERTS-HELP-MAP TO TRUE
           MOVE EZERTS-SEND-HELP TO EZEMSR-FUNCTION-ID
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZEMSR-REQUEST-BLOCK
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF
           PERFORM EZESET-OUTBOUND-SSM
           GO TO EZETERMINATE.
       EZEHANDLE-HELP-REQUEST-X.
           EXIT.

      *-----------------------------------------------------------------
      * RESET SEGMENTATION FLAGS FOR A SEGMENT
      *-----------------------------------------------------------------
       EZESET-OUTBOUND-SSM SECTION.
           IF EZERTS-IN-CONTEXT-SAVED
             SET EZERTS-CONTEXT-SAVED TO TRUE
           END-IF
           IF EZERTS-IN-WSR-SAVED
             SET EZERTS-WSR-SAVED TO TRUE
             CALL "ELAASADR" USING EZER-3
                                   EZERTS-DXFR-XFER-REC-PTR
             MOVE LENGTH OF EZER-3
               TO EZERTS-DXFR-XFER-REC-LEN
           ELSE
             SET EZERTS-DXFR-XFER-REC-PTR TO NULL
             MOVE ZERO TO EZERTS-DXFR-XFER-REC-LEN
           END-IF
           IF EZERTS-IN-XFER-MAP-SAVED
             SET EZERTS-XFER-MAP-SAVED TO TRUE
           END-IF.
       EZESET-OUTBOUND-SSM-X.
           EXIT.

      *-----------------------------------------------------------------
      * SAVE MESSAGE AREA (EXTERNAL MAP FORMAT)
      *-----------------------------------------------------------------
       EZESAVE-MSG-AREA SECTION.
           MOVE EZESEGTR TO EZERTS-TGT-TRANSACTION
           MOVE EZERTS-SSM-SAVE-MSG TO EZERTS-SSM-SVCS-NUM
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-SSM-REQUEST-BLOCK
                                 EZEMAP-IO-AREA
           IF EZERTS-TERMINATE
             GO TO EZETERMINATE
           END-IF.
       EZESAVE-MSG-AREA-X.
           EXIT.

      *-----------------------------------------------------------------
      * MAIN APPLICATION CONTROL LOGIC
      *-----------------------------------------------------------------
       EZECONTROL SECTION.
           MOVE EIBFN TO EZEAPP-ENTRY-FUNCTION
           EXEC CICS IGNORE CONDITION ERROR
           END-EXEC
           EXEC CICS HANDLE ABEND
             PROGRAM("ELAESABD")
           END-EXEC
           CALL "ELARSTWA" USING DFHEIBLK
                                 DFHCOMMAREA
                                 EZEAPP-PROFILE
           SET ADDRESS OF EZERTS-CONTROL-BLOCK
             TO EZEAPP-RTS-PTR
           IF NOT EZERTS-SET
             SET EZERTS-IN-NO-INPUT-YET TO TRUE
             PERFORM EZEAPPL-IDENTIFY
             SET EZERTS-INIT-PROFILE TO EZERTS-CURR-PROFILE
           ELSE
             SET EZEAPP-CALLER-PROFILE TO NULL
             PERFORM EZEAPPL-IDENTIFY
           END-IF
           IF NOT EZERTS-TERMINATE-ON-ERROR
             PERFORM EZEEXTERNAL-INITIALIZATION
           END-IF
           IF NOT EZERTS-TERMINATE-ON-ERROR
             PERFORM EZEAPPLC-SEGMENT-PROCESSING
           END-IF
           IF NOT EZERTS-TERMINATE-ON-ERROR
             PERFORM EZETRANSFER-OF-CONTROL
           END-IF
           IF EZERTS-TERMINATE-ON-ERROR
             PERFORM EZEREPORT-ERRS-ON-TERMINATN
           END-IF
           PERFORM EZECICS-RTS-TERMINATE
           EXEC CICS RETURN
           END-EXEC
           EXEC CICS ABEND
             ABCODE("ELAA")
           END-EXEC.
       EZECONTROL-X.
           EXIT.

      *-----------------------------------------------------------------
      * IDENTIFY APPLICATION
      *-----------------------------------------------------------------
       EZEAPPL-IDENTIFY SECTION.
           IF NOT EZERTS-SET
             MOVE SPACES TO EZELTERM
             MOVE ALL "*" TO EZEUSR EZEUSRID
           END-IF
           SET EZEAPP-CURS-BLK-PTR TO NULL
           SET EZEAPP-LAST-MAPBUF-PTR TO NULL
           SET EZEAPP-FIRST-MAPBUF-PTR TO NULL
           SET EZEAPP-ROWS-USED-PTR TO NULL
           SET EZEAPP-MAPG-MOD-PTR TO NULL
           SET EZEAPP-HELPG-MOD-PTR TO NULL
           SET EZERTS-IDENTIFY-APPL TO TRUE
           MOVE "EZEINITIALIZE" TO EZERTS-PRC-NAME
           MOVE EZEAPP-APPL-NAME TO EZERTS-PGM-NAME
           CALL "ELAASADR" USING EZEWORDS
                                 EZEAPP-EZE-WORDS-PTR
           CALL "ELAASADR" USING EZEWS-EZER-3-GP
                                 EZEAPP-WSR-PTR
           CALL "ELAASADR" USING EZETBL-ARRAY
                                 EZEAPP-LTB-ARRAY-ADDRESS
           CALL "ELAASADR" USING EZEAPP-PROFILE
                                 EZERTS-CURR-PROFILE
           CONTINUE.
       EZEAPPL-IDENTIFY-X.
           EXIT.

      *-----------------------------------------------------------------
      * ISSUE ERROR MESSAGE FOR MSP MODULE NOT FOUND
      *-----------------------------------------------------------------
       EZEMSP-PGM-LOAD-ERROR SECTION.
           EXEC CICS POP HANDLE
           END-EXEC
           IF NOT EZERTS-SET
             SET EZERTS-INIT-PROFILE TO EZERTS-CURR-PROFILE
             CALL "ELARSINT" USING EZERTS-CONTROL-BLOCK
           END-IF
           IF EZEMSR-CLOSE-ALL-PRT-MAPS
             IF NOT EZERTS-TERMINATE
               MOVE 9046 TO EZERTS-ERROR-NUM
               CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                     EZERTS-ERROR-REQUEST-BLOCK
                                     EZERTS-FIRST-PRINT-MSP
               MOVE SPACES TO EZERTS-FIRST-PRINT-MSP
             END-IF
             GO TO EZEPRINT-MAP-CLEANUP-X
           ELSE
             IF NOT EZERTS-TERMINATE
               MOVE 46 TO EZERTS-ERROR-NUM
               CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                     EZERTS-ERROR-REQUEST-BLOCK
             END-IF
             GO TO EZEAPPL-IDENTIFY-X
           END-IF
           CONTINUE.
       EZEMSP-PGM-LOAD-ERROR-X.
           EXIT.

      *-----------------------------------------------------------------
      * INITIALIZE STORAGE
      *-----------------------------------------------------------------
       EZEINITIALIZE-STORAGE SECTION.
           MOVE SPACES TO EZEWORDS-I
           MOVE ZERO TO EZECNVCM
           MOVE ZERO TO EZEFEC EZERCODE EZEREPLY
           MOVE ZERO TO EZEDLERR EZESQISL EZEOVER EZEOVERS
           MOVE SPACES TO EZEDLPSB
           IF EZESEGTR = LOW-VALUES
             MOVE EIBTRNID TO EZESEGTR
           END-IF
           IF EZEAPP-CALLER-PROFILE = NULL
             MOVE EIBTRMID TO EZELTERM EZEUSR
           END-IF
           SET EZESEGM-DEFINED TO TRUE
           MOVE SPACES TO EZEDESTP
           MOVE "N" TO EZEAPP-EZEDESTP-DIFF
           MOVE "N" TO EZEAPP-EZEDESTP-CHANGED
           INITIALIZE EZE77-ITEMS-GROUP
           PERFORM EZESETEMP-EZER-4
           PERFORM EZESETEMP-EZER-5
           PERFORM EZESETEMP-EZER-2
           PERFORM EZESETEMP-EZER-3
           CONTINUE.
       EZEINITIALIZE-STORAGE-X.
           EXIT.

      *-----------------------------------------------------------------
      * REFRESH STORAGE - ONLY THESE FIELDS ARE RESET ACROSS CONVERSE
      *-----------------------------------------------------------------
       EZEREFRESH-STORAGE SECTION.
           MOVE ZERO TO EZEDLCER EZEDLCON
           MOVE ZERO TO EZEREPLY
           MOVE ZERO TO EZETST EZEDLLEV EZEDLSSG
           MOVE ZERO TO EZEMNO EZEDLKYL
           MOVE SPACES TO EZEDLSTC EZEDLDBD EZEDLKEY EZEDLPRO EZEDLSEG
           MOVE SPACES TO EZEMSG OF EZEWORDS
           MOVE "N" TO EZEMNO-MSG-FILE-SW
           MOVE LOW-VALUES TO EZESQLCA
           MOVE "SQLCA" TO EZESQNAM
           MOVE +136 TO EZESQABC
           MOVE ZERO TO EZESQCOD EZESQRRL
           MOVE ZERO TO EZESQRD1 EZESQRD2 EZESQRD3
           MOVE ZERO TO EZESQRD4 EZESQRD5 EZESQRD6
           IF NOT EZERTS-SET OR NOT EZERTS-TERMINATE
             MOVE SPACES TO EZERTS-DXFR-APPLID
             SET EZERTS-XFER-MAP-PTR TO NULL
             SET EZERTS-DXFR-XFER-REC-PTR TO NULL
             MOVE ZERO TO EZERTS-DXFR-XFER-REC-LEN
           END-IF.
       EZEREFRESH-STORAGE-X.
           EXIT.

      *-----------------------------------------------------------------
      * RTS INITIALIZATION
      *-----------------------------------------------------------------
       EZEEXTERNAL-INITIALIZATION SECTION.
           COMPUTE EZEMAP-LENGTH OF EZEMP-EZEM-1
                 = LENGTH OF EZEMP-EZEM-1 - 8
           CALL "ELAASADR" USING EZEMAP-IO-AREA
                                       EZEMSR-EXT-DATA-PTR
           CALL "ELARSINT" USING EZERTS-CONTROL-BLOCK
           IF EZERTS-TERMINATE-ON-ERROR
             GO TO EZEEXTERNAL-INITIALIZATION-X
           END-IF
           SET ADDRESS OF EZEIOP-NLS-INDEP-CTL-BLOCK TO
            EZERTS-IOP-TABLE-PTR
           SET ADDRESS OF EZEIOE-NLS-DEP-CTL-BLOCK TO
            EZERTS-DOP-TABLE-PTR
           MOVE EZERTS-ALLOC-RTB TO EZERTS-SVCS-NUM
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                       EZERTS-REQUEST-BLOCK
                                       EZETBL-ARRAY
           IF NOT EZERTS-TERMINATE
             SET EZERTS-EXECUTE TO TRUE
           END-IF.
       EZEEXTERNAL-INITIALIZATION-X.
           EXIT.

      *-----------------------------------------------------------------
      * INITIALIZE THE MAPS
      *-----------------------------------------------------------------
       EZEINITIALIZE-MAPS SECTION.
           MOVE EZERTS-SET-MAP-CLEAR TO EZEMSR-FUNCTION-ID
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                           EZEMSR-REQUEST-BLOCK
                                           EZEMP-EZEM-1
           IF EZERTS-TERMINATE-ON-ERROR
             GO TO EZEINITIALIZE-MAPS-X
           END-IF
           CONTINUE.
       EZEINITIALIZE-MAPS-X.
           EXIT.

      *-----------------------------------------------------------------
      * MOVE TO THE PRIMARY WORKING STORAGE RECORD
      *-----------------------------------------------------------------
       EZEMOVE-TO-PWSR SECTION.
           IF EZERTS-IN-WSR-REC-LEN >
              LENGTH OF EZER-3 OF EZEWS-EZER-3-GP
             MOVE LENGTH OF EZER-3 OF EZEWS-EZER-3-GP
               TO EZERTS-IN-WSR-REC-LEN
           END-IF
           SET ADDRESS OF EZECTL-IO-AREA TO EZERTS-IN-WSR-REC-PTR
           MOVE EZECTL-IO-AREA(1:EZERTS-IN-WSR-REC-LEN) TO EZER-3
             OF EZEWS-EZER-3-GP(1:EZERTS-IN-WSR-REC-LEN).
       EZEMOVE-TO-PWSR-X.
           EXIT.

      *-----------------------------------------------------------------
      * LOCATE SEGMENTATION CONTROL RECORD AND STATUS
      *-----------------------------------------------------------------
       EZELOCATE-SSM-CONTROL-RECORD SECTION.
           IF EZERTS-IN-TERMINAL-DISPLAY AND EIBCALEN >= 10
             AND EZECOMMAREA-MAP-NAME NOT = LOW-VALUES
             MOVE EZECOMMAREA-SSM-STATUS TO EZERTS-SSM-STATUS-BYTE
           ELSE
             MOVE LOW-VALUES TO EZERTS-SSM-STATUS-BYTE
           END-IF
           IF EZERTS-SSM-INVALID
             GO TO EZELOCATE-SSM-CONTROL-RECORD-X
           END-IF
           SET EZECTL-SSM-SUB TO 1
           SEARCH EZECTL-SSM-STATUS-ENTRY VARYING EZECTL-SSM-SUB
             AT END
               GO TO EZELOCATE-SSM-CONTROL-RECORD-X
             WHEN EZERTS-SSM-STATUS-BYTE =
                  EZECTL-SSM-STATUS-ENTRY (EZECTL-SSM-SUB)
               SET EZECTL-SSM-STATUS-N TO EZECTL-SSM-SUB
           END-SEARCH
           SUBTRACT 1 FROM EZECTL-SSM-STATUS-N
           SET EZERTS-SSM-INVALID TO TRUE
           IF EZECTL-SSM-STATUS-N >= 16
             SET EZERTS-IN-CONTEXT-SAVED TO TRUE
             SUBTRACT 16 FROM EZECTL-SSM-STATUS-N
           END-IF
           IF EZECTL-SSM-STATUS-N >= 8
             SET EZERTS-IN-OUTPUT-MSG-SAVED TO TRUE
             SUBTRACT 8 FROM EZECTL-SSM-STATUS-N
           END-IF
           IF EZECTL-SSM-STATUS-N >= 4
             SET EZERTS-IN-HELP-ACTIVE TO TRUE
             SUBTRACT 4 FROM EZECTL-SSM-STATUS-N
           END-IF
           IF EZECTL-SSM-STATUS-N >= 2
             SET EZERTS-IN-WSR-SAVED TO TRUE
             SUBTRACT 2 FROM EZECTL-SSM-STATUS-N
           END-IF
           IF EZECTL-SSM-STATUS-N >= 1
             SET EZERTS-IN-XFER-MAP-SAVED TO TRUE
           END-IF
           IF EZERTS-IN-CONTEXT-SAVED OR
              EZERTS-IN-XFER-MAP-SAVED OR
              EZERTS-IN-WSR-SAVED OR
              EZERTS-IN-HELP-ACTIVE OR
              EZERTS-IN-OUTPUT-MSG-SAVED
             MOVE EZERTS-SSM-LOCATE TO EZERTS-SSM-SVCS-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-SSM-REQUEST-BLOCK
             IF EZERTS-TERMINATE-ON-ERROR
               GO TO EZELOCATE-SSM-CONTROL-RECORD-X
             END-IF
           END-IF
           IF NOT (EZERTS-IN-CONTEXT-SAVED OR
                   EZERTS-IN-HELP-ACTIVE OR
                   EZERTS-IN-OUTPUT-MSG-SAVED)
             MOVE 9135 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
                                   EZECOMMAREA-MAP-NAME
           END-IF.
       EZELOCATE-SSM-CONTROL-RECORD-X.
           EXIT.

      *-----------------------------------------------------------------
      * ESTABLISH THE SEGMENTATION STATUS FLAG
      *-----------------------------------------------------------------
       EZEESTABLISH-SSM-STATUS SECTION.
           MOVE 1 TO EZECTL-SSM-STATUS-N
           IF EZERTS-XFER-MAP-SAVED
             ADD 1 TO EZECTL-SSM-STATUS-N
           END-IF
           IF EZERTS-WSR-SAVED
             ADD 2 TO EZECTL-SSM-STATUS-N
           END-IF
           IF EZERTS-HELP-ACTIVE
             ADD 4 TO EZECTL-SSM-STATUS-N
           END-IF
           IF EZERTS-OUTPUT-MSG-SAVED
             ADD 8 TO EZECTL-SSM-STATUS-N
           END-IF
           IF EZERTS-CONTEXT-SAVED
             ADD 16 TO EZECTL-SSM-STATUS-N
           END-IF
           SET EZECTL-SSM-SUB TO EZECTL-SSM-STATUS-N
           MOVE EZECTL-SSM-STATUS-ENTRY(EZECTL-SSM-SUB)
             TO EZERTS-SSM-STATUS-BYTE
           MOVE EZERTS-SSM-STATUS-BYTE TO EZECOMMAREA-SSM-STATUS
           MOVE EZECTL-OUTBOUND-MAP TO EZECOMMAREA-MAP-NAME
           CONTINUE.
       EZEESTABLISH-SSM-STATUS-X.
           EXIT.

      *-----------------------------------------------------------------
      * RECEIVE PARAMETERS THAT WERE PASSED
      *-----------------------------------------------------------------
       EZEGATHER-INPUT SECTION.
           SET EZERTS-CTL-REQUEST-NULL TO TRUE
           MOVE SPACES TO EZERTS-FIRST-PRINT-MSP
           MOVE EZEAPP-APPL-NAME TO EZERTS-SSM-APPL-NAME
           MOVE ALL "N" TO EZERTS-SSM-IN-STATUS
           PERFORM EZELOCATE-SSM-CONTROL-RECORD
           IF EZERTS-TERMINATE-ON-ERROR
             GO TO EZEGATHER-INPUT-X
           END-IF
           IF EZERTS-IN-TERMINAL-DISPLAY AND EIBCALEN >= 10
             MOVE EZECOMMAREA-MAP-NAME TO EZECTL-INBOUND-MAP
           END-IF
           IF EZERTS-IN-HELP-ACTIVE
             MOVE EZECOMMAREA-MAP-NAME TO EZEAPP-CURRENT-HELP-MAP
           END-IF
           MOVE ALL "N" TO EZERTS-SSM-STATUS
           MOVE SPACES TO EZERTS-EZEMSG
           MOVE EZERTS-TRANSACTION TO EZERTS-TGT-TRANSACTION
           IF NOT EZERTS-TERMINATE-ON-ERROR
              AND EZERTS-IN-CONTEXT-NOT-SAVED
               PERFORM EZEINITIALIZE-STORAGE
               PERFORM EZEINITIALIZE-MAPS
           ELSE
             MOVE EIBTRNID TO EZESEGTR
           END-IF
           IF EIBCALEN = 0
             IF EZERTS-IN-START-WITH-DATA
               OR EZERTS-IN-START-NO-DATA
               PERFORM EZERETRIEVE-SD-DATA
               IF EZERTS-TERMINATE-ON-ERROR
                 GO TO EZEGATHER-INPUT-X
               END-IF
             ELSE
               SET EZERTS-IN-WSR-REC-PTR TO NULL
               MOVE 0 TO EZERTS-IN-WSR-REC-LEN
             END-IF
           ELSE
             IF EZERTS-IN-TERMINAL-DISPLAY
               IF EIBCALEN > 10
                 SET EZERTS-IN-WSR-REC-PTR
                  TO ADDRESS OF DFHCOMMAREA(11:1)
                 COMPUTE EZERTS-IN-WSR-REC-LEN
                         = EIBCALEN - 10
               ELSE
                 SET EZERTS-IN-WSR-REC-PTR TO NULL
                 MOVE 0 TO EZERTS-IN-WSR-REC-LEN
               END-IF
             ELSE
               SET EZERTS-IN-WSR-REC-PTR
                TO ADDRESS OF DFHCOMMAREA
               COMPUTE EZERTS-IN-WSR-REC-LEN = EIBCALEN
             END-IF
           END-IF
           IF EZERTS-IN-WSR-REC-PTR NOT = NULL
             PERFORM EZEMOVE-TO-PWSR
           END-IF
           IF EIBCALEN < 351
             EXEC CICS GETMAIN
               SET(EZEPOINTER-PTR)
               FLENGTH(351)
               INITIMG(EZEWRK-INIT-STORAGE)
             END-EXEC
             SET ADDRESS OF DFHCOMMAREA TO EZEPOINTER-PTR
             IF EIBRESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND
                 ABCODE("ELA7")
               END-EXEC
             END-IF
           END-IF
           IF EZERTS-IN-CONTEXT-SAVED
             PERFORM EZERESTORE-WS
           ELSE
             PERFORM EZEREFRESH-STORAGE
           END-IF
           CONTINUE.
       EZEGATHER-INPUT-X.
           EXIT.

      *-----------------------------------------------------------------
      * RETRIEVE RECORD DATA THAT WAS PASSED ON AN XFER
      *-----------------------------------------------------------------
       EZERETRIEVE-SD-DATA SECTION.
           EXEC CICS RETRIEVE
             SET(EZERTS-IN-WSR-REC-PTR)
             LENGTH(EZECICS-TMP-2BYTE-COMP)
             RTERMID(EZECHRWK8)
           END-EXEC
           MOVE EZECICS-TMP-2BYTE-COMP TO EZERTS-IN-WSR-REC-LEN
           IF EIBRESP NOT = DFHRESP(NORMAL)
             AND EIBRESP NOT = DFHRESP(ENDDATA)
             AND EIBRESP NOT = DFHRESP(ENVDEFERR)
             MOVE 231 TO EZERTS-ERROR-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-ERROR-REQUEST-BLOCK
             GO TO EZERETRIEVE-SD-DATA-X
           END-IF
           IF EIBRESP = DFHRESP(NORMAL)
             AND EZECHRWK8(1:4) NOT = LOW-VALUES
             MOVE EZECHRWK8 TO EZEDESTP
             MOVE EZECHRWK8 TO EZERTS-INIT-EZEDESTP
           END-IF
           IF EZERTS-IN-WSR-REC-LEN > 0
             SET EZERTS-IN-WSR-SAVED TO TRUE
           END-IF.
       EZERETRIEVE-SD-DATA-X.
           EXIT.

      *-----------------------------------------------------------------
      * PROCESS THE INPUT
      *-----------------------------------------------------------------
       EZEPROCESS-INPUT-MESSAGE SECTION.
           MOVE EZEAPP-APPL-NAME TO EZERTS-SSM-APPL-NAME
           PERFORM EZEPERFORM-APPLICATION.
       EZEPROCESS-INPUT-MESSAGE-X SECTION.
           EXIT.

      *-----------------------------------------------------------------
      * APPLICATION SEGMENT PROCESSING
      *-----------------------------------------------------------------
       EZEAPPLC-SEGMENT-PROCESSING SECTION.
           IF NOT EZERTS-TERMINATE-ON-ERROR
             PERFORM EZEGATHER-INPUT
           END-IF
           IF NOT EZERTS-TERMINATE-ON-ERROR
             PERFORM EZEPROCESS-INPUT-MESSAGE
           END-IF
           IF NOT EZERTS-TERMINATE-ON-ERROR
             PERFORM EZEDISPATCH-OUTPUT
           END-IF.
       EZEAPPLC-SEGMENT-PROCESSING-X.
           EXIT.

      *-----------------------------------------------------------------
      * PERFORM APPLICATION
      *-----------------------------------------------------------------
       EZEPERFORM-APPLICATION SECTION.
           PERFORM EZERUN-PROCESSES
           IF EZERTS-EZECLOS AND NOT EZERTS-TERMINATE-ON-ERROR
             PERFORM EZESETUP-RETURN-TRANS
           END-IF
           CONTINUE.
       EZEPERFORM-APPLICATION-X.
           EXIT.

      *-----------------------------------------------------------------
      * RUN APPLICATION PROCESSES
      *-----------------------------------------------------------------
       EZERUN-PROCESSES SECTION.
           SET EZECTL-NOT-IN-EZETERMINATE TO TRUE
           SET EZERTS-XFER-MAP-NOT-SAVED TO TRUE
           GO TO EZEBEGIN-PROCESSES.
       EZERUN-PROCESSES-X.
           EXIT.

      *-----------------------------------------------------------------
      * DISPATCH THE OUTPUT
      *-----------------------------------------------------------------
       EZEDISPATCH-OUTPUT SECTION.
           IF EZERTS-XFER
             AND EZERTS-DXFR-XFER-REC-LEN > 0
             MOVE LOW-VALUES TO DFHCOMMAREA(1:351)
             SET ADDRESS OF EZECTL-IO-AREA
              TO EZERTS-DXFR-XFER-REC-PTR
             MOVE EZECTL-IO-DATA(1:EZERTS-DXFR-XFER-REC-LEN)
               TO EZECOMMAREA(1:EZERTS-DXFR-XFER-REC-LEN)
           ELSE
             IF EZERTS-EZECLOS AND EZERTS-TERMINAL-ATTACHED
               PERFORM EZESEND-EZECLOSE-MESSAGE
             ELSE
               PERFORM EZEESTABLISH-SSM-STATUS
             END-IF
           END-IF.
       EZEDISPATCH-OUTPUT-X.
           EXIT.

      *-----------------------------------------------------------------
      * SET UP FOR THE RETURN TRANSACTION
      *-----------------------------------------------------------------
       EZESETUP-RETURN-TRANS SECTION.
           IF EZERTS-OPTION-RT NOT = LOW-VALUES AND
               EZERTS-OPTION-RT NOT = SPACES
             MOVE ZERO TO EZERTS-DXFR-XFER-REC-LEN
             SET EZERTS-DXFR-XFER-REC-PTR TO NULL
             SET EZERTS-XFER TO TRUE
             MOVE EZERTS-OPTION-RT TO EZERTS-TGT-TRANSACTION
             MOVE 0 TO EZERTS-PRC-NUM
           END-IF.
       EZESETUP-RETURN-TRANS-X.
           EXIT.

      *-----------------------------------------------------------------
      * CLEAR THE SCREEN
      *-----------------------------------------------------------------
       EZESEND-EZECLOSE-MESSAGE SECTION.
           SET EZERTS-FMR-SEND-ERASE TO TRUE
           MOVE EZERTS-DISPLAY-SVC-SEND TO EZERTS-FMR-SVCS-NUM
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-FMR-REQUEST-BLOCK
                                 EZEDUMMY-ARGUMENT
                                 EZEDUMMY-ARGUMENT
                                 EZEDUMMY-ARGUMENT.
       EZESEND-EZECLOSE-MESSAGE-X.
           EXIT.

      *-----------------------------------------------------------------
      * REPORT THE ERRORS
      *-----------------------------------------------------------------
       EZEREPORT-ERRS-ON-TERMINATN SECTION.
           IF NOT EZERTS-ROLLBACK-TAKEN
             SET EZERTS-ROLLBACK-TAKEN TO TRUE
             MOVE EZERTS-ROLLBACK TO EZERTS-SVCS-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-REQUEST-BLOCK
           END-IF
           IF EZERTS-TERMINAL-ATTACHED
             MOVE EZERTS-DISPLAY-ERROR-MAP TO EZERTS-SVCS-NUM
             CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                   EZERTS-REQUEST-BLOCK
           END-IF
           MOVE 693 TO EZECTL-RETURN-CODE.
       EZEREPORT-ERRS-ON-TERMINATN-X.
           EXIT.

      *-----------------------------------------------------------------
      * TRANSFER OF CONTROL TO ANOTHER PROGRAM
      *-----------------------------------------------------------------
       EZETRANSFER-OF-CONTROL SECTION.
           IF EZERTS-CONVERSE OR
              EZERTS-HELP-MAP OR
              EZERTS-HELP-REDISPLAY OR
              EZERTS-1ST-MAP-EDIT-ERROR OR
              EZERTS-ERR-REDISPLAY
             EXEC CICS INQUIRE TRANSACTION(EZESEGTR)
               STATUS(EZEBINWK)
             END-EXEC
             IF (EIBRESP NOT = DFHRESP(NORMAL))
               OR (EZEBINWK NOT = DFHVALUE(ENABLED))
               MOVE 9179 TO EZERTS-ERROR-NUM
               CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                     EZERTS-ERROR-REQUEST-BLOCK
                                     EZESEGTR
               GO TO EZETRANSFER-OF-CONTROL-X
             END-IF
             PERFORM EZECICS-RTS-TERMINATE
             EXEC CICS RETURN TRANSID(EZESEGTR)
               COMMAREA(DFHCOMMAREA)
               LENGTH(10)
             END-EXEC
             EXEC CICS ABEND
               ABCODE("ELAA")
             END-EXEC
           END-IF
           IF EZERTS-XFER
             IF EZERTS-DXFR-XFER-REC-LEN > 0
               MOVE EZERTS-DXFR-XFER-REC-LEN
                 TO EZECICS-TMP-2BYTE-COMP
               EXEC CICS START TRANSID(EZERTS-TGT-TRANSACTION)
                 FROM(DFHCOMMAREA)
                 LENGTH(EZECICS-TMP-2BYTE-COMP)
                 TERMID(EIBTRMID)
               END-EXEC
               IF EIBRESP NOT = DFHRESP(NORMAL)
                 MOVE 8179 TO EZERTS-ERROR-NUM
                 CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                       EZERTS-ERROR-REQUEST-BLOCK
                                       EZERTS-TGT-TRANSACTION
               END-IF
             ELSE
               EXEC CICS START TRANSID(EZERTS-TGT-TRANSACTION)
                 TERMID(EIBTRMID)
               END-EXEC
               IF EIBRESP NOT = DFHRESP(NORMAL)
                 IF EZERTS-PRC-NUM = 0
                   MOVE 9179 TO EZERTS-ERROR-NUM
                 ELSE
                   MOVE 8179 TO EZERTS-ERROR-NUM
                 END-IF
                 CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                       EZERTS-ERROR-REQUEST-BLOCK
                                       EZERTS-TGT-TRANSACTION
               END-IF
             END-IF
           END-IF
           CONTINUE.
       EZETRANSFER-OF-CONTROL-X.
           EXIT.

      *-----------------------------------------------------------------
      * RESOURCE CLEAN-UP ROUTINE
      *-----------------------------------------------------------------
       EZERESRC-CLEANUP SECTION.
           SET EZECTL-IN-EZETERMINATE TO TRUE
           MOVE "EZERESRC-CLEAN" TO EZERTS-PRC-NAME
           MOVE EZERTS-FREE-MAP-STORAGE TO EZERTS-SVCS-NUM
           CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-REQUEST-BLOCK
           IF NOT EZERTS-TERMINATE
             SET EZERTS-TERMINATE TO TRUE
           END-IF
           IF EZERTS-RSCT-PTR NOT = NULL
             SET ADDRESS OF EZERSC-CONTROL-TABLE TO EZERTS-RSCT-PTR
             PERFORM VARYING EZECTL-INDEX FROM 1 BY 1
                     UNTIL EZECTL-INDEX > EZERSC-CNT
               IF EZERSC-OPEN(EZECTL-INDEX)
                 MOVE EZERSC-CALL-PARM-FORM(EZECTL-INDEX)
                   TO EZERTS-CALL-PARM-FORM
                 MOVE EZERSC-CALL-LINK-TYPE(EZECTL-INDEX)
                   TO EZERTS-CALL-LINK-TYPE
                 IF EZERTS-DYNAMIC-CALL
                   IF EZERTS-CALL-MSP
                     SET EZEMSR-TERMINATE TO TRUE
                     SET EZEMSR-RTS-CB-PTR
                       TO ADDRESS OF EZERTS-CONTROL-BLOCK
                     CALL EZERSC-APPL-ID(EZECTL-INDEX)
                       USING EZEMSR-REQUEST-BLOCK
                   ELSE
                     CALL EZERSC-APPL-ID(EZECTL-INDEX)
                       USING DFHEIBLK DFHCOMMAREA
                   END-IF
                 ELSE
                   EXEC CICS LINK
                     PROGRAM(EZERSC-APPL-ID(EZECTL-INDEX))
                   END-EXEC
                 END-IF
               END-IF
             END-PERFORM
             MOVE ZERO TO EZERSC-CNT
           END-IF
           CONTINUE.
       EZERESRC-CLEANUP-X.
           EXIT.

      *-----------------------------------------------------------------
      * RESOURCE CLEAN-UP / RELEASE ALL TABLES
      *-----------------------------------------------------------------
       EZERESRC-TABLE-CLEANUP SECTION.
           MOVE "EZERESRC-TBL-CLEAN" TO EZERTS-PRC-NAME
           PERFORM WITH TEST BEFORE
             VARYING EZETBL-INDEX FROM 1 BY 1
             UNTIL EZETBL-INDEX > EZETBL-NO-OF-TABLES
             SET ADDRESS OF EZERTB-CONTROL
               TO EZELTB-RTBPTR(EZETBL-INDEX)
             IF NOT (EZELTB-NOT-REFERENCED(EZETBL-INDEX))
               COMPUTE EZERTB-USE-COUNT = EZERTB-USE-COUNT - 1
             END-IF
             IF (EZERTB-USE-COUNT = ZERO AND
                 NOT (EZELTB-NOT-REFERENCED(EZETBL-INDEX)))
               MOVE EZERTS-RELS-TABLE TO EZERTS-SVCS-NUM
               CALL "ELARSVCS" USING EZERTS-CONTROL-BLOCK
                                     EZERTS-REQUEST-BLOCK
                                     EZELTB-CONTROL(EZETBL-INDEX)
               IF EZERTS-TERMINATE-ON-ERROR
                  AND EZECTL-NOT-IN-EZETERMINATE
                 GO TO EZERESRC-TABLE-CLEANUP-X
               END-IF
             END-IF
             SET EZELTB-NOT-REFERENCED(EZETBL-INDEX) TO TRUE
           END-PERFORM.
       EZERESRC-TABLE-CLEANUP-X.
           EXIT.

      *-----------------------------------------------------------------
      * RESOURCE CLEAN-UP / CLOSE ALL PRINT MAPS
      *-----------------------------------------------------------------
       EZEPRINT-MAP-CLEANUP SECTION.
           IF EZERTS-FIRST-PRINT-MSP NOT = SPACES AND
              EZERTS-FIRST-PRINT-MSP NOT = LOW-VALUES
             MOVE SPACES TO EZEMSR-DEST-NAME
                            EZEMSR-EZEDESTP-NAME
             SET EZEMSR-CLOSE-ALL-PRT-MAPS TO TRUE
             SET EZEMSR-RTS-CB-PTR TO EZEAPP-RTS-PTR
             EXEC CICS PUSH HANDLE
             END-EXEC
             EXEC CICS HANDLE CONDITION
               NOTAUTH(EZEMSP-PGM-LOAD-ERROR)
               PGMIDERR(EZEMSP-PGM-LOAD-ERROR)
             END-EXEC
             EXEC CICS HANDLE ABEND
               LABEL(EZEMSP-PGM-LOAD-ERROR)
             END-EXEC
             CALL EZERTS-FIRST-PRINT-MSP USING EZEMSR-REQUEST-BLOCK
                                               EZEDUMMY-ARGUMENT
               ON OVERFLOW
                  PERFORM EZEMSP-PGM-LOAD-ERROR
             END-CALL
             EXEC CICS POP HANDLE
             END-EXEC
             MOVE SPACES TO EZERTS-FIRST-PRINT-MSP
           END-IF.
       EZEPRINT-MAP-CLEANUP-X.
           EXIT.

       EZECICS-RTS-TERMINATE SECTION.
           CALL "ELAASTRM" USING EZERTS-CONTROL-BLOCK
                                 EZERTS-REQUEST-BLOCK
           EXEC CICS HANDLE ABEND CANCEL
           END-EXEC.
       EZECICS-RTS-TERMINATE-X.
           EXIT.

      *-----------------------------------------------------------------
      * END OF PROGRAM NSA01
      *-----------------------------------------------------------------
