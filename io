ROHITH*****************START OF IR 24090001***********************
ROHITH$SET ilsmartnest
000000$SET SQL(ALLOWNULLCHAR)
ROHITH*****************END   OF IR 24090001***********************
      ******************************************************************
      *   (C) COPYRIGHT FINANCIAL NETWORK SERVICES PTY LTD. 1982       *
      *       ALL RIGHTS RESERVED. NO PART OF THIS PROGRAM MAY         *
      *       BE PHOTOCOPIED, REPRODUCED, TRANSLATED TO ANOTHER        *
      *       PROGRAM LANGUAGE OR USED IN ANY WAY WITHOUT THE          *
      *       PRIOR WRITTEN CONSENT OF FINANCIAL NETWORK SERVICES      *
      *       OR THE AUTHORISED SELLING AGENT.                         *
      *----------------------------------------------------------------*
      *             FINANCIAL NETWORK SERVICES PTY LTD.                *
      *             UNIT 6, 70 ROSEHILL ST REDFERN 2016                *
      *                   SYDNEY, AUSTRALIA.                           *
      *                  TEL. (612) 9318-1088                          *
      ******************************************************************
ROHITH*-----------------------------------------------------------------
ROHITH* MALAPATI ROHITH :02/09/2024:24090001: TO MOVE SQLCODE 1403     *
ROHITH*                                       IN VISUAL COBOL ECLIPSE  *
ROHITH*                                       FOR CBS MICROSERVICES IN *
ROHITH*                                       PLACE OF OPENESQL ERROR  *
ROHITH*                                       100                      *
ROHITH*----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      IOCUSVCC.
       AUTHOR.          FNS.
       DATE-WRITTEN.    20240328.
       DATE-COMPILED.   XXXXXXXXXXX.
       SECURITY.
      *    SQL  MODULE FOR CUSVCC.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-PROGRAM-NAME              PIC X(12) VALUE  "IOCUSVCC:".
       01  W-PROGRAM-CALLED            PIC X(8)  VALUE  "UT9004".
       01  UT9004CA-CALL-AREA.
           COPY UT9004CA IN LIBRYMIS
                REPLACING ==(UT9004CA)== BY ==UT9004CA==.
       01  WORKING-VARIABLES.
RBBXXX     03  REMOTE-TABLE            PIC X(13) VALUE SPACES.
           03  OPEN-CURSOR-FLAG        PIC X     VALUE SPACE.
               88  OPEN-CURSOR-FAIL    VALUE "N".
           03  DATA-OUT-FLAG           PIC X     VALUE SPACE.
               88  DATA-OUT            VALUE "Y".
RBBDEL*    03  PASSED-VIEW-INDICATOR   PIC X     VALUE "N".
RBBDEL*        88  PASSED-VIEW         VALUE "Y".
RBBDEL*        88  DID-NOT-PASSED-VIEW VALUE "N".
RBBCSR     03  SAVE-MASTER-IN-USE      PIC 9     VALUE 0.
           03  SAVE-SYSTEM-MODE        PIC X     VALUE SPACE.
               88  DAY-MODE            VALUE "D".
               88  NIGHT-MODE          VALUE "N".
           03  WHICH-MASTER            PIC X     VALUE SPACE.
               88  USE-MASTER-1        VALUE "1".
               88  USE-MASTER-2        VALUE "2".
           03  SELECT-PK               PIC X(58) VALUE SPACES.

       01  IVAL-PK.
           03  IV-PK-KEY-1                   PIC X(023).

       01  WA-TRAIL-SPACES             PIC 9(4) VALUE 0.
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  MAX-IND                           PIC S9(4) COMP.
       01  SELECT-STR                        PIC X(2048) VALUES SPACES.

       01  D-PK.
           03  D-PK-KEY-1                    PIC X(023) .

       01  MAX-KEY                           PIC X(023) .

       01  D-CUSVCC.
           03  D-CODE                      PIC X(002) .
           03  D-DELI                      PIC X(001) .
           03  D-KEY-1                     PIC X(023) .
           03  D-NO-ASSOC                  PIC X(002) .
           03  D-STATUS-01                 PIC X(002) .
           03  D-RELAT-01                  PIC X(002) .
           03  D-CURRENCY-01               PIC X(003) .
           03  D-ACCT-NO-01                PIC S9(17) COMP-3.
           03  D-SYST-01                   PIC X(001) .
           03  D-ACCT-PER-OWN-01           PIC S9(03)V9(02) COMP-3.
           03  D-LINK-CUA-ACNO-01          PIC S9(17) COMP-3.
           03  D-LINK-SYST-01              PIC X(001) .
           03  D-LINK-BRCH-01              PIC S9(05) COMP.
           03  D-OPEN-DATE-01              PIC S9(09) COMP.
           03  D-MOD-CLS-DATE-01           PIC S9(09) COMP.
           03  D-LINK-TYPE-01              PIC X(001) .
           03  D-LINK-DEPTH-01             PIC X(001) .
           03  D-LINK-ACCT-TYPE-01         PIC S9(05) COMP.
           03  D-LINK-CUST-NO-01           PIC S9(17) COMP-3.
           03  D-STATUS-02                 PIC X(002) .
           03  D-RELAT-02                  PIC X(002) .
           03  D-CURRENCY-02               PIC X(003) .
           03  D-ACCT-NO-02                PIC S9(17) COMP-3.
           03  D-SYST-02                   PIC X(001) .
           03  D-ACCT-PER-OWN-02           PIC S9(03)V9(02) COMP-3.
           03  D-LINK-CUA-ACNO-02          PIC S9(17) COMP-3.
           03  D-LINK-SYST-02              PIC X(001) .
           03  D-LINK-BRCH-02              PIC S9(05) COMP.
           03  D-OPEN-DATE-02              PIC S9(09) COMP.
           03  D-MOD-CLS-DATE-02           PIC S9(09) COMP.
           03  D-LINK-TYPE-02              PIC X(001) .
           03  D-LINK-DEPTH-02             PIC X(001) .
           03  D-LINK-ACCT-TYPE-02         PIC S9(05) COMP.
           03  D-LINK-CUST-NO-02           PIC S9(17) COMP-3.
           03  D-STATUS-03                 PIC X(002) .
           03  D-RELAT-03                  PIC X(002) .
           03  D-CURRENCY-03               PIC X(003) .
           03  D-ACCT-NO-03                PIC S9(17) COMP-3.
           03  D-SYST-03                   PIC X(001) .
           03  D-ACCT-PER-OWN-03           PIC S9(03)V9(02) COMP-3.
           03  D-LINK-CUA-ACNO-03          PIC S9(17) COMP-3.
           03  D-LINK-SYST-03              PIC X(001) .
           03  D-LINK-BRCH-03              PIC S9(05) COMP.
           03  D-OPEN-DATE-03              PIC S9(09) COMP.
           03  D-MOD-CLS-DATE-03           PIC S9(09) COMP.
           03  D-LINK-TYPE-03              PIC X(001) .
           03  D-LINK-DEPTH-03             PIC X(001) .
           03  D-LINK-ACCT-TYPE-03         PIC S9(05) COMP.
           03  D-LINK-CUST-NO-03           PIC S9(17) COMP-3.
           03  D-STATUS-04                 PIC X(002) .
           03  D-RELAT-04                  PIC X(002) .
           03  D-CURRENCY-04               PIC X(003) .
           03  D-ACCT-NO-04                PIC S9(17) COMP-3.
           03  D-SYST-04                   PIC X(001) .
           03  D-ACCT-PER-OWN-04           PIC S9(03)V9(02) COMP-3.
           03  D-LINK-CUA-ACNO-04          PIC S9(17) COMP-3.
           03  D-LINK-SYST-04              PIC X(001) .
           03  D-LINK-BRCH-04              PIC S9(05) COMP.
           03  D-OPEN-DATE-04              PIC S9(09) COMP.
           03  D-MOD-CLS-DATE-04           PIC S9(09) COMP.
           03  D-LINK-TYPE-04              PIC X(001) .
           03  D-LINK-DEPTH-04             PIC X(001) .
           03  D-LINK-ACCT-TYPE-04         PIC S9(05) COMP.
           03  D-LINK-CUST-NO-04           PIC S9(17) COMP-3.
           03  D-STATUS-05                 PIC X(002) .
           03  D-RELAT-05                  PIC X(002) .
           03  D-CURRENCY-05               PIC X(003) .
           03  D-ACCT-NO-05                PIC S9(17) COMP-3.
           03  D-SYST-05                   PIC X(001) .
           03  D-ACCT-PER-OWN-05           PIC S9(03)V9(02) COMP-3.
           03  D-LINK-CUA-ACNO-05          PIC S9(17) COMP-3.
           03  D-LINK-SYST-05              PIC X(001) .
           03  D-LINK-BRCH-05              PIC S9(05) COMP.
           03  D-OPEN-DATE-05              PIC S9(09) COMP.
           03  D-MOD-CLS-DATE-05           PIC S9(09) COMP.
           03  D-LINK-TYPE-05              PIC X(001) .
           03  D-LINK-DEPTH-05             PIC X(001) .
           03  D-LINK-ACCT-TYPE-05         PIC S9(05) COMP.
           03  D-LINK-CUST-NO-05           PIC S9(17) COMP-3.
           03  D-STATUS-06                 PIC X(002) .
           03  D-RELAT-06                  PIC X(002) .
           03  D-CURRENCY-06               PIC X(003) .
           03  D-ACCT-NO-06                PIC S9(17) COMP-3.
           03  D-SYST-06                   PIC X(001) .
           03  D-ACCT-PER-OWN-06           PIC S9(03)V9(02) COMP-3.
           03  D-LINK-CUA-ACNO-06          PIC S9(17) COMP-3.
           03  D-LINK-SYST-06              PIC X(001) .
           03  D-LINK-BRCH-06              PIC S9(05) COMP.
           03  D-OPEN-DATE-06              PIC S9(09) COMP.
           03  D-MOD-CLS-DATE-06           PIC S9(09) COMP.
           03  D-LINK-TYPE-06              PIC X(001) .
           03  D-LINK-DEPTH-06             PIC X(001) .
           03  D-LINK-ACCT-TYPE-06         PIC S9(05) COMP.
           03  D-LINK-CUST-NO-06           PIC S9(17) COMP-3.
           03  D-STATUS-07                 PIC X(002) .
           03  D-RELAT-07                  PIC X(002) .
           03  D-CURRENCY-07               PIC X(003) .
           03  D-ACCT-NO-07                PIC S9(17) COMP-3.
           03  D-SYST-07                   PIC X(001) .
           03  D-ACCT-PER-OWN-07           PIC S9(03)V9(02) COMP-3.
           03  D-LINK-CUA-ACNO-07          PIC S9(17) COMP-3.
           03  D-LINK-SYST-07              PIC X(001) .
           03  D-LINK-BRCH-07              PIC S9(05) COMP.
           03  D-OPEN-DATE-07              PIC S9(09) COMP.
           03  D-MOD-CLS-DATE-07           PIC S9(09) COMP.
           03  D-LINK-TYPE-07              PIC X(001) .
           03  D-LINK-DEPTH-07             PIC X(001) .
           03  D-LINK-ACCT-TYPE-07         PIC S9(05) COMP.
           03  D-LINK-CUST-NO-07           PIC S9(17) COMP-3.
           03  D-STATUS-08                 PIC X(002) .
           03  D-RELAT-08                  PIC X(002) .
           03  D-CURRENCY-08               PIC X(003) .
           03  D-ACCT-NO-08                PIC S9(17) COMP-3.
           03  D-SYST-08                   PIC X(001) .
           03  D-ACCT-PER-OWN-08           PIC S9(03)V9(02) COMP-3.
           03  D-LINK-CUA-ACNO-08          PIC S9(17) COMP-3.
           03  D-LINK-SYST-08              PIC X(001) .
           03  D-LINK-BRCH-08              PIC S9(05) COMP.
           03  D-OPEN-DATE-08              PIC S9(09) COMP.
           03  D-MOD-CLS-DATE-08           PIC S9(09) COMP.
           03  D-LINK-TYPE-08              PIC X(001) .
           03  D-LINK-DEPTH-08             PIC X(001) .
           03  D-LINK-ACCT-TYPE-08         PIC S9(05) COMP.
           03  D-LINK-CUST-NO-08           PIC S9(17) COMP-3.

            EXEC SQL END DECLARE SECTION END-EXEC.

            EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01  IOMOD-FILE                  PIC X(14).
       01  IOMOD-KEY                   PIC X(100).
       01  IOMOD-RECORD.

           03  W-CODE                      PIC X(002).
           03  W-DELI                      PIC X(001).
           03  W-KEY-1                     PIC X(023).
           03  W-NO-ASSOC                  PIC X(002).
           03  W-STATUS-01                 PIC X(002).
           03  W-RELAT-01                  PIC X(002).
           03  W-CURRENCY-01               PIC X(003).
           03  W-ACCT-NO-01                PIC S9(17) COMP.
           03  W-SYST-01                   PIC X(001).
           03  W-ACCT-PER-OWN-01           PIC S9(03)V9(02) COMP.
           03  W-LINK-CUA-ACNO-01          PIC S9(17) COMP.
           03  W-LINK-SYST-01              PIC X(001).
           03  W-LINK-BRCH-01              PIC S9(05) COMP.
           03  W-OPEN-DATE-01              PIC S9(09) COMP.
           03  W-MOD-CLS-DATE-01           PIC S9(09) COMP.
           03  W-LINK-TYPE-01              PIC X(001).
           03  W-LINK-DEPTH-01             PIC X(001).
           03  W-LINK-ACCT-TYPE-01         PIC S9(05) COMP.
           03  W-LINK-CUST-NO-01           PIC S9(17) COMP.
           03  W-STATUS-02                 PIC X(002).
           03  W-RELAT-02                  PIC X(002).
           03  W-CURRENCY-02               PIC X(003).
           03  W-ACCT-NO-02                PIC S9(17) COMP.
           03  W-SYST-02                   PIC X(001).
           03  W-ACCT-PER-OWN-02           PIC S9(03)V9(02) COMP.
           03  W-LINK-CUA-ACNO-02          PIC S9(17) COMP.
           03  W-LINK-SYST-02              PIC X(001).
           03  W-LINK-BRCH-02              PIC S9(05) COMP.
           03  W-OPEN-DATE-02              PIC S9(09) COMP.
           03  W-MOD-CLS-DATE-02           PIC S9(09) COMP.
           03  W-LINK-TYPE-02              PIC X(001).
           03  W-LINK-DEPTH-02             PIC X(001).
           03  W-LINK-ACCT-TYPE-02         PIC S9(05) COMP.
           03  W-LINK-CUST-NO-02           PIC S9(17) COMP.
           03  W-STATUS-03                 PIC X(002).
           03  W-RELAT-03                  PIC X(002).
           03  W-CURRENCY-03               PIC X(003).
           03  W-ACCT-NO-03                PIC S9(17) COMP.
           03  W-SYST-03                   PIC X(001).
           03  W-ACCT-PER-OWN-03           PIC S9(03)V9(02) COMP.
           03  W-LINK-CUA-ACNO-03          PIC S9(17) COMP.
           03  W-LINK-SYST-03              PIC X(001).
           03  W-LINK-BRCH-03              PIC S9(05) COMP.
           03  W-OPEN-DATE-03              PIC S9(09) COMP.
           03  W-MOD-CLS-DATE-03           PIC S9(09) COMP.
           03  W-LINK-TYPE-03              PIC X(001).
           03  W-LINK-DEPTH-03             PIC X(001).
           03  W-LINK-ACCT-TYPE-03         PIC S9(05) COMP.
           03  W-LINK-CUST-NO-03           PIC S9(17) COMP.
           03  W-STATUS-04                 PIC X(002).
           03  W-RELAT-04                  PIC X(002).
           03  W-CURRENCY-04               PIC X(003).
           03  W-ACCT-NO-04                PIC S9(17) COMP.
           03  W-SYST-04                   PIC X(001).
           03  W-ACCT-PER-OWN-04           PIC S9(03)V9(02) COMP.
           03  W-LINK-CUA-ACNO-04          PIC S9(17) COMP.
           03  W-LINK-SYST-04              PIC X(001).
           03  W-LINK-BRCH-04              PIC S9(05) COMP.
           03  W-OPEN-DATE-04              PIC S9(09) COMP.
           03  W-MOD-CLS-DATE-04           PIC S9(09) COMP.
           03  W-LINK-TYPE-04              PIC X(001).
           03  W-LINK-DEPTH-04             PIC X(001).
           03  W-LINK-ACCT-TYPE-04         PIC S9(05) COMP.
           03  W-LINK-CUST-NO-04           PIC S9(17) COMP.
           03  W-STATUS-05                 PIC X(002).
           03  W-RELAT-05                  PIC X(002).
           03  W-CURRENCY-05               PIC X(003).
           03  W-ACCT-NO-05                PIC S9(17) COMP.
           03  W-SYST-05                   PIC X(001).
           03  W-ACCT-PER-OWN-05           PIC S9(03)V9(02) COMP.
           03  W-LINK-CUA-ACNO-05          PIC S9(17) COMP.
           03  W-LINK-SYST-05              PIC X(001).
           03  W-LINK-BRCH-05              PIC S9(05) COMP.
           03  W-OPEN-DATE-05              PIC S9(09) COMP.
           03  W-MOD-CLS-DATE-05           PIC S9(09) COMP.
           03  W-LINK-TYPE-05              PIC X(001).
           03  W-LINK-DEPTH-05             PIC X(001).
           03  W-LINK-ACCT-TYPE-05         PIC S9(05) COMP.
           03  W-LINK-CUST-NO-05           PIC S9(17) COMP.
           03  W-STATUS-06                 PIC X(002).
           03  W-RELAT-06                  PIC X(002).
           03  W-CURRENCY-06               PIC X(003).
           03  W-ACCT-NO-06                PIC S9(17) COMP.
           03  W-SYST-06                   PIC X(001).
           03  W-ACCT-PER-OWN-06           PIC S9(03)V9(02) COMP.
           03  W-LINK-CUA-ACNO-06          PIC S9(17) COMP.
           03  W-LINK-SYST-06              PIC X(001).
           03  W-LINK-BRCH-06              PIC S9(05) COMP.
           03  W-OPEN-DATE-06              PIC S9(09) COMP.
           03  W-MOD-CLS-DATE-06           PIC S9(09) COMP.
           03  W-LINK-TYPE-06              PIC X(001).
           03  W-LINK-DEPTH-06             PIC X(001).
           03  W-LINK-ACCT-TYPE-06         PIC S9(05) COMP.
           03  W-LINK-CUST-NO-06           PIC S9(17) COMP.
           03  W-STATUS-07                 PIC X(002).
           03  W-RELAT-07                  PIC X(002).
           03  W-CURRENCY-07               PIC X(003).
           03  W-ACCT-NO-07                PIC S9(17) COMP.
           03  W-SYST-07                   PIC X(001).
           03  W-ACCT-PER-OWN-07           PIC S9(03)V9(02) COMP.
           03  W-LINK-CUA-ACNO-07          PIC S9(17) COMP.
           03  W-LINK-SYST-07              PIC X(001).
           03  W-LINK-BRCH-07              PIC S9(05) COMP.
           03  W-OPEN-DATE-07              PIC S9(09) COMP.
           03  W-MOD-CLS-DATE-07           PIC S9(09) COMP.
           03  W-LINK-TYPE-07              PIC X(001).
           03  W-LINK-DEPTH-07             PIC X(001).
           03  W-LINK-ACCT-TYPE-07         PIC S9(05) COMP.
           03  W-LINK-CUST-NO-07           PIC S9(17) COMP.
           03  W-STATUS-08                 PIC X(002).
           03  W-RELAT-08                  PIC X(002).
           03  W-CURRENCY-08               PIC X(003).
           03  W-ACCT-NO-08                PIC S9(17) COMP.
           03  W-SYST-08                   PIC X(001).
           03  W-ACCT-PER-OWN-08           PIC S9(03)V9(02) COMP.
           03  W-LINK-CUA-ACNO-08          PIC S9(17) COMP.
           03  W-LINK-SYST-08              PIC X(001).
           03  W-LINK-BRCH-08              PIC S9(05) COMP.
           03  W-OPEN-DATE-08              PIC S9(09) COMP.
           03  W-MOD-CLS-DATE-08           PIC S9(09) COMP.
           03  W-LINK-TYPE-08              PIC X(001).
           03  W-LINK-DEPTH-08             PIC X(001).
           03  W-LINK-ACCT-TYPE-08         PIC S9(05) COMP.
           03  W-LINK-CUST-NO-08           PIC S9(17) COMP.
       01  IOMOD-FUNCTION              PIC X(0012).
           88  OPEN-CMD                VALUE "OPEN".
           88  CLOSE-CMD               VALUE "CLOSE".
           88  READ-CMD                VALUE "READ".
           88  READFIRST-CMD           VALUE "READFIRST".
           88  READNEXT-CMD            VALUE "READNEXT".
           88  WRITE-CMD               VALUE "WRITE".
           88  REWRITE-CMD             VALUE "REWRITE".
           88  DELETE-CMD              VALUE "DELETE".
           88  COMMIT-CMD              VALUE "COMMIT".
           88  ROLLBACK-CMD            VALUE "ROLLBACK".
           88  SETTXCON-CMD            VALUE "SETTXCON".
           88  RELTXCON-CMD            VALUE "RELTXCON".
           88  SELECTMAX-CMD           VALUE "SELECTMAX".
       01  IOMOD-KEY-NO                PIC 9(3).
       01  IOMOD-RELEASE-LOCK          PIC X.
       01  IOMOD-ACCESS-COMMAND        PIC X(4).
           88  ACCESS-CMD-BEGN         VALUE "BEGN".
       01  IOMOD-ACCESS-COND           PIC X(3).
           88  ACCESS-COND-KEY         VALUE "KEY".
           88  ACCESS-COND-PGT         VALUE "PGT".
           88  ACCESS-COND-PGE         VALUE "PGE".
           COPY IOORAST IN LIBRYMIS.

        PROCEDURE DIVISION USING IOMOD-FILE
                                 IOMOD-KEY
                                 IOMOD-RECORD
                                 IOMOD-FUNCTION
                                 IOMOD-KEY-NO
                                 IOMOD-RELEASE-LOCK
                                 IOMOD-ACCESS-COMMAND
                                 IOMOD-ACCESS-COND
                                 IOMOD-STATUS.

       A000-MAIN SECTION.
       A000-START.
           SET UT9004CA-CALL-FOR-GET TO TRUE.
           MOVE "UT9004" TO W-PROGRAM-CALLED.
           CALL W-PROGRAM-CALLED USING UT9004CA-CALL-AREA.
           IF NOT UT9004CA-RETURN-OK
              STRING "UT9004-RETURN-CODE="
                     UT9004CA-RETURN-CODE
                DELIMITED BY SIZE
                INTO IOMOD-FUNCTION
              END-STRING
           ELSE
RBBCSR        IF UT9004CA-SYSTEM-MODE   NOT = SAVE-SYSTEM-MODE
RBBCSR        OR UT9004CA-MASTER-IN-USE NOT = SAVE-MASTER-IN-USE
RBBCSR           MOVE UT9004CA-SYSTEM-MODE   TO SAVE-SYSTEM-MODE
RBBCSR           MOVE UT9004CA-MASTER-IN-USE TO SAVE-MASTER-IN-USE
RBBCSR*       IF UT9004CA-SYSTEM-MODE-IS-DAY
RBBCSR*          SET DAY-MODE TO TRUE
RBBCSR        IF DAY-MODE
           EXEC SQL
              DECLARE S_SEL01CUSVCC_PK
              CURSOR FOR
              SELECT /*+ INDEX_ASC(CUSVCC CUSVCCPK) */  *
              FROM CUSVCC
              WHERE (KEY_1  > :D-PK-KEY-1 )
                 AND ROWNUM < 2
              ORDER BY KEY_1  ASC
           END-EXEC

RBBCSR           CONTINUE
              ELSE
RBBCSR*          SET NIGHT-MODE TO TRUE
                 IF UT9004CA-MASTER-IN-USE-IS-1
                 SET USE-MASTER-1 TO TRUE
                 MOVE "1" TO WHICH-MASTER
                 MOVE "TMP_1_CUSVCC " TO REMOTE-TABLE
                 ELSE
                 MOVE "2" TO WHICH-MASTER
                 MOVE "TMP_2_CUSVCC " TO REMOTE-TABLE
                 END-IF
                 PERFORM B000-DECLARE
              END-IF
              PERFORM M300-DATA-LEN
RBBCSR        END-IF
           END-IF

           IF UT9004CA-TRACESTATE-ON
              DISPLAY "IOCUSVCC:"
                      "(A000) ENTRY - FUNCTION " IOMOD-FUNCTION
                      " KEY-NO " IOMOD-KEY-NO
                      " VALUE " IOMOD-KEY
           END-IF
           PERFORM Z500-INIT-STATUS
           MOVE IOMOD-FUNCTION TO IOMOD-F2-FUNCT
           MOVE "N" TO DATA-OUT-FLAG

           EVALUATE TRUE
             WHEN READ-CMD
               MOVE "Y" TO DATA-OUT-FLAG
               IF IOMOD-RELEASE-LOCK = "N"
                EVALUATE IOMOD-KEY-NO
                  WHEN 0
                    PERFORM M200-KI-PK
                    PERFORM M210-ID-PK
                    PERFORM D100-SEL-LOCK-PK
                  WHEN OTHER
                    MOVE "IK" TO IOMOD-F2-STAT
                END-EVALUATE
               ELSE
                EVALUATE IOMOD-KEY-NO
                  WHEN 0
                    PERFORM M200-KI-PK
                    PERFORM M210-ID-PK
                    PERFORM D110-SEL-NOLOCK-PK
                  WHEN OTHER
                    MOVE "IK" TO IOMOD-F2-STAT
                END-EVALUATE
               END-IF

             WHEN DELETE-CMD
               PERFORM M200-KI-PK
               PERFORM M210-ID-PK
               PERFORM D200-DELETE
               PERFORM F200-F2-SQLCODE

             WHEN WRITE-CMD
               PERFORM M200-KI-PK
               PERFORM M210-ID-PK
               PERFORM M000-DATA-IN
               PERFORM D300-INSERT

             WHEN REWRITE-CMD
               PERFORM M200-KI-PK
               PERFORM M210-ID-PK
               PERFORM M000-DATA-IN
               PERFORM D400-UPDATE
               PERFORM F200-F2-SQLCODE
               IF NIGHT-MODE AND IOMOD-F2-SQLCODE = 1403
                  PERFORM D300-INSERT
               END-IF

             WHEN READFIRST-CMD
               MOVE "Y" TO DATA-OUT-FLAG
                EVALUATE IOMOD-KEY-NO
                  WHEN 0
                      MOVE LOW-VALUES
                      TO D-PK-KEY-1
                      PERFORM  D000-PK
                  WHEN OTHER
                    MOVE "IK" TO IOMOD-F2-STAT
                END-EVALUATE

             WHEN READNEXT-CMD
               MOVE "Y" TO DATA-OUT-FLAG
                EVALUATE IOMOD-KEY-NO
                  WHEN 0
                      PERFORM  M200-KI-PK
                      PERFORM  M210-ID-PK
                      PERFORM  D000-PK
                  WHEN OTHER
                    MOVE "IK" TO IOMOD-F2-STAT
                END-EVALUATE

             WHEN SELECTMAX-CMD
               PERFORM M200-KI-PK
               PERFORM M210-ID-PK
               PERFORM D500-SELECT-MAX

             WHEN OTHER
               DISPLAY "IOCUSVCC:"
                "(A000) INVALID FUNCTION " IOMOD-FUNCTION
               MOVE -1 TO IOMOD-F1-SQLCODE
                          IOMOD-F2-SQLCODE
               MOVE "ER" TO IOMOD-F1-STAT
           END-EVALUATE

           IF UT9004CA-TRACESTATE-ON
              DISPLAY "IOCUSVCC:"
                      "(A000) AFTER - FUNCTION " IOMOD-FUNCTION
                      " KEY-NO " IOMOD-KEY-NO
                      " VALUE " IOMOD-KEY
              DISPLAY "IOCUSVCC:"
                      "(A000) AFTER - FUNCTION " IOMOD-FUNCTION
                      " F1 FUNCTION = " IOMOD-F1-FUNCT
                      " F1 SQLCODE  = " IOMOD-F1-SQLCODE
              DISPLAY "IOCUSVCC:"
                      "(A000) AFTER - FUNCTION " IOMOD-FUNCTION
                      " F2 FUNCTION = " IOMOD-F2-FUNCT
                      " F2 SQLCODE  = " IOMOD-F2-SQLCODE
           END-IF
           .
       A-GOBACK.
           EXIT PROGRAM
           .
       B000-DECLARE  SECTION.
       B000-START.
           MOVE SPACES TO SELECT-STR.
           STRING
           "SELECT * FROM (SELECT * FROM ("
           "SELECT /*+ INDEX_ASC(CUSVCC CUSVCCPK) */  "
           "KEY_1 "
           "FROM CUSVCC "
           "WHERE  KEY_1 > :V0001"
           "  AND  ROWNUM < 2 "
           ") UNION ALL SELECT * FROM ("
           "SELECT /*+ INDEX_ASC(CUSVCC CUSVCCPK) */ "
           "KEY_1 "
           "FROM "
           REMOTE-TABLE
           "WHERE  KEY_1 > :V0002"
           "  AND  ROWNUM < 2 "
           ")) "
           "ORDER BY KEY_1  ASC "
             DELIMITED BY SIZE
             INTO SELECT-STR
           END-STRING
           EXEC SQL
                PREPARE S_SEL01_PK FROM :SELECT-STR
           END-EXEC
           EXEC SQL
                DECLARE SEL01CUSVCC_PK CURSOR FOR S_SEL01_PK
           END-EXEC
           .
       D000-PK       SECTION.
       D000-PK-START.
           MOVE "FETCH" TO IOMOD-F1-FUNCT
           IF DAY-MODE
              EXEC SQL
                   OPEN S_SEL01CUSVCC_PK
              END-EXEC
              PERFORM F100-F1-SQLCODE
              EXEC SQL
                   FETCH S_SEL01CUSVCC_PK INTO
                   :D-CODE,               :D-DELI,
                   :D-KEY-1,              :D-NO-ASSOC,
                   :D-STATUS-01,          :D-RELAT-01,
                   :D-CURRENCY-01,        :D-ACCT-NO-01,
                   :D-SYST-01,            :D-ACCT-PER-OWN-01,
                   :D-LINK-CUA-ACNO-01,   :D-LINK-SYST-01,
                   :D-LINK-BRCH-01,       :D-OPEN-DATE-01,
                   :D-MOD-CLS-DATE-01,    :D-LINK-TYPE-01,
                   :D-LINK-DEPTH-01,      :D-LINK-ACCT-TYPE-01,
                   :D-LINK-CUST-NO-01,    :D-STATUS-02,
                   :D-RELAT-02,           :D-CURRENCY-02,
                   :D-ACCT-NO-02,         :D-SYST-02,
                   :D-ACCT-PER-OWN-02,    :D-LINK-CUA-ACNO-02,
                   :D-LINK-SYST-02,       :D-LINK-BRCH-02,
                   :D-OPEN-DATE-02,       :D-MOD-CLS-DATE-02,
                   :D-LINK-TYPE-02,       :D-LINK-DEPTH-02,
                   :D-LINK-ACCT-TYPE-02,  :D-LINK-CUST-NO-02,
                   :D-STATUS-03,          :D-RELAT-03,
                   :D-CURRENCY-03,        :D-ACCT-NO-03,
                   :D-SYST-03,            :D-ACCT-PER-OWN-03,
                   :D-LINK-CUA-ACNO-03,   :D-LINK-SYST-03,
                   :D-LINK-BRCH-03,       :D-OPEN-DATE-03,
                   :D-MOD-CLS-DATE-03,    :D-LINK-TYPE-03,
                   :D-LINK-DEPTH-03,      :D-LINK-ACCT-TYPE-03,
                   :D-LINK-CUST-NO-03,    :D-STATUS-04,
                   :D-RELAT-04,           :D-CURRENCY-04,
                   :D-ACCT-NO-04,         :D-SYST-04,
                   :D-ACCT-PER-OWN-04,    :D-LINK-CUA-ACNO-04,
                   :D-LINK-SYST-04,       :D-LINK-BRCH-04,
                   :D-OPEN-DATE-04,       :D-MOD-CLS-DATE-04,
                   :D-LINK-TYPE-04,       :D-LINK-DEPTH-04,
                   :D-LINK-ACCT-TYPE-04,  :D-LINK-CUST-NO-04,
                   :D-STATUS-05,          :D-RELAT-05,
                   :D-CURRENCY-05,        :D-ACCT-NO-05,
                   :D-SYST-05,            :D-ACCT-PER-OWN-05,
                   :D-LINK-CUA-ACNO-05,   :D-LINK-SYST-05,
                   :D-LINK-BRCH-05,       :D-OPEN-DATE-05,
                   :D-MOD-CLS-DATE-05,    :D-LINK-TYPE-05,
                   :D-LINK-DEPTH-05,      :D-LINK-ACCT-TYPE-05,
                   :D-LINK-CUST-NO-05,    :D-STATUS-06,
                   :D-RELAT-06,           :D-CURRENCY-06,
                   :D-ACCT-NO-06,         :D-SYST-06,
                   :D-ACCT-PER-OWN-06,    :D-LINK-CUA-ACNO-06,
                   :D-LINK-SYST-06,       :D-LINK-BRCH-06,
                   :D-OPEN-DATE-06,       :D-MOD-CLS-DATE-06,
                   :D-LINK-TYPE-06,       :D-LINK-DEPTH-06,
                   :D-LINK-ACCT-TYPE-06,  :D-LINK-CUST-NO-06,
                   :D-STATUS-07,          :D-RELAT-07,
                   :D-CURRENCY-07,        :D-ACCT-NO-07,
                   :D-SYST-07,            :D-ACCT-PER-OWN-07,
                   :D-LINK-CUA-ACNO-07,   :D-LINK-SYST-07,
                   :D-LINK-BRCH-07,       :D-OPEN-DATE-07,
                   :D-MOD-CLS-DATE-07,    :D-LINK-TYPE-07,
                   :D-LINK-DEPTH-07,      :D-LINK-ACCT-TYPE-07,
                   :D-LINK-CUST-NO-07,    :D-STATUS-08,
                   :D-RELAT-08,           :D-CURRENCY-08,
                   :D-ACCT-NO-08,         :D-SYST-08,
                   :D-ACCT-PER-OWN-08,    :D-LINK-CUA-ACNO-08,
                   :D-LINK-SYST-08,       :D-LINK-BRCH-08,
                   :D-OPEN-DATE-08,       :D-MOD-CLS-DATE-08,
                   :D-LINK-TYPE-08,       :D-LINK-DEPTH-08,
                   :D-LINK-ACCT-TYPE-08,  :D-LINK-CUST-NO-08
              END-EXEC
              PERFORM F200-F2-SQLCODE
           ELSE
              EXEC SQL
                   OPEN SEL01CUSVCC_PK USING
                   :D-PK-KEY-1
                   ,
                   :D-PK-KEY-1
              END-EXEC
              PERFORM F100-F1-SQLCODE
              EXEC SQL
                   FETCH SEL01CUSVCC_PK INTO
                   :D-KEY-1
              END-EXEC
              PERFORM F200-F2-SQLCODE
              IF IOMOD-F2-SQLCODE = 0
                 MOVE D-KEY-1 TO D-PK-KEY-1
                 PERFORM D110-SEL-NOLOCK-PK
              END-IF
           END-IF
           .
       D100-SEL-LOCK-PK     SECTION.
       D100-START-PK.
           EXEC SQL
              SELECT /*+ INDEX_ASC(CUSVCC CUSVCCPK) */* INTO
                   :D-CODE,               :D-DELI,
                   :D-KEY-1,              :D-NO-ASSOC,
                   :D-STATUS-01,          :D-RELAT-01,
                   :D-CURRENCY-01,        :D-ACCT-NO-01,
                   :D-SYST-01,            :D-ACCT-PER-OWN-01,
                   :D-LINK-CUA-ACNO-01,   :D-LINK-SYST-01,
                   :D-LINK-BRCH-01,       :D-OPEN-DATE-01,
                   :D-MOD-CLS-DATE-01,    :D-LINK-TYPE-01,
                   :D-LINK-DEPTH-01,      :D-LINK-ACCT-TYPE-01,
                   :D-LINK-CUST-NO-01,    :D-STATUS-02,
                   :D-RELAT-02,           :D-CURRENCY-02,
                   :D-ACCT-NO-02,         :D-SYST-02,
                   :D-ACCT-PER-OWN-02,    :D-LINK-CUA-ACNO-02,
                   :D-LINK-SYST-02,       :D-LINK-BRCH-02,
                   :D-OPEN-DATE-02,       :D-MOD-CLS-DATE-02,
                   :D-LINK-TYPE-02,       :D-LINK-DEPTH-02,
                   :D-LINK-ACCT-TYPE-02,  :D-LINK-CUST-NO-02,
                   :D-STATUS-03,          :D-RELAT-03,
                   :D-CURRENCY-03,        :D-ACCT-NO-03,
                   :D-SYST-03,            :D-ACCT-PER-OWN-03,
                   :D-LINK-CUA-ACNO-03,   :D-LINK-SYST-03,
                   :D-LINK-BRCH-03,       :D-OPEN-DATE-03,
                   :D-MOD-CLS-DATE-03,    :D-LINK-TYPE-03,
                   :D-LINK-DEPTH-03,      :D-LINK-ACCT-TYPE-03,
                   :D-LINK-CUST-NO-03,    :D-STATUS-04,
                   :D-RELAT-04,           :D-CURRENCY-04,
                   :D-ACCT-NO-04,         :D-SYST-04,
                   :D-ACCT-PER-OWN-04,    :D-LINK-CUA-ACNO-04,
                   :D-LINK-SYST-04,       :D-LINK-BRCH-04,
                   :D-OPEN-DATE-04,       :D-MOD-CLS-DATE-04,
                   :D-LINK-TYPE-04,       :D-LINK-DEPTH-04,
                   :D-LINK-ACCT-TYPE-04,  :D-LINK-CUST-NO-04,
                   :D-STATUS-05,          :D-RELAT-05,
                   :D-CURRENCY-05,        :D-ACCT-NO-05,
                   :D-SYST-05,            :D-ACCT-PER-OWN-05,
                   :D-LINK-CUA-ACNO-05,   :D-LINK-SYST-05,
                   :D-LINK-BRCH-05,       :D-OPEN-DATE-05,
                   :D-MOD-CLS-DATE-05,    :D-LINK-TYPE-05,
                   :D-LINK-DEPTH-05,      :D-LINK-ACCT-TYPE-05,
                   :D-LINK-CUST-NO-05,    :D-STATUS-06,
                   :D-RELAT-06,           :D-CURRENCY-06,
                   :D-ACCT-NO-06,         :D-SYST-06,
                   :D-ACCT-PER-OWN-06,    :D-LINK-CUA-ACNO-06,
                   :D-LINK-SYST-06,       :D-LINK-BRCH-06,
                   :D-OPEN-DATE-06,       :D-MOD-CLS-DATE-06,
                   :D-LINK-TYPE-06,       :D-LINK-DEPTH-06,
                   :D-LINK-ACCT-TYPE-06,  :D-LINK-CUST-NO-06,
                   :D-STATUS-07,          :D-RELAT-07,
                   :D-CURRENCY-07,        :D-ACCT-NO-07,
                   :D-SYST-07,            :D-ACCT-PER-OWN-07,
                   :D-LINK-CUA-ACNO-07,   :D-LINK-SYST-07,
                   :D-LINK-BRCH-07,       :D-OPEN-DATE-07,
                   :D-MOD-CLS-DATE-07,    :D-LINK-TYPE-07,
                   :D-LINK-DEPTH-07,      :D-LINK-ACCT-TYPE-07,
                   :D-LINK-CUST-NO-07,    :D-STATUS-08,
                   :D-RELAT-08,           :D-CURRENCY-08,
                   :D-ACCT-NO-08,         :D-SYST-08,
                   :D-ACCT-PER-OWN-08,    :D-LINK-CUA-ACNO-08,
                   :D-LINK-SYST-08,       :D-LINK-BRCH-08,
                   :D-OPEN-DATE-08,       :D-MOD-CLS-DATE-08,
                   :D-LINK-TYPE-08,       :D-LINK-DEPTH-08,
                   :D-LINK-ACCT-TYPE-08,  :D-LINK-CUST-NO-08
              FROM       CUSVCC
              WHERE  KEY_1 =  :D-PK-KEY-1
              FOR UPDATE NOWAIT
           END-EXEC
           PERFORM F200-F2-SQLCODE
           IF NIGHT-MODE AND IOMOD-F2-SQLCODE = 1403
              MOVE "Y" TO OPEN-CURSOR-FLAG
              IF USE-MASTER-1
              EXEC SQL
              SELECT /*+ INDEX_ASC(CUSVCC CUSVCCPK) */* INTO
                   :D-CODE,               :D-DELI,
                   :D-KEY-1,              :D-NO-ASSOC,
                   :D-STATUS-01,          :D-RELAT-01,
                   :D-CURRENCY-01,        :D-ACCT-NO-01,
                   :D-SYST-01,            :D-ACCT-PER-OWN-01,
                   :D-LINK-CUA-ACNO-01,   :D-LINK-SYST-01,
                   :D-LINK-BRCH-01,       :D-OPEN-DATE-01,
                   :D-MOD-CLS-DATE-01,    :D-LINK-TYPE-01,
                   :D-LINK-DEPTH-01,      :D-LINK-ACCT-TYPE-01,
                   :D-LINK-CUST-NO-01,    :D-STATUS-02,
                   :D-RELAT-02,           :D-CURRENCY-02,
                   :D-ACCT-NO-02,         :D-SYST-02,
                   :D-ACCT-PER-OWN-02,    :D-LINK-CUA-ACNO-02,
                   :D-LINK-SYST-02,       :D-LINK-BRCH-02,
                   :D-OPEN-DATE-02,       :D-MOD-CLS-DATE-02,
                   :D-LINK-TYPE-02,       :D-LINK-DEPTH-02,
                   :D-LINK-ACCT-TYPE-02,  :D-LINK-CUST-NO-02,
                   :D-STATUS-03,          :D-RELAT-03,
                   :D-CURRENCY-03,        :D-ACCT-NO-03,
                   :D-SYST-03,            :D-ACCT-PER-OWN-03,
                   :D-LINK-CUA-ACNO-03,   :D-LINK-SYST-03,
                   :D-LINK-BRCH-03,       :D-OPEN-DATE-03,
                   :D-MOD-CLS-DATE-03,    :D-LINK-TYPE-03,
                   :D-LINK-DEPTH-03,      :D-LINK-ACCT-TYPE-03,
                   :D-LINK-CUST-NO-03,    :D-STATUS-04,
                   :D-RELAT-04,           :D-CURRENCY-04,
                   :D-ACCT-NO-04,         :D-SYST-04,
                   :D-ACCT-PER-OWN-04,    :D-LINK-CUA-ACNO-04,
                   :D-LINK-SYST-04,       :D-LINK-BRCH-04,
                   :D-OPEN-DATE-04,       :D-MOD-CLS-DATE-04,
                   :D-LINK-TYPE-04,       :D-LINK-DEPTH-04,
                   :D-LINK-ACCT-TYPE-04,  :D-LINK-CUST-NO-04,
                   :D-STATUS-05,          :D-RELAT-05,
                   :D-CURRENCY-05,        :D-ACCT-NO-05,
                   :D-SYST-05,            :D-ACCT-PER-OWN-05,
                   :D-LINK-CUA-ACNO-05,   :D-LINK-SYST-05,
                   :D-LINK-BRCH-05,       :D-OPEN-DATE-05,
                   :D-MOD-CLS-DATE-05,    :D-LINK-TYPE-05,
                   :D-LINK-DEPTH-05,      :D-LINK-ACCT-TYPE-05,
                   :D-LINK-CUST-NO-05,    :D-STATUS-06,
                   :D-RELAT-06,           :D-CURRENCY-06,
                   :D-ACCT-NO-06,         :D-SYST-06,
                   :D-ACCT-PER-OWN-06,    :D-LINK-CUA-ACNO-06,
                   :D-LINK-SYST-06,       :D-LINK-BRCH-06,
                   :D-OPEN-DATE-06,       :D-MOD-CLS-DATE-06,
                   :D-LINK-TYPE-06,       :D-LINK-DEPTH-06,
                   :D-LINK-ACCT-TYPE-06,  :D-LINK-CUST-NO-06,
                   :D-STATUS-07,          :D-RELAT-07,
                   :D-CURRENCY-07,        :D-ACCT-NO-07,
                   :D-SYST-07,            :D-ACCT-PER-OWN-07,
                   :D-LINK-CUA-ACNO-07,   :D-LINK-SYST-07,
                   :D-LINK-BRCH-07,       :D-OPEN-DATE-07,
                   :D-MOD-CLS-DATE-07,    :D-LINK-TYPE-07,
                   :D-LINK-DEPTH-07,      :D-LINK-ACCT-TYPE-07,
                   :D-LINK-CUST-NO-07,    :D-STATUS-08,
                   :D-RELAT-08,           :D-CURRENCY-08,
                   :D-ACCT-NO-08,         :D-SYST-08,
                   :D-ACCT-PER-OWN-08,    :D-LINK-CUA-ACNO-08,
                   :D-LINK-SYST-08,       :D-LINK-BRCH-08,
                   :D-OPEN-DATE-08,       :D-MOD-CLS-DATE-08,
                   :D-LINK-TYPE-08,       :D-LINK-DEPTH-08,
                   :D-LINK-ACCT-TYPE-08,  :D-LINK-CUST-NO-08
              FROM TMP_1_CUSVCC
              WHERE  KEY_1 =  :D-PK-KEY-1
              FOR UPDATE NOWAIT
              END-EXEC
              ELSE
              EXEC SQL
              SELECT /*+ INDEX_ASC(CUSVCC CUSVCCPK) */* INTO
                   :D-CODE,               :D-DELI,
                   :D-KEY-1,              :D-NO-ASSOC,
                   :D-STATUS-01,          :D-RELAT-01,
                   :D-CURRENCY-01,        :D-ACCT-NO-01,
                   :D-SYST-01,            :D-ACCT-PER-OWN-01,
                   :D-LINK-CUA-ACNO-01,   :D-LINK-SYST-01,
                   :D-LINK-BRCH-01,       :D-OPEN-DATE-01,
                   :D-MOD-CLS-DATE-01,    :D-LINK-TYPE-01,
                   :D-LINK-DEPTH-01,      :D-LINK-ACCT-TYPE-01,
                   :D-LINK-CUST-NO-01,    :D-STATUS-02,
                   :D-RELAT-02,           :D-CURRENCY-02,
                   :D-ACCT-NO-02,         :D-SYST-02,
                   :D-ACCT-PER-OWN-02,    :D-LINK-CUA-ACNO-02,
                   :D-LINK-SYST-02,       :D-LINK-BRCH-02,
                   :D-OPEN-DATE-02,       :D-MOD-CLS-DATE-02,
                   :D-LINK-TYPE-02,       :D-LINK-DEPTH-02,
                   :D-LINK-ACCT-TYPE-02,  :D-LINK-CUST-NO-02,
                   :D-STATUS-03,          :D-RELAT-03,
                   :D-CURRENCY-03,        :D-ACCT-NO-03,
                   :D-SYST-03,            :D-ACCT-PER-OWN-03,
                   :D-LINK-CUA-ACNO-03,   :D-LINK-SYST-03,
                   :D-LINK-BRCH-03,       :D-OPEN-DATE-03,
                   :D-MOD-CLS-DATE-03,    :D-LINK-TYPE-03,
                   :D-LINK-DEPTH-03,      :D-LINK-ACCT-TYPE-03,
                   :D-LINK-CUST-NO-03,    :D-STATUS-04,
                   :D-RELAT-04,           :D-CURRENCY-04,
                   :D-ACCT-NO-04,         :D-SYST-04,
                   :D-ACCT-PER-OWN-04,    :D-LINK-CUA-ACNO-04,
                   :D-LINK-SYST-04,       :D-LINK-BRCH-04,
                   :D-OPEN-DATE-04,       :D-MOD-CLS-DATE-04,
                   :D-LINK-TYPE-04,       :D-LINK-DEPTH-04,
                   :D-LINK-ACCT-TYPE-04,  :D-LINK-CUST-NO-04,
                   :D-STATUS-05,          :D-RELAT-05,
                   :D-CURRENCY-05,        :D-ACCT-NO-05,
                   :D-SYST-05,            :D-ACCT-PER-OWN-05,
                   :D-LINK-CUA-ACNO-05,   :D-LINK-SYST-05,
                   :D-LINK-BRCH-05,       :D-OPEN-DATE-05,
                   :D-MOD-CLS-DATE-05,    :D-LINK-TYPE-05,
                   :D-LINK-DEPTH-05,      :D-LINK-ACCT-TYPE-05,
                   :D-LINK-CUST-NO-05,    :D-STATUS-06,
                   :D-RELAT-06,           :D-CURRENCY-06,
                   :D-ACCT-NO-06,         :D-SYST-06,
                   :D-ACCT-PER-OWN-06,    :D-LINK-CUA-ACNO-06,
                   :D-LINK-SYST-06,       :D-LINK-BRCH-06,
                   :D-OPEN-DATE-06,       :D-MOD-CLS-DATE-06,
                   :D-LINK-TYPE-06,       :D-LINK-DEPTH-06,
                   :D-LINK-ACCT-TYPE-06,  :D-LINK-CUST-NO-06,
                   :D-STATUS-07,          :D-RELAT-07,
                   :D-CURRENCY-07,        :D-ACCT-NO-07,
                   :D-SYST-07,            :D-ACCT-PER-OWN-07,
                   :D-LINK-CUA-ACNO-07,   :D-LINK-SYST-07,
                   :D-LINK-BRCH-07,       :D-OPEN-DATE-07,
                   :D-MOD-CLS-DATE-07,    :D-LINK-TYPE-07,
                   :D-LINK-DEPTH-07,      :D-LINK-ACCT-TYPE-07,
                   :D-LINK-CUST-NO-07,    :D-STATUS-08,
                   :D-RELAT-08,           :D-CURRENCY-08,
                   :D-ACCT-NO-08,         :D-SYST-08,
                   :D-ACCT-PER-OWN-08,    :D-LINK-CUA-ACNO-08,
                   :D-LINK-SYST-08,       :D-LINK-BRCH-08,
                   :D-OPEN-DATE-08,       :D-MOD-CLS-DATE-08,
                   :D-LINK-TYPE-08,       :D-LINK-DEPTH-08,
                   :D-LINK-ACCT-TYPE-08,  :D-LINK-CUST-NO-08
              FROM TMP_2_CUSVCC
              WHERE  KEY_1 =  :D-PK-KEY-1
              FOR UPDATE NOWAIT
              END-EXEC
              END-IF
              PERFORM F200-F2-SQLCODE
           END-IF
           .
       D110-SEL-NOLOCK-PK     SECTION.
       D110-START-PK.
           EXEC SQL
              SELECT /*+ INDEX_ASC(CUSVCC CUSVCCPK) */* INTO
                   :D-CODE,               :D-DELI,
                   :D-KEY-1,              :D-NO-ASSOC,
                   :D-STATUS-01,          :D-RELAT-01,
                   :D-CURRENCY-01,        :D-ACCT-NO-01,
                   :D-SYST-01,            :D-ACCT-PER-OWN-01,
                   :D-LINK-CUA-ACNO-01,   :D-LINK-SYST-01,
                   :D-LINK-BRCH-01,       :D-OPEN-DATE-01,
                   :D-MOD-CLS-DATE-01,    :D-LINK-TYPE-01,
                   :D-LINK-DEPTH-01,      :D-LINK-ACCT-TYPE-01,
                   :D-LINK-CUST-NO-01,    :D-STATUS-02,
                   :D-RELAT-02,           :D-CURRENCY-02,
                   :D-ACCT-NO-02,         :D-SYST-02,
                   :D-ACCT-PER-OWN-02,    :D-LINK-CUA-ACNO-02,
                   :D-LINK-SYST-02,       :D-LINK-BRCH-02,
                   :D-OPEN-DATE-02,       :D-MOD-CLS-DATE-02,
                   :D-LINK-TYPE-02,       :D-LINK-DEPTH-02,
                   :D-LINK-ACCT-TYPE-02,  :D-LINK-CUST-NO-02,
                   :D-STATUS-03,          :D-RELAT-03,
                   :D-CURRENCY-03,        :D-ACCT-NO-03,
                   :D-SYST-03,            :D-ACCT-PER-OWN-03,
                   :D-LINK-CUA-ACNO-03,   :D-LINK-SYST-03,
                   :D-LINK-BRCH-03,       :D-OPEN-DATE-03,
                   :D-MOD-CLS-DATE-03,    :D-LINK-TYPE-03,
                   :D-LINK-DEPTH-03,      :D-LINK-ACCT-TYPE-03,
                   :D-LINK-CUST-NO-03,    :D-STATUS-04,
                   :D-RELAT-04,           :D-CURRENCY-04,
                   :D-ACCT-NO-04,         :D-SYST-04,
                   :D-ACCT-PER-OWN-04,    :D-LINK-CUA-ACNO-04,
                   :D-LINK-SYST-04,       :D-LINK-BRCH-04,
                   :D-OPEN-DATE-04,       :D-MOD-CLS-DATE-04,
                   :D-LINK-TYPE-04,       :D-LINK-DEPTH-04,
                   :D-LINK-ACCT-TYPE-04,  :D-LINK-CUST-NO-04,
                   :D-STATUS-05,          :D-RELAT-05,
                   :D-CURRENCY-05,        :D-ACCT-NO-05,
                   :D-SYST-05,            :D-ACCT-PER-OWN-05,
                   :D-LINK-CUA-ACNO-05,   :D-LINK-SYST-05,
                   :D-LINK-BRCH-05,       :D-OPEN-DATE-05,
                   :D-MOD-CLS-DATE-05,    :D-LINK-TYPE-05,
                   :D-LINK-DEPTH-05,      :D-LINK-ACCT-TYPE-05,
                   :D-LINK-CUST-NO-05,    :D-STATUS-06,
                   :D-RELAT-06,           :D-CURRENCY-06,
                   :D-ACCT-NO-06,         :D-SYST-06,
                   :D-ACCT-PER-OWN-06,    :D-LINK-CUA-ACNO-06,
                   :D-LINK-SYST-06,       :D-LINK-BRCH-06,
                   :D-OPEN-DATE-06,       :D-MOD-CLS-DATE-06,
                   :D-LINK-TYPE-06,       :D-LINK-DEPTH-06,
                   :D-LINK-ACCT-TYPE-06,  :D-LINK-CUST-NO-06,
                   :D-STATUS-07,          :D-RELAT-07,
                   :D-CURRENCY-07,        :D-ACCT-NO-07,
                   :D-SYST-07,            :D-ACCT-PER-OWN-07,
                   :D-LINK-CUA-ACNO-07,   :D-LINK-SYST-07,
                   :D-LINK-BRCH-07,       :D-OPEN-DATE-07,
                   :D-MOD-CLS-DATE-07,    :D-LINK-TYPE-07,
                   :D-LINK-DEPTH-07,      :D-LINK-ACCT-TYPE-07,
                   :D-LINK-CUST-NO-07,    :D-STATUS-08,
                   :D-RELAT-08,           :D-CURRENCY-08,
                   :D-ACCT-NO-08,         :D-SYST-08,
                   :D-ACCT-PER-OWN-08,    :D-LINK-CUA-ACNO-08,
                   :D-LINK-SYST-08,       :D-LINK-BRCH-08,
                   :D-OPEN-DATE-08,       :D-MOD-CLS-DATE-08,
                   :D-LINK-TYPE-08,       :D-LINK-DEPTH-08,
                   :D-LINK-ACCT-TYPE-08,  :D-LINK-CUST-NO-08
              FROM       CUSVCC
              WHERE  KEY_1 =  :D-PK-KEY-1
           END-EXEC
           PERFORM F200-F2-SQLCODE
           IF NIGHT-MODE AND IOMOD-F2-SQLCODE = 1403
              MOVE "Y" TO OPEN-CURSOR-FLAG
              IF USE-MASTER-1
              EXEC SQL
              SELECT /*+ INDEX_ASC(CUSVCC CUSVCCPK) */* INTO
                   :D-CODE,               :D-DELI,
                   :D-KEY-1,              :D-NO-ASSOC,
                   :D-STATUS-01,          :D-RELAT-01,
                   :D-CURRENCY-01,        :D-ACCT-NO-01,
                   :D-SYST-01,            :D-ACCT-PER-OWN-01,
                   :D-LINK-CUA-ACNO-01,   :D-LINK-SYST-01,
                   :D-LINK-BRCH-01,       :D-OPEN-DATE-01,
                   :D-MOD-CLS-DATE-01,    :D-LINK-TYPE-01,
                   :D-LINK-DEPTH-01,      :D-LINK-ACCT-TYPE-01,
                   :D-LINK-CUST-NO-01,    :D-STATUS-02,
                   :D-RELAT-02,           :D-CURRENCY-02,
                   :D-ACCT-NO-02,         :D-SYST-02,
                   :D-ACCT-PER-OWN-02,    :D-LINK-CUA-ACNO-02,
                   :D-LINK-SYST-02,       :D-LINK-BRCH-02,
                   :D-OPEN-DATE-02,       :D-MOD-CLS-DATE-02,
                   :D-LINK-TYPE-02,       :D-LINK-DEPTH-02,
                   :D-LINK-ACCT-TYPE-02,  :D-LINK-CUST-NO-02,
                   :D-STATUS-03,          :D-RELAT-03,
                   :D-CURRENCY-03,        :D-ACCT-NO-03,
                   :D-SYST-03,            :D-ACCT-PER-OWN-03,
                   :D-LINK-CUA-ACNO-03,   :D-LINK-SYST-03,
                   :D-LINK-BRCH-03,       :D-OPEN-DATE-03,
                   :D-MOD-CLS-DATE-03,    :D-LINK-TYPE-03,
                   :D-LINK-DEPTH-03,      :D-LINK-ACCT-TYPE-03,
                   :D-LINK-CUST-NO-03,    :D-STATUS-04,
                   :D-RELAT-04,           :D-CURRENCY-04,
                   :D-ACCT-NO-04,         :D-SYST-04,
                   :D-ACCT-PER-OWN-04,    :D-LINK-CUA-ACNO-04,
                   :D-LINK-SYST-04,       :D-LINK-BRCH-04,
                   :D-OPEN-DATE-04,       :D-MOD-CLS-DATE-04,
                   :D-LINK-TYPE-04,       :D-LINK-DEPTH-04,
                   :D-LINK-ACCT-TYPE-04,  :D-LINK-CUST-NO-04,
                   :D-STATUS-05,          :D-RELAT-05,
                   :D-CURRENCY-05,        :D-ACCT-NO-05,
                   :D-SYST-05,            :D-ACCT-PER-OWN-05,
                   :D-LINK-CUA-ACNO-05,   :D-LINK-SYST-05,
                   :D-LINK-BRCH-05,       :D-OPEN-DATE-05,
                   :D-MOD-CLS-DATE-05,    :D-LINK-TYPE-05,
                   :D-LINK-DEPTH-05,      :D-LINK-ACCT-TYPE-05,
                   :D-LINK-CUST-NO-05,    :D-STATUS-06,
                   :D-RELAT-06,           :D-CURRENCY-06,
                   :D-ACCT-NO-06,         :D-SYST-06,
                   :D-ACCT-PER-OWN-06,    :D-LINK-CUA-ACNO-06,
                   :D-LINK-SYST-06,       :D-LINK-BRCH-06,
                   :D-OPEN-DATE-06,       :D-MOD-CLS-DATE-06,
                   :D-LINK-TYPE-06,       :D-LINK-DEPTH-06,
                   :D-LINK-ACCT-TYPE-06,  :D-LINK-CUST-NO-06,
                   :D-STATUS-07,          :D-RELAT-07,
                   :D-CURRENCY-07,        :D-ACCT-NO-07,
                   :D-SYST-07,            :D-ACCT-PER-OWN-07,
                   :D-LINK-CUA-ACNO-07,   :D-LINK-SYST-07,
                   :D-LINK-BRCH-07,       :D-OPEN-DATE-07,
                   :D-MOD-CLS-DATE-07,    :D-LINK-TYPE-07,
                   :D-LINK-DEPTH-07,      :D-LINK-ACCT-TYPE-07,
                   :D-LINK-CUST-NO-07,    :D-STATUS-08,
                   :D-RELAT-08,           :D-CURRENCY-08,
                   :D-ACCT-NO-08,         :D-SYST-08,
                   :D-ACCT-PER-OWN-08,    :D-LINK-CUA-ACNO-08,
                   :D-LINK-SYST-08,       :D-LINK-BRCH-08,
                   :D-OPEN-DATE-08,       :D-MOD-CLS-DATE-08,
                   :D-LINK-TYPE-08,       :D-LINK-DEPTH-08,
                   :D-LINK-ACCT-TYPE-08,  :D-LINK-CUST-NO-08
              FROM TMP_1_CUSVCC
              WHERE  KEY_1 =  :D-PK-KEY-1
              END-EXEC
              ELSE
              EXEC SQL
              SELECT /*+ INDEX_ASC(CUSVCC CUSVCCPK) */* INTO
                   :D-CODE,               :D-DELI,
                   :D-KEY-1,              :D-NO-ASSOC,
                   :D-STATUS-01,          :D-RELAT-01,
                   :D-CURRENCY-01,        :D-ACCT-NO-01,
                   :D-SYST-01,            :D-ACCT-PER-OWN-01,
                   :D-LINK-CUA-ACNO-01,   :D-LINK-SYST-01,
                   :D-LINK-BRCH-01,       :D-OPEN-DATE-01,
                   :D-MOD-CLS-DATE-01,    :D-LINK-TYPE-01,
                   :D-LINK-DEPTH-01,      :D-LINK-ACCT-TYPE-01,
                   :D-LINK-CUST-NO-01,    :D-STATUS-02,
                   :D-RELAT-02,           :D-CURRENCY-02,
                   :D-ACCT-NO-02,         :D-SYST-02,
                   :D-ACCT-PER-OWN-02,    :D-LINK-CUA-ACNO-02,
                   :D-LINK-SYST-02,       :D-LINK-BRCH-02,
                   :D-OPEN-DATE-02,       :D-MOD-CLS-DATE-02,
                   :D-LINK-TYPE-02,       :D-LINK-DEPTH-02,
                   :D-LINK-ACCT-TYPE-02,  :D-LINK-CUST-NO-02,
                   :D-STATUS-03,          :D-RELAT-03,
                   :D-CURRENCY-03,        :D-ACCT-NO-03,
                   :D-SYST-03,            :D-ACCT-PER-OWN-03,
                   :D-LINK-CUA-ACNO-03,   :D-LINK-SYST-03,
                   :D-LINK-BRCH-03,       :D-OPEN-DATE-03,
                   :D-MOD-CLS-DATE-03,    :D-LINK-TYPE-03,
                   :D-LINK-DEPTH-03,      :D-LINK-ACCT-TYPE-03,
                   :D-LINK-CUST-NO-03,    :D-STATUS-04,
                   :D-RELAT-04,           :D-CURRENCY-04,
                   :D-ACCT-NO-04,         :D-SYST-04,
                   :D-ACCT-PER-OWN-04,    :D-LINK-CUA-ACNO-04,
                   :D-LINK-SYST-04,       :D-LINK-BRCH-04,
                   :D-OPEN-DATE-04,       :D-MOD-CLS-DATE-04,
                   :D-LINK-TYPE-04,       :D-LINK-DEPTH-04,
                   :D-LINK-ACCT-TYPE-04,  :D-LINK-CUST-NO-04,
                   :D-STATUS-05,          :D-RELAT-05,
                   :D-CURRENCY-05,        :D-ACCT-NO-05,
                   :D-SYST-05,            :D-ACCT-PER-OWN-05,
                   :D-LINK-CUA-ACNO-05,   :D-LINK-SYST-05,
                   :D-LINK-BRCH-05,       :D-OPEN-DATE-05,
                   :D-MOD-CLS-DATE-05,    :D-LINK-TYPE-05,
                   :D-LINK-DEPTH-05,      :D-LINK-ACCT-TYPE-05,
                   :D-LINK-CUST-NO-05,    :D-STATUS-06,
                   :D-RELAT-06,           :D-CURRENCY-06,
                   :D-ACCT-NO-06,         :D-SYST-06,
                   :D-ACCT-PER-OWN-06,    :D-LINK-CUA-ACNO-06,
                   :D-LINK-SYST-06,       :D-LINK-BRCH-06,
                   :D-OPEN-DATE-06,       :D-MOD-CLS-DATE-06,
                   :D-LINK-TYPE-06,       :D-LINK-DEPTH-06,
                   :D-LINK-ACCT-TYPE-06,  :D-LINK-CUST-NO-06,
                   :D-STATUS-07,          :D-RELAT-07,
                   :D-CURRENCY-07,        :D-ACCT-NO-07,
                   :D-SYST-07,            :D-ACCT-PER-OWN-07,
                   :D-LINK-CUA-ACNO-07,   :D-LINK-SYST-07,
                   :D-LINK-BRCH-07,       :D-OPEN-DATE-07,
                   :D-MOD-CLS-DATE-07,    :D-LINK-TYPE-07,
                   :D-LINK-DEPTH-07,      :D-LINK-ACCT-TYPE-07,
                   :D-LINK-CUST-NO-07,    :D-STATUS-08,
                   :D-RELAT-08,           :D-CURRENCY-08,
                   :D-ACCT-NO-08,         :D-SYST-08,
                   :D-ACCT-PER-OWN-08,    :D-LINK-CUA-ACNO-08,
                   :D-LINK-SYST-08,       :D-LINK-BRCH-08,
                   :D-OPEN-DATE-08,       :D-MOD-CLS-DATE-08,
                   :D-LINK-TYPE-08,       :D-LINK-DEPTH-08,
                   :D-LINK-ACCT-TYPE-08,  :D-LINK-CUST-NO-08
              FROM TMP_2_CUSVCC
              WHERE  KEY_1 =  :D-PK-KEY-1
              END-EXEC
              END-IF
              PERFORM F200-F2-SQLCODE
           END-IF
           .
       D200-DELETE   SECTION.
       D200-START.
           EXEC SQL
             DELETE FROM CUSVCC
              WHERE  KEY_1 =  :D-PK-KEY-1
           END-EXEC
           .
       D300-INSERT   SECTION.
       D300-START.
           EXEC SQL
             INSERT INTO CUSVCC
                   (CODE,               DELI,
                    KEY_1,              NO_ASSOC,
                    STATUS_01,          RELAT_01,
                    CURRENCY_01,        ACCT_NO_01,
                    SYST_01,            ACCT_PER_OWN_01,
                    LINK_CUA_ACNO_01,   LINK_SYST_01,
                    LINK_BRCH_01,       OPEN_DATE_01,
                    MOD_CLS_DATE_01,    LINK_TYPE_01,
                    LINK_DEPTH_01,      LINK_ACCT_TYPE_01,
                    LINK_CUST_NO_01,    STATUS_02,
                    RELAT_02,           CURRENCY_02,
                    ACCT_NO_02,         SYST_02,
                    ACCT_PER_OWN_02,    LINK_CUA_ACNO_02,
                    LINK_SYST_02,       LINK_BRCH_02,
                    OPEN_DATE_02,       MOD_CLS_DATE_02,
                    LINK_TYPE_02,       LINK_DEPTH_02,
                    LINK_ACCT_TYPE_02,  LINK_CUST_NO_02,
                    STATUS_03,          RELAT_03,
                    CURRENCY_03,        ACCT_NO_03,
                    SYST_03,            ACCT_PER_OWN_03,
                    LINK_CUA_ACNO_03,   LINK_SYST_03,
                    LINK_BRCH_03,       OPEN_DATE_03,
                    MOD_CLS_DATE_03,    LINK_TYPE_03,
                    LINK_DEPTH_03,      LINK_ACCT_TYPE_03,
                    LINK_CUST_NO_03,    STATUS_04,
                    RELAT_04,           CURRENCY_04,
                    ACCT_NO_04,         SYST_04,
                    ACCT_PER_OWN_04,    LINK_CUA_ACNO_04,
                    LINK_SYST_04,       LINK_BRCH_04,
                    OPEN_DATE_04,       MOD_CLS_DATE_04,
                    LINK_TYPE_04,       LINK_DEPTH_04,
                    LINK_ACCT_TYPE_04,  LINK_CUST_NO_04,
                    STATUS_05,          RELAT_05,
                    CURRENCY_05,        ACCT_NO_05,
                    SYST_05,            ACCT_PER_OWN_05,
                    LINK_CUA_ACNO_05,   LINK_SYST_05,
                    LINK_BRCH_05,       OPEN_DATE_05,
                    MOD_CLS_DATE_05,    LINK_TYPE_05,
                    LINK_DEPTH_05,      LINK_ACCT_TYPE_05,
                    LINK_CUST_NO_05,    STATUS_06,
                    RELAT_06,           CURRENCY_06,
                    ACCT_NO_06,         SYST_06,
                    ACCT_PER_OWN_06,    LINK_CUA_ACNO_06,
                    LINK_SYST_06,       LINK_BRCH_06,
                    OPEN_DATE_06,       MOD_CLS_DATE_06,
                    LINK_TYPE_06,       LINK_DEPTH_06,
                    LINK_ACCT_TYPE_06,  LINK_CUST_NO_06,
                    STATUS_07,          RELAT_07,
                    CURRENCY_07,        ACCT_NO_07,
                    SYST_07,            ACCT_PER_OWN_07,
                    LINK_CUA_ACNO_07,   LINK_SYST_07,
                    LINK_BRCH_07,       OPEN_DATE_07,
                    MOD_CLS_DATE_07,    LINK_TYPE_07,
                    LINK_DEPTH_07,      LINK_ACCT_TYPE_07,
                    LINK_CUST_NO_07,    STATUS_08,
                    RELAT_08,           CURRENCY_08,
                    ACCT_NO_08,         SYST_08,
                    ACCT_PER_OWN_08,    LINK_CUA_ACNO_08,
                    LINK_SYST_08,       LINK_BRCH_08,
                    OPEN_DATE_08,       MOD_CLS_DATE_08,
                    LINK_TYPE_08,       LINK_DEPTH_08,
                    LINK_ACCT_TYPE_08,  LINK_CUST_NO_08)
             VALUES(:D-CODE,               :D-DELI,
                    :D-KEY-1,              :D-NO-ASSOC,
                    :D-STATUS-01,          :D-RELAT-01,
                    :D-CURRENCY-01,        :D-ACCT-NO-01,
                    :D-SYST-01,            :D-ACCT-PER-OWN-01,
                    :D-LINK-CUA-ACNO-01,   :D-LINK-SYST-01,
                    :D-LINK-BRCH-01,       :D-OPEN-DATE-01,
                    :D-MOD-CLS-DATE-01,    :D-LINK-TYPE-01,
                    :D-LINK-DEPTH-01,      :D-LINK-ACCT-TYPE-01,
                    :D-LINK-CUST-NO-01,    :D-STATUS-02,
                    :D-RELAT-02,           :D-CURRENCY-02,
                    :D-ACCT-NO-02,         :D-SYST-02,
                    :D-ACCT-PER-OWN-02,    :D-LINK-CUA-ACNO-02,
                    :D-LINK-SYST-02,       :D-LINK-BRCH-02,
                    :D-OPEN-DATE-02,       :D-MOD-CLS-DATE-02,
                    :D-LINK-TYPE-02,       :D-LINK-DEPTH-02,
                    :D-LINK-ACCT-TYPE-02,  :D-LINK-CUST-NO-02,
                    :D-STATUS-03,          :D-RELAT-03,
                    :D-CURRENCY-03,        :D-ACCT-NO-03,
                    :D-SYST-03,            :D-ACCT-PER-OWN-03,
                    :D-LINK-CUA-ACNO-03,   :D-LINK-SYST-03,
                    :D-LINK-BRCH-03,       :D-OPEN-DATE-03,
                    :D-MOD-CLS-DATE-03,    :D-LINK-TYPE-03,
                    :D-LINK-DEPTH-03,      :D-LINK-ACCT-TYPE-03,
                    :D-LINK-CUST-NO-03,    :D-STATUS-04,
                    :D-RELAT-04,           :D-CURRENCY-04,
                    :D-ACCT-NO-04,         :D-SYST-04,
                    :D-ACCT-PER-OWN-04,    :D-LINK-CUA-ACNO-04,
                    :D-LINK-SYST-04,       :D-LINK-BRCH-04,
                    :D-OPEN-DATE-04,       :D-MOD-CLS-DATE-04,
                    :D-LINK-TYPE-04,       :D-LINK-DEPTH-04,
                    :D-LINK-ACCT-TYPE-04,  :D-LINK-CUST-NO-04,
                    :D-STATUS-05,          :D-RELAT-05,
                    :D-CURRENCY-05,        :D-ACCT-NO-05,
                    :D-SYST-05,            :D-ACCT-PER-OWN-05,
                    :D-LINK-CUA-ACNO-05,   :D-LINK-SYST-05,
                    :D-LINK-BRCH-05,       :D-OPEN-DATE-05,
                    :D-MOD-CLS-DATE-05,    :D-LINK-TYPE-05,
                    :D-LINK-DEPTH-05,      :D-LINK-ACCT-TYPE-05,
                    :D-LINK-CUST-NO-05,    :D-STATUS-06,
                    :D-RELAT-06,           :D-CURRENCY-06,
                    :D-ACCT-NO-06,         :D-SYST-06,
                    :D-ACCT-PER-OWN-06,    :D-LINK-CUA-ACNO-06,
                    :D-LINK-SYST-06,       :D-LINK-BRCH-06,
                    :D-OPEN-DATE-06,       :D-MOD-CLS-DATE-06,
                    :D-LINK-TYPE-06,       :D-LINK-DEPTH-06,
                    :D-LINK-ACCT-TYPE-06,  :D-LINK-CUST-NO-06,
                    :D-STATUS-07,          :D-RELAT-07,
                    :D-CURRENCY-07,        :D-ACCT-NO-07,
                    :D-SYST-07,            :D-ACCT-PER-OWN-07,
                    :D-LINK-CUA-ACNO-07,   :D-LINK-SYST-07,
                    :D-LINK-BRCH-07,       :D-OPEN-DATE-07,
                    :D-MOD-CLS-DATE-07,    :D-LINK-TYPE-07,
                    :D-LINK-DEPTH-07,      :D-LINK-ACCT-TYPE-07,
                    :D-LINK-CUST-NO-07,    :D-STATUS-08,
                    :D-RELAT-08,           :D-CURRENCY-08,
                    :D-ACCT-NO-08,         :D-SYST-08,
                    :D-ACCT-PER-OWN-08,    :D-LINK-CUA-ACNO-08,
                    :D-LINK-SYST-08,       :D-LINK-BRCH-08,
                    :D-OPEN-DATE-08,       :D-MOD-CLS-DATE-08,
                    :D-LINK-TYPE-08,       :D-LINK-DEPTH-08,
                    :D-LINK-ACCT-TYPE-08,  :D-LINK-CUST-NO-08)
           END-EXEC
           PERFORM F200-F2-SQLCODE
           .
       D400-UPDATE  SECTION.
       D400-START.
           EXEC SQL
             UPDATE CUSVCC
              SET CODE = :D-CODE,
                  DELI = :D-DELI,
                  NO_ASSOC = :D-NO-ASSOC,
                  STATUS_01 = :D-STATUS-01,
                  RELAT_01 = :D-RELAT-01,
                  CURRENCY_01 = :D-CURRENCY-01,
                  ACCT_NO_01 = :D-ACCT-NO-01,
                  SYST_01 = :D-SYST-01,
                  ACCT_PER_OWN_01 = :D-ACCT-PER-OWN-01,
                  LINK_CUA_ACNO_01 = :D-LINK-CUA-ACNO-01,
                  LINK_SYST_01 = :D-LINK-SYST-01,
                  LINK_BRCH_01 = :D-LINK-BRCH-01,
                  OPEN_DATE_01 = :D-OPEN-DATE-01,
                  MOD_CLS_DATE_01 = :D-MOD-CLS-DATE-01,
                  LINK_TYPE_01 = :D-LINK-TYPE-01,
                  LINK_DEPTH_01 = :D-LINK-DEPTH-01,
                  LINK_ACCT_TYPE_01 = :D-LINK-ACCT-TYPE-01,
                  LINK_CUST_NO_01 = :D-LINK-CUST-NO-01,
                  STATUS_02 = :D-STATUS-02,
                  RELAT_02 = :D-RELAT-02,
                  CURRENCY_02 = :D-CURRENCY-02,
                  ACCT_NO_02 = :D-ACCT-NO-02,
                  SYST_02 = :D-SYST-02,
                  ACCT_PER_OWN_02 = :D-ACCT-PER-OWN-02,
                  LINK_CUA_ACNO_02 = :D-LINK-CUA-ACNO-02,
                  LINK_SYST_02 = :D-LINK-SYST-02,
                  LINK_BRCH_02 = :D-LINK-BRCH-02,
                  OPEN_DATE_02 = :D-OPEN-DATE-02,
                  MOD_CLS_DATE_02 = :D-MOD-CLS-DATE-02,
                  LINK_TYPE_02 = :D-LINK-TYPE-02,
                  LINK_DEPTH_02 = :D-LINK-DEPTH-02,
                  LINK_ACCT_TYPE_02 = :D-LINK-ACCT-TYPE-02,
                  LINK_CUST_NO_02 = :D-LINK-CUST-NO-02,
                  STATUS_03 = :D-STATUS-03,
                  RELAT_03 = :D-RELAT-03,
                  CURRENCY_03 = :D-CURRENCY-03,
                  ACCT_NO_03 = :D-ACCT-NO-03,
                  SYST_03 = :D-SYST-03,
                  ACCT_PER_OWN_03 = :D-ACCT-PER-OWN-03,
                  LINK_CUA_ACNO_03 = :D-LINK-CUA-ACNO-03,
                  LINK_SYST_03 = :D-LINK-SYST-03,
                  LINK_BRCH_03 = :D-LINK-BRCH-03,
                  OPEN_DATE_03 = :D-OPEN-DATE-03,
                  MOD_CLS_DATE_03 = :D-MOD-CLS-DATE-03,
                  LINK_TYPE_03 = :D-LINK-TYPE-03,
                  LINK_DEPTH_03 = :D-LINK-DEPTH-03,
                  LINK_ACCT_TYPE_03 = :D-LINK-ACCT-TYPE-03,
                  LINK_CUST_NO_03 = :D-LINK-CUST-NO-03,
                  STATUS_04 = :D-STATUS-04,
                  RELAT_04 = :D-RELAT-04,
                  CURRENCY_04 = :D-CURRENCY-04,
                  ACCT_NO_04 = :D-ACCT-NO-04,
                  SYST_04 = :D-SYST-04,
                  ACCT_PER_OWN_04 = :D-ACCT-PER-OWN-04,
                  LINK_CUA_ACNO_04 = :D-LINK-CUA-ACNO-04,
                  LINK_SYST_04 = :D-LINK-SYST-04,
                  LINK_BRCH_04 = :D-LINK-BRCH-04,
                  OPEN_DATE_04 = :D-OPEN-DATE-04,
                  MOD_CLS_DATE_04 = :D-MOD-CLS-DATE-04,
                  LINK_TYPE_04 = :D-LINK-TYPE-04,
                  LINK_DEPTH_04 = :D-LINK-DEPTH-04,
                  LINK_ACCT_TYPE_04 = :D-LINK-ACCT-TYPE-04,
                  LINK_CUST_NO_04 = :D-LINK-CUST-NO-04,
                  STATUS_05 = :D-STATUS-05,
                  RELAT_05 = :D-RELAT-05,
                  CURRENCY_05 = :D-CURRENCY-05,
                  ACCT_NO_05 = :D-ACCT-NO-05,
                  SYST_05 = :D-SYST-05,
                  ACCT_PER_OWN_05 = :D-ACCT-PER-OWN-05,
                  LINK_CUA_ACNO_05 = :D-LINK-CUA-ACNO-05,
                  LINK_SYST_05 = :D-LINK-SYST-05,
                  LINK_BRCH_05 = :D-LINK-BRCH-05,
                  OPEN_DATE_05 = :D-OPEN-DATE-05,
                  MOD_CLS_DATE_05 = :D-MOD-CLS-DATE-05,
                  LINK_TYPE_05 = :D-LINK-TYPE-05,
                  LINK_DEPTH_05 = :D-LINK-DEPTH-05,
                  LINK_ACCT_TYPE_05 = :D-LINK-ACCT-TYPE-05,
                  LINK_CUST_NO_05 = :D-LINK-CUST-NO-05,
                  STATUS_06 = :D-STATUS-06,
                  RELAT_06 = :D-RELAT-06,
                  CURRENCY_06 = :D-CURRENCY-06,
                  ACCT_NO_06 = :D-ACCT-NO-06,
                  SYST_06 = :D-SYST-06,
                  ACCT_PER_OWN_06 = :D-ACCT-PER-OWN-06,
                  LINK_CUA_ACNO_06 = :D-LINK-CUA-ACNO-06,
                  LINK_SYST_06 = :D-LINK-SYST-06,
                  LINK_BRCH_06 = :D-LINK-BRCH-06,
                  OPEN_DATE_06 = :D-OPEN-DATE-06,
                  MOD_CLS_DATE_06 = :D-MOD-CLS-DATE-06,
                  LINK_TYPE_06 = :D-LINK-TYPE-06,
                  LINK_DEPTH_06 = :D-LINK-DEPTH-06,
                  LINK_ACCT_TYPE_06 = :D-LINK-ACCT-TYPE-06,
                  LINK_CUST_NO_06 = :D-LINK-CUST-NO-06,
                  STATUS_07 = :D-STATUS-07,
                  RELAT_07 = :D-RELAT-07,
                  CURRENCY_07 = :D-CURRENCY-07,
                  ACCT_NO_07 = :D-ACCT-NO-07,
                  SYST_07 = :D-SYST-07,
                  ACCT_PER_OWN_07 = :D-ACCT-PER-OWN-07,
                  LINK_CUA_ACNO_07 = :D-LINK-CUA-ACNO-07,
                  LINK_SYST_07 = :D-LINK-SYST-07,
                  LINK_BRCH_07 = :D-LINK-BRCH-07,
                  OPEN_DATE_07 = :D-OPEN-DATE-07,
                  MOD_CLS_DATE_07 = :D-MOD-CLS-DATE-07,
                  LINK_TYPE_07 = :D-LINK-TYPE-07,
                  LINK_DEPTH_07 = :D-LINK-DEPTH-07,
                  LINK_ACCT_TYPE_07 = :D-LINK-ACCT-TYPE-07,
                  LINK_CUST_NO_07 = :D-LINK-CUST-NO-07,
                  STATUS_08 = :D-STATUS-08,
                  RELAT_08 = :D-RELAT-08,
                  CURRENCY_08 = :D-CURRENCY-08,
                  ACCT_NO_08 = :D-ACCT-NO-08,
                  SYST_08 = :D-SYST-08,
                  ACCT_PER_OWN_08 = :D-ACCT-PER-OWN-08,
                  LINK_CUA_ACNO_08 = :D-LINK-CUA-ACNO-08,
                  LINK_SYST_08 = :D-LINK-SYST-08,
                  LINK_BRCH_08 = :D-LINK-BRCH-08,
                  OPEN_DATE_08 = :D-OPEN-DATE-08,
                  MOD_CLS_DATE_08 = :D-MOD-CLS-DATE-08,
                  LINK_TYPE_08 = :D-LINK-TYPE-08,
                  LINK_DEPTH_08 = :D-LINK-DEPTH-08,
                  LINK_ACCT_TYPE_08 = :D-LINK-ACCT-TYPE-08,
                  LINK_CUST_NO_08 = :D-LINK-CUST-NO-08
              WHERE  KEY_1 =  :D-PK-KEY-1
           END-EXEC
           .
       D500-SELECT-MAX SECTION.
       D500-START.
           IF DAY-MODE
           EXEC SQL
              SELECT MAX(KEY_1)
                INTO :MAX-KEY:MAX-IND
              FROM CUSVCC
              WHERE  KEY_1 LIKE :D-PK-KEY-1
           END-EXEC
           ELSE
           IF UT9004CA-MASTER-IN-USE-IS-1
           EXEC SQL
              SELECT MAX(KEY_1)
                INTO :MAX-KEY:MAX-IND
              FROM VIEW_1_CUSVCC_PK
              WHERE  KEY_1 LIKE :D-PK-KEY-1
           END-EXEC
           ELSE
           EXEC SQL
              SELECT MAX(KEY_1)
                INTO :MAX-KEY:MAX-IND
              FROM VIEW_2_CUSVCC_PK
              WHERE  KEY_1 LIKE :D-PK-KEY-1
           END-EXEC
           END-IF
           END-IF
           PERFORM F200-F2-SQLCODE
           IF IOMOD-F2-SQLCODE = 0
              IF MAX-IND = 0
                 MOVE MAX-KEY TO W-KEY-1
              ELSE
                 MOVE -1405 TO IOMOD-F2-SQLCODE
              END-IF
           END-IF
           .
       F100-F1-SQLCODE   SECTION.
       F100-START.
           MOVE SQLCODE TO IOMOD-F1-SQLCODE
ROHITH*****************START OF IR 24090001***********************
ROHITH     IF IOMOD-F1-SQLCODE = 100
ROHITH        MOVE 1403        TO IOMOD-F1-SQLCODE
ROHITH     END-IF
ROHITH*****************END   OF IR 24090001***********************
           IF IOMOD-F1-SQLCODE NOT = 0
              MOVE "ER"     TO IOMOD-F1-STAT
              MOVE "N"      TO OPEN-CURSOR-FLAG
           ELSE
              MOVE "OK"     TO IOMOD-F1-STAT
           END-IF
           .
       F200-F2-SQLCODE   SECTION.
       F200-START.
           MOVE SQLCODE TO IOMOD-F2-SQLCODE
ROHITH*****************START OF IR 24090001***********************
ROHITH     IF IOMOD-F2-SQLCODE = 100
ROHITH        MOVE 1403        TO IOMOD-F2-SQLCODE
ROHITH     END-IF
ROHITH*****************END   OF IR 24090001***********************
           IF IOMOD-F2-SQLCODE NOT = 0
              MOVE "ER"     TO IOMOD-F2-STAT
           ELSE
              MOVE "OK"     TO IOMOD-F2-STAT
              IF DATA-OUT
                 PERFORM M100-DATA-OUT
              END-IF
           END-IF
           .
       M000-DATA-IN      SECTION.
       M000-START.
           MOVE W-CODE               TO D-CODE
           MOVE W-DELI               TO D-DELI
           MOVE W-KEY-1              TO D-KEY-1
           MOVE W-NO-ASSOC           TO D-NO-ASSOC
           MOVE W-STATUS-01          TO D-STATUS-01
           MOVE W-RELAT-01           TO D-RELAT-01
           MOVE W-CURRENCY-01        TO D-CURRENCY-01
           MOVE W-ACCT-NO-01         TO D-ACCT-NO-01
           MOVE W-SYST-01            TO D-SYST-01
           MOVE W-ACCT-PER-OWN-01    TO D-ACCT-PER-OWN-01
           MOVE W-LINK-CUA-ACNO-01   TO D-LINK-CUA-ACNO-01
           MOVE W-LINK-SYST-01       TO D-LINK-SYST-01
           MOVE W-LINK-BRCH-01       TO D-LINK-BRCH-01
           MOVE W-OPEN-DATE-01       TO D-OPEN-DATE-01
           MOVE W-MOD-CLS-DATE-01    TO D-MOD-CLS-DATE-01
           MOVE W-LINK-TYPE-01       TO D-LINK-TYPE-01
           MOVE W-LINK-DEPTH-01      TO D-LINK-DEPTH-01
           MOVE W-LINK-ACCT-TYPE-01  TO D-LINK-ACCT-TYPE-01
           MOVE W-LINK-CUST-NO-01    TO D-LINK-CUST-NO-01
           MOVE W-STATUS-02          TO D-STATUS-02
           MOVE W-RELAT-02           TO D-RELAT-02
           MOVE W-CURRENCY-02        TO D-CURRENCY-02
           MOVE W-ACCT-NO-02         TO D-ACCT-NO-02
           MOVE W-SYST-02            TO D-SYST-02
           MOVE W-ACCT-PER-OWN-02    TO D-ACCT-PER-OWN-02
           MOVE W-LINK-CUA-ACNO-02   TO D-LINK-CUA-ACNO-02
           MOVE W-LINK-SYST-02       TO D-LINK-SYST-02
           MOVE W-LINK-BRCH-02       TO D-LINK-BRCH-02
           MOVE W-OPEN-DATE-02       TO D-OPEN-DATE-02
           MOVE W-MOD-CLS-DATE-02    TO D-MOD-CLS-DATE-02
           MOVE W-LINK-TYPE-02       TO D-LINK-TYPE-02
           MOVE W-LINK-DEPTH-02      TO D-LINK-DEPTH-02
           MOVE W-LINK-ACCT-TYPE-02  TO D-LINK-ACCT-TYPE-02
           MOVE W-LINK-CUST-NO-02    TO D-LINK-CUST-NO-02
           MOVE W-STATUS-03          TO D-STATUS-03
           MOVE W-RELAT-03           TO D-RELAT-03
           MOVE W-CURRENCY-03        TO D-CURRENCY-03
           MOVE W-ACCT-NO-03         TO D-ACCT-NO-03
           MOVE W-SYST-03            TO D-SYST-03
           MOVE W-ACCT-PER-OWN-03    TO D-ACCT-PER-OWN-03
           MOVE W-LINK-CUA-ACNO-03   TO D-LINK-CUA-ACNO-03
           MOVE W-LINK-SYST-03       TO D-LINK-SYST-03
           MOVE W-LINK-BRCH-03       TO D-LINK-BRCH-03
           MOVE W-OPEN-DATE-03       TO D-OPEN-DATE-03
           MOVE W-MOD-CLS-DATE-03    TO D-MOD-CLS-DATE-03
           MOVE W-LINK-TYPE-03       TO D-LINK-TYPE-03
           MOVE W-LINK-DEPTH-03      TO D-LINK-DEPTH-03
           MOVE W-LINK-ACCT-TYPE-03  TO D-LINK-ACCT-TYPE-03
           MOVE W-LINK-CUST-NO-03    TO D-LINK-CUST-NO-03
           MOVE W-STATUS-04          TO D-STATUS-04
           MOVE W-RELAT-04           TO D-RELAT-04
           MOVE W-CURRENCY-04        TO D-CURRENCY-04
           MOVE W-ACCT-NO-04         TO D-ACCT-NO-04
           MOVE W-SYST-04            TO D-SYST-04
           MOVE W-ACCT-PER-OWN-04    TO D-ACCT-PER-OWN-04
           MOVE W-LINK-CUA-ACNO-04   TO D-LINK-CUA-ACNO-04
           MOVE W-LINK-SYST-04       TO D-LINK-SYST-04
           MOVE W-LINK-BRCH-04       TO D-LINK-BRCH-04
           MOVE W-OPEN-DATE-04       TO D-OPEN-DATE-04
           MOVE W-MOD-CLS-DATE-04    TO D-MOD-CLS-DATE-04
           MOVE W-LINK-TYPE-04       TO D-LINK-TYPE-04
           MOVE W-LINK-DEPTH-04      TO D-LINK-DEPTH-04
           MOVE W-LINK-ACCT-TYPE-04  TO D-LINK-ACCT-TYPE-04
           MOVE W-LINK-CUST-NO-04    TO D-LINK-CUST-NO-04
           MOVE W-STATUS-05          TO D-STATUS-05
           MOVE W-RELAT-05           TO D-RELAT-05
           MOVE W-CURRENCY-05        TO D-CURRENCY-05
           MOVE W-ACCT-NO-05         TO D-ACCT-NO-05
           MOVE W-SYST-05            TO D-SYST-05
           MOVE W-ACCT-PER-OWN-05    TO D-ACCT-PER-OWN-05
           MOVE W-LINK-CUA-ACNO-05   TO D-LINK-CUA-ACNO-05
           MOVE W-LINK-SYST-05       TO D-LINK-SYST-05
           MOVE W-LINK-BRCH-05       TO D-LINK-BRCH-05
           MOVE W-OPEN-DATE-05       TO D-OPEN-DATE-05
           MOVE W-MOD-CLS-DATE-05    TO D-MOD-CLS-DATE-05
           MOVE W-LINK-TYPE-05       TO D-LINK-TYPE-05
           MOVE W-LINK-DEPTH-05      TO D-LINK-DEPTH-05
           MOVE W-LINK-ACCT-TYPE-05  TO D-LINK-ACCT-TYPE-05
           MOVE W-LINK-CUST-NO-05    TO D-LINK-CUST-NO-05
           MOVE W-STATUS-06          TO D-STATUS-06
           MOVE W-RELAT-06           TO D-RELAT-06
           MOVE W-CURRENCY-06        TO D-CURRENCY-06
           MOVE W-ACCT-NO-06         TO D-ACCT-NO-06
           MOVE W-SYST-06            TO D-SYST-06
           MOVE W-ACCT-PER-OWN-06    TO D-ACCT-PER-OWN-06
           MOVE W-LINK-CUA-ACNO-06   TO D-LINK-CUA-ACNO-06
           MOVE W-LINK-SYST-06       TO D-LINK-SYST-06
           MOVE W-LINK-BRCH-06       TO D-LINK-BRCH-06
           MOVE W-OPEN-DATE-06       TO D-OPEN-DATE-06
           MOVE W-MOD-CLS-DATE-06    TO D-MOD-CLS-DATE-06
           MOVE W-LINK-TYPE-06       TO D-LINK-TYPE-06
           MOVE W-LINK-DEPTH-06      TO D-LINK-DEPTH-06
           MOVE W-LINK-ACCT-TYPE-06  TO D-LINK-ACCT-TYPE-06
           MOVE W-LINK-CUST-NO-06    TO D-LINK-CUST-NO-06
           MOVE W-STATUS-07          TO D-STATUS-07
           MOVE W-RELAT-07           TO D-RELAT-07
           MOVE W-CURRENCY-07        TO D-CURRENCY-07
           MOVE W-ACCT-NO-07         TO D-ACCT-NO-07
           MOVE W-SYST-07            TO D-SYST-07
           MOVE W-ACCT-PER-OWN-07    TO D-ACCT-PER-OWN-07
           MOVE W-LINK-CUA-ACNO-07   TO D-LINK-CUA-ACNO-07
           MOVE W-LINK-SYST-07       TO D-LINK-SYST-07
           MOVE W-LINK-BRCH-07       TO D-LINK-BRCH-07
           MOVE W-OPEN-DATE-07       TO D-OPEN-DATE-07
           MOVE W-MOD-CLS-DATE-07    TO D-MOD-CLS-DATE-07
           MOVE W-LINK-TYPE-07       TO D-LINK-TYPE-07
           MOVE W-LINK-DEPTH-07      TO D-LINK-DEPTH-07
           MOVE W-LINK-ACCT-TYPE-07  TO D-LINK-ACCT-TYPE-07
           MOVE W-LINK-CUST-NO-07    TO D-LINK-CUST-NO-07
           MOVE W-STATUS-08          TO D-STATUS-08
           MOVE W-RELAT-08           TO D-RELAT-08
           MOVE W-CURRENCY-08        TO D-CURRENCY-08
           MOVE W-ACCT-NO-08         TO D-ACCT-NO-08
           MOVE W-SYST-08            TO D-SYST-08
           MOVE W-ACCT-PER-OWN-08    TO D-ACCT-PER-OWN-08
           MOVE W-LINK-CUA-ACNO-08   TO D-LINK-CUA-ACNO-08
           MOVE W-LINK-SYST-08       TO D-LINK-SYST-08
           MOVE W-LINK-BRCH-08       TO D-LINK-BRCH-08
           MOVE W-OPEN-DATE-08       TO D-OPEN-DATE-08
           MOVE W-MOD-CLS-DATE-08    TO D-MOD-CLS-DATE-08
           MOVE W-LINK-TYPE-08       TO D-LINK-TYPE-08
           MOVE W-LINK-DEPTH-08      TO D-LINK-DEPTH-08
           MOVE W-LINK-ACCT-TYPE-08  TO D-LINK-ACCT-TYPE-08
           MOVE W-LINK-CUST-NO-08    TO D-LINK-CUST-NO-08
           .
       M100-DATA-OUT     SECTION.
       M100-START.
           MOVE D-CODE               TO W-CODE
           MOVE D-DELI               TO W-DELI
           MOVE D-KEY-1              TO W-KEY-1
           MOVE D-NO-ASSOC           TO W-NO-ASSOC
           MOVE D-STATUS-01          TO W-STATUS-01
           MOVE D-RELAT-01           TO W-RELAT-01
           MOVE D-CURRENCY-01        TO W-CURRENCY-01
           MOVE D-ACCT-NO-01             TO W-ACCT-NO-01
           MOVE D-SYST-01            TO W-SYST-01
           MOVE D-ACCT-PER-OWN-01        TO W-ACCT-PER-OWN-01
           MOVE D-LINK-CUA-ACNO-01       TO W-LINK-CUA-ACNO-01
           MOVE D-LINK-SYST-01       TO W-LINK-SYST-01
           MOVE D-LINK-BRCH-01           TO W-LINK-BRCH-01
           MOVE D-OPEN-DATE-01           TO W-OPEN-DATE-01
           MOVE D-MOD-CLS-DATE-01        TO W-MOD-CLS-DATE-01
           MOVE D-LINK-TYPE-01       TO W-LINK-TYPE-01
           MOVE D-LINK-DEPTH-01      TO W-LINK-DEPTH-01
           MOVE D-LINK-ACCT-TYPE-01      TO W-LINK-ACCT-TYPE-01
           MOVE D-LINK-CUST-NO-01        TO W-LINK-CUST-NO-01
           MOVE D-STATUS-02          TO W-STATUS-02
           MOVE D-RELAT-02           TO W-RELAT-02
           MOVE D-CURRENCY-02        TO W-CURRENCY-02
           MOVE D-ACCT-NO-02             TO W-ACCT-NO-02
           MOVE D-SYST-02            TO W-SYST-02
           MOVE D-ACCT-PER-OWN-02        TO W-ACCT-PER-OWN-02
           MOVE D-LINK-CUA-ACNO-02       TO W-LINK-CUA-ACNO-02
           MOVE D-LINK-SYST-02       TO W-LINK-SYST-02
           MOVE D-LINK-BRCH-02           TO W-LINK-BRCH-02
           MOVE D-OPEN-DATE-02           TO W-OPEN-DATE-02
           MOVE D-MOD-CLS-DATE-02        TO W-MOD-CLS-DATE-02
           MOVE D-LINK-TYPE-02       TO W-LINK-TYPE-02
           MOVE D-LINK-DEPTH-02      TO W-LINK-DEPTH-02
           MOVE D-LINK-ACCT-TYPE-02      TO W-LINK-ACCT-TYPE-02
           MOVE D-LINK-CUST-NO-02        TO W-LINK-CUST-NO-02
           MOVE D-STATUS-03          TO W-STATUS-03
           MOVE D-RELAT-03           TO W-RELAT-03
           MOVE D-CURRENCY-03        TO W-CURRENCY-03
           MOVE D-ACCT-NO-03             TO W-ACCT-NO-03
           MOVE D-SYST-03            TO W-SYST-03
           MOVE D-ACCT-PER-OWN-03        TO W-ACCT-PER-OWN-03
           MOVE D-LINK-CUA-ACNO-03       TO W-LINK-CUA-ACNO-03
           MOVE D-LINK-SYST-03       TO W-LINK-SYST-03
           MOVE D-LINK-BRCH-03           TO W-LINK-BRCH-03
           MOVE D-OPEN-DATE-03           TO W-OPEN-DATE-03
           MOVE D-MOD-CLS-DATE-03        TO W-MOD-CLS-DATE-03
           MOVE D-LINK-TYPE-03       TO W-LINK-TYPE-03
           MOVE D-LINK-DEPTH-03      TO W-LINK-DEPTH-03
           MOVE D-LINK-ACCT-TYPE-03      TO W-LINK-ACCT-TYPE-03
           MOVE D-LINK-CUST-NO-03        TO W-LINK-CUST-NO-03
           MOVE D-STATUS-04          TO W-STATUS-04
           MOVE D-RELAT-04           TO W-RELAT-04
           MOVE D-CURRENCY-04        TO W-CURRENCY-04
           MOVE D-ACCT-NO-04             TO W-ACCT-NO-04
           MOVE D-SYST-04            TO W-SYST-04
           MOVE D-ACCT-PER-OWN-04        TO W-ACCT-PER-OWN-04
           MOVE D-LINK-CUA-ACNO-04       TO W-LINK-CUA-ACNO-04
           MOVE D-LINK-SYST-04       TO W-LINK-SYST-04
           MOVE D-LINK-BRCH-04           TO W-LINK-BRCH-04
           MOVE D-OPEN-DATE-04           TO W-OPEN-DATE-04
           MOVE D-MOD-CLS-DATE-04        TO W-MOD-CLS-DATE-04
           MOVE D-LINK-TYPE-04       TO W-LINK-TYPE-04
           MOVE D-LINK-DEPTH-04      TO W-LINK-DEPTH-04
           MOVE D-LINK-ACCT-TYPE-04      TO W-LINK-ACCT-TYPE-04
           MOVE D-LINK-CUST-NO-04        TO W-LINK-CUST-NO-04
           MOVE D-STATUS-05          TO W-STATUS-05
           MOVE D-RELAT-05           TO W-RELAT-05
           MOVE D-CURRENCY-05        TO W-CURRENCY-05
           MOVE D-ACCT-NO-05             TO W-ACCT-NO-05
           MOVE D-SYST-05            TO W-SYST-05
           MOVE D-ACCT-PER-OWN-05        TO W-ACCT-PER-OWN-05
           MOVE D-LINK-CUA-ACNO-05       TO W-LINK-CUA-ACNO-05
           MOVE D-LINK-SYST-05       TO W-LINK-SYST-05
           MOVE D-LINK-BRCH-05           TO W-LINK-BRCH-05
           MOVE D-OPEN-DATE-05           TO W-OPEN-DATE-05
           MOVE D-MOD-CLS-DATE-05        TO W-MOD-CLS-DATE-05
           MOVE D-LINK-TYPE-05       TO W-LINK-TYPE-05
           MOVE D-LINK-DEPTH-05      TO W-LINK-DEPTH-05
           MOVE D-LINK-ACCT-TYPE-05      TO W-LINK-ACCT-TYPE-05
           MOVE D-LINK-CUST-NO-05        TO W-LINK-CUST-NO-05
           MOVE D-STATUS-06          TO W-STATUS-06
           MOVE D-RELAT-06           TO W-RELAT-06
           MOVE D-CURRENCY-06        TO W-CURRENCY-06
           MOVE D-ACCT-NO-06             TO W-ACCT-NO-06
           MOVE D-SYST-06            TO W-SYST-06
           MOVE D-ACCT-PER-OWN-06        TO W-ACCT-PER-OWN-06
           MOVE D-LINK-CUA-ACNO-06       TO W-LINK-CUA-ACNO-06
           MOVE D-LINK-SYST-06       TO W-LINK-SYST-06
           MOVE D-LINK-BRCH-06           TO W-LINK-BRCH-06
           MOVE D-OPEN-DATE-06           TO W-OPEN-DATE-06
           MOVE D-MOD-CLS-DATE-06        TO W-MOD-CLS-DATE-06
           MOVE D-LINK-TYPE-06       TO W-LINK-TYPE-06
           MOVE D-LINK-DEPTH-06      TO W-LINK-DEPTH-06
           MOVE D-LINK-ACCT-TYPE-06      TO W-LINK-ACCT-TYPE-06
           MOVE D-LINK-CUST-NO-06        TO W-LINK-CUST-NO-06
           MOVE D-STATUS-07          TO W-STATUS-07
           MOVE D-RELAT-07           TO W-RELAT-07
           MOVE D-CURRENCY-07        TO W-CURRENCY-07
           MOVE D-ACCT-NO-07             TO W-ACCT-NO-07
           MOVE D-SYST-07            TO W-SYST-07
           MOVE D-ACCT-PER-OWN-07        TO W-ACCT-PER-OWN-07
           MOVE D-LINK-CUA-ACNO-07       TO W-LINK-CUA-ACNO-07
           MOVE D-LINK-SYST-07       TO W-LINK-SYST-07
           MOVE D-LINK-BRCH-07           TO W-LINK-BRCH-07
           MOVE D-OPEN-DATE-07           TO W-OPEN-DATE-07
           MOVE D-MOD-CLS-DATE-07        TO W-MOD-CLS-DATE-07
           MOVE D-LINK-TYPE-07       TO W-LINK-TYPE-07
           MOVE D-LINK-DEPTH-07      TO W-LINK-DEPTH-07
           MOVE D-LINK-ACCT-TYPE-07      TO W-LINK-ACCT-TYPE-07
           MOVE D-LINK-CUST-NO-07        TO W-LINK-CUST-NO-07
           MOVE D-STATUS-08          TO W-STATUS-08
           MOVE D-RELAT-08           TO W-RELAT-08
           MOVE D-CURRENCY-08        TO W-CURRENCY-08
           MOVE D-ACCT-NO-08             TO W-ACCT-NO-08
           MOVE D-SYST-08            TO W-SYST-08
           MOVE D-ACCT-PER-OWN-08        TO W-ACCT-PER-OWN-08
           MOVE D-LINK-CUA-ACNO-08       TO W-LINK-CUA-ACNO-08
           MOVE D-LINK-SYST-08       TO W-LINK-SYST-08
           MOVE D-LINK-BRCH-08           TO W-LINK-BRCH-08
           MOVE D-OPEN-DATE-08           TO W-OPEN-DATE-08
           MOVE D-MOD-CLS-DATE-08        TO W-MOD-CLS-DATE-08
           MOVE D-LINK-TYPE-08       TO W-LINK-TYPE-08
           MOVE D-LINK-DEPTH-08      TO W-LINK-DEPTH-08
           MOVE D-LINK-ACCT-TYPE-08      TO W-LINK-ACCT-TYPE-08
           MOVE D-LINK-CUST-NO-08        TO W-LINK-CUST-NO-08
           .
       M200-KI-PK     SECTION.
       M200-START-KI-PK.
           MOVE IOMOD-KEY TO IVAL-PK
           .

       M210-ID-PK     SECTION.
       M210-START-ID-PK.
           MOVE IV-PK-KEY-1              TO D-PK-KEY-1
           .

       M300-DATA-LEN     SECTION.
       M300-START.
           MOVE 002 TO D-CODE
           MOVE 001 TO D-DELI
           MOVE 023 TO D-KEY-1
           MOVE 002 TO D-NO-ASSOC
           MOVE 002 TO D-STATUS-01
           MOVE 002 TO D-RELAT-01
           MOVE 003 TO D-CURRENCY-01
           MOVE 001 TO D-SYST-01
           MOVE 001 TO D-LINK-SYST-01
           MOVE 001 TO D-LINK-TYPE-01
           MOVE 001 TO D-LINK-DEPTH-01
           MOVE 002 TO D-STATUS-02
           MOVE 002 TO D-RELAT-02
           MOVE 003 TO D-CURRENCY-02
           MOVE 001 TO D-SYST-02
           MOVE 001 TO D-LINK-SYST-02
           MOVE 001 TO D-LINK-TYPE-02
           MOVE 001 TO D-LINK-DEPTH-02
           MOVE 002 TO D-STATUS-03
           MOVE 002 TO D-RELAT-03
           MOVE 003 TO D-CURRENCY-03
           MOVE 001 TO D-SYST-03
           MOVE 001 TO D-LINK-SYST-03
           MOVE 001 TO D-LINK-TYPE-03
           MOVE 001 TO D-LINK-DEPTH-03
           MOVE 002 TO D-STATUS-04
           MOVE 002 TO D-RELAT-04
           MOVE 003 TO D-CURRENCY-04
           MOVE 001 TO D-SYST-04
           MOVE 001 TO D-LINK-SYST-04
           MOVE 001 TO D-LINK-TYPE-04
           MOVE 001 TO D-LINK-DEPTH-04
           MOVE 002 TO D-STATUS-05
           MOVE 002 TO D-RELAT-05
           MOVE 003 TO D-CURRENCY-05
           MOVE 001 TO D-SYST-05
           MOVE 001 TO D-LINK-SYST-05
           MOVE 001 TO D-LINK-TYPE-05
           MOVE 001 TO D-LINK-DEPTH-05
           MOVE 002 TO D-STATUS-06
           MOVE 002 TO D-RELAT-06
           MOVE 003 TO D-CURRENCY-06
           MOVE 001 TO D-SYST-06
           MOVE 001 TO D-LINK-SYST-06
           MOVE 001 TO D-LINK-TYPE-06
           MOVE 001 TO D-LINK-DEPTH-06
           MOVE 002 TO D-STATUS-07
           MOVE 002 TO D-RELAT-07
           MOVE 003 TO D-CURRENCY-07
           MOVE 001 TO D-SYST-07
           MOVE 001 TO D-LINK-SYST-07
           MOVE 001 TO D-LINK-TYPE-07
           MOVE 001 TO D-LINK-DEPTH-07
           MOVE 002 TO D-STATUS-08
           MOVE 002 TO D-RELAT-08
           MOVE 003 TO D-CURRENCY-08
           MOVE 001 TO D-SYST-08
           MOVE 001 TO D-LINK-SYST-08
           MOVE 001 TO D-LINK-TYPE-08
           MOVE 001 TO D-LINK-DEPTH-08
           MOVE 0023 TO D-PK-KEY-1
           .
       Z500-INIT-STATUS    SECTION.
       Z500-START.
           MOVE 0 TO IOMOD-F1-SQLCODE
                     IOMOD-F2-SQLCODE
                     IOMOD-F1-ERRL
                     IOMOD-F2-ERRL
           MOVE SPACES TO IOMOD-F1-STAT
                          IOMOD-F2-STAT
                          IOMOD-F1-FUNCT
                          IOMOD-F2-FUNCT
                          IOMOD-F1-ERRM
                          IOMOD-F2-ERRM
           .
