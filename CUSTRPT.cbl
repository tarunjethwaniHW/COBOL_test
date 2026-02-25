       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTRPT.
       AUTHOR. ACCT-MGMT-TEAM.
      *================================================================*
      * CUSTRPT - CUSTOMER REPORT GENERATOR SUBPROGRAM                 *
      * CALLED BY CUSTPROC (SUMMARY REPORT) AND CUSTARCH (ARCHIVE      *
      * REPORT). FORMATS AND WRITES REPORT LINES TO SYSPRINT.         *
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPTFILE ASSIGN TO 'SYSPRINT'
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD RPTFILE
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.
       01 RPT-LINE              PIC X(132).

       WORKING-STORAGE SECTION.
       01 WS-RPT-STATUS         PIC XX VALUE SPACES.
       01 WS-LINE-COUNT         PIC 9(03) VALUE 0.
       01 WS-PAGE-NUM           PIC 9(04) VALUE 0.
       01 WS-LINES-PER-PAGE     PIC 9(03) VALUE 55.
       01 WS-CURRENT-DATE       PIC X(10).
       01 WS-FORMATTED-BAL      PIC ZZZ,ZZZ,ZZ9.99.
       01 WS-FORMATTED-CNT      PIC ZZZ,ZZ9.
       01 WS-FORMATTED-PCT      PIC ZZ9.99.
       01 WS-HIGH-VALUE-PCT     PIC 9(5)V99 VALUE 0.

       01 WS-HEADER-1.
          05 FILLER             PIC X(01) VALUE SPACES.
          05 FILLER             PIC X(40).
          05 FILLER             PIC X(30)
             VALUE '  CUSTOMER ACCOUNT MANAGEMENT'.
          05 FILLER             PIC X(20) VALUE SPACES.
          05 HDR1-DATE          PIC X(10).
          05 FILLER             PIC X(06) VALUE ' PAGE '.
          05 HDR1-PAGE          PIC ZZZ9.
          05 FILLER             PIC X(21) VALUE SPACES.

       01 WS-HEADER-2.
          05 FILLER             PIC X(01) VALUE SPACES.
          05 HDR2-TITLE         PIC X(40).
          05 FILLER             PIC X(91) VALUE SPACES.

       01 WS-HEADER-3.
          05 FILLER             PIC X(01) VALUE SPACES.
          05 FILLER             PIC X(131) VALUE ALL '-'.

       01 WS-DETAIL-SUMMARY.
          05 FILLER             PIC X(05) VALUE SPACES.
          05 DTL-LABEL          PIC X(40).
          05 FILLER             PIC X(05) VALUE SPACES.
          05 DTL-VALUE          PIC X(20).
          05 FILLER             PIC X(62) VALUE SPACES.

       LINKAGE SECTION.
       01 LS-RPT-TYPE           PIC X(01).
       01 LS-RPT-TITLE          PIC X(40).
       01 LS-RECORD-COUNT       PIC 9(07).
       01 LS-TOTAL-BALANCE      PIC 9(11)V99.
       01 LS-HIGH-COUNT         PIC 9(07).
       01 LS-ERROR-COUNT        PIC 9(07).
       01 LS-AVERAGE-BAL        PIC 9(9)V99.

       PROCEDURE DIVISION USING LS-RPT-TYPE
                                LS-RPT-TITLE
                                LS-RECORD-COUNT
                                LS-TOTAL-BALANCE
                                LS-HIGH-COUNT
                                LS-ERROR-COUNT
                                LS-AVERAGE-BAL.
       MAIN-REPORT.
           PERFORM INIT-REPORT.
           PERFORM WRITE-HEADERS.
           EVALUATE LS-RPT-TYPE
             WHEN 'S'
               PERFORM WRITE-PROCESSING-SUMMARY
             WHEN 'A'
               PERFORM WRITE-ARCHIVE-SUMMARY
             WHEN OTHER
               PERFORM WRITE-PROCESSING-SUMMARY
           END-EVALUATE.
           PERFORM WRITE-FOOTER.
           PERFORM CLOSE-REPORT.
           GOBACK.

       INIT-REPORT.
           OPEN OUTPUT RPTFILE.
           IF WS-RPT-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING REPORT FILE: ' WS-RPT-STATUS
             GOBACK
           END-IF.
           MOVE FUNCTION CURRENT-DATE(1:10) TO WS-CURRENT-DATE.
           MOVE 0 TO WS-PAGE-NUM WS-LINE-COUNT.

       WRITE-HEADERS.
           ADD 1 TO WS-PAGE-NUM.
           MOVE WS-CURRENT-DATE TO HDR1-DATE.
           MOVE WS-PAGE-NUM TO HDR1-PAGE.
           WRITE RPT-LINE FROM WS-HEADER-1 AFTER PAGE.
           MOVE LS-RPT-TITLE TO HDR2-TITLE.
           WRITE RPT-LINE FROM WS-HEADER-2 AFTER 1.
           WRITE RPT-LINE FROM WS-HEADER-3 AFTER 1.
           MOVE SPACES TO RPT-LINE.
           WRITE RPT-LINE AFTER 1.
           MOVE 4 TO WS-LINE-COUNT.

       WRITE-PROCESSING-SUMMARY.
           MOVE 'RECORDS PROCESSED:' TO DTL-LABEL.
           MOVE LS-RECORD-COUNT TO WS-FORMATTED-CNT.
           MOVE WS-FORMATTED-CNT TO DTL-VALUE.
           WRITE RPT-LINE FROM WS-DETAIL-SUMMARY AFTER 1.
           ADD 1 TO WS-LINE-COUNT.

           MOVE 'TOTAL BALANCE:' TO DTL-LABEL.
           MOVE LS-TOTAL-BALANCE TO WS-FORMATTED-BAL.
           MOVE WS-FORMATTED-BAL TO DTL-VALUE.
           WRITE RPT-LINE FROM WS-DETAIL-SUMMARY AFTER 1.
           ADD 1 TO WS-LINE-COUNT.

           MOVE 'AVERAGE BALANCE:' TO DTL-LABEL.
           MOVE LS-AVERAGE-BAL TO WS-FORMATTED-BAL.
           MOVE WS-FORMATTED-BAL TO DTL-VALUE.
           WRITE RPT-LINE FROM WS-DETAIL-SUMMARY AFTER 1.
           ADD 1 TO WS-LINE-COUNT.

           MOVE 'HIGH-VALUE CUSTOMERS:' TO DTL-LABEL.
           MOVE LS-HIGH-COUNT TO WS-FORMATTED-CNT.
           MOVE WS-FORMATTED-CNT TO DTL-VALUE.
           WRITE RPT-LINE FROM WS-DETAIL-SUMMARY AFTER 1.
           ADD 1 TO WS-LINE-COUNT.

           IF LS-RECORD-COUNT > 0
             COMPUTE WS-HIGH-VALUE-PCT =
               (LS-HIGH-COUNT / LS-RECORD-COUNT) * 100
             MOVE WS-HIGH-VALUE-PCT TO WS-FORMATTED-PCT
             STRING WS-FORMATTED-PCT '%' DELIMITED BY SIZE
               INTO DTL-VALUE
             MOVE 'HIGH-VALUE PERCENTAGE:' TO DTL-LABEL
             WRITE RPT-LINE FROM WS-DETAIL-SUMMARY AFTER 1
             ADD 1 TO WS-LINE-COUNT
           END-IF.

           MOVE 'VALIDATION ERRORS:' TO DTL-LABEL.
           MOVE LS-ERROR-COUNT TO WS-FORMATTED-CNT.
           MOVE WS-FORMATTED-CNT TO DTL-VALUE.
           WRITE RPT-LINE FROM WS-DETAIL-SUMMARY AFTER 1.
           ADD 1 TO WS-LINE-COUNT.

           PERFORM LOG-REPORT-GENERATION.

       WRITE-ARCHIVE-SUMMARY.
           MOVE 'RECORDS ARCHIVED:' TO DTL-LABEL.
           MOVE LS-RECORD-COUNT TO WS-FORMATTED-CNT.
           MOVE WS-FORMATTED-CNT TO DTL-VALUE.
           WRITE RPT-LINE FROM WS-DETAIL-SUMMARY AFTER 1.
           ADD 1 TO WS-LINE-COUNT.

           MOVE 'ARCHIVED BALANCE TOTAL:' TO DTL-LABEL.
           MOVE LS-TOTAL-BALANCE TO WS-FORMATTED-BAL.
           MOVE WS-FORMATTED-BAL TO DTL-VALUE.
           WRITE RPT-LINE FROM WS-DETAIL-SUMMARY AFTER 1.
           ADD 1 TO WS-LINE-COUNT.

           MOVE 'INACTIVE HIGH-VALUE:' TO DTL-LABEL.
           MOVE LS-HIGH-COUNT TO WS-FORMATTED-CNT.
           MOVE WS-FORMATTED-CNT TO DTL-VALUE.
           WRITE RPT-LINE FROM WS-DETAIL-SUMMARY AFTER 1.
           ADD 1 TO WS-LINE-COUNT.

           PERFORM LOG-REPORT-GENERATION.

       LOG-REPORT-GENERATION.
           EXEC SQL
             INSERT INTO REPORT_LOG
               (RPT_TYPE, RPT_TITLE, RECORD_COUNT,
                RPT_DATE, RPT_PROGRAM)
             VALUES
               (:LS-RPT-TYPE, :LS-RPT-TITLE, :LS-RECORD-COUNT,
                CURRENT DATE, 'CUSTRPT')
           END-EXEC.

       WRITE-FOOTER.
           MOVE SPACES TO RPT-LINE.
           WRITE RPT-LINE AFTER 1.
           WRITE RPT-LINE FROM WS-HEADER-3 AFTER 1.
           MOVE SPACES TO RPT-LINE.
           STRING '*** END OF REPORT - GENERATED BY CUSTRPT ON '
                  WS-CURRENT-DATE ' ***'
             DELIMITED BY SIZE INTO RPT-LINE.
           WRITE RPT-LINE AFTER 1.

       CLOSE-REPORT.
           CLOSE RPTFILE.
