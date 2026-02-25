       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTARCH.
       AUTHOR. ACCT-MGMT-TEAM.
      *================================================================*
      * CUSTARCH - CUSTOMER DATA ARCHIVER                              *
      * READS THE CUSTOMER MASTER FILE, IDENTIFIES INACTIVE ACCOUNTS   *
      * (NO ACTIVITY IN LAST 365 DAYS), MOVES THEM TO AN ARCHIVE      *
      * FILE, DELETES FROM ACTIVE DB2 TABLES, AND CALLS CUSTRPT FOR   *
      * AN ARCHIVE SUMMARY REPORT.                                     *
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTFILE ASSIGN TO 'CUSTDATA'
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS WS-CUST-STATUS.
           SELECT ARCHFILE ASSIGN TO 'ARCHDATA'
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS WS-ARCH-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTFILE.
       COPY CUSTCOPY.

       FD ARCHFILE.
       01 ARCH-RECORD.
          05 ARCH-CUST-ID       PIC X(10).
          05 ARCH-CUST-NAME     PIC X(30).
          05 ARCH-BALANCE       PIC 9(7)V99.
          05 ARCH-ACCT-TYPE     PIC X(01).
          05 ARCH-STATUS        PIC X(01).
          05 ARCH-LAST-ACTIVITY PIC X(10).
          05 ARCH-OPEN-DATE     PIC X(10).
          05 ARCH-DATE          PIC X(10).
          05 ARCH-REASON        PIC X(20).

       WORKING-STORAGE SECTION.
       01 WS-CUST-STATUS        PIC XX VALUE SPACES.
       01 WS-ARCH-STATUS        PIC XX VALUE SPACES.
       01 WS-EOF-FLAG           PIC X VALUE 'N'.
          88 WS-EOF             VALUE 'Y'.
       01 WS-CURRENT-DATE       PIC X(10).
       01 WS-CUTOFF-DATE        PIC X(10).
       01 WS-READ-COUNT         PIC 9(07) VALUE 0.
       01 WS-ARCH-COUNT         PIC 9(07) VALUE 0.
       01 WS-SKIP-COUNT         PIC 9(07) VALUE 0.
       01 WS-ARCH-BALANCE-TOT   PIC 9(11)V99 VALUE 0.
       01 WS-HIGH-ARCH-COUNT    PIC 9(07) VALUE 0.
       01 WS-HIGH-VALUE-LIMIT   PIC 9(7)V99 VALUE 10000.00.
       01 WS-ERROR-COUNT        PIC 9(07) VALUE 0.
       01 WS-AVERAGE-ARCH-BAL   PIC 9(9)V99 VALUE 0.

       01 WS-DATE-WORK.
          05 WS-DATE-YYYY       PIC 9(04).
          05 WS-DATE-MM         PIC 9(02).
          05 WS-DATE-DD         PIC 9(02).

       01 WS-RPT-TYPE           PIC X(01).
       01 WS-RPT-TITLE          PIC X(40).

       PROCEDURE DIVISION.
       MAIN-PROCESSING.
           PERFORM INIT-PARA.
           PERFORM PROCESS-CUSTOMERS UNTIL WS-EOF.
           PERFORM CALC-ARCHIVE-STATS.
           PERFORM GENERATE-ARCHIVE-REPORT.
           PERFORM CLEANUP-PARA.
           STOP RUN.

       INIT-PARA.
           OPEN INPUT CUSTFILE.
           IF WS-CUST-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING CUSTFILE: ' WS-CUST-STATUS
             STOP RUN
           END-IF.
           OPEN OUTPUT ARCHFILE.
           IF WS-ARCH-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING ARCHFILE: ' WS-ARCH-STATUS
             CLOSE CUSTFILE
             STOP RUN
           END-IF.
           MOVE FUNCTION CURRENT-DATE(1:10) TO WS-CURRENT-DATE.
           PERFORM CALCULATE-CUTOFF-DATE.
           MOVE 0 TO WS-READ-COUNT WS-ARCH-COUNT WS-SKIP-COUNT
                      WS-ARCH-BALANCE-TOT WS-HIGH-ARCH-COUNT
                      WS-ERROR-COUNT.

       CALCULATE-CUTOFF-DATE.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WS-DATE-YYYY.
           MOVE FUNCTION CURRENT-DATE(5:2) TO WS-DATE-MM.
           MOVE FUNCTION CURRENT-DATE(7:2) TO WS-DATE-DD.
           SUBTRACT 1 FROM WS-DATE-YYYY.
           STRING WS-DATE-YYYY '-' WS-DATE-MM '-' WS-DATE-DD
             DELIMITED BY SIZE INTO WS-CUTOFF-DATE.
           DISPLAY 'ARCHIVE CUTOFF DATE: ' WS-CUTOFF-DATE.

       PROCESS-CUSTOMERS.
           READ CUSTFILE INTO CUST-RECORD
             AT END SET WS-EOF TO TRUE
           END-READ.
           IF NOT WS-EOF
             ADD 1 TO WS-READ-COUNT
             EVALUATE TRUE
               WHEN CUST-INACTIVE
                 PERFORM ARCHIVE-CUSTOMER
               WHEN CUST-CLOSED
                 PERFORM ARCHIVE-CUSTOMER
               WHEN CUST-ACTIVE
                 IF CUST-LAST-ACTIVITY < WS-CUTOFF-DATE
                   PERFORM ARCHIVE-CUSTOMER
                 ELSE
                   ADD 1 TO WS-SKIP-COUNT
                 END-IF
               WHEN OTHER
                 ADD 1 TO WS-SKIP-COUNT
             END-EVALUATE
           END-IF.

       ARCHIVE-CUSTOMER.
           MOVE CUST-ID TO ARCH-CUST-ID.
           MOVE CUST-NAME TO ARCH-CUST-NAME.
           MOVE CUST-BALANCE TO ARCH-BALANCE.
           MOVE CUST-ACCT-TYPE TO ARCH-ACCT-TYPE.
           MOVE CUST-STATUS TO ARCH-STATUS.
           MOVE CUST-LAST-ACTIVITY TO ARCH-LAST-ACTIVITY.
           MOVE CUST-OPEN-DATE TO ARCH-OPEN-DATE.
           MOVE WS-CURRENT-DATE TO ARCH-DATE.
           IF CUST-INACTIVE
             MOVE 'INACTIVE ACCOUNT' TO ARCH-REASON
           ELSE IF CUST-CLOSED
             MOVE 'CLOSED ACCOUNT' TO ARCH-REASON
           ELSE
             MOVE 'NO RECENT ACTIVITY' TO ARCH-REASON
           END-IF.
           WRITE ARCH-RECORD.
           IF WS-ARCH-STATUS = '00'
             ADD 1 TO WS-ARCH-COUNT
             ADD CUST-BALANCE TO WS-ARCH-BALANCE-TOT
             IF CUST-BALANCE > WS-HIGH-VALUE-LIMIT
               ADD 1 TO WS-HIGH-ARCH-COUNT
             END-IF
             PERFORM DELETE-FROM-ACTIVE-TABLES
           ELSE
             DISPLAY 'ERROR WRITING ARCHIVE: ' WS-ARCH-STATUS
             ADD 1 TO WS-ERROR-COUNT
           END-IF.

       DELETE-FROM-ACTIVE-TABLES.
           EXEC SQL
             INSERT INTO ARCHIVE_AUDIT
               (CUST_ID, CUST_NAME, BALANCE, ARCHIVE_DATE,
                ARCHIVE_REASON, ARCHIVED_BY)
             VALUES
               (:CUST-ID, :CUST-NAME, :CUST-BALANCE,
                CURRENT DATE, :ARCH-REASON, 'CUSTARCH')
           END-EXEC.
           EXEC SQL
             DELETE FROM CUSTOMER_FLAGS
             WHERE CUSTOMER_ID = :CUST-ID
           END-EXEC.
           EXEC SQL
             UPDATE CUSTOMER_MASTER
             SET ACCT_STATUS = 'A',
                 ARCHIVE_DATE = CURRENT DATE
             WHERE CUSTOMER_ID = :CUST-ID
           END-EXEC.

       CALC-ARCHIVE-STATS.
           IF WS-ARCH-COUNT > 0
             COMPUTE WS-AVERAGE-ARCH-BAL =
               WS-ARCH-BALANCE-TOT / WS-ARCH-COUNT
           END-IF.

       GENERATE-ARCHIVE-REPORT.
           MOVE 'A' TO WS-RPT-TYPE.
           MOVE 'CUSTOMER ARCHIVE SUMMARY REPORT' TO WS-RPT-TITLE.
           CALL 'CUSTRPT' USING WS-RPT-TYPE
                                 WS-RPT-TITLE
                                 WS-ARCH-COUNT
                                 WS-ARCH-BALANCE-TOT
                                 WS-HIGH-ARCH-COUNT
                                 WS-ERROR-COUNT
                                 WS-AVERAGE-ARCH-BAL.

       CLEANUP-PARA.
           CLOSE CUSTFILE ARCHFILE.
           EXEC SQL COMMIT END-EXEC.
           DISPLAY 'CUSTARCH COMPLETE.'.
           DISPLAY '  RECORDS READ:     ' WS-READ-COUNT.
           DISPLAY '  RECORDS ARCHIVED: ' WS-ARCH-COUNT.
           DISPLAY '  RECORDS SKIPPED:  ' WS-SKIP-COUNT.
           DISPLAY '  ERRORS:           ' WS-ERROR-COUNT.
