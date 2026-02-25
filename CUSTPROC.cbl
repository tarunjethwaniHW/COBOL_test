       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPROC.
       AUTHOR. ACCT-MGMT-TEAM.
      *================================================================*
      * CUSTPROC - CUSTOMER BALANCE PROCESSOR (MAIN BATCH DRIVER)      *
      * READS CUSTOMER MASTER FILE, VALIDATES EACH RECORD VIA          *
      * CUSTVALD, AUDITS BALANCES TO DB2, FLAGS HIGH-VALUE CUSTOMERS,  *
      * AND DELEGATES REPORT GENERATION TO CUSTRPT.                    *
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTFILE ASSIGN TO 'CUSTDATA'
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTFILE.
       COPY CUSTCOPY REPLACING ==:PREFIX:== BY ==CUST==.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS        PIC XX VALUE SPACES.
       01 WS-EOF-FLAG           PIC X VALUE 'N'.
          88 WS-EOF             VALUE 'Y'.
       01 WS-TOTAL              PIC 9(11)V99 VALUE 0.
       01 WS-COUNT              PIC 9(07) VALUE 0.
       01 WS-HIGH-VALUE-COUNT   PIC 9(07) VALUE 0.
       01 WS-ERROR-COUNT        PIC 9(07) VALUE 0.
       01 WS-AVERAGE            PIC 9(9)V99 VALUE 0.
       01 WS-HIGH-VALUE-LIMIT   PIC 9(7)V99 VALUE 10000.00.

       01 WS-VALID-STATUS       PIC X(01).
          88 WS-REC-VALID       VALUE 'Y'.
          88 WS-REC-INVALID     VALUE 'N'.
       01 WS-ERROR-MSG          PIC X(60).

       01 WS-RPT-TYPE           PIC X(01).
       01 WS-RPT-TITLE          PIC X(40).

       PROCEDURE DIVISION.
       MAIN-PROCESSING.
           PERFORM INIT-PARA.
           PERFORM PROCESS-FILE UNTIL WS-EOF.
           PERFORM CALC-AVERAGE.
           PERFORM GENERATE-REPORT.
           PERFORM CLEANUP-PARA.
           STOP RUN.

       INIT-PARA.
           OPEN INPUT CUSTFILE.
           IF WS-FILE-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING CUSTFILE: ' WS-FILE-STATUS
             STOP RUN
           END-IF.
           MOVE 0 TO WS-TOTAL WS-COUNT WS-HIGH-VALUE-COUNT
                      WS-ERROR-COUNT.

       PROCESS-FILE.
           READ CUSTFILE INTO CUST-RECORD
             AT END SET WS-EOF TO TRUE
           END-READ.
           IF NOT WS-EOF
             PERFORM VALIDATE-RECORD
             IF WS-REC-VALID
               ADD CUST-BALANCE TO WS-TOTAL
               ADD 1 TO WS-COUNT
               PERFORM AUDIT-BALANCE
               IF CUST-BALANCE > WS-HIGH-VALUE-LIMIT
                 ADD 1 TO WS-HIGH-VALUE-COUNT
                 DISPLAY 'HIGH VALUE CUSTOMER: ' CUST-NAME
                 PERFORM FLAG-HIGH-VALUE
               END-IF
             ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'INVALID RECORD SKIPPED: ' CUST-ID
                       ' - ' WS-ERROR-MSG
             END-IF
           END-IF.

       VALIDATE-RECORD.
           CALL 'CUSTVALD' USING CUST-RECORD
                                  WS-VALID-STATUS
                                  WS-ERROR-MSG.

       AUDIT-BALANCE.
           EXEC SQL
             INSERT INTO AUDIT_LOG
               (CUST_ID, CUST_NAME, BALANCE, ACCT_TYPE,
                PROC_DATE, PROC_PROGRAM)
             VALUES
               (:CUST-ID, :CUST-NAME, :CUST-BALANCE,
                :CUST-ACCT-TYPE, CURRENT DATE, 'CUSTPROC')
           END-EXEC.

       FLAG-HIGH-VALUE.
           EXEC SQL
             UPDATE CUSTOMER_FLAGS
             SET HIGH_VALUE = 'Y',
                 FLAG_DATE = CURRENT DATE,
                 FLAGGED_BY = 'CUSTPROC'
             WHERE CUSTOMER_ID = :CUST-ID
           END-EXEC.

       CALC-AVERAGE.
           IF WS-COUNT > 0
             COMPUTE WS-AVERAGE = WS-TOTAL / WS-COUNT
             DISPLAY 'AVERAGE BALANCE: ' WS-AVERAGE
           ELSE
             DISPLAY 'NO VALID RECORDS PROCESSED'
           END-IF.

       GENERATE-REPORT.
           MOVE 'S' TO WS-RPT-TYPE.
           MOVE 'CUSTOMER BALANCE PROCESSING SUMMARY' TO WS-RPT-TITLE.
           CALL 'CUSTRPT' USING WS-RPT-TYPE
                                 WS-RPT-TITLE
                                 WS-COUNT
                                 WS-TOTAL
                                 WS-HIGH-VALUE-COUNT
                                 WS-ERROR-COUNT
                                 WS-AVERAGE.

       CLEANUP-PARA.
           CLOSE CUSTFILE.
           EXEC SQL COMMIT END-EXEC.
           DISPLAY 'CUSTPROC COMPLETE. RECORDS: ' WS-COUNT
                   ' ERRORS: ' WS-ERROR-COUNT.
