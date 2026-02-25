       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST001.
       AUTHOR. PARSER-AGENT-TEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTFILE ASSIGN TO 'CUSTDATA'
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTFILE.
       01 CUST-RECORD.
          05 CUST-ID        PIC X(10).
          05 CUST-NAME      PIC X(30).
          05 CUST-BALANCE   PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS    PIC XX VALUE SPACES.
       01 WS-EOF-FLAG       PIC X VALUE 'N'.
          88 WS-EOF         VALUE 'Y'.
       01 WS-TOTAL          PIC 9(9)V99 VALUE 0.
       01 WS-COUNT          PIC 9(5) VALUE 0.
       01 WS-AVERAGE        PIC 9(7)V99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCESSING.
           PERFORM INIT-PARA.
           PERFORM PROCESS-FILE UNTIL WS-EOF.
           PERFORM CALC-AVERAGE.
           PERFORM CLEANUP-PARA.
           STOP RUN.

       INIT-PARA.
           OPEN INPUT CUSTFILE.
           IF WS-FILE-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING FILE: ' WS-FILE-STATUS
             STOP RUN
           END-IF.

       PROCESS-FILE.
           READ CUSTFILE INTO CUST-RECORD
             AT END SET WS-EOF TO TRUE
           END-READ.
           IF NOT WS-EOF
             ADD CUST-BALANCE TO WS-TOTAL
             ADD 1 TO WS-COUNT
             IF CUST-BALANCE > 10000
               DISPLAY 'HIGH VALUE CUSTOMER: ' CUST-NAME
               PERFORM LOG-HIGH-VALUE
             END-IF
             EXEC SQL
               INSERT INTO AUDIT_LOG (CUST_ID, BALANCE, PROC_DATE)
               VALUES (:CUST-ID, :CUST-BALANCE, CURRENT DATE)
             END-EXEC
           END-IF.

       CALC-AVERAGE.
           IF WS-COUNT > 0
             COMPUTE WS-AVERAGE = WS-TOTAL / WS-COUNT
             DISPLAY 'AVERAGE BALANCE: ' WS-AVERAGE
           ELSE
             DISPLAY 'NO RECORDS PROCESSED'
           END-IF.

       LOG-HIGH-VALUE.
           EXEC SQL
             UPDATE CUSTOMER_FLAGS
             SET HIGH_VALUE = 'Y'
             WHERE CUSTOMER_ID = :CUST-ID
           END-EXEC.

       CLEANUP-PARA.
           CLOSE CUSTFILE.
           EXEC SQL COMMIT END-EXEC.
           DISPLAY 'PROCESSING COMPLETE. RECORDS: ' WS-COUNT.
