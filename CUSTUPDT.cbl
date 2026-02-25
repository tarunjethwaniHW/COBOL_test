       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTUPDT.
       AUTHOR. ACCT-MGMT-TEAM.
      *================================================================*
      * CUSTUPDT - CUSTOMER ACCOUNT UPDATE PROGRAM                     *
      * READS A TRANSACTION FILE OF BALANCE ADJUSTMENTS, VALIDATES     *
      * EACH VIA CUSTVALD, APPLIES UPDATES TO CUSTOMER MASTER FILE,   *
      * AND WRITES UPDATED RECORDS TO AN OUTPUT FILE.                  *
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANFILE ASSIGN TO 'TRANDATA'
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS WS-TRAN-STATUS.
           SELECT CUSTFILE ASSIGN TO 'CUSTDATA'
             ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM
             RECORD KEY IS CUST-ID
             FILE STATUS IS WS-CUST-STATUS.
           SELECT ERRFILE ASSIGN TO 'ERRDATA'
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS WS-ERR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD TRANFILE.
       01 TRAN-RECORD.
          05 TRAN-CUST-ID       PIC X(10).
          05 TRAN-TYPE          PIC X(01).
             88 TRAN-CREDIT     VALUE 'C'.
             88 TRAN-DEBIT      VALUE 'D'.
             88 TRAN-ADJUST     VALUE 'A'.
          05 TRAN-AMOUNT        PIC 9(7)V99.
          05 TRAN-REFERENCE     PIC X(20).

       FD CUSTFILE.
       COPY CUSTCOPY.

       FD ERRFILE.
       01 ERR-RECORD.
          05 ERR-CUST-ID        PIC X(10).
          05 ERR-TRAN-TYPE      PIC X(01).
          05 ERR-AMOUNT         PIC 9(7)V99.
          05 ERR-REASON         PIC X(60).
          05 ERR-DATE           PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-TRAN-STATUS        PIC XX VALUE SPACES.
       01 WS-CUST-STATUS        PIC XX VALUE SPACES.
       01 WS-ERR-STATUS         PIC XX VALUE SPACES.
       01 WS-EOF-FLAG           PIC X VALUE 'N'.
          88 WS-EOF             VALUE 'Y'.
       01 WS-TRAN-COUNT         PIC 9(07) VALUE 0.
       01 WS-UPDATE-COUNT       PIC 9(07) VALUE 0.
       01 WS-REJECT-COUNT       PIC 9(07) VALUE 0.
       01 WS-NEW-BALANCE        PIC 9(7)V99 VALUE 0.
       01 WS-CURRENT-DATE       PIC X(10).

       01 WS-VALID-STATUS       PIC X(01).
       01 WS-ERROR-MSG          PIC X(60).

       PROCEDURE DIVISION.
       MAIN-PROCESSING.
           PERFORM INIT-PARA.
           PERFORM PROCESS-TRANSACTIONS UNTIL WS-EOF.
           PERFORM CLEANUP-PARA.
           STOP RUN.

       INIT-PARA.
           OPEN INPUT TRANFILE.
           IF WS-TRAN-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING TRANFILE: ' WS-TRAN-STATUS
             STOP RUN
           END-IF.
           OPEN I-O CUSTFILE.
           IF WS-CUST-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING CUSTFILE: ' WS-CUST-STATUS
             CLOSE TRANFILE
             STOP RUN
           END-IF.
           OPEN OUTPUT ERRFILE.
           IF WS-ERR-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING ERRFILE: ' WS-ERR-STATUS
             CLOSE TRANFILE CUSTFILE
             STOP RUN
           END-IF.
           MOVE FUNCTION CURRENT-DATE(1:10) TO WS-CURRENT-DATE.
           MOVE 0 TO WS-TRAN-COUNT WS-UPDATE-COUNT WS-REJECT-COUNT.

       PROCESS-TRANSACTIONS.
           READ TRANFILE INTO TRAN-RECORD
             AT END SET WS-EOF TO TRUE
           END-READ.
           IF NOT WS-EOF
             ADD 1 TO WS-TRAN-COUNT
             PERFORM LOOKUP-CUSTOMER
             EVALUATE WS-CUST-STATUS
               WHEN '00'
                 PERFORM VALIDATE-AND-APPLY
               WHEN '23'
                 PERFORM REJECT-NOT-FOUND
               WHEN OTHER
                 PERFORM REJECT-FILE-ERROR
             END-EVALUATE
           END-IF.

       LOOKUP-CUSTOMER.
           MOVE TRAN-CUST-ID TO CUST-ID.
           READ CUSTFILE INTO CUST-RECORD
             INVALID KEY CONTINUE
           END-READ.

       VALIDATE-AND-APPLY.
           CALL 'CUSTVALD' USING CUST-RECORD
                                  WS-VALID-STATUS
                                  WS-ERROR-MSG.
           IF WS-VALID-STATUS = 'Y'
             PERFORM APPLY-TRANSACTION
           ELSE
             PERFORM WRITE-ERROR-RECORD
             ADD 1 TO WS-REJECT-COUNT
           END-IF.

       APPLY-TRANSACTION.
           EVALUATE TRUE
             WHEN TRAN-CREDIT
               ADD TRAN-AMOUNT TO CUST-BALANCE
             WHEN TRAN-DEBIT
               IF TRAN-AMOUNT > CUST-BALANCE
                 MOVE 'INSUFFICIENT BALANCE FOR DEBIT' TO
                   WS-ERROR-MSG
                 PERFORM WRITE-ERROR-RECORD
                 ADD 1 TO WS-REJECT-COUNT
                 GO TO APPLY-TRANSACTION-EXIT
               ELSE
                 SUBTRACT TRAN-AMOUNT FROM CUST-BALANCE
               END-IF
             WHEN TRAN-ADJUST
               MOVE TRAN-AMOUNT TO CUST-BALANCE
           END-EVALUATE.
           MOVE WS-CURRENT-DATE TO CUST-LAST-ACTIVITY.
           REWRITE CUST-RECORD.
           IF WS-CUST-STATUS = '00'
             ADD 1 TO WS-UPDATE-COUNT
             PERFORM LOG-UPDATE-TO-DB
           ELSE
             MOVE 'REWRITE FAILED' TO WS-ERROR-MSG
             PERFORM WRITE-ERROR-RECORD
             ADD 1 TO WS-REJECT-COUNT
           END-IF.
       APPLY-TRANSACTION-EXIT.
           EXIT.

       LOG-UPDATE-TO-DB.
           EXEC SQL
             INSERT INTO TRANSACTION_LOG
               (CUST_ID, TRAN_TYPE, TRAN_AMOUNT,
                OLD_BALANCE, NEW_BALANCE,
                TRAN_REF, TRAN_DATE, TRAN_PROGRAM)
             VALUES
               (:TRAN-CUST-ID, :TRAN-TYPE, :TRAN-AMOUNT,
                :WS-NEW-BALANCE, :CUST-BALANCE,
                :TRAN-REFERENCE, CURRENT DATE, 'CUSTUPDT')
           END-EXEC.

       REJECT-NOT-FOUND.
           MOVE 'CUSTOMER NOT FOUND IN MASTER FILE' TO WS-ERROR-MSG.
           PERFORM WRITE-ERROR-RECORD.
           ADD 1 TO WS-REJECT-COUNT.

       REJECT-FILE-ERROR.
           STRING 'FILE I/O ERROR: STATUS=' WS-CUST-STATUS
             DELIMITED BY SIZE INTO WS-ERROR-MSG.
           PERFORM WRITE-ERROR-RECORD.
           ADD 1 TO WS-REJECT-COUNT.

       WRITE-ERROR-RECORD.
           MOVE TRAN-CUST-ID TO ERR-CUST-ID.
           MOVE TRAN-TYPE TO ERR-TRAN-TYPE.
           MOVE TRAN-AMOUNT TO ERR-AMOUNT.
           MOVE WS-ERROR-MSG TO ERR-REASON.
           MOVE WS-CURRENT-DATE TO ERR-DATE.
           WRITE ERR-RECORD.

       CLEANUP-PARA.
           CLOSE TRANFILE CUSTFILE ERRFILE.
           EXEC SQL COMMIT END-EXEC.
           DISPLAY 'CUSTUPDT COMPLETE.'.
           DISPLAY '  TRANSACTIONS READ:    ' WS-TRAN-COUNT.
           DISPLAY '  UPDATES APPLIED:      ' WS-UPDATE-COUNT.
           DISPLAY '  TRANSACTIONS REJECTED:' WS-REJECT-COUNT.
