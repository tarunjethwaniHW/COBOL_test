       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTVALD.
       AUTHOR. ACCT-MGMT-TEAM.
      *================================================================*
      * CUSTVALD - CUSTOMER RECORD VALIDATION SUBPROGRAM               *
      * CALLED BY CUSTPROC AND CUSTUPDT TO VALIDATE CUSTOMER RECORDS   *
      * BEFORE PROCESSING. CHECKS ID FORMAT, BALANCE RANGE, ACCOUNT    *
      * TYPE, AND VERIFIES CUSTOMER EXISTS IN CUSTOMER_MASTER TABLE.   *
      *================================================================*

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DB-CUST-COUNT     PIC 9(05) VALUE 0.
       01 WS-NUMERIC-CHECK      PIC X(10).
       01 WS-BALANCE-LIMIT      PIC 9(7)V99 VALUE 9999999.99.

       LINKAGE SECTION.
       01 LS-CUST-RECORD.
          05 LS-CUST-ID         PIC X(10).
          05 LS-CUST-NAME       PIC X(30).
          05 LS-CUST-BALANCE    PIC 9(7)V99.
          05 LS-CUST-ACCT-TYPE  PIC X(01).
          05 LS-CUST-STATUS     PIC X(01).
          05 LS-CUST-LAST-ACT   PIC X(10).
          05 LS-CUST-OPEN-DATE  PIC X(10).
       01 LS-VALID-STATUS       PIC X(01).
       01 LS-ERROR-MSG          PIC X(60).

       PROCEDURE DIVISION USING LS-CUST-RECORD
                                LS-VALID-STATUS
                                LS-ERROR-MSG.
       MAIN-VALIDATION.
           MOVE 'Y' TO LS-VALID-STATUS.
           MOVE SPACES TO LS-ERROR-MSG.

           PERFORM VALIDATE-CUST-ID.
           IF LS-VALID-STATUS = 'Y'
             PERFORM VALIDATE-BALANCE
           END-IF.
           IF LS-VALID-STATUS = 'Y'
             PERFORM VALIDATE-ACCT-TYPE
           END-IF.
           IF LS-VALID-STATUS = 'Y'
             PERFORM VALIDATE-CUST-EXISTS
           END-IF.
           IF LS-VALID-STATUS = 'Y'
             PERFORM LOG-VALIDATION-SUCCESS
           ELSE
             PERFORM LOG-VALIDATION-FAILURE
           END-IF.
           GOBACK.

       VALIDATE-CUST-ID.
           IF LS-CUST-ID = SPACES OR LS-CUST-ID = LOW-VALUES
             MOVE 'N' TO LS-VALID-STATUS
             MOVE 'E001: CUSTOMER ID IS BLANK OR EMPTY' TO
               LS-ERROR-MSG
           END-IF.
           IF LS-VALID-STATUS = 'Y'
             MOVE LS-CUST-ID TO WS-NUMERIC-CHECK
             INSPECT WS-NUMERIC-CHECK
               TALLYING WS-DB-CUST-COUNT
               FOR ALL SPACES
             IF WS-DB-CUST-COUNT = 10
               MOVE 'N' TO LS-VALID-STATUS
               MOVE 'E002: CUSTOMER ID IS ALL SPACES' TO
                 LS-ERROR-MSG
             END-IF
           END-IF.

       VALIDATE-BALANCE.
           IF LS-CUST-BALANCE < 0
             MOVE 'N' TO LS-VALID-STATUS
             MOVE 'E003: CUSTOMER BALANCE IS NEGATIVE' TO
               LS-ERROR-MSG
           END-IF.
           IF LS-VALID-STATUS = 'Y'
             IF LS-CUST-BALANCE > WS-BALANCE-LIMIT
               MOVE 'N' TO LS-VALID-STATUS
               MOVE 'E004: CUSTOMER BALANCE EXCEEDS LIMIT' TO
                 LS-ERROR-MSG
             END-IF
           END-IF.

       VALIDATE-ACCT-TYPE.
           EVALUATE LS-CUST-ACCT-TYPE
             WHEN 'C'
               CONTINUE
             WHEN 'S'
               CONTINUE
             WHEN 'L'
               CONTINUE
             WHEN OTHER
               MOVE 'N' TO LS-VALID-STATUS
               MOVE 'E005: INVALID ACCOUNT TYPE' TO
                 LS-ERROR-MSG
           END-EVALUATE.

       VALIDATE-CUST-EXISTS.
           EXEC SQL
             SELECT COUNT(*) INTO :WS-DB-CUST-COUNT
             FROM CUSTOMER_MASTER
             WHERE CUSTOMER_ID = :LS-CUST-ID
               AND ACCT_STATUS <> 'C'
           END-EXEC.
           IF WS-DB-CUST-COUNT = 0
             MOVE 'N' TO LS-VALID-STATUS
             MOVE 'E006: CUSTOMER NOT FOUND IN MASTER TABLE'
               TO LS-ERROR-MSG
           END-IF.

       LOG-VALIDATION-SUCCESS.
           EXEC SQL
             INSERT INTO VALIDATION_LOG
               (CUST_ID, VALID_STATUS, VALID_DATE, VALID_PROGRAM)
             VALUES
               (:LS-CUST-ID, 'PASS', CURRENT DATE, 'CUSTVALD')
           END-EXEC.

       LOG-VALIDATION-FAILURE.
           EXEC SQL
             INSERT INTO VALIDATION_LOG
               (CUST_ID, VALID_STATUS, ERROR_MSG,
                VALID_DATE, VALID_PROGRAM)
             VALUES
               (:LS-CUST-ID, 'FAIL', :LS-ERROR-MSG,
                CURRENT DATE, 'CUSTVALD')
           END-EXEC.
