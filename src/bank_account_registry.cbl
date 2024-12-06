       IDENTIFICATION DIVISION.
       PROGRAM-ID. BankAccountRegistry.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNTS-FILE.
       01 ACCOUNT-RECORD.
           05 ACCOUNT-NUMBER   PIC 9(10).
           05 ACCOUNT-NAME     PIC X(30).
           05 ACCOUNT-BALANCE  PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01 WS-OPTION          PIC 9 VALUE 0.
       01 WS-ACCOUNT-NUMBER  PIC 9(10).
       01 WS-ACCOUNT-NAME    PIC X(30).
       01 WS-ACCOUNT-BALANCE PIC 9(8)V99.
       01 EOF-SWITCH         PIC X VALUE "N".
           88 END-OF-FILE VALUE "Y" FALSE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM CHECK-FILE
           PERFORM UNTIL WS-OPTION = 3
               PERFORM DISPLAY-MENU
               PERFORM HANDLE-OPTION
           END-PERFORM.

           DISPLAY "Goodbye!".
           STOP RUN.

       CHECK-FILE.
           OPEN INPUT ACCOUNTS-FILE
           IF NOT END-OF-FILE
               CLOSE ACCOUNTS-FILE
           ELSE
               OPEN OUTPUT ACCOUNTS-FILE
               CLOSE ACCOUNTS-FILE
           END-IF.

       DISPLAY-MENU.
           DISPLAY "===== Bank Account Registry =====".
           DISPLAY "1 - Register a New Account".
           DISPLAY "2 - View All Accounts".
           DISPLAY "3 - Exit".
           DISPLAY "Enter your option: ".
           ACCEPT WS-OPTION.

       HANDLE-OPTION.
           EVALUATE WS-OPTION
               WHEN 1
                   PERFORM REGISTER-ACCOUNT
               WHEN 2
                   PERFORM VIEW-ACCOUNTS
               WHEN 3
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
           END-EVALUATE.

       REGISTER-ACCOUNT.
           DISPLAY "Enter Account Number (10 digits): ".
           ACCEPT WS-ACCOUNT-NUMBER.
           DISPLAY "Enter Account Holder Name: ".
           ACCEPT WS-ACCOUNT-NAME.
           DISPLAY "Enter Initial Balance: ".
           ACCEPT WS-ACCOUNT-BALANCE.

           MOVE WS-ACCOUNT-NUMBER TO ACCOUNT-NUMBER.
           MOVE WS-ACCOUNT-NAME TO ACCOUNT-NAME.
           MOVE WS-ACCOUNT-BALANCE TO ACCOUNT-BALANCE.

           OPEN EXTEND ACCOUNTS-FILE
           WRITE ACCOUNT-RECORD.
           CLOSE ACCOUNTS-FILE.

           DISPLAY "Account registered successfully!".

       VIEW-ACCOUNTS.
           DISPLAY "===== Registered Accounts =====".

           OPEN INPUT ACCOUNTS-FILE
           PERFORM UNTIL END-OF-FILE
               READ ACCOUNTS-FILE INTO ACCOUNT-RECORD
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       DISPLAY "Account Number: ", ACCOUNT-NUMBER
                       DISPLAY "Account Name: ", ACCOUNT-NAME
                       DISPLAY "Balance: $", ACCOUNT-BALANCE
                       DISPLAY "------------------------------"
               END-READ
           END-PERFORM.
           CLOSE ACCOUNTS-FILE.

           SET END-OF-FILE TO FALSE.
