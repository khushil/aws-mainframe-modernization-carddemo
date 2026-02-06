# C4 Level 4: Code Patterns - CardDemo Application

## Executive Summary

This document catalogs the recurring code patterns found in the CardDemo mainframe application. These patterns represent standardized approaches to common programming challenges in CICS/COBOL development. Understanding these patterns is essential for maintenance, modernization, and knowledge transfer.

---

## Pattern Inventory

| # | Pattern Name | Category | Primary Source |
|---|--------------|----------|----------------|
| 1 | Pseudo-Conversational Pattern | CICS | COSGN00C.cbl |
| 2 | Screen-Program Coupling | BMS | COSGN00C.cbl |
| 3 | VSAM CRUD Operations | Data Access | COBIL00C.cbl |
| 4 | Menu Navigation | Flow Control | COADM01C.cbl |
| 5 | Input Validation | Validation | COSGN00C.cbl |
| 6 | Error Handling | Error Mgmt | CBTRN02C.cbl |
| 7 | Batch Sequential Processing | Batch | CBTRN02C.cbl |

---

## Pattern 1: Pseudo-Conversational Pattern

### Description

The pseudo-conversational pattern is the fundamental CICS programming model. Instead of holding resources during user think time, the program terminates after each screen interaction and restarts when the user responds. State is preserved in the COMMAREA.

### Intent

- Release CICS resources during user think time
- Support high transaction throughput
- Maintain conversational flow without holding threads

### Structure

```
┌─────────────────────────────────────────────────────┐
│                  Program Entry                       │
├─────────────────────────────────────────────────────┤
│  IF EIBCALEN = 0 (First Time Entry)                 │
│      Initialize screen                               │
│      Send initial map                                │
│  ELSE (Re-entry from user)                          │
│      Restore COMMAREA                               │
│      Check CDEMO-PGM-CONTEXT                        │
│      IF first reentry                               │
│          Set CDEMO-PGM-REENTER = TRUE               │
│          Send map                                    │
│      ELSE                                           │
│          Receive map                                 │
│          Process user input                          │
│      END-IF                                         │
│  END-IF                                             │
│                                                     │
│  EXEC CICS RETURN                                   │
│      TRANSID(transaction-id)                        │
│      COMMAREA(state-data)                           │
│  END-EXEC                                           │
└─────────────────────────────────────────────────────┘
```

### Code Example

**Source**: `app/cbl/COSGN00C.cbl:80-102`

```cobol
       IF EIBCALEN = 0
           MOVE LOW-VALUES TO COSGN0AO
           MOVE -1       TO USERIDL OF COSGN0AI
           PERFORM SEND-SIGNON-SCREEN
       ELSE
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM PROCESS-ENTER-KEY
               WHEN DFHPF3
                   MOVE CCDA-MSG-THANK-YOU        TO WS-MESSAGE
                   PERFORM SEND-PLAIN-TEXT
               WHEN OTHER
                   MOVE 'Y'                       TO WS-ERR-FLG
                   MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                   PERFORM SEND-SIGNON-SCREEN
           END-EVALUATE
       END-IF.

       EXEC CICS RETURN
                 TRANSID (WS-TRANID)
                 COMMAREA (CARDDEMO-COMMAREA)
                 LENGTH(LENGTH OF CARDDEMO-COMMAREA)
       END-EXEC.
```

### Context Flag Pattern

**Source**: `app/cpy/COCOM01Y.cpy:29-31`

```cobol
       10 CDEMO-PGM-CONTEXT             PIC 9(01).
          88 CDEMO-PGM-ENTER            VALUE 0.
          88 CDEMO-PGM-REENTER          VALUE 1.
```

**Usage** (`app/cbl/COADM01C.cbl:91-94`):

```cobol
       IF NOT CDEMO-PGM-REENTER
           SET CDEMO-PGM-REENTER    TO TRUE
           MOVE LOW-VALUES          TO COADM1AO
           PERFORM SEND-MENU-SCREEN
       ELSE
           PERFORM RECEIVE-MENU-SCREEN
           ...
```

### Key Points

- `EIBCALEN = 0`: First invocation (no COMMAREA passed)
- `CDEMO-PGM-ENTER (0)`: Program entered from another program
- `CDEMO-PGM-REENTER (1)`: Program re-entered from user interaction
- Always end with `EXEC CICS RETURN TRANSID COMMAREA`

---

## Pattern 2: Screen-Program Coupling

### Description

Each CICS program is coupled with a BMS mapset that defines the screen layout. The program uses symbolic map copybooks for data exchange between the program and the terminal.

### Intent

- Separate screen definition from program logic
- Enable screen changes without recompiling programs
- Provide typed data structures for screen fields

### Structure

```
┌─────────────────────────────────────────────────────┐
│                BMS Mapset Definition                │
│  (Defines physical screen layout)                   │
├─────────────────────────────────────────────────────┤
│                Symbolic Map Copybook                │
│  (Generated data structure for I/O)                 │
├─────────────────────────────────────────────────────┤
│  Program uses:                                      │
│  - Input map suffix 'I' (e.g., COSGN0AI)           │
│  - Output map suffix 'O' (e.g., COSGN0AO)          │
│  - Field suffix 'I'/'O' + 'L' for length           │
│  - Field suffix 'A' for attributes                 │
└─────────────────────────────────────────────────────┘
```

### Code Example

**Send Map** (`app/cbl/COSGN00C.cbl:151-157`):

```cobol
       SEND-SIGNON-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COSGN0AO

           EXEC CICS SEND
                     MAP('COSGN0A')
                     MAPSET('COSGN00')
                     FROM(COSGN0AO)
                     ERASE
                     CURSOR
           END-EXEC.
```

**Receive Map** (`app/cbl/COSGN00C.cbl:110-115`):

```cobol
           EXEC CICS RECEIVE
                     MAP('COSGN0A')
                     MAPSET('COSGN00')
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.
```

### Symbolic Map Field Access

```cobol
* Input field access (received from user)
USERIDI OF COSGN0AI       * User ID value
USERIDL OF COSGN0AI       * User ID length (for cursor positioning)

* Output field access (sent to screen)
ERRMSGO OF COSGN0AO       * Error message output
TITLE01O OF COSGN0AO      * Title line output
```

### Cursor Positioning

```cobol
* Set cursor to User ID field
MOVE -1 TO USERIDL OF COSGN0AI
* The CURSOR option on SEND MAP uses this length field
```

### Key Points

- Map names follow pattern: `COXXXX0A` (first map in set)
- Mapset names follow pattern: `COXXXX00`
- Always use `ERASE` on initial send
- Use `CURSOR` option with length field set to -1

---

## Pattern 3: VSAM CRUD Operations

### Description

Standard patterns for Create, Read, Update, and Delete operations on VSAM KSDS files using EXEC CICS commands.

### Intent

- Consistent data access across all programs
- Proper error handling for file operations
- Support for alternate index access

### Read Pattern

**Source**: `app/cbl/COBIL00C.cbl:345-372`

```cobol
       READ-ACCTDAT-FILE.

           EXEC CICS READ
                DATASET   (WS-ACCTDAT-FILE)
                INTO      (ACCOUNT-RECORD)
                LENGTH    (LENGTH OF ACCOUNT-RECORD)
                RIDFLD    (ACCT-ID)
                KEYLENGTH (LENGTH OF ACCT-ID)
                UPDATE
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Account ID NOT found...' TO WS-MESSAGE
                   MOVE -1       TO ACTIDINL OF COBIL0AI
                   PERFORM SEND-BILLPAY-SCREEN
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lookup Account...' TO WS-MESSAGE
                   PERFORM SEND-BILLPAY-SCREEN
           END-EVALUATE.
```

### Write Pattern

**Source**: `app/cbl/COBIL00C.cbl:510-547`

```cobol
       WRITE-TRANSACT-FILE.

           EXEC CICS WRITE
                DATASET   (WS-TRANSACT-FILE)
                FROM      (TRAN-RECORD)
                LENGTH    (LENGTH OF TRAN-RECORD)
                RIDFLD    (TRAN-ID)
                KEYLENGTH (LENGTH OF TRAN-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   PERFORM INITIALIZE-ALL-FIELDS
                   MOVE 'Payment successful...' TO WS-MESSAGE
               WHEN DFHRESP(DUPKEY)
               WHEN DFHRESP(DUPREC)
                   MOVE 'Tran ID already exist...' TO WS-MESSAGE
               WHEN OTHER
                   MOVE 'Unable to Add Bill pay Transaction...'
                       TO WS-MESSAGE
           END-EVALUATE.
```

### Update (Rewrite) Pattern

**Source**: `app/cbl/COBIL00C.cbl:377-403`

```cobol
       UPDATE-ACCTDAT-FILE.

           EXEC CICS REWRITE
                DATASET   (WS-ACCTDAT-FILE)
                FROM      (ACCOUNT-RECORD)
                LENGTH    (LENGTH OF ACCOUNT-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE 'Account ID NOT found...' TO WS-MESSAGE
               WHEN OTHER
                   MOVE 'Unable to Update Account...' TO WS-MESSAGE
           END-EVALUATE.
```

### Browse Pattern (STARTBR/READNEXT/READPREV/ENDBR)

**Source**: `app/cbl/COBIL00C.cbl:441-505`

```cobol
       STARTBR-TRANSACT-FILE.
           EXEC CICS STARTBR
                DATASET   (WS-TRANSACT-FILE)
                RIDFLD    (TRAN-ID)
                KEYLENGTH (LENGTH OF TRAN-ID)
                RESP      (WS-RESP-CD)
           END-EXEC.

       READPREV-TRANSACT-FILE.
           EXEC CICS READPREV
                DATASET   (WS-TRANSACT-FILE)
                INTO      (TRAN-RECORD)
                LENGTH    (LENGTH OF TRAN-RECORD)
                RIDFLD    (TRAN-ID)
                KEYLENGTH (LENGTH OF TRAN-ID)
                RESP      (WS-RESP-CD)
           END-EXEC.

       ENDBR-TRANSACT-FILE.
           EXEC CICS ENDBR
                DATASET   (WS-TRANSACT-FILE)
           END-EXEC.
```

### Alternate Index Access Pattern

**Source**: `app/cbl/COBIL00C.cbl:408-436`

```cobol
       READ-CXACAIX-FILE.
* Read via alternate index (CXACAIX) using Account ID
           EXEC CICS READ
                DATASET   (WS-CXACAIX-FILE)
                INTO      (CARD-XREF-RECORD)
                LENGTH    (LENGTH OF CARD-XREF-RECORD)
                RIDFLD    (XREF-ACCT-ID)
                KEYLENGTH (LENGTH OF XREF-ACCT-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.
```

### Key Points

- Always use `RESP`/`RESP2` for inline error handling
- Use `UPDATE` option when planning to REWRITE
- Use `DFHRESP(name)` constants for response checking
- AIX files accessed as separate datasets

---

## Pattern 4: Menu Navigation

### Description

Menu-driven navigation using EVALUATE statements and EXEC CICS XCTL for program transfer.

### Intent

- Provide user-friendly menu interface
- Transfer control with state preservation
- Support return navigation via PF3

### Structure

```
┌─────────────────────────────────────────────────────┐
│  1. Receive user menu selection                     │
│  2. Validate selection against menu option count    │
│  3. EVALUATE selection                              │
│     WHEN valid option                               │
│         Set FROM-PROGRAM in COMMAREA                │
│         XCTL to target program with COMMAREA        │
│     WHEN invalid                                    │
│         Display error message                       │
│  4. On PF3, XCTL back to calling program            │
└─────────────────────────────────────────────────────┘
```

### Code Example

**Source**: `app/cbl/COADM01C.cbl:140-158`

```cobol
           IF NOT ERR-FLG-ON
               IF CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION)(1:5) NOT = 'DUMMY'
                   MOVE WS-TRANID    TO CDEMO-FROM-TRANID
                   MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
                   MOVE ZEROS        TO CDEMO-PGM-CONTEXT
                   EXEC CICS
                       XCTL PROGRAM(CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION))
                       COMMAREA(CARDDEMO-COMMAREA)
                   END-EXEC
               END-IF
               MOVE SPACES             TO WS-MESSAGE
               MOVE DFHGREEN           TO ERRMSGC  OF COADM1AO
               STRING 'This option '       DELIMITED BY SIZE
                      'is not installed ...'   DELIMITED BY SIZE
                 INTO WS-MESSAGE
               PERFORM SEND-MENU-SCREEN
           END-IF.
```

### Menu Option Definition

**Source**: `app/cpy/COADM02Y.cpy:22-59`

```cobol
       01 CARDDEMO-ADMIN-MENU-OPTIONS.
          05 CDEMO-ADMIN-OPT-COUNT     PIC 9(02) VALUE 6.
          05 CDEMO-ADMIN-OPTIONS-DATA.
             10 FILLER                 PIC 9(02) VALUE 1.
             10 FILLER                 PIC X(35) VALUE
                 'User List (Security)               '.
             10 FILLER                 PIC X(08) VALUE 'COUSR00C'.
             ...
          05 CDEMO-ADMIN-OPTIONS REDEFINES CDEMO-ADMIN-OPTIONS-DATA.
             10 CDEMO-ADMIN-OPT OCCURS 9 TIMES.
                15 CDEMO-ADMIN-OPT-NUM      PIC 9(02).
                15 CDEMO-ADMIN-OPT-NAME     PIC X(35).
                15 CDEMO-ADMIN-OPT-PGMNAME  PIC X(08).
```

### Return Navigation

**Source**: `app/cbl/COADM01C.cbl:163-170`

```cobol
       RETURN-TO-SIGNON-SCREEN.

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
           END-IF
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
           END-EXEC.
```

### Key Points

- Menu options defined in copybooks (COADM02Y, COMEN02Y)
- Use XCTL (not LINK) for menu transfers
- Always set FROM-PROGRAM before XCTL
- Reset PGM-CONTEXT to 0 before XCTL
- Check for 'DUMMY' programs (not yet implemented)

---

## Pattern 5: Input Validation

### Description

Validation of user input with error message display and cursor positioning to the field in error.

### Intent

- Ensure data integrity before processing
- Provide clear error feedback to users
- Position cursor for quick correction

### Structure

```
┌─────────────────────────────────────────────────────┐
│  EVALUATE TRUE                                       │
│      WHEN field = SPACES OR LOW-VALUES              │
│          Set error flag                              │
│          Set error message                           │
│          Position cursor (MOVE -1 to field length)  │
│          Re-send screen                              │
│      WHEN field violates business rule              │
│          ... same pattern ...                       │
│      WHEN OTHER                                      │
│          CONTINUE (validation passed)               │
│  END-EVALUATE                                        │
└─────────────────────────────────────────────────────┘
```

### Code Example

**Source**: `app/cbl/COSGN00C.cbl:117-130`

```cobol
           EVALUATE TRUE
               WHEN USERIDI OF COSGN0AI = SPACES OR LOW-VALUES
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'Please enter User ID ...' TO WS-MESSAGE
                   MOVE -1       TO USERIDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
               WHEN PASSWDI OF COSGN0AI = SPACES OR LOW-VALUES
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'Please enter Password ...' TO WS-MESSAGE
                   MOVE -1       TO PASSWDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
```

### Confirm Value Validation

**Source**: `app/cbl/COBIL00C.cbl:173-191`

```cobol
               EVALUATE CONFIRMI OF COBIL0AI
                   WHEN 'Y'
                   WHEN 'y'
                       SET CONF-PAY-YES TO TRUE
                       PERFORM READ-ACCTDAT-FILE
                   WHEN 'N'
                   WHEN 'n'
                       PERFORM CLEAR-CURRENT-SCREEN
                       MOVE 'Y'     TO WS-ERR-FLG
                   WHEN SPACES
                   WHEN LOW-VALUES
                       PERFORM READ-ACCTDAT-FILE
                   WHEN OTHER
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE 'Invalid value. Valid values are (Y/N)...'
                                    TO WS-MESSAGE
                       MOVE -1      TO CONFIRML OF COBIL0AI
                       PERFORM SEND-BILLPAY-SCREEN
               END-EVALUATE.
```

### Key Points

- Check for both SPACES and LOW-VALUES
- Set error flag to prevent further processing
- Use -1 in length field for cursor positioning
- Upper/lower case handling for Y/N responses

---

## Pattern 6: Error Handling

### Description

Structured error handling using response codes for CICS commands and file status for batch operations.

### Intent

- Graceful handling of error conditions
- Meaningful error messages for users
- Proper cleanup and recovery

### CICS Response Code Pattern

**Source**: `app/cbl/COSGN00C.cbl:221-257`

```cobol
           EVALUATE WS-RESP-CD
               WHEN 0
                   IF SEC-USR-PWD = WS-USER-PWD
                       ... success processing ...
                   ELSE
                       MOVE 'Wrong Password. Try again ...' TO WS-MESSAGE
                       PERFORM SEND-SIGNON-SCREEN
                   END-IF
               WHEN 13
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'User not found. Try again ...' TO WS-MESSAGE
                   PERFORM SEND-SIGNON-SCREEN
               WHEN OTHER
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'Unable to verify the User ...' TO WS-MESSAGE
                   PERFORM SEND-SIGNON-SCREEN
           END-EVALUATE.
```

### Batch File Status Pattern

**Source**: `app/cbl/CBTRN02C.cbl:236-252`

```cobol
       0000-DALYTRAN-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT DALYTRAN-FILE
           IF  DALYTRAN-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING DALYTRAN'
               MOVE DALYTRAN-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
```

### Display IO Status Pattern

**Source**: `app/cbl/CBTRN02C.cbl:714-727`

```cobol
       9910-DISPLAY-IO-STATUS.
           IF  IO-STATUS NOT NUMERIC
           OR  IO-STAT1 = '9'
               MOVE IO-STAT1 TO IO-STATUS-04(1:1)
               MOVE 0        TO TWO-BYTES-BINARY
               MOVE IO-STAT2 TO TWO-BYTES-RIGHT
               MOVE TWO-BYTES-BINARY TO IO-STATUS-0403
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           ELSE
               MOVE '0000' TO IO-STATUS-04
               MOVE IO-STATUS TO IO-STATUS-04(3:2)
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           END-IF
           EXIT.
```

### Abend Pattern

**Source**: `app/cbl/CBTRN02C.cbl:707-711`

```cobol
       9999-ABEND-PROGRAM.
           DISPLAY 'ABENDING PROGRAM'
           MOVE 0 TO TIMING
           MOVE 999 TO ABCODE
           CALL 'CEE3ABD' USING ABCODE, TIMING.
```

### Key Points

- Common RESP codes: 0 (NORMAL), 13 (NOTFND), 12 (FILENOTFOUND)
- File status '00' = success, '10' = EOF, '23' = record not found
- Display error details before abend
- Use 88-level conditions for readability (APPL-AOK)

---

## Pattern 7: Batch Sequential Processing

### Description

Standard batch processing pattern: open files, process records in a loop, close files, report statistics.

### Intent

- Process large volumes of records efficiently
- Track processing statistics
- Handle exceptions without aborting entire job

### Structure

```
┌─────────────────────────────────────────────────────┐
│  DISPLAY 'START OF EXECUTION'                       │
│  OPEN all files                                     │
│                                                     │
│  PERFORM UNTIL END-OF-FILE = 'Y'                    │
│      READ next record                               │
│      IF not EOF                                     │
│          ADD 1 TO record-count                      │
│          PERFORM validation                         │
│          IF valid                                   │
│              PERFORM processing                     │
│          ELSE                                       │
│              ADD 1 TO reject-count                  │
│              WRITE reject record                    │
│          END-IF                                     │
│      END-IF                                         │
│  END-PERFORM                                        │
│                                                     │
│  CLOSE all files                                    │
│  DISPLAY statistics                                 │
│  IF reject-count > 0                                │
│      SET return-code = 4                           │
│  END-IF                                             │
│  DISPLAY 'END OF EXECUTION'                         │
│  GOBACK                                             │
└─────────────────────────────────────────────────────┘
```

### Code Example

**Source**: `app/cbl/CBTRN02C.cbl:193-234`

```cobol
       PROCEDURE DIVISION.
           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN02C'.
           PERFORM 0000-DALYTRAN-OPEN.
           PERFORM 0100-TRANFILE-OPEN.
           PERFORM 0200-XREFFILE-OPEN.
           PERFORM 0300-DALYREJS-OPEN.
           PERFORM 0400-ACCTFILE-OPEN.
           PERFORM 0500-TCATBALF-OPEN.

           PERFORM UNTIL END-OF-FILE = 'Y'
               IF  END-OF-FILE = 'N'
                   PERFORM 1000-DALYTRAN-GET-NEXT
                   IF  END-OF-FILE = 'N'
                     ADD 1 TO WS-TRANSACTION-COUNT
                     MOVE 0 TO WS-VALIDATION-FAIL-REASON
                     MOVE SPACES TO WS-VALIDATION-FAIL-REASON-DESC
                     PERFORM 1500-VALIDATE-TRAN
                     IF WS-VALIDATION-FAIL-REASON = 0
                       PERFORM 2000-POST-TRANSACTION
                     ELSE
                       ADD 1 TO WS-REJECT-COUNT
                       PERFORM 2500-WRITE-REJECT-REC
                     END-IF
                   END-IF
               END-IF
           END-PERFORM.

           PERFORM 9000-DALYTRAN-CLOSE.
           PERFORM 9100-TRANFILE-CLOSE.
           PERFORM 9200-XREFFILE-CLOSE.
           PERFORM 9300-DALYREJS-CLOSE.
           PERFORM 9400-ACCTFILE-CLOSE.
           PERFORM 9500-TCATBALF-CLOSE.
           DISPLAY 'TRANSACTIONS PROCESSED :' WS-TRANSACTION-COUNT
           DISPLAY 'TRANSACTIONS REJECTED  :' WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
              MOVE 4 TO RETURN-CODE
           END-IF
           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN02C'.

           GOBACK.
```

### Key Points

- Numbered paragraphs (0000-, 1000-, etc.) indicate sequence
- Use END-OF-FILE flag rather than checking status in loop
- Track both processed and rejected counts
- Set non-zero return code if any errors
- Always display start/end messages for job logs

---

## Key Algorithms

### Algorithm 1: Transaction ID Generation

**Source**: `app/cbl/COBIL00C.cbl:212-219`

**Purpose**: Generate unique sequential transaction IDs

```cobol
* Position to end of file to find highest ID
MOVE HIGH-VALUES TO TRAN-ID
PERFORM STARTBR-TRANSACT-FILE
PERFORM READPREV-TRANSACT-FILE
PERFORM ENDBR-TRANSACT-FILE

* Increment for new ID
MOVE TRAN-ID     TO WS-TRAN-ID-NUM
ADD 1 TO WS-TRAN-ID-NUM
```

**Pattern**: Start browse at HIGH-VALUES, read previous to get last record, add 1.

---

### Algorithm 2: Credit Limit Validation

**Source**: `app/cbl/CBTRN02C.cbl:403-413`

**Purpose**: Ensure transaction doesn't exceed credit limit

```cobol
COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
                    - ACCT-CURR-CYC-DEBIT
                    + DALYTRAN-AMT

IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
    CONTINUE
ELSE
    MOVE 102 TO WS-VALIDATION-FAIL-REASON
    MOVE 'OVERLIMIT TRANSACTION'
      TO WS-VALIDATION-FAIL-REASON-DESC
END-IF
```

**Formula**: `Available = Credit_Limit - (Cycle_Credit - Cycle_Debit + New_Amount)`

---

### Algorithm 3: Balance Update

**Source**: `app/cbl/CBTRN02C.cbl:545-552`

**Purpose**: Update account balance based on transaction amount

```cobol
ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
IF DALYTRAN-AMT >= 0
   ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
ELSE
   ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
END-IF
```

**Logic**: Positive amounts are credits, negative are debits.

---

### Algorithm 4: Timestamp Generation

**Source**: `app/cbl/CBTRN02C.cbl:692-705`

**Purpose**: Create DB2-compatible timestamp

```cobol
       Z-GET-DB2-FORMAT-TIMESTAMP.
           MOVE FUNCTION CURRENT-DATE TO COBOL-TS
           MOVE COB-YYYY TO DB2-YYYY
           MOVE COB-MM   TO DB2-MM
           MOVE COB-DD   TO DB2-DD
           MOVE COB-HH   TO DB2-HH
           MOVE COB-MIN  TO DB2-MIN
           MOVE COB-SS   TO DB2-SS
           MOVE COB-MIL  TO DB2-MIL
           MOVE '0000'   TO DB2-REST
           MOVE '-' TO DB2-STREEP-1 DB2-STREEP-2 DB2-STREEP-3
           MOVE '.' TO DB2-DOT-1 DB2-DOT-2 DB2-DOT-3
           EXIT.
```

**Format**: `YYYY-MM-DD-HH.MM.SS.NNNNNN`

---

## Cross-References

- **Level 3 Component View**: [C4-L3-COMPONENT.md](./C4-L3-COMPONENT.md)
- **Context Map**: [../03-context-model/CONTEXT-MAP.md](../03-context-model/CONTEXT-MAP.md)
- **COMMAREA Specification**: [../03-context-model/COMMAREA-SPECIFICATION.md](../03-context-model/COMMAREA-SPECIFICATION.md)
- **Navigation Flows**: [../03-context-model/NAVIGATION-FLOWS.md](../03-context-model/NAVIGATION-FLOWS.md)

---

## Source File References

| Pattern | Primary Source |
|---------|----------------|
| Pseudo-Conversational | `app/cbl/COSGN00C.cbl:80-102` |
| Screen-Program Coupling | `app/cbl/COSGN00C.cbl:145-157` |
| VSAM CRUD | `app/cbl/COBIL00C.cbl:345-547` |
| Menu Navigation | `app/cbl/COADM01C.cbl:140-170` |
| Input Validation | `app/cbl/COSGN00C.cbl:117-130` |
| Error Handling | `app/cbl/CBTRN02C.cbl:236-252, 707-727` |
| Batch Sequential | `app/cbl/CBTRN02C.cbl:193-234` |
| COMMAREA Definition | `app/cpy/COCOM01Y.cpy` |
| Menu Definitions | `app/cpy/COADM02Y.cpy`, `app/cpy/COMEN02Y.cpy` |
