# C4 Level 3: Component View - CardDemo Application

## Executive Summary

This document describes the Component (L3) view of the CardDemo mainframe application. Components represent individual COBOL programs within each container. This level details the internal structure of the CICS Online Region and Batch Subsystem containers.

---

## Component Inventory Summary

| Container | Component Groups | Total Programs |
|-----------|------------------|----------------|
| CICS Online Region | 10 groups | 18 core + 9 extension |
| Batch Subsystem | 4 groups | 10 programs |
| **Total** | **14 groups** | **37 programs** |

---

## CICS Online Region Components

### Component Groups Overview

| Group | Programs | Primary VSAM Files | Responsibility |
|-------|----------|-------------------|----------------|
| Authentication | COSGN00C | USRSEC | User login, session init |
| Menu Navigation | COADM01C, COMEN01C | -- | Admin/User menu routing |
| Account Management | COACTVWC, COACTUPC | ACCTDAT, CUSTDAT, CCXREF | Account view/update |
| Card Management | COCRDLIC, COCRDSLC, COCRDUPC | CARDDAT, CCXREF | Card lifecycle |
| Transaction Processing | COTRN00C, COTRN01C, COTRN02C | TRANSACT | Transaction CRUD |
| Bill Payment | COBIL00C | ACCTDAT, TRANSACT, CCXREF | Payment processing |
| User Administration | COUSR00C-03C | USRSEC | Security user CRUD |
| Reports | CORPT00C | TRANSACT | Report generation |
| Authorization [Ext] | COPAUA0C, COPAUS0-2C | IMS/DB2 | Real-time auth |
| Transaction Type [Ext] | COTRTLIC, COTRTUPC | DB2 | Type maintenance |

---

### 1. Authentication Component

**Program**: COSGN00C
**Transaction ID**: CC00
**Source**: `app/cbl/COSGN00C.cbl`

| Aspect | Details |
|--------|---------|
| Function | User signon and session establishment |
| BMS Map | COSGN00/COSGN0A |
| VSAM Files | USRSEC (READ) |
| Outbound | COADM01C (Admin) or COMEN01C (User) via XCTL |

**Key Logic** (`COSGN00C.cbl:230-240`):
```cobol
IF CDEMO-USRTYP-ADMIN
    EXEC CICS XCTL PROGRAM ('COADM01C')
        COMMAREA(CARDDEMO-COMMAREA)
    END-EXEC
ELSE
    EXEC CICS XCTL PROGRAM ('COMEN01C')
        COMMAREA(CARDDEMO-COMMAREA)
    END-EXEC
END-IF
```

**COMMAREA Initialization**:
- Sets `CDEMO-USER-ID` from validated credentials
- Sets `CDEMO-USER-TYPE` ('A' or 'U') from USRSEC record
- Sets `CDEMO-PGM-CONTEXT` to 0 (first entry)

---

### 2. Menu Navigation Components

#### Admin Menu - COADM01C

**Transaction ID**: CA00
**Source**: `app/cbl/COADM01C.cbl`

| Aspect | Details |
|--------|---------|
| Function | Admin menu display and navigation |
| BMS Map | COADM01/COADM1A |
| Menu Config | COADM02Y.cpy (6 options) |
| Inbound | COSGN00C (Admin users only) |
| Outbound | COUSR00C-03C, COTRTLIC, COTRTUPC |

**Menu Options** (from `COADM02Y.cpy`):

| Option | Function | Target Program |
|--------|----------|----------------|
| 1 | User List | COUSR00C |
| 2 | User Add | COUSR01C |
| 3 | User Update | COUSR02C |
| 4 | User Delete | COUSR03C |
| 5 | Transaction Type List [DB2] | COTRTLIC |
| 6 | Transaction Type Maint [DB2] | COTRTUPC |

#### User Menu - COMEN01C

**Transaction ID**: CM00
**Source**: `app/cbl/COMEN01C.cbl`

| Aspect | Details |
|--------|---------|
| Function | User menu display and navigation |
| BMS Map | COMEN01/COMEN1A |
| Menu Config | COMEN02Y.cpy (11 options) |
| Inbound | COSGN00C (All users) |
| Outbound | All feature programs |

**Menu Options** (from `COMEN02Y.cpy`):

| Option | Function | Target Program | User Type |
|--------|----------|----------------|-----------|
| 1 | Account View | COACTVWC | U |
| 2 | Account Update | COACTUPC | U |
| 3 | Credit Card List | COCRDLIC | U |
| 4 | Credit Card View | COCRDSLC | U |
| 5 | Credit Card Update | COCRDUPC | U |
| 6 | Transaction List | COTRN00C | U |
| 7 | Transaction View | COTRN01C | U |
| 8 | Transaction Add | COTRN02C | U |
| 9 | Transaction Reports | CORPT00C | U |
| 10 | Bill Payment | COBIL00C | U |
| 11 | Pending Auth View [IMS] | COPAUS0C | U |

---

### 3. Account Management Components

#### Account View - COACTVWC

**Source**: `app/cbl/COACTVWC.cbl`

| Aspect | Details |
|--------|---------|
| Function | Display account details |
| BMS Map | COACTVW/COACTVWA |
| VSAM Files | ACCTDAT (READ), CUSTDAT (READ), CCXREF (READ) |
| Operations | READ only |

#### Account Update - COACTUPC

**Source**: `app/cbl/COACTUPC.cbl`

| Aspect | Details |
|--------|---------|
| Function | Modify account attributes |
| BMS Map | COACTUP/COACTUPA |
| VSAM Files | ACCTDAT (READ/REWRITE), CUSTDAT (READ), CCXREF (READ) |
| Operations | READ, REWRITE |

---

### 4. Card Management Components

#### Card List - COCRDLIC

**Source**: `app/cbl/COCRDLIC.cbl`

| Aspect | Details |
|--------|---------|
| Function | Browse/list credit cards |
| BMS Map | COCRDLI/COCRDLIA |
| VSAM Files | CARDDAT (BROWSE), CCXREF (READ) |
| Operations | STARTBR, READNEXT, ENDBR |

#### Card View/Select - COCRDSLC

**Source**: `app/cbl/COCRDSLC.cbl`

| Aspect | Details |
|--------|---------|
| Function | View card details, select for update |
| BMS Map | COCRDSL/COCRDSLA |
| VSAM Files | CARDDAT (READ), CCXREF (READ) |
| Operations | READ |

#### Card Update - COCRDUPC

**Source**: `app/cbl/COCRDUPC.cbl`

| Aspect | Details |
|--------|---------|
| Function | Modify card attributes |
| BMS Map | COCRDUP/COCRDUPA |
| VSAM Files | CARDDAT (READ/REWRITE), CCXREF (READ) |
| Operations | READ, REWRITE |

---

### 5. Transaction Processing Components

#### Transaction List - COTRN00C

**Source**: `app/cbl/COTRN00C.cbl`

| Aspect | Details |
|--------|---------|
| Function | Browse transactions |
| BMS Map | COTRN00/COTRN00A |
| VSAM Files | TRANSACT (BROWSE) |
| Operations | STARTBR, READNEXT, ENDBR |

#### Transaction View - COTRN01C

**Source**: `app/cbl/COTRN01C.cbl`

| Aspect | Details |
|--------|---------|
| Function | Display transaction details |
| BMS Map | COTRN01/COTRN01A |
| VSAM Files | TRANSACT (READ) |
| Operations | READ |

#### Transaction Add - COTRN02C

**Source**: `app/cbl/COTRN02C.cbl`

| Aspect | Details |
|--------|---------|
| Function | Create new transaction |
| BMS Map | COTRN02/COTRN02A |
| VSAM Files | TRANSACT (WRITE), ACCTDAT (READ) |
| Operations | READ, WRITE |

---

### 6. Bill Payment Component

**Program**: COBIL00C
**Transaction ID**: CB00
**Source**: `app/cbl/COBIL00C.cbl`

| Aspect | Details |
|--------|---------|
| Function | Process bill payments |
| BMS Map | COBIL00/COBIL0A |
| VSAM Files | ACCTDAT (READ/REWRITE), TRANSACT (READ/WRITE), CXACAIX (READ) |
| Operations | READ, REWRITE, STARTBR, READPREV, ENDBR, WRITE |

**Key Business Logic**:

1. **Transaction ID Generation** (`COBIL00C.cbl:212-219`):
```cobol
MOVE HIGH-VALUES TO TRAN-ID
PERFORM STARTBR-TRANSACT-FILE
PERFORM READPREV-TRANSACT-FILE
PERFORM ENDBR-TRANSACT-FILE
MOVE TRAN-ID     TO WS-TRAN-ID-NUM
ADD 1 TO WS-TRAN-ID-NUM
```

2. **Balance Update** (`COBIL00C.cbl:234`):
```cobol
COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
```

---

### 7. User Administration Components

| Program | Function | Operations |
|---------|----------|------------|
| COUSR00C | User List | STARTBR, READNEXT, ENDBR |
| COUSR01C | User Add | WRITE |
| COUSR02C | User Update | READ, REWRITE |
| COUSR03C | User Delete | READ, DELETE |

**Source**: `app/cbl/COUSR00C.cbl` through `COUSR03C.cbl`
**VSAM File**: USRSEC (all CRUD operations)

---

### 8. Reports Component

**Program**: CORPT00C
**Source**: `app/cbl/CORPT00C.cbl`

| Aspect | Details |
|--------|---------|
| Function | Generate transaction reports |
| BMS Map | CORPT00/CORPT00A |
| VSAM Files | TRANSACT (BROWSE) |
| Operations | STARTBR, READNEXT, ENDBR |

---

### 9. Authorization Components [Optional Extension]

**Directory**: `app/app-authorization-ims-db2-mq/cbl/`

| Program | Function | Technology |
|---------|----------|------------|
| COPAUA0C | Authorization request processing | IMS + DB2 + MQ |
| COPAUS0C | Pending authorization view | IMS + DB2 |
| COPAUS1C | Authorization detail view | IMS + DB2 |
| COPAUS2C | Authorization update | IMS + DB2 |

---

### 10. Transaction Type Components [Optional Extension]

**Directory**: `app/app-transaction-type-db2/cbl/`

| Program | Function | Technology |
|---------|----------|------------|
| COTRTLIC | Transaction type list | DB2 |
| COTRTUPC | Transaction type update | DB2 |
| COBTUPDT | Batch type update | DB2 |

---

## Batch Subsystem Components

### Component Groups Overview

| Group | Programs | Primary Files | Responsibility |
|-------|----------|---------------|----------------|
| Account Maintenance | CBACT01C-04C | ACCTDAT | Account file operations |
| Transaction Processing | CBTRN01C-03C | TRANSACT, DALYTRAN | Daily processing |
| Customer Maintenance | CBCUS01C | CUSTDAT | Customer file operations |
| Data Exchange | CBEXPORT, CBIMPORT | All files | ASCII/EBCDIC conversion |

---

### 1. Account Maintenance Components

| Program | Function | Files | I/O Mode |
|---------|----------|-------|----------|
| CBACT01C | Account file read | ACCTDAT | INPUT |
| CBACT02C | Account file update | ACCTDAT | I-O |
| CBACT03C | Account add | ACCTDAT | OUTPUT |
| CBACT04C | Account delete | ACCTDAT | I-O |

**Source**: `app/cbl/CBACT01C.cbl` through `CBACT04C.cbl`

---

### 2. Transaction Processing Components

#### Daily Transaction Posting - CBTRN02C

**Source**: `app/cbl/CBTRN02C.cbl`

| Aspect | Details |
|--------|---------|
| Function | Post daily transactions to master file |
| Input Files | DALYTRAN (Sequential), XREFFILE (KSDS), ACCTFILE (KSDS) |
| Output Files | TRANSACT (KSDS), TCATBALF (KSDS), DALYREJS (Sequential) |

**Processing Flow** (`CBTRN02C.cbl:193-234`):
```cobol
PERFORM UNTIL END-OF-FILE = 'Y'
    PERFORM 1000-DALYTRAN-GET-NEXT
    IF  END-OF-FILE = 'N'
        ADD 1 TO WS-TRANSACTION-COUNT
        PERFORM 1500-VALIDATE-TRAN
        IF WS-VALIDATION-FAIL-REASON = 0
            PERFORM 2000-POST-TRANSACTION
        ELSE
            ADD 1 TO WS-REJECT-COUNT
            PERFORM 2500-WRITE-REJECT-REC
        END-IF
    END-IF
END-PERFORM
```

**Validation Rules** (`CBTRN02C.cbl:370-422`):

| Code | Reason | Check |
|------|--------|-------|
| 100 | Invalid card number | XREF lookup failed |
| 101 | Account not found | Account lookup failed |
| 102 | Over limit | Transaction exceeds credit limit |
| 103 | Expired account | Transaction date > expiration |

#### Interest Calculation

| Program | Function |
|---------|----------|
| CBTRN01C | Read transactions for interest calc |
| CBTRN03C | Calculate and post interest charges |

**Source**: `app/cbl/CBTRN01C.cbl`, `app/cbl/CBTRN03C.cbl`

---

### 3. Customer Maintenance Component

**Program**: CBCUS01C
**Source**: `app/cbl/CBCUS01C.cbl`

| Aspect | Details |
|--------|---------|
| Function | Customer file maintenance |
| Files | CUSTDAT (I-O) |
| Operations | READ, REWRITE, DELETE |

---

### 4. Data Exchange Components

#### Data Export - CBEXPORT

**Source**: `app/cbl/CBEXPORT.cbl`

| Aspect | Details |
|--------|---------|
| Function | Export VSAM data to ASCII format |
| Input | VSAM files (EBCDIC) |
| Output | Sequential ASCII files |

#### Data Import - CBIMPORT

**Source**: `app/cbl/CBIMPORT.cbl`

| Aspect | Details |
|--------|---------|
| Function | Import ASCII data to VSAM |
| Input | Sequential ASCII files |
| Output | VSAM files (EBCDIC) |

---

## Component Interactions

### CICS Program Navigation

```
COSGN00C (Entry)
    │
    ├──[Admin]──► COADM01C
    │                │
    │                ├──► COUSR00C (User List)
    │                ├──► COUSR01C (User Add)
    │                ├──► COUSR02C (User Update)
    │                ├──► COUSR03C (User Delete)
    │                ├──► COTRTLIC [DB2]
    │                └──► COTRTUPC [DB2]
    │
    └──[User]───► COMEN01C
                     │
                     ├──► COACTVWC (Account View)
                     ├──► COACTUPC (Account Update)
                     ├──► COCRDLIC (Card List)
                     ├──► COCRDSLC (Card View)
                     ├──► COCRDUPC (Card Update)
                     ├──► COTRN00C (Transaction List)
                     ├──► COTRN01C (Transaction View)
                     ├──► COTRN02C (Transaction Add)
                     ├──► CORPT00C (Reports)
                     ├──► COBIL00C (Bill Payment)
                     └──► COPAUS0C [IMS/DB2]
```

### Batch Job Dependencies

```
Daily Batch Cycle:
    ┌─────────────┐
    │  CBACT02C   │  (Account refresh)
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │  CBTRN02C   │  (Transaction posting)
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │  CBTRN03C   │  (Interest calculation)
    └─────────────┘
```

---

## VSAM File Access Matrix

| Component | ACCTDAT | CARDDAT | CUSTDAT | TRANSACT | USRSEC | CCXREF |
|-----------|:-------:|:-------:|:-------:|:--------:|:------:|:------:|
| COSGN00C | | | | | R | |
| COADM01C | | | | | | |
| COMEN01C | | | | | | |
| COACTVWC | R | | R | | | R |
| COACTUPC | RW | | R | | | R |
| COCRDLIC | | R | | | | R |
| COCRDSLC | | R | | | | R |
| COCRDUPC | | RW | | | | R |
| COTRN00C | | | | R | | |
| COTRN01C | | | | R | | |
| COTRN02C | R | | | W | | |
| COBIL00C | RW | | | RW | | R |
| CORPT00C | | | | R | | |
| COUSR00C | | | | | R | |
| COUSR01C | | | | | W | |
| COUSR02C | | | | | RW | |
| COUSR03C | | | | | RD | |
| CBTRN02C | RW | | | W | | R |

**Legend**: R=Read, W=Write, RW=Read/Write, RD=Read/Delete

---

## Cross-References

- **Level 2 Container View**: [C4-L2-CONTAINER.md](./C4-L2-CONTAINER.md)
- **Level 4 Code Patterns**: [C4-L4-CODE-PATTERNS.md](./C4-L4-CODE-PATTERNS.md)
- **CICS Component Diagram**: [diagrams/component-cics.md](./diagrams/component-cics.md)
- **Batch Component Diagram**: [diagrams/component-batch.md](./diagrams/component-batch.md)
- **Context Map**: [../03-context-model/CONTEXT-MAP.md](../03-context-model/CONTEXT-MAP.md)

---

## Source File References

| Component Group | Source Files |
|-----------------|--------------|
| Authentication | `app/cbl/COSGN00C.cbl` |
| Menu Navigation | `app/cbl/COADM01C.cbl`, `app/cbl/COMEN01C.cbl` |
| Account Management | `app/cbl/COACTVWC.cbl`, `app/cbl/COACTUPC.cbl` |
| Card Management | `app/cbl/COCRDLIC.cbl`, `app/cbl/COCRDSLC.cbl`, `app/cbl/COCRDUPC.cbl` |
| Transaction Processing | `app/cbl/COTRN00C.cbl`, `app/cbl/COTRN01C.cbl`, `app/cbl/COTRN02C.cbl` |
| Bill Payment | `app/cbl/COBIL00C.cbl` |
| User Administration | `app/cbl/COUSR00C.cbl` - `COUSR03C.cbl` |
| Reports | `app/cbl/CORPT00C.cbl` |
| Authorization [Ext] | `app/app-authorization-ims-db2-mq/cbl/COPAU*.cbl` |
| Transaction Type [Ext] | `app/app-transaction-type-db2/cbl/COTR*.cbl` |
| Batch Account | `app/cbl/CBACT01C.cbl` - `CBACT04C.cbl` |
| Batch Transaction | `app/cbl/CBTRN01C.cbl` - `CBTRN03C.cbl` |
| Batch Customer | `app/cbl/CBCUS01C.cbl` |
| Data Exchange | `app/cbl/CBEXPORT.cbl`, `app/cbl/CBIMPORT.cbl` |
