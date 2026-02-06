# Cross-Container Data Flow Diagram

## CardDemo Data Flow Architecture

This document illustrates how data flows between the major containers in the CardDemo application.

---

## Online Transaction Data Flow

```mermaid
flowchart TB
    subgraph User["User Interaction"]
        TERM[/"3270 Terminal"/]
    end

    subgraph Presentation["BMS Presentation Layer"]
        MAP["BMS Mapset<br/>(Symbolic/Physical Map)"]
    end

    subgraph Processing["CICS Processing"]
        PGM["COBOL Program"]
        COMM[/"COMMAREA<br/>(Session State)"/]
    end

    subgraph Data["VSAM Data Layer"]
        VSAM[(VSAM KSDS)]
    end

    %% Request flow
    TERM -->|"1. User Input<br/>3270 datastream"| MAP
    MAP -->|"2. RECEIVE MAP<br/>Symbolic map data"| PGM
    PGM -->|"3. READ request<br/>EXEC CICS READ"| VSAM
    VSAM -->|"4. Record data"| PGM
    PGM -->|"5. Process & Format"| MAP
    MAP -->|"6. SEND MAP<br/>Screen output"| TERM

    %% State management
    PGM <-->|"State preserved<br/>between interactions"| COMM

    style TERM fill:#999,color:#fff
    style MAP fill:#bbf
    style PGM fill:#bfb
    style VSAM fill:#fbf
    style COMM fill:#ff9
```

---

## Batch Processing Data Flow

```mermaid
flowchart TB
    subgraph Input["Input Sources"]
        DAILY[/"DALYTRAN<br/>Daily Transactions"/]
        PARAM[/"JCL Parameters"/]
    end

    subgraph BatchProcessing["Batch Processing"]
        BPGM["Batch COBOL Program"]
        WORK[/"Working Storage<br/>(Counters, Flags)"/]
    end

    subgraph DataStores["Data Stores"]
        direction TB
        XREF[(XREFFILE)]
        ACCT[(ACCTDAT)]
        TRAN[(TRANSACT)]
        TCAT[(TCATBALF)]
    end

    subgraph Output["Output Files"]
        REJECT[/"DALYREJS<br/>Rejected Records"/]
        REPORT[/"SYSOUT<br/>Job Report"/]
    end

    %% Input flow
    DAILY -->|"1. Sequential READ"| BPGM
    PARAM -->|"JCL PARM"| BPGM

    %% Validation lookups
    BPGM -->|"2. Validate card"| XREF
    XREF -->|"Account ID"| BPGM
    BPGM -->|"3. Validate account"| ACCT

    %% Processing
    BPGM <-->|"Track counts"| WORK

    %% Output flow
    BPGM -->|"4. Valid: WRITE"| TRAN
    BPGM -->|"5. Update balance"| ACCT
    BPGM -->|"6. Update category"| TCAT
    BPGM -->|"7. Invalid: WRITE"| REJECT
    BPGM -->|"8. Display counts"| REPORT

    style DAILY fill:#ffe0e0
    style REJECT fill:#ffe0e0
    style REPORT fill:#e0ffe0
    style XREF fill:#e0e0ff
    style ACCT fill:#e0e0ff
    style TRAN fill:#e0e0ff
    style TCAT fill:#e0e0ff
```

---

## Bill Payment Complete Data Flow

```mermaid
flowchart TB
    subgraph UserInput["User Input"]
        USER((User))
        SCREEN["Bill Payment Screen<br/>COBIL0A"]
    end

    subgraph Processing["COBIL00C Processing"]
        VALIDATE["Validate Account ID"]
        LOOKUP["Lookup Account Balance"]
        CONFIRM["Confirm Payment"]
        GENID["Generate Transaction ID"]
        CREATE["Create Transaction Record"]
        UPDATE["Update Account Balance"]
    end

    subgraph DataAccess["Data Access"]
        ACCT[(ACCTDAT)]
        XREF[(CXACAIX)]
        TRAN[(TRANSACT)]
    end

    subgraph Output["Output"]
        SUCCESS["Payment Successful<br/>Transaction ID displayed"]
        ERROR["Error Message"]
    end

    USER -->|"Enter Account ID"| SCREEN
    SCREEN --> VALIDATE

    VALIDATE -->|"Account ID"| ACCT
    ACCT -->|"Account Record"| LOOKUP
    VALIDATE -->|"Invalid"| ERROR

    LOOKUP -->|"Balance > 0"| CONFIRM
    LOOKUP -->|"Balance <= 0"| ERROR

    CONFIRM -->|"User confirms Y"| XREF
    XREF -->|"Card Number"| GENID

    GENID -->|"STARTBR HIGH-VALUES<br/>READPREV<br/>ENDBR"| TRAN
    TRAN -->|"Last TRAN-ID"| GENID
    GENID -->|"New ID = Last + 1"| CREATE

    CREATE -->|"WRITE new record"| TRAN
    UPDATE -->|"REWRITE with<br/>reduced balance"| ACCT
    CREATE --> UPDATE
    UPDATE --> SUCCESS

    SUCCESS --> SCREEN
    ERROR --> SCREEN
    SCREEN --> USER

    style USER fill:#08427b,color:#fff
    style SUCCESS fill:#0f0
    style ERROR fill:#f00,color:#fff
```

---

## Authentication Data Flow

```mermaid
flowchart TB
    subgraph Entry["Session Entry"]
        TERM[/"Terminal"/]
    end

    subgraph SignOn["COSGN00C"]
        RECEIVE["RECEIVE MAP<br/>User ID + Password"]
        VALIDATE["Validate Input"]
        LOOKUP["EXEC CICS READ<br/>USRSEC"]
        CHECK["Verify Password"]
        INIT["Initialize COMMAREA"]
        ROUTE["Route by User Type"]
    end

    subgraph Security["Security Data"]
        USRSEC[(USRSEC)]
    end

    subgraph Destinations["Destination Programs"]
        ADMIN["COADM01C<br/>Admin Menu"]
        USER["COMEN01C<br/>User Menu"]
    end

    subgraph State["Session State"]
        COMM[/"COMMAREA<br/>CDEMO-USER-ID<br/>CDEMO-USER-TYPE<br/>CDEMO-PGM-CONTEXT"/]
    end

    TERM --> RECEIVE
    RECEIVE --> VALIDATE
    VALIDATE -->|"Valid"| LOOKUP
    VALIDATE -->|"Empty fields"| TERM

    LOOKUP --> USRSEC
    USRSEC --> CHECK

    CHECK -->|"Password match"| INIT
    CHECK -->|"Wrong password"| TERM

    INIT --> COMM
    COMM --> ROUTE

    ROUTE -->|"Type = 'A'"| ADMIN
    ROUTE -->|"Type = 'U'"| USER

    style TERM fill:#999,color:#fff
    style USRSEC fill:#e0e0ff
    style COMM fill:#ff9
    style ADMIN fill:#fbb
    style USER fill:#bbf
```

---

## VSAM Cross-Reference Data Flow

```mermaid
flowchart LR
    subgraph Programs["CICS Programs"]
        PGM1["Account Programs"]
        PGM2["Card Programs"]
        PGM3["Transaction Programs"]
        PGM4["Bill Payment"]
    end

    subgraph CrossRef["CCXREF Cross-Reference"]
        PRIM["Primary Key:<br/>CARD-NUM (16)"]
        AIX["Alternate Index:<br/>ACCT-ID (11)"]
    end

    subgraph MasterFiles["Master Files"]
        CARD[(CARDDAT)]
        ACCT[(ACCTDAT)]
    end

    %% Card lookup by card number
    PGM2 -->|"Card Number"| PRIM
    PRIM -->|"Account ID"| PGM2
    PGM2 --> ACCT

    %% Account lookup by account ID (via AIX)
    PGM1 -->|"Account ID"| AIX
    AIX -->|"Card Numbers"| PGM1
    PGM1 --> CARD

    %% Transaction processing
    PGM3 -->|"Card Number"| PRIM
    PRIM -->|"Account ID"| PGM3
    PGM3 --> ACCT

    %% Bill payment
    PGM4 -->|"Account ID"| AIX
    AIX -->|"Card Number"| PGM4

    style PRIM fill:#e0e0ff
    style AIX fill:#ffe0e0
    style CARD fill:#bfb
    style ACCT fill:#fbf
```

---

## Online-Batch Data Separation

```mermaid
flowchart TB
    subgraph OnlineWindow["Online Processing Window"]
        CICS["CICS Region"]
        VSAM1[(VSAM Files<br/>Shared)]
    end

    subgraph BatchWindow["Batch Processing Window"]
        BATCH["Batch Jobs"]
        VSAM2[(VSAM Files<br/>Exclusive)]
    end

    subgraph Timeline["Time Separation"]
        DAY["Business Hours<br/>Online Active"]
        NIGHT["Batch Window<br/>Online Quiet"]
    end

    CICS <-->|"Real-time<br/>updates"| VSAM1
    BATCH <-->|"Bulk<br/>processing"| VSAM2

    DAY --> CICS
    NIGHT --> BATCH

    VSAM1 -.->|"Same physical<br/>files"| VSAM2

    style DAY fill:#bbf
    style NIGHT fill:#333,color:#fff
```

---

## Transaction Category Balance Flow

```mermaid
flowchart TB
    subgraph DailyInput["Daily Transaction Input"]
        TRANS["Transaction Record<br/>TRAN-TYPE-CD<br/>TRAN-CAT-CD<br/>TRAN-AMT"]
    end

    subgraph Processing["CBTRN02C Processing"]
        LOOKUP["Lookup TCATBAL<br/>by ACCT+TYPE+CAT"]
        EXISTS{Record<br/>Exists?}
        CREATE["Create new<br/>TCATBAL record"]
        UPDATE["Add TRAN-AMT<br/>to TRAN-CAT-BAL"]
    end

    subgraph CategoryBalance["TCATBALF"]
        KEY["Key: ACCT-ID +<br/>TYPE-CD + CAT-CD"]
        BAL["TRAN-CAT-BAL"]
    end

    TRANS --> LOOKUP
    LOOKUP --> KEY
    KEY --> EXISTS

    EXISTS -->|"No"| CREATE
    EXISTS -->|"Yes"| UPDATE

    CREATE --> BAL
    UPDATE --> BAL

    style TRANS fill:#ffe0e0
    style KEY fill:#e0e0ff
    style BAL fill:#e0ffe0
```

---

## Optional Extension Data Flows

### DB2 Transaction Type Flow

```mermaid
flowchart LR
    subgraph Online["CICS Online"]
        TRTL["COTRTLIC<br/>Type List"]
        TRUP["COTRTUPC<br/>Type Update"]
    end

    subgraph DB2["DB2 Database"]
        TABLE[(TRAN_TYPE<br/>Table)]
    end

    TRTL -->|"EXEC SQL<br/>SELECT"| TABLE
    TABLE -->|"Result Set"| TRTL

    TRUP -->|"EXEC SQL<br/>UPDATE"| TABLE

    style TABLE fill:#ffb
```

### IMS Authorization Flow

```mermaid
flowchart LR
    subgraph Online["CICS Online"]
        AUTH["COPAUA0C<br/>Auth Process"]
        VIEW["COPAUS0C<br/>Pending View"]
    end

    subgraph IMS["IMS Database"]
        SEG[(AUTH Segments)]
    end

    subgraph MQ["IBM MQ"]
        Q1[[AUTH.REQUEST]]
        Q2[[AUTH.RESPONSE]]
    end

    AUTH -->|"DL/I GU/GN"| SEG
    AUTH -->|"MQPUT"| Q1
    Q2 -->|"MQGET"| AUTH

    VIEW -->|"DL/I GN"| SEG

    style SEG fill:#e0ffe0
    style Q1 fill:#ffe0e0
    style Q2 fill:#ffe0e0
```

---

## Cross-References

- **CICS Components**: [component-cics.md](./component-cics.md)
- **Batch Components**: [component-batch.md](./component-batch.md)
- **Container View**: [container.md](./container.md)
- **Data Model**: [../../02-data-model/DATA-MODEL.md](../../02-data-model/DATA-MODEL.md)
- **Source**: `app/cbl/COBIL00C.cbl`, `app/cbl/CBTRN02C.cbl`, `app/cbl/COSGN00C.cbl`
