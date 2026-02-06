# C4 Level 1: System Context Diagram

## CardDemo System Context

```mermaid
C4Context
    title CardDemo System Context Diagram

    Person(cardholder, "Card Holder", "Credit card customer viewing accounts and making payments")
    Person(csr, "Customer Service Rep", "Staff managing accounts, cards, and transactions")
    Person(admin, "System Administrator", "IT staff managing users and system configuration")
    Person(batchop, "Batch Operator", "Operations staff submitting and monitoring batch jobs")

    System(carddemo, "CardDemo", "Credit card account management system providing account inquiry, card management, bill payment, and transaction processing")

    System_Ext(terminal, "3270 Terminal Network", "IBM 3270 terminal emulation for user interface")
    System_Ext(jes2, "JES2 Job Scheduler", "IBM batch job entry subsystem")
    System_Ext(cardnetwork, "Card Authorization Network", "External card network for real-time authorization [Optional]")
    System_Ext(corebanking, "Core Banking System", "General ledger and banking integration [Optional]")
    System_Ext(printservice, "Statement Printing", "Customer statement generation service [Optional]")

    Rel(cardholder, terminal, "Uses", "3270 protocol")
    Rel(csr, terminal, "Uses", "3270 protocol")
    Rel(admin, terminal, "Uses", "3270 protocol")
    Rel(batchop, jes2, "Submits jobs to", "JCL")

    Rel(terminal, carddemo, "Sends transactions to", "CICS/BMS")
    Rel(jes2, carddemo, "Executes batch in", "JCL/COBOL")

    Rel(carddemo, cardnetwork, "Authorizes transactions via", "IMS/MQ [Optional]")
    Rel(carddemo, corebanking, "Posts to GL via", "Batch files [Optional]")
    Rel(carddemo, printservice, "Generates statements via", "Print files [Optional]")

    UpdateLayoutConfig($c4ShapeInRow="3", $c4BoundaryInRow="1")
```

---

## Alternative: Mermaid Flowchart Version

For environments that don't support C4 notation:

```mermaid
flowchart TB
    subgraph Actors["Human Actors"]
        CH[/"Card Holder<br/>(User type 'U')"/]
        CSR[/"Customer Service Rep<br/>(User type 'U')"/]
        ADMIN[/"System Administrator<br/>(User type 'A')"/]
        BATCHOP[/"Batch Operator"/]
    end

    subgraph External["External Systems"]
        TERM[("3270 Terminal<br/>Network")]
        JES2[("JES2 Job<br/>Scheduler")]
        CARDNET[("Card Authorization<br/>Network<br/>[Optional]")]
        COREBANK[("Core Banking<br/>System<br/>[Optional]")]
        PRINT[("Statement<br/>Printing<br/>[Optional]")]
    end

    subgraph CardDemo["CardDemo System"]
        SYSTEM[["CardDemo<br/>Credit Card Account<br/>Management System"]]
    end

    %% Actor to External System connections
    CH -->|"3270 protocol"| TERM
    CSR -->|"3270 protocol"| TERM
    ADMIN -->|"3270 protocol"| TERM
    BATCHOP -->|"JCL submission"| JES2

    %% External System to CardDemo connections
    TERM -->|"CICS/BMS<br/>transactions"| SYSTEM
    JES2 -->|"Batch jobs"| SYSTEM

    %% CardDemo to External System connections
    SYSTEM -->|"IMS/MQ<br/>[Optional]"| CARDNET
    SYSTEM -->|"Batch files<br/>[Optional]"| COREBANK
    SYSTEM -->|"Print files<br/>[Optional]"| PRINT

    %% Styling
    classDef actor fill:#08427b,stroke:#052e56,color:#fff
    classDef external fill:#999999,stroke:#666666,color:#fff
    classDef system fill:#1168bd,stroke:#0b4884,color:#fff

    class CH,CSR,ADMIN,BATCHOP actor
    class TERM,JES2,CARDNET,COREBANK,PRINT external
    class SYSTEM system
```

---

## Actor Interaction Summary

```mermaid
flowchart LR
    subgraph "User Types"
        direction TB
        U["User ('U')<br/>Card Holder<br/>CSR"]
        A["Admin ('A')<br/>System Administrator"]
    end

    subgraph "Entry Point"
        SIGN["COSGN00C<br/>Signon<br/>CC00"]
    end

    subgraph "Menu Access"
        USERMENU["COMEN01C<br/>User Menu<br/>CM00"]
        ADMINMENU["COADM01C<br/>Admin Menu<br/>CA00"]
    end

    U -->|"Login"| SIGN
    A -->|"Login"| SIGN
    SIGN -->|"CDEMO-USRTYP-USER"| USERMENU
    SIGN -->|"CDEMO-USRTYP-ADMIN"| ADMINMENU

    style SIGN fill:#f9f,stroke:#333
    style USERMENU fill:#bbf,stroke:#333
    style ADMINMENU fill:#fbb,stroke:#333
```

---

## Data Flow Context

```mermaid
flowchart TB
    subgraph "Online Processing"
        direction LR
        USER((User)) --> BMS[BMS Maps]
        BMS --> CICS[CICS Region]
        CICS --> VSAM[(VSAM Files)]
    end

    subgraph "Batch Processing"
        direction LR
        JOB[JCL Job] --> BATCH[Batch Programs]
        BATCH --> VSAM2[(VSAM Files)]
        BATCH --> SEQ[(Sequential Files)]
    end

    subgraph "Optional Extensions"
        direction LR
        DB2[(DB2 Database)]
        IMS[(IMS Database)]
        MQ[IBM MQ]
    end

    CICS -.->|"[Optional]"| DB2
    CICS -.->|"[Optional]"| IMS
    CICS -.->|"[Optional]"| MQ

    style USER fill:#08427b,color:#fff
    style JOB fill:#08427b,color:#fff
```

---

## Security Context

```mermaid
flowchart TB
    subgraph "Authentication Boundary"
        LOGIN[/"User Login"/]
        USRSEC[(USRSEC File)]
        LOGIN --> USRSEC
        USRSEC --> AUTH{Authenticated?}
    end

    subgraph "Authorization Zones"
        AUTH -->|"Yes + Type='A'"| ADMIN_ZONE["Admin Zone<br/>User Management<br/>DB2 Config"]
        AUTH -->|"Yes + Type='U'"| USER_ZONE["User Zone<br/>Account/Card/Transaction<br/>Bill Payment"]
        AUTH -->|"No"| REJECT[/"Access Denied"/]
    end

    style ADMIN_ZONE fill:#fbb,stroke:#333
    style USER_ZONE fill:#bbf,stroke:#333
    style REJECT fill:#f00,color:#fff
```

---

## Cross-References

- **Full Documentation**: [C4-L1-SYSTEM-CONTEXT.md](../C4-L1-SYSTEM-CONTEXT.md)
- **Container Diagram**: [container.md](./container.md)
- **Source**: `app/cpy/COCOM01Y.cpy:26-28`, `app/cbl/COSGN00C.cbl:230-240`
