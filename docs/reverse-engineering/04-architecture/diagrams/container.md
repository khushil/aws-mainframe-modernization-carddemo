# C4 Level 2: Container Diagram

## CardDemo Container Architecture

```mermaid
C4Container
    title CardDemo Container Diagram

    Person(user, "User", "Card holder or CSR")
    Person(admin, "Admin", "System administrator")
    Person(batchop, "Batch Operator", "Operations staff")

    System_Boundary(carddemo, "CardDemo System") {
        Container(bms, "BMS Presentation", "BMS Mapsets", "17 mapsets defining 3270 screen layouts")
        Container(cics, "CICS Online Region", "CICS TS / COBOL", "18+ programs handling interactive transactions")
        Container(batch, "Batch Subsystem", "JES2 / COBOL", "10 programs for scheduled processing")
        ContainerDb(vsam, "VSAM Data Store", "VSAM KSDS", "6+ files storing accounts, cards, transactions")
        ContainerDb(db2, "DB2 Database", "IBM DB2", "Transaction type tables [Optional]")
        ContainerDb(ims, "IMS Database", "IBM IMS", "Authorization data [Optional]")
        Container(mq, "MQ Messaging", "IBM MQ", "Async account extraction [Optional]")
    }

    System_Ext(terminal, "3270 Terminals", "User interface")
    System_Ext(jes2, "JES2 Scheduler", "Job scheduling")

    Rel(user, terminal, "Uses")
    Rel(admin, terminal, "Uses")
    Rel(batchop, jes2, "Submits jobs")

    Rel(terminal, bms, "3270 datastream")
    Rel(bms, cics, "SEND/RECEIVE MAP")
    Rel(cics, vsam, "EXEC CICS FILE")
    Rel(cics, db2, "EXEC SQL [Optional]")
    Rel(cics, ims, "DL/I [Optional]")
    Rel(cics, mq, "MQPUT/GET [Optional]")

    Rel(jes2, batch, "Executes")
    Rel(batch, vsam, "COBOL I/O")

    UpdateLayoutConfig($c4ShapeInRow="3", $c4BoundaryInRow="1")
```

---

## Alternative: Mermaid Flowchart Version

```mermaid
flowchart TB
    subgraph Actors["Actors"]
        USER((User/Admin))
        BATCHOP((Batch Operator))
    end

    subgraph External["External Systems"]
        TERM[/"3270 Terminals"/]
        JES2[/"JES2 Scheduler"/]
    end

    subgraph CardDemo["CardDemo System Boundary"]
        subgraph Presentation["Presentation Layer"]
            BMS["BMS Mapsets<br/>(17 mapsets)<br/>3270 screen definitions"]
        end

        subgraph Online["Online Processing"]
            CICS["CICS Online Region<br/>(18+ programs)<br/>Transaction processing"]
        end

        subgraph Batch["Batch Processing"]
            BATCHPGM["Batch Subsystem<br/>(10 programs)<br/>Scheduled jobs"]
        end

        subgraph DataStores["Data Stores"]
            VSAM[("VSAM Data Store<br/>(6+ files)<br/>KSDS")]
            DB2[("DB2 Database<br/>[Optional]<br/>Transaction types")]
            IMS[("IMS Database<br/>[Optional]<br/>Authorization")]
            MQ[["MQ Messaging<br/>[Optional]<br/>Async"]]
        end
    end

    %% Actor connections
    USER --> TERM
    BATCHOP --> JES2

    %% External to internal
    TERM -->|"3270 datastream"| BMS
    JES2 -->|"Execute jobs"| BATCHPGM

    %% Internal connections
    BMS <-->|"SEND/RECEIVE MAP"| CICS
    CICS -->|"EXEC CICS FILE"| VSAM
    CICS -.->|"EXEC SQL"| DB2
    CICS -.->|"DL/I calls"| IMS
    CICS -.->|"MQPUT/GET"| MQ

    BATCHPGM -->|"COBOL I/O"| VSAM

    %% Styling
    classDef actor fill:#08427b,stroke:#052e56,color:#fff
    classDef external fill:#999999,stroke:#666666,color:#fff
    classDef container fill:#438dd5,stroke:#2e6295,color:#fff
    classDef database fill:#438dd5,stroke:#2e6295,color:#fff
    classDef optional fill:#85bbf0,stroke:#5d82a8,color:#000

    class USER,BATCHOP actor
    class TERM,JES2 external
    class BMS,CICS,BATCHPGM container
    class VSAM database
    class DB2,IMS,MQ optional
```

---

## Container Communication Matrix

```mermaid
flowchart LR
    subgraph "Communication Protocols"
        direction TB

        subgraph "Online Path"
            T1[Terminal] -->|"3270"| B1[BMS]
            B1 -->|"SEND MAP<br/>RECEIVE MAP"| C1[CICS]
            C1 -->|"EXEC CICS<br/>READ/WRITE"| V1[(VSAM)]
            C1 -->|"XCTL"| C2[CICS Program]
        end

        subgraph "Batch Path"
            J1[JES2] -->|"JCL"| BP[Batch Program]
            BP -->|"OPEN/READ<br/>WRITE/CLOSE"| V2[(VSAM)]
            BP -->|"WRITE"| S1[Sequential]
        end

        subgraph "Optional Paths"
            C1 -.->|"EXEC SQL"| D1[(DB2)]
            C1 -.->|"DL/I"| I1[(IMS)]
            C1 -.->|"MQPUT/GET"| M1[[MQ]]
        end
    end

    style T1 fill:#999,color:#fff
    style J1 fill:#999,color:#fff
    style V1 fill:#438dd5,color:#fff
    style V2 fill:#438dd5,color:#fff
    style D1 fill:#85bbf0,color:#000
    style I1 fill:#85bbf0,color:#000
    style M1 fill:#85bbf0,color:#000
```

---

## Data Flow Between Containers

```mermaid
flowchart TB
    subgraph "Online Transaction Flow"
        direction LR

        USER((User)) --> |"Input"| SCREEN[BMS Screen]
        SCREEN --> |"Map Data"| PGM[CICS Program]
        PGM --> |"COMMAREA"| PGM2[Next Program]
        PGM --> |"Read/Write"| DATA[(VSAM)]
        DATA --> |"Data"| PGM
        PGM --> |"Output"| SCREEN
        SCREEN --> |"Display"| USER
    end

    subgraph "Batch Transaction Flow"
        direction LR

        INPUT[/Input File/] --> BPGM[Batch Program]
        BPGM --> |"Validate"| XREF[(XREF File)]
        XREF --> |"Lookup"| BPGM
        BPGM --> |"Update"| ACCT[(Account File)]
        BPGM --> |"Write"| TRAN[(Transaction File)]
        BPGM --> |"Reject"| REJECT[/Reject File/]
        BPGM --> |"Report"| REPORT[/Report/]
    end
```

---

## State Management

```mermaid
flowchart TB
    subgraph "COMMAREA State Flow"
        SIGN[COSGN00C] -->|"Initialize<br/>COMMAREA"| CA1[/"COMMAREA<br/>User ID<br/>User Type<br/>Context=0"/]
        CA1 -->|"XCTL"| MENU[Menu Program]
        MENU -->|"Set Context=1<br/>RETURN TRANSID"| CA2[/"COMMAREA<br/>Context=1"/]
        CA2 -->|"Reenter"| MENU
        MENU -->|"XCTL to<br/>Feature"| CA3[/"COMMAREA<br/>From=Menu<br/>Context=0"/]
        CA3 --> FEATURE[Feature Program]
        FEATURE -->|"PF3"| CA4[/"COMMAREA<br/>To=Menu"/]
        CA4 -->|"XCTL"| MENU
    end

    style CA1 fill:#f9f,stroke:#333
    style CA2 fill:#f9f,stroke:#333
    style CA3 fill:#f9f,stroke:#333
    style CA4 fill:#f9f,stroke:#333
```

---

## VSAM File Relationships

```mermaid
erDiagram
    ACCTDAT ||--o{ CCXREF : "has cards via"
    CARDDAT ||--|| CCXREF : "cross-referenced"
    ACCTDAT ||--o{ TRANSACT : "has transactions"
    CARDDAT ||--o{ TRANSACT : "records against"
    CUSTDAT ||--o{ ACCTDAT : "owns"
    USRSEC ||--|| CICS_SESSION : "authenticates"

    ACCTDAT {
        numeric ACCT-ID PK
        numeric ACCT-CURR-BAL
        numeric ACCT-CREDIT-LIMIT
        date ACCT-EXPIRATION-DATE
    }

    CARDDAT {
        numeric CARD-NUM PK
        char CARD-STATUS
        date CARD-EXPIRY
    }

    CCXREF {
        numeric CARD-NUM PK
        numeric ACCT-ID AIX
    }

    TRANSACT {
        char TRAN-ID PK
        numeric TRAN-AMT
        timestamp TRAN-ORIG-TS
    }

    CUSTDAT {
        numeric CUST-ID PK
        char CUST-NAME
    }

    USRSEC {
        char USER-ID PK
        char USER-PWD
        char USER-TYPE
    }
```

---

## Technology Stack Summary

```mermaid
mindmap
    root((CardDemo<br/>Containers))
        Presentation
            BMS Mapsets
                17 mapsets
                3270 datastream
        Processing
            CICS Region
                18+ programs
                COBOL
                EXEC CICS
            Batch Subsystem
                10 programs
                JCL
                COBOL I/O
        Data
            VSAM
                6+ KSDS files
                Alternate indexes
            DB2 Optional
                Transaction types
                EXEC SQL
            IMS Optional
                Authorization
                DL/I
        Messaging
            MQ Optional
                Async extraction
                MQPUT/MQGET
```

---

## Cross-References

- **Full Documentation**: [C4-L2-CONTAINER.md](../C4-L2-CONTAINER.md)
- **System Context Diagram**: [system-context.md](./system-context.md)
- **Component Diagrams**: [component-cics.md](./component-cics.md), [component-batch.md](./component-batch.md)
- **Source**: `app/cbl/`, `app/bms/`, `app/cpy/`
