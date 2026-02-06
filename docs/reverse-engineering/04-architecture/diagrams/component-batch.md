# C4 Level 3: Batch Subsystem Component Diagram

## Batch Subsystem Component Architecture

```mermaid
C4Component
    title CardDemo Batch Subsystem - Component Diagram

    Container_Boundary(batch, "Batch Subsystem") {
        Component(act01, "Account Read", "CBACT01C", "Read account records")
        Component(act02, "Account Update", "CBACT02C", "Update account records")
        Component(act03, "Account Add", "CBACT03C", "Add new accounts")
        Component(act04, "Account Delete", "CBACT04C", "Delete accounts")

        Component(trn01, "Transaction Read", "CBTRN01C", "Read transaction records")
        Component(trn02, "Transaction Posting", "CBTRN02C", "Post daily transactions")
        Component(trn03, "Interest Calc", "CBTRN03C", "Calculate interest charges")

        Component(cus01, "Customer Maint", "CBCUS01C", "Customer file operations")

        Component(export, "Data Export", "CBEXPORT", "Export to ASCII format")
        Component(import, "Data Import", "CBIMPORT", "Import from ASCII format")
    }

    ContainerDb(vsam, "VSAM Data Store", "VSAM KSDS", "Account, Transaction files")
    Container(seq, "Sequential Files", "PS/QSAM", "Input/Output/Reject files")
    Container_Ext(jes2, "JES2", "Job Scheduler", "JCL execution")

    Rel(jes2, act01, "Executes")
    Rel(jes2, trn02, "Executes")

    Rel(act02, vsam, "I-O ACCTDAT")
    Rel(trn02, vsam, "READ XREF, WRITE TRANSACT")
    Rel(trn02, seq, "READ DALYTRAN, WRITE REJECTS")

    Rel(export, vsam, "READ")
    Rel(export, seq, "WRITE ASCII")

    UpdateLayoutConfig($c4ShapeInRow="4", $c4BoundaryInRow="1")
```

---

## Alternative: Mermaid Flowchart Version

```mermaid
flowchart TB
    subgraph JES2["JES2 Job Scheduler"]
        JCL[/"JCL Jobs"/]
    end

    subgraph BatchSubsystem["Batch Subsystem"]
        direction TB

        subgraph AccountOps["Account Maintenance"]
            ACT01["CBACT01C<br/>Account Read"]
            ACT02["CBACT02C<br/>Account Update"]
            ACT03["CBACT03C<br/>Account Add"]
            ACT04["CBACT04C<br/>Account Delete"]
        end

        subgraph TransOps["Transaction Processing"]
            TRN01["CBTRN01C<br/>Transaction Read"]
            TRN02["CBTRN02C<br/>Daily Posting"]
            TRN03["CBTRN03C<br/>Interest Calc"]
        end

        subgraph CustOps["Customer Maintenance"]
            CUS01["CBCUS01C<br/>Customer Maint"]
        end

        subgraph DataExchange["Data Exchange"]
            EXPORT["CBEXPORT<br/>Export to ASCII"]
            IMPORT["CBIMPORT<br/>Import from ASCII"]
        end
    end

    subgraph DataStores["Data Stores"]
        subgraph VSAM["VSAM Files"]
            ACCTDAT[(ACCTDAT)]
            TRANSACT[(TRANSACT)]
            XREFFILE[(XREFFILE)]
            CUSTDAT[(CUSTDAT)]
            TCATBALF[(TCATBALF)]
        end

        subgraph Sequential["Sequential Files"]
            DALYTRAN[/DALYTRAN<br/>Daily Trans Input/]
            DALYREJS[/DALYREJS<br/>Rejected Trans/]
            ASCII[/ASCII Files/]
        end
    end

    %% JCL to Programs
    JCL --> ACT01
    JCL --> ACT02
    JCL --> ACT03
    JCL --> ACT04
    JCL --> TRN01
    JCL --> TRN02
    JCL --> TRN03
    JCL --> CUS01
    JCL --> EXPORT
    JCL --> IMPORT

    %% Account operations
    ACT01 -->|"INPUT"| ACCTDAT
    ACT02 -->|"I-O"| ACCTDAT
    ACT03 -->|"OUTPUT"| ACCTDAT
    ACT04 -->|"I-O"| ACCTDAT

    %% Transaction operations
    TRN01 -->|"INPUT"| TRANSACT
    TRN02 -->|"INPUT"| DALYTRAN
    TRN02 -->|"INPUT"| XREFFILE
    TRN02 -->|"I-O"| ACCTDAT
    TRN02 -->|"OUTPUT"| TRANSACT
    TRN02 -->|"I-O"| TCATBALF
    TRN02 -->|"OUTPUT"| DALYREJS

    TRN03 -->|"INPUT"| TRANSACT
    TRN03 -->|"I-O"| ACCTDAT

    %% Customer operations
    CUS01 -->|"I-O"| CUSTDAT

    %% Data exchange
    EXPORT -->|"INPUT"| ACCTDAT
    EXPORT -->|"INPUT"| TRANSACT
    EXPORT -->|"OUTPUT"| ASCII

    IMPORT -->|"INPUT"| ASCII
    IMPORT -->|"OUTPUT"| ACCTDAT
    IMPORT -->|"OUTPUT"| TRANSACT

    %% Styling
    classDef acct fill:#bfb,stroke:#333
    classDef tran fill:#bbf,stroke:#333
    classDef cust fill:#fbf,stroke:#333
    classDef exch fill:#ff9,stroke:#333
    classDef vsam fill:#e0e0ff
    classDef seq fill:#ffe0e0

    class ACT01,ACT02,ACT03,ACT04 acct
    class TRN01,TRN02,TRN03 tran
    class CUS01 cust
    class EXPORT,IMPORT exch
    class ACCTDAT,TRANSACT,XREFFILE,CUSTDAT,TCATBALF vsam
    class DALYTRAN,DALYREJS,ASCII seq
```

---

## Daily Batch Workflow

```mermaid
flowchart TB
    subgraph "Daily Batch Cycle"
        direction TB

        START([Start Batch Window])

        subgraph Phase1["Phase 1: Data Refresh"]
            ACT02[CBACT02C<br/>Account Refresh]
        end

        subgraph Phase2["Phase 2: Transaction Posting"]
            TRN02[CBTRN02C<br/>Post Daily Trans]
        end

        subgraph Phase3["Phase 3: Interest Calculation"]
            TRN03[CBTRN03C<br/>Calculate Interest]
        end

        FINISH([End Batch Window])

        START --> Phase1
        Phase1 --> Phase2
        Phase2 --> Phase3
        Phase3 --> FINISH
    end

    subgraph "Files Processed"
        DALYTRAN[/DALYTRAN/]
        ACCTDAT[(ACCTDAT)]
        TRANSACT[(TRANSACT)]
        TCATBALF[(TCATBALF)]
        DALYREJS[/DALYREJS/]
    end

    ACT02 -.-> ACCTDAT
    TRN02 --> DALYTRAN
    TRN02 --> ACCTDAT
    TRN02 --> TRANSACT
    TRN02 --> TCATBALF
    TRN02 --> DALYREJS
    TRN03 --> ACCTDAT
    TRN03 --> TRANSACT
```

---

## CBTRN02C Transaction Posting Detail

```mermaid
flowchart TB
    subgraph "CBTRN02C Processing Logic"
        START([Open All Files])

        LOOP{Read Next<br/>DALYTRAN?}
        VALIDATE["1500-VALIDATE-TRAN<br/>Check XREF & Account"]
        VALID{Valid?}

        POST["2000-POST-TRANSACTION<br/>Write TRANSACT<br/>Update TCATBAL<br/>Update ACCTDAT"]

        REJECT["2500-WRITE-REJECT-REC<br/>Write to DALYREJS"]

        COUNT[Increment Counter]

        CLOSE([Close All Files<br/>Display Counts])

        START --> LOOP
        LOOP -->|"Record Found"| VALIDATE
        LOOP -->|"EOF"| CLOSE
        VALIDATE --> VALID
        VALID -->|"Yes"| POST
        VALID -->|"No"| REJECT
        POST --> COUNT
        REJECT --> COUNT
        COUNT --> LOOP
    end

    subgraph "Validation Codes"
        V100["100: Invalid Card Number"]
        V101["101: Account Not Found"]
        V102["102: Over Credit Limit"]
        V103["103: Account Expired"]
    end

    VALIDATE -.-> V100
    VALIDATE -.-> V101
    VALIDATE -.-> V102
    VALIDATE -.-> V103
```

---

## Batch File I/O Patterns

```mermaid
flowchart LR
    subgraph "Input Files"
        DT[/DALYTRAN<br/>Sequential/]
        IN[/Input File<br/>Sequential/]
    end

    subgraph "Batch Programs"
        TRN02[CBTRN02C]
        ACT02[CBACT02C]
        IMPORT[CBIMPORT]
    end

    subgraph "VSAM Files"
        ACCT[(ACCTDAT<br/>KSDS)]
        TRAN[(TRANSACT<br/>KSDS)]
        XREF[(XREFFILE<br/>KSDS)]
        TCAT[(TCATBALF<br/>KSDS)]
    end

    subgraph "Output Files"
        REJ[/DALYREJS<br/>Sequential/]
        OUT[/Output<br/>Sequential/]
    end

    DT -->|"READ"| TRN02
    TRN02 -->|"READ"| XREF
    TRN02 -->|"READ/REWRITE"| ACCT
    TRN02 -->|"WRITE"| TRAN
    TRN02 -->|"READ/REWRITE/WRITE"| TCAT
    TRN02 -->|"WRITE"| REJ

    IN -->|"READ"| ACT02
    ACT02 -->|"REWRITE"| ACCT

    IN -->|"READ"| IMPORT
    IMPORT -->|"WRITE"| ACCT
    IMPORT -->|"WRITE"| TRAN

    style DT fill:#ffe0e0
    style IN fill:#ffe0e0
    style REJ fill:#ffe0e0
    style OUT fill:#ffe0e0
    style ACCT fill:#e0e0ff
    style TRAN fill:#e0e0ff
    style XREF fill:#e0e0ff
    style TCAT fill:#e0e0ff
```

---

## Error Handling Flow

```mermaid
flowchart TB
    subgraph "File Operation"
        OPEN[OPEN FILE]
        CHECK1{Status = '00'?}
        READ[READ/WRITE/REWRITE]
        CHECK2{Status = '00'?}
        CLOSE[CLOSE FILE]
    end

    subgraph "Error Handling"
        DISPLAY["DISPLAY 'ERROR...'"]
        IOSTATUS["9910-DISPLAY-IO-STATUS"]
        ABEND["9999-ABEND-PROGRAM<br/>CALL 'CEE3ABD'"]
    end

    subgraph "Success Path"
        CONTINUE[Continue Processing]
    end

    OPEN --> CHECK1
    CHECK1 -->|"Yes"| READ
    CHECK1 -->|"No"| DISPLAY
    READ --> CHECK2
    CHECK2 -->|"Yes"| CONTINUE
    CHECK2 -->|"No"| DISPLAY
    DISPLAY --> IOSTATUS
    IOSTATUS --> ABEND

    CONTINUE --> CLOSE

    style ABEND fill:#f99
    style DISPLAY fill:#ff9
```

---

## Job Dependencies

```mermaid
gantt
    title Daily Batch Schedule
    dateFormat HH:mm
    axisFormat %H:%M

    section Account Ops
    CBACT02C (Account Refresh)    :a1, 00:00, 30m

    section Transaction Posting
    CBTRN02C (Daily Posting)      :a2, after a1, 60m

    section Interest Calc
    CBTRN03C (Interest)           :a3, after a2, 45m

    section Optional
    CBEXPORT (Export)             :a4, after a3, 30m
```

---

## Data Exchange Components

```mermaid
flowchart LR
    subgraph "Source System"
        EBCDIC[(VSAM<br/>EBCDIC)]
    end

    subgraph "Export Process"
        EXP[CBEXPORT]
    end

    subgraph "External Exchange"
        ASCII[/ASCII<br/>Files/]
    end

    subgraph "Import Process"
        IMP[CBIMPORT]
    end

    subgraph "Target System"
        VSAM[(VSAM<br/>EBCDIC)]
    end

    EBCDIC -->|"Read EBCDIC"| EXP
    EXP -->|"Write ASCII"| ASCII
    ASCII -->|"Read ASCII"| IMP
    IMP -->|"Write EBCDIC"| VSAM

    style EBCDIC fill:#e0e0ff
    style ASCII fill:#e0ffe0
    style VSAM fill:#e0e0ff
```

---

## Cross-References

- **Full Documentation**: [C4-L3-COMPONENT.md](../C4-L3-COMPONENT.md)
- **CICS Components**: [component-cics.md](./component-cics.md)
- **Data Flow**: [data-flow.md](./data-flow.md)
- **Source**: `app/cbl/CB*.cbl`, `app/jcl/*.jcl`
