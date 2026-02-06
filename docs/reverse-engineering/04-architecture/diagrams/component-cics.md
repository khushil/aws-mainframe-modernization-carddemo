# C4 Level 3: CICS Online Component Diagram

## CICS Region Component Architecture

```mermaid
C4Component
    title CardDemo CICS Online Region - Component Diagram

    Container_Boundary(cics, "CICS Online Region") {
        Component(auth, "Authentication", "COSGN00C", "User login and session establishment")
        Component(adminmenu, "Admin Menu", "COADM01C", "Admin navigation and routing")
        Component(usermenu, "User Menu", "COMEN01C", "User navigation and routing")

        Component(acctview, "Account View", "COACTVWC", "Display account details")
        Component(acctupd, "Account Update", "COACTUPC", "Modify account attributes")

        Component(cardlist, "Card List", "COCRDLIC", "Browse credit cards")
        Component(cardview, "Card View", "COCRDSLC", "Display card details")
        Component(cardupd, "Card Update", "COCRDUPC", "Modify card attributes")

        Component(trnlist, "Transaction List", "COTRN00C", "Browse transactions")
        Component(trnview, "Transaction View", "COTRN01C", "Display transaction details")
        Component(trnadd, "Transaction Add", "COTRN02C", "Create new transaction")

        Component(billpay, "Bill Payment", "COBIL00C", "Process bill payments")
        Component(reports, "Reports", "CORPT00C", "Generate reports")

        Component(usrlist, "User List", "COUSR00C", "Browse users")
        Component(usradd, "User Add", "COUSR01C", "Create user")
        Component(usrupd, "User Update", "COUSR02C", "Modify user")
        Component(usrdel, "User Delete", "COUSR03C", "Remove user")
    }

    ContainerDb(vsam, "VSAM Data Store", "VSAM KSDS", "Primary data files")
    Container(bms, "BMS Presentation", "BMS Mapsets", "Screen definitions")

    Rel(auth, adminmenu, "XCTL (Admin)")
    Rel(auth, usermenu, "XCTL (User)")

    Rel(adminmenu, usrlist, "XCTL")
    Rel(adminmenu, usradd, "XCTL")
    Rel(adminmenu, usrupd, "XCTL")
    Rel(adminmenu, usrdel, "XCTL")

    Rel(usermenu, acctview, "XCTL")
    Rel(usermenu, acctupd, "XCTL")
    Rel(usermenu, cardlist, "XCTL")
    Rel(usermenu, trnlist, "XCTL")
    Rel(usermenu, billpay, "XCTL")
    Rel(usermenu, reports, "XCTL")

    Rel(auth, vsam, "READ USRSEC")
    Rel(acctview, vsam, "READ ACCTDAT")
    Rel(billpay, vsam, "READ/WRITE TRANSACT, ACCTDAT")

    Rel(auth, bms, "SEND/RECEIVE")

    UpdateLayoutConfig($c4ShapeInRow="4", $c4BoundaryInRow="1")
```

---

## Alternative: Mermaid Flowchart Version

```mermaid
flowchart TB
    subgraph CICS["CICS Online Region"]
        direction TB

        subgraph Authentication["Authentication Group"]
            AUTH["COSGN00C<br/>Signon<br/>CC00"]
        end

        subgraph Navigation["Menu Navigation"]
            ADMIN["COADM01C<br/>Admin Menu<br/>CA00"]
            USER["COMEN01C<br/>User Menu<br/>CM00"]
        end

        subgraph AccountMgmt["Account Management"]
            AVWC["COACTVWC<br/>Account View"]
            AUPC["COACTUPC<br/>Account Update"]
        end

        subgraph CardMgmt["Card Management"]
            CLIC["COCRDLIC<br/>Card List"]
            CSLC["COCRDSLC<br/>Card View"]
            CUPC["COCRDUPC<br/>Card Update"]
        end

        subgraph TranProc["Transaction Processing"]
            T00C["COTRN00C<br/>List"]
            T01C["COTRN01C<br/>View"]
            T02C["COTRN02C<br/>Add"]
        end

        subgraph BillPay["Bill Payment"]
            BILC["COBIL00C<br/>Payment<br/>CB00"]
        end

        subgraph UserAdmin["User Administration"]
            U00C["COUSR00C<br/>List"]
            U01C["COUSR01C<br/>Add"]
            U02C["COUSR02C<br/>Update"]
            U03C["COUSR03C<br/>Delete"]
        end

        subgraph Reports["Reports"]
            RPT["CORPT00C<br/>Reports"]
        end
    end

    subgraph DataStores["Data Stores"]
        VSAM[("VSAM Files")]
        BMS["BMS Maps"]
    end

    %% Authentication routing
    AUTH -->|"Admin Type"| ADMIN
    AUTH -->|"User Type"| USER

    %% Admin menu routing
    ADMIN --> U00C
    ADMIN --> U01C
    ADMIN --> U02C
    ADMIN --> U03C

    %% User menu routing
    USER --> AVWC
    USER --> AUPC
    USER --> CLIC
    USER --> CSLC
    USER --> CUPC
    USER --> T00C
    USER --> T01C
    USER --> T02C
    USER --> BILC
    USER --> RPT

    %% Card navigation
    CLIC --> CSLC
    CSLC --> CUPC

    %% All programs use BMS and VSAM
    AUTH --> BMS
    AUTH --> VSAM

    %% Styling
    classDef auth fill:#f9f,stroke:#333
    classDef menu fill:#bbf,stroke:#333
    classDef feature fill:#bfb,stroke:#333
    classDef admin fill:#fbb,stroke:#333

    class AUTH auth
    class ADMIN,USER menu
    class AVWC,AUPC,CLIC,CSLC,CUPC,T00C,T01C,T02C,BILC,RPT feature
    class U00C,U01C,U02C,U03C admin
```

---

## Program Navigation Flow

```mermaid
flowchart LR
    subgraph Entry["Entry Point"]
        SIGN[COSGN00C<br/>CC00]
    end

    subgraph AdminPath["Admin Path"]
        ADM[COADM01C<br/>CA00]
        USR0[COUSR00C]
        USR1[COUSR01C]
        USR2[COUSR02C]
        USR3[COUSR03C]
    end

    subgraph UserPath["User Path"]
        MEN[COMEN01C<br/>CM00]
        ACCT[Account<br/>Group]
        CARD[Card<br/>Group]
        TRAN[Transaction<br/>Group]
        BILL[COBIL00C<br/>CB00]
        RPT[CORPT00C]
    end

    SIGN -->|"Type='A'"| ADM
    SIGN -->|"Type='U'"| MEN

    ADM --> USR0
    ADM --> USR1
    ADM --> USR2
    ADM --> USR3

    MEN --> ACCT
    MEN --> CARD
    MEN --> TRAN
    MEN --> BILL
    MEN --> RPT

    USR0 -->|"PF3"| ADM
    USR1 -->|"PF3"| ADM
    USR2 -->|"PF3"| ADM
    USR3 -->|"PF3"| ADM

    ACCT -->|"PF3"| MEN
    CARD -->|"PF3"| MEN
    TRAN -->|"PF3"| MEN
    BILL -->|"PF3"| MEN
    RPT -->|"PF3"| MEN

    ADM -->|"PF3"| SIGN
    MEN -->|"PF3"| SIGN

    style SIGN fill:#f9f
    style ADM fill:#fbb
    style MEN fill:#bbf
```

---

## Component-VSAM Relationship

```mermaid
flowchart TB
    subgraph Programs["CICS Programs"]
        AUTH[COSGN00C]
        AVWC[COACTVWC]
        AUPC[COACTUPC]
        CLIC[COCRDLIC]
        BILC[COBIL00C]
        T02C[COTRN02C]
        USR[COUSR*]
    end

    subgraph VSAM["VSAM Files"]
        USRSEC[(USRSEC)]
        ACCTDAT[(ACCTDAT)]
        CARDDAT[(CARDDAT)]
        CUSTDAT[(CUSTDAT)]
        TRANSACT[(TRANSACT)]
        CCXREF[(CCXREF)]
    end

    AUTH -->|"R"| USRSEC
    USR -->|"CRUD"| USRSEC

    AVWC -->|"R"| ACCTDAT
    AVWC -->|"R"| CUSTDAT
    AVWC -->|"R"| CCXREF

    AUPC -->|"RW"| ACCTDAT
    AUPC -->|"R"| CUSTDAT

    CLIC -->|"R"| CARDDAT
    CLIC -->|"R"| CCXREF

    BILC -->|"RW"| ACCTDAT
    BILC -->|"RW"| TRANSACT
    BILC -->|"R"| CCXREF

    T02C -->|"W"| TRANSACT
    T02C -->|"R"| ACCTDAT

    style USRSEC fill:#fbb
    style ACCTDAT fill:#bfb
    style CARDDAT fill:#bbf
    style TRANSACT fill:#ff9
```

---

## Optional Extension Components

```mermaid
flowchart TB
    subgraph Core["Core CICS"]
        MEN[COMEN01C]
        ADM[COADM01C]
    end

    subgraph AuthExt["Authorization Extension<br/>[Optional - IMS/DB2/MQ]"]
        PA0[COPAUA0C<br/>Auth Processing]
        PS0[COPAUS0C<br/>Pending View]
        PS1[COPAUS1C<br/>Auth Details]
        PS2[COPAUS2C<br/>Auth Update]
    end

    subgraph TranTypeExt["Transaction Type Extension<br/>[Optional - DB2]"]
        TLI[COTRTLIC<br/>Type List]
        TUP[COTRTUPC<br/>Type Update]
    end

    subgraph VSAMMQExt["VSAM-MQ Extension<br/>[Optional - MQ]"]
        AC1[COACCT01<br/>Account Extract]
        DT1[CODATE01<br/>Date Utility]
    end

    MEN -.->|"Option 11"| PS0
    ADM -.->|"Option 5"| TLI
    ADM -.->|"Option 6"| TUP

    PS0 --> PS1
    PS0 --> PS2
    PA0 --> PS0

    subgraph ExtData["Extension Data Stores"]
        IMS[(IMS DB)]
        DB2[(DB2)]
        MQ[[IBM MQ]]
    end

    PA0 -.-> IMS
    PA0 -.-> DB2
    PA0 -.-> MQ
    PS0 -.-> IMS
    PS0 -.-> DB2

    TLI -.-> DB2
    TUP -.-> DB2

    AC1 -.-> MQ

    style AuthExt fill:#e0f0ff
    style TranTypeExt fill:#fff0e0
    style VSAMMQExt fill:#e0ffe0
```

---

## Component State Machine

```mermaid
stateDiagram-v2
    [*] --> COSGN00C: User connects

    COSGN00C --> COADM01C: Admin login
    COSGN00C --> COMEN01C: User login
    COSGN00C --> [*]: PF3/Invalid

    state AdminMenu {
        COADM01C --> COUSR00C: Option 1
        COADM01C --> COUSR01C: Option 2
        COADM01C --> COUSR02C: Option 3
        COADM01C --> COUSR03C: Option 4
        COUSR00C --> COADM01C: PF3
        COUSR01C --> COADM01C: PF3
        COUSR02C --> COADM01C: PF3
        COUSR03C --> COADM01C: PF3
    }

    state UserMenu {
        COMEN01C --> COACTVWC: Option 1
        COMEN01C --> COBIL00C: Option 10
        COACTVWC --> COMEN01C: PF3
        COBIL00C --> COMEN01C: PF3
    }

    COADM01C --> COSGN00C: PF3
    COMEN01C --> COSGN00C: PF3
```

---

## Cross-References

- **Full Documentation**: [C4-L3-COMPONENT.md](../C4-L3-COMPONENT.md)
- **Batch Components**: [component-batch.md](./component-batch.md)
- **Data Flow**: [data-flow.md](./data-flow.md)
- **Source**: `app/cbl/CO*.cbl`, `app/cpy/COCOM01Y.cpy`
