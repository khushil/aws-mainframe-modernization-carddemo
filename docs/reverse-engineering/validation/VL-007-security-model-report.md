# VL-007: Security Model Validation Report

**Validation Date:** 2026-02-06
**Document Under Validation:** `docs/reverse-engineering/05-specialized/SECURITY-MODEL.md`
**Validator:** Claude Code (Automated Cross-Reference Validation)
**Work Files:** `.work/reverse-engineering/validation/vl-007/`

---

## 1. Verdict

| Metric | Value |
|--------|-------|
| **Overall Score** | **90 / 100** |
| **Verdict** | **PASS** |
| **Hallucinations Detected** | 0 |
| **Critical Findings** | 0 |
| **Major Findings** | 3 |
| **Minor Findings** | 1 |
| **Claims Verified** | 50 |

The Security Model document is **accurate and well-sourced**. All 10 security findings (SEC-001 through SEC-010) are verified against source code with correct line references and exact string literal matches. Three major issues were identified: an incorrect COMMAREA size calculation, one missing program in the access control matrix, and an undocumented security-relevant code pattern. No hallucinations were detected.

---

## 2. Score Breakdown

| Category | Weight | Score | Weighted | Rationale |
|----------|--------|-------|----------|-----------|
| Source Reference Accuracy | 35% | 95 | 33.25 | All file paths, line numbers, and code snippets verified. COSGN00C.cbl lines 209-257 match exactly. CSUSR01Y.cpy:17-23, COCOM01Y.cpy:19-44 all correct. Minor: doc shows lines 17-22 for 6 fields but actual is 17-23 (01-level at 17). |
| Factual Accuracy | 25% | 93 | 23.25 | Authentication flow (12 steps), role routing logic, error messages, password comparison, menu authorization check all verified. All 10 SEC findings confirmed. Deduction: COMMAREA size error. |
| Completeness | 20% | 83 | 16.60 | 16 of 18 CO*.cbl programs in access matrix (COBSWAIT missing). SET CDEMO-USRTYP-USER pattern (5 programs, 9 locations) undocumented. COPAUS0C referenced but non-existent without notation. |
| Quantitative Accuracy | 10% | 78 | 7.80 | COMMAREA stated as "~130 bytes" but actual is 160 bytes (23% error). SEC-USER-DATA 80-byte record correct. Menu option counts (11 user, 6 admin) correct. CVSS scores reasonable. |
| Documentation Quality | 10% | 90 | 9.00 | Well-structured with Mermaid diagrams, tables, PCI-DSS gap analysis, and modernization recommendations. Cross-references to other RE documents provided. Code snippets properly formatted. |
| **Total** | **100%** | | **89.9 ~ 90** | |

---

## 3. Critical Findings

**None.**

No critical issues (hallucinations, fabricated code, invented file references, or fundamentally wrong security claims) were found.

---

## 4. Major Findings

### M-001: COMMAREA Size Incorrect

| Attribute | Value |
|-----------|-------|
| **Severity** | MAJOR |
| **Location in Doc** | Section 4.1, line stating "**Total Size:** ~130 bytes" |
| **Source File** | `app/cpy/COCOM01Y.cpy:19-44` |
| **Document Claims** | "~130 bytes" |
| **Actual Value** | **160 bytes** |

**Evidence:**

The COMMAREA is defined in `COCOM01Y.cpy:19-44` with the following group totals:

| Group | Fields | Bytes |
|-------|--------|-------|
| CDEMO-GENERAL-INFO | FROM-TRANID(4) + FROM-PROGRAM(8) + TO-TRANID(4) + TO-PROGRAM(8) + USER-ID(8) + USER-TYPE(1) + PGM-CONTEXT(1) | 34 |
| CDEMO-CUSTOMER-INFO | CUST-ID(9) + CUST-FNAME(25) + CUST-MNAME(25) + CUST-LNAME(25) | 84 |
| CDEMO-ACCOUNT-INFO | ACCT-ID(11) + ACCT-STATUS(1) | 12 |
| CDEMO-CARD-INFO | CARD-NUM(16) | 16 |
| CDEMO-MORE-INFO | LAST-MAP(7) + LAST-MAPSET(7) | 14 |
| **Total** | | **160** |

The document correctly lists every individual field and PIC clause. Only the summary total is wrong (30-byte / 23% discrepancy).

**Required Fix:** Change "~130 bytes" to "160 bytes" in Section 4.1.

---

### M-002: COBSWAIT Missing from Access Control Matrix

| Attribute | Value |
|-----------|-------|
| **Severity** | MAJOR |
| **Location in Doc** | Section 3.2, Access Control Matrix table |
| **Source File** | `app/cbl/COBSWAIT.cbl` (42 lines) |
| **Document Claims** | 16 programs listed in the matrix |
| **Actual Count** | 18 CO*.cbl files exist; 17 are interactive CICS programs + COBSWAIT |

**Evidence:**

`COBSWAIT.cbl` is a batch utility program (accepts PARM from SYSIN, calls MVSWAIT). It uses the `CO*` prefix but has no CICS commands and no COMMAREA. The codebase contains 18 files matching `CO*.cbl`:

```
COACTUPC COACTVWC COADM01C COBIL00C COBSWAIT COCRDLIC
COCRDSLC COCRDUPC COMEN01C CORPT00C COSGN00C COTRN00C
COTRN01C COTRN02C COUSR00C COUSR01C COUSR02C COUSR03C
```

The access control matrix accounts for 16 of these (all except COBSWAIT and the non-existent COPAUS0C referenced in COMEN02Y.cpy).

**Required Fix:** Add COBSWAIT row to the access matrix with a note that it is a batch utility with no CICS security context.

---

### M-003: Undocumented SET CDEMO-USRTYP-USER TO TRUE Pattern

| Attribute | Value |
|-----------|-------|
| **Severity** | MAJOR |
| **Location in Doc** | Not present (should be in Section 3 or Section 5) |
| **Affected Programs** | 5 programs, 9 code locations |

**Evidence:**

Multiple online programs forcibly set the user type to 'U' (regular user) during execution:

| Program | Line(s) | Context |
|---------|---------|---------|
| `COACTVWC.cbl` | 344 | Account View |
| `COACTUPC.cbl` | 947 | Account Update |
| `COCRDLIC.cbl` | 320, 388, 466, 522, 550 | Credit Card List (5 instances) |
| `COCRDSLC.cbl` | 326 | Credit Card View |
| `COCRDUPC.cbl` | 464 | Credit Card Update |

This pattern modifies the COMMAREA session state, resetting `CDEMO-USER-TYPE` from 'A' to 'U'. An admin user navigating through these programs would have their role downgraded in the session, potentially losing admin menu access upon return. This is security-relevant behavior that should be documented in the security model.

**Required Fix:** Add a subsection in Section 3 or Section 5 documenting this privilege-reset pattern, its locations, and its impact on admin sessions.

---

## 5. Minor Findings

### m-001: COPAUS0C Referenced but Program File Does Not Exist

| Attribute | Value |
|-----------|-------|
| **Severity** | MINOR |
| **Location in Doc** | Section 3.4, User Menu table, option 11 |
| **Source Reference** | `app/cpy/COMEN02Y.cpy:86-90` |

**Evidence:**

The user menu copybook `COMEN02Y.cpy` defines option 11 as `COPAUS0C` (Pending Authorization View) with user type 'U'. The document correctly lists this in the menu options table. However, no file `app/cbl/COPAUS0C.cbl` exists in the codebase. This appears to be a planned but unimplemented feature. The `COMEN01C.cbl` program actually has special handling for this case (line 147: `WHEN CDEMO-MENU-OPT-PGMNAME(WS-OPTION) = 'COPAUS0C'` with a CICS INQUIRE check).

**Suggested Fix:** Add a footnote noting that COPAUS0C is referenced in the menu configuration but not present in the codebase.

---

## 6. Hallucination Inventory

| ID | Check Description | Target | Result | Notes |
|----|-------------------|--------|--------|-------|
| H-001 | Non-existent copybook COUSR00Y.cpy referenced | Entire document | **PASS** | Document correctly uses `CSUSR01Y.cpy` throughout |
| H-002 | Fabricated COMMAREA fields CDEMO-USER-FNAME/LNAME | Section 4.1 | **PASS** | Only actual fields from `COCOM01Y.cpy` shown |
| H-003 | Non-existent copybook CVCAR00Y.cpy referenced | Section 5 | **PASS** | Correct `CVACT02Y.cpy` used for card data |
| H-004 | Fabricated PIC clause S9(7)V99 | Entire document | **PASS** | No invented PIC clauses found |

**Verdict: Zero hallucinations detected.** All file references, field names, PIC clauses, and code snippets correspond to actual source code artifacts.

---

## 7. Completeness Gaps - Program Authorization Matrix

| # | Program | In Doc Matrix | Auth Check | SET Pattern | Status |
|---|---------|:------------:|------------|:-----------:|--------|
| 1 | COSGN00C | Yes | Entry point (authentication) | No | Verified |
| 2 | COADM01C | Yes | Role routing at login | No | Verified |
| 3 | COMEN01C | Yes | Lines 136-143 | No | Verified |
| 4 | COACTVWC | Yes | NONE | Line 344 | Verified (SET undocumented) |
| 5 | COACTUPC | Yes | NONE | Line 947 | Verified (SET undocumented) |
| 6 | COCRDLIC | Yes | NONE | Lines 320,388,466,522,550 | Verified (SET undocumented) |
| 7 | COCRDSLC | Yes | NONE | Line 326 | Verified (SET undocumented) |
| 8 | COCRDUPC | Yes | NONE | Line 464 | Verified (SET undocumented) |
| 9 | COTRN00C | Yes | NONE | No | Verified |
| 10 | COTRN01C | Yes | NONE | No | Verified |
| 11 | COTRN02C | Yes | NONE | No | Verified |
| 12 | CORPT00C | Yes | NONE | No | Verified |
| 13 | COBIL00C | Yes | NONE | No | Verified |
| 14 | COUSR00C | Yes | Menu only (COADM01C) | No | Verified |
| 15 | COUSR01C | Yes | Menu only (COADM01C) | No | Verified |
| 16 | COUSR02C | Yes | Menu only (COADM01C) | No | Verified |
| 17 | COUSR03C | Yes | Menu only (COADM01C) | No | Verified |
| 18 | **COBSWAIT** | **No** | NONE (batch utility) | No | **MISSING (M-002)** |

---

## 8. Source Reference Verification

### 8.1 Authentication Flow (Section 2.1)

All 12 steps of the authentication flow verified against `app/cbl/COSGN00C.cbl` (261 lines):

| Step | Document Claim | Code Reference | Verified |
|------|---------------|----------------|:--------:|
| UPPER-CASE transform | Lines 132-136 | `FUNCTION UPPER-CASE(USERIDI OF COSGN0AI)` at line 132 | Yes |
| VSAM READ | Lines 211-219 | `EXEC CICS READ DATASET(WS-USRSEC-FILE) INTO(SEC-USER-DATA) RIDFLD(WS-USER-ID)` | Yes |
| Password comparison | Line 223 | `IF SEC-USR-PWD = WS-USER-PWD` | Yes |
| Session MOVEs | Lines 224-228 | 5 MOVE statements populating COMMAREA | Yes |
| Admin routing | Lines 230-234 | `IF CDEMO-USRTYP-ADMIN` -> `XCTL PROGRAM('COADM01C')` | Yes |
| User routing | Lines 236-239 | `ELSE` -> `XCTL PROGRAM('COMEN01C')` | Yes |
| Wrong password msg | Line 242 | `'Wrong Password. Try again ...'` (exact match) | Yes |
| User not found msg | Line 249 | `'User not found. Try again ...'` (exact match) | Yes |

### 8.2 User Security Record (Section 2.3)

`app/cpy/CSUSR01Y.cpy` (27 lines) verified:

| Field | Doc PIC | Actual PIC | Line | Match |
|-------|---------|------------|------|:-----:|
| SEC-USR-ID | PIC X(08) | PIC X(08) | 18 | Yes |
| SEC-USR-FNAME | PIC X(20) | PIC X(20) | 19 | Yes |
| SEC-USR-LNAME | PIC X(20) | PIC X(20) | 20 | Yes |
| SEC-USR-PWD | PIC X(08) | PIC X(08) | 21 | Yes |
| SEC-USR-TYPE | PIC X(01) | PIC X(01) | 22 | Yes |
| SEC-USR-FILLER | PIC X(23) | PIC X(23) | 23 | Yes |

Record length: 8+20+20+8+1+23 = **80 bytes** (matches document claim).

### 8.3 COMMAREA Structure (Section 4.1)

`app/cpy/COCOM01Y.cpy` (48 lines) verified. All 14 field names and PIC clauses match. 88-level conditions at lines 27-28 (`CDEMO-USRTYP-ADMIN VALUE 'A'`, `CDEMO-USRTYP-USER VALUE 'U'`) confirmed. Only the total size claim is incorrect (see M-001).

### 8.4 Menu Configurations (Sections 3.4)

| Source | Doc Count | Actual Count | Verified |
|--------|-----------|-------------|:--------:|
| `COMEN02Y.cpy` (user menu) | 11 options | `CDEMO-MENU-OPT-COUNT VALUE 11` at line 21 | Yes |
| `COADM02Y.cpy` (admin menu) | 6 options | `CDEMO-ADMIN-OPT-COUNT VALUE 6` at line 22 | Yes |

All option names, program names, and user types match between document and source.

### 8.5 Security Finding SEC-009 (SSN Display)

`app/cbl/COACTVWC.cbl:496-504` confirmed:
```cobol
STRING
    CUST-SSN(1:3)
    '-'
    CUST-SSN(4:2)
    '-'
    CUST-SSN(6:4)
    DELIMITED BY SIZE
    INTO ACSTSSNO OF CACTVWAO
END-STRING
```
Full SSN displayed with formatting only (no masking). Document claim verified.

### 8.6 Sensitive Data Fields (SEC-006)

| File | Field | Doc PIC | Actual PIC | Line | Verified |
|------|-------|---------|------------|------|:--------:|
| `CVACT02Y.cpy` | CARD-NUM | PIC X(16) | PIC X(16) | 5 | Yes |
| `CVACT02Y.cpy` | CARD-CVV-CD | PIC 9(03) | PIC 9(03) | 7 | Yes |
| `CVTRA05Y.cpy` | TRAN-CARD-NUM | PIC X(16) | PIC X(16) | 15 | Yes |
| `COCOM01Y.cpy` | CDEMO-CARD-NUM | PIC 9(16) | PIC 9(16) | 41 | Yes |

---

## 9. Recommendations

### Required Fixes (3)

1. **M-001:** In Section 4.1, change `**Total Size:** ~130 bytes` to `**Total Size:** 160 bytes`. Add the breakdown: `34 (General) + 84 (Customer) + 12 (Account) + 16 (Card) + 14 (More) = 160`.

2. **M-002:** Add a row to the Section 3.2 Access Control Matrix for COBSWAIT:

   | Function | Program | Admin (A) | User (U) | Auth Check Location | Notes |
   |----------|---------|:---------:|:--------:|---------------------|-------|
   | **Wait Utility** | COBSWAIT | N/A | N/A | NONE | Batch utility, no CICS context |

3. **M-003:** Add a new subsection (e.g., Section 3.5 "Runtime Role Modification") documenting the `SET CDEMO-USRTYP-USER TO TRUE` pattern found in 5 programs (COACTVWC, COACTUPC, COCRDLIC, COCRDSLC, COCRDUPC) at 9 code locations. Note the security implication: admin sessions may be downgraded to user-level during navigation.

### Optional Enhancements

- Add a footnote to the COPAUS0C entry in the user menu table (Section 3.4) noting the program file does not exist in the codebase.
- Consider adding the WHEN OTHER error path (`COSGN00C.cbl:252-256`) to the authentication sequence diagram for completeness.
- Cross-reference the SET CDEMO-USRTYP-USER pattern with SEC-008 (Unsigned COMMAREA) as both relate to session state integrity.

---

## 10. Remediation Manifest

| ID | Target File | Section | Location | Current Value | Correct Value | Priority |
|----|-------------|---------|----------|---------------|---------------|----------|
| M-001 | `SECURITY-MODEL.md` | 4.1 | "Total Size" line | `~130 bytes` | `160 bytes` | Required |
| M-002 | `SECURITY-MODEL.md` | 3.2 | Access Control Matrix table | 16 rows (COBSWAIT absent) | 17 rows (add COBSWAIT) | Required |
| M-003 | `SECURITY-MODEL.md` | 3.x (new) | New subsection needed | N/A (not documented) | Document SET CDEMO-USRTYP-USER pattern in 5 programs | Required |
| m-001 | `SECURITY-MODEL.md` | 3.4 | User Menu table, option 11 | `COPAUS0C` listed without note | Add footnote: "Program not present in codebase" | Optional |

---

## Appendix: Validation Methodology

### Process
1. **Claims Inventory:** Extracted 50 verifiable claims from the document (field names, line references, PIC clauses, error messages, quantitative values).
2. **Source Code Cross-Reference:** Each claim verified against actual source files using exact file reads and grep searches.
3. **Hallucination Detection:** Specifically checked for 4 common hallucination patterns (fabricated copybooks, invented fields, wrong PIC clauses, non-existent files).
4. **Completeness Audit:** Enumerated all CO*.cbl programs and compared against the documented access control matrix.
5. **Pattern Discovery:** Searched for undocumented security-relevant patterns in the codebase.

### Files Examined
| File | Lines | Purpose |
|------|-------|---------|
| `app/cbl/COSGN00C.cbl` | 261 | Authentication entry point |
| `app/cpy/CSUSR01Y.cpy` | 27 | User security record layout |
| `app/cpy/COCOM01Y.cpy` | 48 | COMMAREA session structure |
| `app/cbl/COMEN01C.cbl` | 149+ | User menu with auth check |
| `app/cbl/COADM01C.cbl` | 289 | Admin menu |
| `app/cbl/COBSWAIT.cbl` | 42 | Wait utility (missing from doc) |
| `app/cpy/COMEN02Y.cpy` | 102 | User menu options (11 entries) |
| `app/cpy/COADM02Y.cpy` | 63 | Admin menu options (6 entries) |
| `app/cpy/CVACT02Y.cpy` | 15 | Card record with CVV field |
| `app/cpy/CVTRA05Y.cpy` | 22 | Transaction record with PAN |
| `app/cbl/COACTVWC.cbl` | 509+ | Account view (SSN display, SET pattern) |
| `app/cbl/COACTUPC.cbl` | 947+ | Account update (SET pattern) |
| `app/cbl/COCRDLIC.cbl` | 550+ | Card list (5x SET pattern) |
| `app/cbl/COCRDSLC.cbl` | 326+ | Card view (SET pattern) |
| `app/cbl/COCRDUPC.cbl` | 464+ | Card update (SET pattern) |

### Work Files
All intermediate validation artifacts are stored in `.work/reverse-engineering/validation/vl-007/`:
- `progress.yaml` - Phase tracking
- `claims-inventory.yaml` - 50 verifiable claims with status
- `auth-flow-findings.yaml` - 12-step authentication verification
- `field-findings.yaml` - Field-level data structure verification
- `authz-findings.yaml` - 18-program authorization check status
- `security-findings-check.yaml` - SEC-001 through SEC-010 verification + hallucination checks

---

*Validation performed by Claude Code on 2026-02-06. This report validates the RE-007 Security Model documentation against the CardDemo mainframe application source code.*
