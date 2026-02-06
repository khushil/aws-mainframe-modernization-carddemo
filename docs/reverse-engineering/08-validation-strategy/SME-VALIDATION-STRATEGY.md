# SME Validation Strategy for Reverse-Engineered Mainframe Documentation

**Document Version:** 1.0
**Date:** 2026-02-06
**Scope:** Subject Matter Expert validation strategy for AI-generated CardDemo reverse engineering documentation
**Classification:** Internal — Methodology & Planning

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [SME Profiles](#2-sme-profiles)
3. [Interview Strategies](#3-interview-strategies)
4. [Confidence Framework](#4-confidence-framework)
5. [Validation Matrix](#5-validation-matrix)
6. [Question Bank](#6-question-bank)
7. [Methodology Validation](#7-methodology-validation)
8. [Risk Assessment](#8-risk-assessment)
9. [Full Interview Playbook](#9-full-interview-playbook)
10. [Tacit Knowledge Capture Framework](#10-tacit-knowledge-capture-framework)

---

## 1. Executive Summary

### The Validation Gap

The AI-driven reverse engineering of CardDemo produced 43 documents (~21,500 lines) across 7 analysis domains. The AI validation suite (VL-001 through VL-007) confirmed **zero hallucinations** in 6 of 7 reports and high factual accuracy, with scores ranging from 73.8 to 96.8 out of 100.

However, AI validation can only verify **what the code says** — not **what the code means**, **what's missing from the code**, or **whether interpretations are correct**.

**Key insight:** The documentation is 97% accurate on syntax, structure, and factual code references. Only SMEs can validate whether the *semantic interpretations* — bounded contexts, business rule intent, security exploitability, operational procedures, and design rationale — are correct.

### The Constraint

The client's technical team lacks mainframe expertise. Only **1–2 retired or former mainframe staff** are available to engage. This strategy is designed to extract maximum validation value from severely limited SME access.

### Scope

- **Ideal roster:** 8 SME roles across 4 priority tiers (documented for reference and future engagement)
- **Realistic engagement:** 1–2 SMEs, 3–5 sessions of 3–4 hours each
- **Deliverable:** Validated confidence scores per documentation area, tacit knowledge capture, methodology assessment

### Downstream Uses

This documentation serves four purposes. Each imposes different confidence requirements:

| Use Case | Tolerance for Error | Key Validation Need |
|----------|--------------------|--------------------|
| **Modernization project** | Low — wrong architecture decisions are expensive | Architecture, data model, business rules |
| **RFP / due diligence** | Medium — vendors add discovery phase | Scope accuracy, risk identification |
| **Compliance / audit** | Very low — regulators require evidence | Security model, data handling, controls |
| **Knowledge preservation** | High — even imperfect capture has value | Tacit knowledge, design rationale |

### AI Validation Score Summary

| Report | Target | Score | Verdict | Hallucinations | Key Gaps |
|--------|--------|-------|---------|----------------|----------|
| VL-001 | Domain Model | 88.0 | PASS | 0 | 13/29 copybooks missing from coverage |
| VL-002 | Data Model | 96.8 | PASS | 0 | Minor field count discrepancy |
| VL-003 | Context Model | 80.6 | FAIL | 0 | 5/17 BMS names wrong, TransID table 17.6% complete |
| VL-004 | C4 Architecture | 90.0 | FAIL | 0 | 3 programs missing, numeric tallies wrong |
| VL-005 | Screen Flows | 90.6 | FAIL | 0 | Missing BMS copybook refs, 2 PF key gaps |
| VL-006 | Batch Workflows | 73.8 | FAIL | 3 | 6/38 JCL missing, phantom MNTTRDB2, wrong counts |
| VL-007 | Security Model | 90.0 | PASS | 0 | COMMAREA size wrong (~130 vs 160 bytes) |

**The weakest areas requiring priority SME attention are VL-006 (Batch Workflows, 73.8) and VL-003 (Context Model, 80.6).**

---

## 2. SME Profiles

### 2.1 Full Roster (8 Roles, 4 Priority Tiers)

This is the ideal SME team for reference. In practice, 1–2 people will cover multiple roles.

#### CRITICAL (Must-Have)

**Role 1: Mainframe Technical Architect**
- **Experience:** 10+ years COBOL/CICS/VSAM application development
- **Validates:** C4 architecture (VL-004), bounded contexts (VL-003), COMMAREA design, program structure rationale, CICS transaction flow patterns
- **Key question areas:** Why are programs structured this way? Are the bounded context boundaries correct? Is the COMMAREA design typical or unusual?
- **Cannot be replaced by:** AI validation — architecture *interpretation* requires human judgment

**Role 2: Credit Card Processing Domain Expert / Business Analyst**
- **Experience:** 5+ years in credit card operations or issuer processing
- **Validates:** 58 business rules (BR-V001 through BR-R005), domain model accuracy, ubiquitous language correctness, domain event completeness
- **Key question areas:** Are the interest calculation rules complete? What business rules exist outside the code? Are rejection codes 100–109 the full set?
- **Cannot be replaced by:** Code analysis — business rules enforced by operations, policy, or external systems are invisible to AI

#### HIGH (Strongly Recommended)

**Role 3: VSAM Data Architect / DBA**
- **Experience:** 8+ years VSAM design, alternate indexes, data modeling
- **Validates:** Data model (VL-002), record layouts, FILLER field purpose, alternate index design rationale, data lineage
- **Key question areas:** Why FILLER in SEC-USR-FILLER (23 bytes)? Is KEYS(9,0) vs KEYS(10,0) for CUSTDAT an error or variant? Are the alternate index choices optimal?

**Role 4: CICS Systems Programmer**
- **Experience:** 8+ years CICS administration, security, BMS maps
- **Validates:** Security model (VL-007) exploitability, screen flows (VL-005), BMS map accuracy, session management, RACF/external security controls
- **Key question areas:** Is SEC-005 (missing per-program auth) exploitable in a real CICS environment with RACF? Are there external controls the code doesn't show?

#### MEDIUM (Recommended)

**Role 5: JCL/Batch Operations Specialist**
- **Experience:** 5+ years batch scheduling, JCL, operations
- **Validates:** Batch workflows (VL-006, scored 73.8 — weakest area), job dependencies, restart/recovery procedures, SLAs
- **Key question areas:** What are the 6 missing JCL files (CREASTMT, FTPJCL, INTRDRJ1/2, TXT2PDF1, DEFCUST)? Is the phantom MNTTRDB2 a real external job? What are the batch scheduling SLAs?

**Role 6: Security / Compliance Specialist**
- **Experience:** PCI-DSS audit, mainframe security (RACF/ACF2/Top Secret)
- **Validates:** PCI-DSS gap analysis accuracy (15% compliance score), 10 security findings exploitability, compliance readiness assessment
- **Key question areas:** Are the CVSS scores reasonable? Is the 15% PCI-DSS score accurate given mainframe context (RACF, network isolation)?

**Role 7: Modernization / Cloud Architect**
- **Experience:** Mainframe-to-cloud migrations, API design
- **Validates:** API candidates, migration roadmap, microservice boundary decisions, modernization complexity estimates
- **Key question areas:** Are the proposed bounded contexts viable as microservice boundaries? What migration patterns have worked for similar applications?

#### LOW-MEDIUM (Nice-to-Have)

**Role 8: End User / Business User Representative**
- **Experience:** Actual use of the card processing application
- **Validates:** Screen flows, user journeys, terminology, undocumented shortcuts, operational workarounds
- **Key question areas:** Do the 6 documented user journeys match real usage? Are there undocumented keyboard shortcuts or screen bypasses?

### 2.2 Realistic Scenarios

#### Scenario A: 1 SME Available

**Ideal profile:** Senior COBOL/CICS developer with 15+ years who performed both application development AND system administration. Likely title: "Senior Systems Analyst," "Lead Application Developer," or "Technical Lead — Mainframe."

**Coverage this person provides:**

| Documentation Area | Coverage Level | Notes |
|-------------------|---------------|-------|
| Architecture (VL-004) | High | Can validate program structure, CICS patterns |
| Data Model (VL-002) | High | Knows VSAM layouts, copybook conventions |
| Context Model (VL-003) | High | Understands COMMAREA, bounded contexts |
| Security Model (VL-007) | Medium | Knows CICS security, may not know RACF deeply |
| Screen Flows (VL-005) | Medium | Familiar with BMS but may not recall all maps |
| Batch Workflows (VL-006) | Medium | Knows JCL but may not know operations scheduling |
| Business Rules (BR-*) | Low–Medium | Knows code logic but may not know business intent |
| Domain Model (VL-001) | Medium | Can validate technical terms, not business terms |

**Coverage heat map:**

```
                       Confidence Ceiling (1 SME)
Architecture   ████████████████████░░  ~85%
Data Model     ████████████████████░░  ~85%
Context Model  ████████████████████░░  ~85%
Security       ██████████████░░░░░░░░  ~65%
Screen Flows   ████████████████░░░░░░  ~75%
Batch Workflow ██████████████░░░░░░░░  ~65%
Business Rules ██████████░░░░░░░░░░░░  ~50%
Domain Model   ██████████████░░░░░░░░  ~65%
```

**Session plan:** 3 sessions × 4 hours each

| Session | Focus | Duration |
|---------|-------|----------|
| 1 | Architecture + Data Model | 4 hours |
| 2 | Security + Batch + Business Rules | 4 hours |
| 3 | Methodology + Follow-ups + Open Questions | 3 hours |

**Cannot cover:** Pure business domain expertise, formal security/compliance assessment, cloud modernization architecture validation.

#### Scenario B: 2 SMEs Available

**SME #1:** Senior COBOL/CICS application developer
- Covers: Architecture, CICS, data model, screen flows, context model

**SME #2:** Batch operations specialist or business analyst
- Covers: Batch workflows, business rules, operational procedures, JCL scheduling

**Coverage heat map:**

```
                       Confidence Ceiling (2 SMEs)
Architecture   ██████████████████████  ~90% (SME #1)
Data Model     ██████████████████████  ~90% (SME #1)
Context Model  ██████████████████████  ~90% (SME #1)
Security       ████████████████░░░░░░  ~75% (SME #1 partial)
Screen Flows   ████████████████████░░  ~85% (SME #1)
Batch Workflow ██████████████████████  ~90% (SME #2)
Business Rules ████████████████████░░  ~85% (SME #2)
Domain Model   ████████████████████░░  ~85% (SME #2 domain + SME #1 tech)
```

**Session plan:** 4 individual sessions + 1 joint session

| Session | SME | Focus | Duration |
|---------|-----|-------|----------|
| 1 | SME #1 | Architecture + Data Model | 4 hours |
| 2 | SME #1 | Security + Screen Flows + Context Model | 4 hours |
| 3 | SME #2 | Batch Workflows + Business Rules | 4 hours |
| 4 | SME #2 | Domain Model + Operational Procedures | 3 hours |
| 5 | Both | Cross-validation + Reconciliation + Methodology | 3 hours |

**Covers ~80% of validation needs.** Security/compliance and cloud modernization remain at lower confidence.

### 2.3 Recruitment Guidance

**Where to find retired mainframe staff:**

- **LinkedIn keywords:** "COBOL," "CICS," "VSAM," "mainframe," "z/OS," "MVS," combined with "retired," "consultant," "available"
- **Job titles to search:** Senior Systems Analyst, Mainframe Application Developer, CICS Systems Programmer, MVS Systems Programmer, Technical Lead — Mainframe, Batch Operations Manager
- **Professional associations:** SHARE Inc. (IBM mainframe user group), GUIDE International (merged with SHARE), local IBM user groups
- **Consulting firms:** Micro Focus, Astadia, Advanced, Modern Systems — many employ retired mainframe specialists
- **Mainframe communities:** IBM Z and LinuxONE Community, Planet Mainframe, Mainframe DEV community
- **Recruiting approach:** Frame as knowledge preservation engagement. Many retired mainframe professionals are motivated by preventing knowledge loss. Short-term consulting rates ($150–250/hr) are typical for this expertise level.

---

## 3. Interview Strategies

### 3.1 Blind Validation

**Purpose:** Capture the SME's independent mental model before exposure to AI-generated documentation. This detects AI interpretations that sound plausible but diverge from how the system actually works.

**Procedure:**
1. Describe the application at a high level: "This is a CICS-based credit card account management application using VSAM files."
2. Ask the SME to describe the expected architecture, program structure, and data flow *without* showing any documentation.
3. Record the SME's description verbatim.
4. After the blind portion, reveal the AI-generated documentation and compare.
5. Flag all divergences for investigation.

**When to use:** First 45 minutes of Session 1. Architecture, bounded contexts, high-level design.

**Advantages:** Eliminates anchoring bias. If the SME's mental model matches the AI documentation independently, confidence is very high.

**Disadvantages:** Time-consuming. SME may not recall details without prompting. Works best for high-level architecture, not field-level details.

**CardDemo example:**
- "For a CICS credit card application, how would you expect the program structure to be organized?"
- "What bounded contexts or functional areas would you expect?"
- Compare SME's answer against the 7 bounded contexts in CONTEXT-MAP.md.

### 3.2 Targeted Technical Probes

**Purpose:** Verify specific factual claims from the documentation with focused true/false or multiple-choice questions. High throughput — can validate many claims per hour.

**Procedure:**
1. Present a specific claim from the documentation (e.g., a PIC clause, record layout, VSAM key definition).
2. Ask: "Is this correct?" or "Which of these is correct?"
3. If the SME confirms, mark as validated. If the SME disagrees, record the correct value and source.
4. Move to next claim.

**When to use:** Data model validation, VSAM structures, BMS map details, PIC clause verification.

**Advantages:** High throughput (can validate 20–30 claims per hour). Clear pass/fail. Easy for facilitator.

**Disadvantages:** Requires SME to recall specific details. May need code printouts as memory aids.

**CardDemo example:**
- "The USRSEC record is 80 bytes: SEC-USR-ID PIC X(08), SEC-USR-FNAME PIC X(20), SEC-USR-LNAME PIC X(20), SEC-USR-PWD PIC X(08), SEC-USR-TYPE PIC X(01), SEC-USR-FILLER PIC X(23). Correct?"
- "The ACCTDAT VSAM key is KEYS(11,0) with record size 300. Correct?"

### 3.3 Tacit Knowledge Extraction

**Purpose:** Capture knowledge that exists *outside* the code — operational procedures, business rules enforced by humans, performance workarounds, design rationale, and historical context. This is the highest-value activity for knowledge preservation.

**Procedure:**
1. Ask open-ended questions about specific areas: "What happens when the batch job fails at step X?"
2. Use follow-up probes: "How did you know that? Was it documented anywhere?"
3. Ask about exceptions: "What unusual situations did you have to handle?"
4. Ask about history: "Why was it built this way? What changed over time?"
5. Record everything — tacit knowledge is perishable.

**When to use:** After technical validation, dedicated time in each session (last 45–60 minutes).

**Advantages:** Captures irreplaceable knowledge. Often surfaces the most valuable insights.

**Disadvantages:** Unpredictable — depends on SME memory and engagement. Hard to structure.

**CardDemo example:**
- "The batch posting job (CBTRN02C) uses rejection codes 100–109. Are these the complete set, or are there others enforced outside the application?"
- "What happens operationally when a batch job fails mid-run? Is there a restart procedure?"
- "Why does the COMMAREA carry the full 16-digit card number instead of a token?"

### 3.4 Contradiction Detection

**Purpose:** Present two plausible interpretations of the same code or design pattern. The SME picks the correct one. This is particularly effective for ambiguous areas where AI might have chosen the wrong interpretation.

**Procedure:**
1. Present two interpretations: "Interpretation A says X. Interpretation B says Y. Which is correct, or is neither correct?"
2. If the SME picks one, ask why. If neither, ask what the correct interpretation is.
3. Record the rationale, not just the answer.

**When to use:** Security exploitability (real vs. false positive), FILLER field purpose, architectural decisions, ambiguous COBOL patterns.

**Advantages:** Efficient for binary decisions. Forces clear thinking. Reduces SME effort (recognition vs. recall).

**Disadvantages:** Requires careful question construction to avoid leading the SME.

**CardDemo example:**
- "SEC-005 says any user can invoke CICS transactions directly, bypassing menu authorization. Interpretation A: This is exploitable because CICS allows direct transaction invocation from any terminal. Interpretation B: This is mitigated because RACF transaction-level security prevents unauthorized invocation. Which applies?"
- "The SEC-USR-FILLER PIC X(23) in the user security record: Interpretation A: Reserved space for future fields. Interpretation B: Padding to align the record to an 80-byte block. Which is correct?"

### 3.5 Cross-Reference Validation

**Purpose:** Extract the same concept from 2–3 different documents and check whether the descriptions are consistent. Inconsistencies indicate either an error in one document or a nuance the AI missed.

**Procedure:**
1. Show the SME how the same entity/concept appears in multiple documents.
2. Ask: "Are these descriptions consistent? Do they describe the same thing?"
3. If inconsistent, ask which is correct and why.

**When to use:** Data lineage, domain-to-physical data mapping, transaction flow descriptions.

**Advantages:** Catches subtle inconsistencies AI validation missed. Validates cross-document coherence.

**Disadvantages:** Requires facilitator to pre-identify cross-reference points.

**CardDemo example:**
- ACCTDAT appears in the Data Model (VSAM layout), Context Model (data ownership matrix), and Batch Workflows (job dependencies). Are the descriptions consistent?
- The COMMAREA is described in the Context Model (COMMAREA-SPECIFICATION.md), Security Model (Section 4.1), and Architecture (C4-L4). Do all three match?
- Note: VL-007 found the Security Model claims COMMAREA is "~130 bytes" while VL-003 says "~155 bytes," and the actual value is 160 bytes. This is a concrete cross-reference validation target.

### 3.6 Code Walkthrough with Commentary

**Purpose:** The SME reads actual code while the facilitator compares their narrative against the AI-generated documentation in real time. This catches interpretation errors that other methods miss.

**Procedure:**
1. Print or display a key program section.
2. Ask the SME to narrate what the code does, paragraph by paragraph.
3. The facilitator silently checks the SME's narrative against the documentation.
4. Flag divergences for discussion after the walkthrough.

**When to use:** Complex programs (COACTUPC at 4000+ lines), authentication flow (COSGN00C), batch posting logic (CBTRN02C).

**Advantages:** Highest fidelity validation. Captures reasoning, not just answers.

**Disadvantages:** Slow — 15–30 minutes per code section. Requires code printouts.

**CardDemo example:**
- Walk through COSGN00C.cbl lines 209–257 (authentication flow) and compare against SECURITY-MODEL.md Section 2.1
- Walk through CBTRN02C.cbl lines 380–560 (transaction validation and posting) and compare against BUSINESS-RULES.md rejection codes BR-R001 through BR-R005

---

## 4. Confidence Framework

### 4.1 Five-Point Confidence Scale

| Level | Label | Criteria | Example |
|-------|-------|----------|---------|
| **5** | **Verified** | SME confirms with source code cross-reference; no ambiguity | "Yes, that PIC clause is correct — I can see it in the copybook." |
| **4** | **High Confidence** | SME confirms based on expertise; minor details unverified | "That's how CICS COMMAREA passing works — standard pattern." |
| **3** | **Moderate Confidence** | SME provides qualified agreement | "Probably correct, but I'd want to check the actual RACF definitions." |
| **2** | **Low Confidence** | SME identifies potential issues or missing context | "This looks right for the code, but there were operational procedures around this that aren't captured." |
| **1** | **Unverified** | No SME validation or SME disagrees | "I don't think that's right — the bounded contexts don't map to how we thought about the system." |

### 4.2 Minimum Thresholds by Downstream Use

| Documentation Area | Modernization | RFP | Compliance | Knowledge Preservation |
|-------------------|---------------|-----|------------|----------------------|
| Security Model (VL-007) | 5 | 4 | 5 | 3 |
| Data Model (VL-002) | 5 | 5 | 5 | 4 |
| Business Rules (BR-*) | 4 | 4 | 5 | 3 |
| Architecture (VL-004) | 4 | 4 | 3 | 3 |
| Batch Workflows (VL-006) | 5 | 4 | 4 | 3 |
| Screen Flows (VL-005) | 4 | 3 | N/A | 4 |
| API Candidates | 3 | 3 | N/A | 2 |

**Reading the table:** A modernization project requires Level 5 (Verified) confidence in the Security Model and Data Model before proceeding. An RFP can tolerate Level 3 for API Candidates because vendors will conduct their own discovery.

### 4.3 Compounding Rule

When multiple SMEs validate the same area independently:

**Combined Confidence = 1 − (1 − C₁)(1 − C₂)**

Where C₁ and C₂ are normalized confidence scores (Level / 5).

| SME #1 | SME #2 | Combined | Effective Level |
|--------|--------|----------|----------------|
| 4 (0.80) | 4 (0.80) | 0.96 | ~5 |
| 3 (0.60) | 4 (0.80) | 0.92 | ~5 |
| 3 (0.60) | 3 (0.60) | 0.84 | ~4 |
| 2 (0.40) | 3 (0.60) | 0.76 | ~4 |

**Disagreement rule:** If SMEs disagree on the same claim, use the **LOWER** score and flag the item for reconciliation in a joint session. Disagreement itself is a finding — it indicates ambiguity in the documentation or differing interpretations of the same code.

---

## 5. Validation Matrix

This matrix maps each RE document to the primary SME role, recommended validation method, current AI validation score, and target confidence level for modernization use.

| RE Doc | Document Area | AI Score | AI Verdict | Primary SME Role | Secondary Role | Recommended Method | Target Confidence |
|--------|--------------|----------|------------|-----------------|----------------|-------------------|------------------|
| RE-001 | Domain Model | 88.0 | PASS | Domain Expert | Technical Architect | Blind Validation + Targeted Probes | 4 |
| RE-002 | Data Model | 96.8 | PASS | VSAM Data Architect | Technical Architect | Targeted Probes + Cross-Reference | 5 |
| RE-003 | Context Model | 80.6 | FAIL | Technical Architect | CICS Sys Programmer | Blind Validation + Code Walkthrough | 4 |
| RE-004 | C4 Architecture | 90.0 | FAIL | Technical Architect | Modernization Architect | Blind Validation + Contradiction Detection | 4 |
| RE-005 | Screen Flows | 90.6 | FAIL | CICS Sys Programmer | End User | Targeted Probes + Code Walkthrough | 4 |
| RE-006 | Batch Workflows | 73.8 | FAIL | JCL/Batch Specialist | Technical Architect | Tacit Knowledge + Targeted Probes | 5 |
| RE-007 | Security Model | 90.0 | PASS | CICS Sys Programmer | Security Specialist | Contradiction Detection + Tacit Knowledge | 5 |
| — | Business Rules | 88.0* | PASS* | Domain Expert | Technical Architect | Targeted Probes + Tacit Knowledge | 4 |

*Business rules are part of the RE-001 domain model validation.

**Priority order for limited SME time:**

1. **VL-006 Batch Workflows (73.8)** — Lowest score, 3 hallucinations found, 6 missing JCL files, critical for modernization scope
2. **VL-003 Context Model (80.6)** — Second-lowest score, 5 BMS name errors, TransID table only 17.6% complete
3. **VL-007 Security Model (90.0)** — High score but security findings require human judgment on exploitability
4. **Business Rules (BR-*)** — AI cannot validate meaning or completeness of business rules
5. **VL-004 Architecture (90.0)** — Bounded context decisions drive microservice boundaries
6. **VL-001 Domain Model (88.0)** — Domain language accuracy affects all downstream work
7. **VL-005 Screen Flows (90.6)** — Important for knowledge preservation, lower priority for modernization
8. **VL-002 Data Model (96.8)** — Highest AI score, lowest SME validation urgency

---

## 6. Question Bank

### 6.1 Tier 1: Essential Questions (~30)

These are the minimum viable validation set. If you only get one 4-hour session, ask these.

#### Architecture & CICS (8 questions)

**Q-A01: Bounded Context Validity** (Blind Validation)
- **Question:** "The AI identified 7 bounded contexts: Authentication, Account Management, Card Management, Transaction Processing, Bill Payment, User Administration, and Batch Processing. Without looking at the documentation, how would you divide this application into functional areas?"
- **Validates:** CONTEXT-MAP.md (VL-003, scored 80.6)
- **Method:** Blind Validation
- **Correct answer looks like:** SME's mental model substantially overlaps with the 7 contexts, even if named differently.
- **Concerning answer looks like:** SME describes a fundamentally different decomposition (e.g., "We thought of it as online vs. batch" with no further subdivision).
- **Priority:** Tier 1

**Q-A02: COMMAREA Design Rationale** (Tacit Knowledge)
- **Question:** "The COMMAREA carries 160 bytes of session state including the full 16-digit card number (CDEMO-CARD-NUM PIC 9(16)). Why was the full PAN included in session state rather than a reference key or token?"
- **Validates:** COMMAREA-SPECIFICATION.md, SEC-006 (unencrypted card data)
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** Explanation of design rationale (e.g., "COMMAREA was the only way to pass data between programs in CICS — there was no shared cache").
- **Concerning answer looks like:** "That shouldn't be there" or "We never stored the full PAN" — indicates documentation may be wrong or system was modified.
- **Priority:** Tier 1

**Q-A03: RACF External Security Controls** (Contradiction Detection)
- **Question:** "The security model documents SEC-005: Missing Per-Program Authorization Checks (CVSS 8.8). Programs like COACTVWC don't check user type at entry. Interpretation A: This is exploitable because any user can invoke any CICS transaction directly. Interpretation B: This is mitigated by RACF transaction-level security that restricts which users can invoke which TransIDs. Which applies to this system?"
- **Validates:** SECURITY-MODEL.md Section 3.2, SEC-005
- **Method:** Contradiction Detection
- **Correct answer looks like:** Clear statement about whether RACF was configured for transaction-level security, with specifics about the CICS region security settings.
- **Concerning answer looks like:** "I'm not sure — we might have had RACF but I don't know the configuration" — means SEC-005 cannot be confirmed or denied.
- **Priority:** Tier 1

**Q-A04: SET CDEMO-USRTYP-USER Pattern** (Targeted Probe)
- **Question:** "VL-007 found that 5 programs (COACTVWC, COACTUPC, COCRDLIC, COCRDSLC, COCRDUPC) contain `SET CDEMO-USRTYP-USER TO TRUE`, which resets admin sessions to regular user type. Was this intentional? What's the impact on admin navigation?"
- **Validates:** VL-007 M-003, SECURITY-MODEL.md
- **Method:** Targeted Probe
- **Correct answer looks like:** "That was a bug we knew about" or "It was intentional — admin users accessing card functions should operate as regular users."
- **Concerning answer looks like:** "I never noticed that" — confirms the AI finding is novel and needs further investigation.
- **Priority:** Tier 1

**Q-A05: Session Timeout** (Tacit Knowledge)
- **Question:** "The documentation says there's no CICS session timeout (SEC-004). Was session timeout handled externally — for example, by CICS TIMEOUT settings, VTAM configuration, or terminal inactivity timers?"
- **Validates:** SECURITY-MODEL.md SEC-004
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** "CICS had a TIMEOUT transaction that would sign off inactive terminals after N minutes" or "The network controller (VTAM/TCPIP) had inactivity settings."
- **Concerning answer looks like:** "No, there really was no timeout" — confirms the security finding is accurate.
- **Priority:** Tier 1

**Q-A06: C4 Architecture Accuracy** (Cross-Reference Validation)
- **Question:** "The C4 Level 2 container diagram shows 18 CICS online programs and 10 batch programs. The actual count is 19 online and 12 batch (including CBSTM03A/B). Were CBSTM03A and CBSTM03B part of the original application, or were they added later?"
- **Validates:** C4-L2-CONTAINER.md (VL-004, scored 90.0)
- **Method:** Cross-Reference Validation
- **Correct answer looks like:** Clear statement about when these programs were added and their purpose.
- **Concerning answer looks like:** "I don't recognize those program names" — may indicate they were added after the SME's tenure.
- **Priority:** Tier 1

**Q-A07: Program Naming Convention** (Targeted Probe)
- **Question:** "The documentation states CO* prefix = online CICS programs and CB* prefix = batch programs. Is COBSWAIT (which uses the CO* prefix but is a batch utility with no CICS commands) an exception to this convention? Are there other naming convention exceptions?"
- **Validates:** CLAUDE.md naming conventions, VL-007 M-002
- **Method:** Targeted Probe
- **Correct answer looks like:** "COBSWAIT was a utility — the naming convention wasn't strict for utilities" or a more detailed explanation of the naming scheme.
- **Concerning answer looks like:** "The naming convention was always strict" — contradicts what the code shows.
- **Priority:** Tier 1

**Q-A08: Extension Modules** (Tacit Knowledge)
- **Question:** "Three extension directories exist: app-authorization-ims-db2-mq, app-transaction-type-db2, and app-vsam-mq. Were these part of the original application or added as optional extensions? Are they used in production?"
- **Validates:** CONTEXT-MAP.md (extension programs COTRTLIC, COTRTUPC, COPAUS0C)
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** Clear statement about extension module status and deployment history.
- **Concerning answer looks like:** "Those are demo-only modules" — changes the documentation scope.
- **Priority:** Tier 1

#### Data & VSAM (5 questions)

**Q-D01: FILLER Field Purpose** (Contradiction Detection)
- **Question:** "The user security record (CSUSR01Y.cpy) has SEC-USR-FILLER PIC X(23) — 23 bytes of unused space at the end of an 80-byte record. Interpretation A: This is reserved space for future fields (password history, last login date, etc.). Interpretation B: This is padding to align the record to a standard 80-byte VSAM record size. Which is correct?"
- **Validates:** DATA-MODEL.md, VL-002
- **Method:** Contradiction Detection
- **Correct answer looks like:** Definitive explanation of the FILLER purpose.
- **Concerning answer looks like:** "Could be either" — means the design rationale is truly lost.
- **Priority:** Tier 1

**Q-D02: Alternate Index Usage** (Targeted Probe)
- **Question:** "CARDDAT has an alternate index on bytes 11–16 (CARD-ACCT-ID), and TRANSACT has an alternate index on bytes 26–304. Are these alternate indexes actively used in online programs, or only in batch? Are there performance implications?"
- **Validates:** DATA-MODEL.md VSAM specifications, VL-002
- **Method:** Targeted Probe
- **Correct answer looks like:** Specific usage scenarios and performance context.
- **Concerning answer looks like:** "We never used the TRANSACT AIX online" — changes the data access patterns documented.
- **Priority:** Tier 1

**Q-D03: DEFCUST Alternate Schema** (Targeted Probe)
- **Question:** "VL-006 found DEFCUST.jcl uses DSN `AWS.CCDA.CUSTDATA` with KEYS(10,0), while CUSTFILE.jcl uses `AWS.M2.CARDDEMO.CUSTDATA` with KEYS(9,0). Is this a different environment, a migration artifact, or an error?"
- **Validates:** VL-006 MAJ-004
- **Method:** Targeted Probe
- **Correct answer looks like:** Clear explanation of the two schemas and their relationship.
- **Concerning answer looks like:** "That's definitely wrong — the key should be 9" — indicates DEFCUST.jcl may be erroneous.
- **Priority:** Tier 1

**Q-D04: Data Retention and Archival** (Tacit Knowledge)
- **Question:** "The transaction file uses GDG (Generation Data Groups) with limits of 5 and 10. What was the data retention policy? How long were transactions kept before archival? Was there an archival process beyond what's in the JCL?"
- **Validates:** BATCH-WORKFLOWS.md, data lifecycle
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** Specific retention periods and archival procedures.
- **Concerning answer looks like:** "There was no formal retention policy" — important gap for compliance.
- **Priority:** Tier 1

**Q-D05: Referential Integrity** (Tacit Knowledge)
- **Question:** "VSAM doesn't enforce referential integrity between files. How was data consistency maintained between ACCTDAT, CARDDAT, CUSTDAT, and CCXREF? Were there reconciliation jobs or manual checks?"
- **Validates:** DATA-MODEL.md relationships, DATA-LINEAGE.md
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** Description of reconciliation procedures, data validation jobs, or operational checks.
- **Concerning answer looks like:** "We relied on the application to keep them in sync" — means data integrity is application-enforced only.
- **Priority:** Tier 1

#### Business Rules (5 questions)

**Q-B01: Interest Calculation Completeness** (Tacit Knowledge)
- **Question:** "The documentation identifies BR-C001 (interest rate lookup by group, type, and category) and CBACT04C as the interest calculation program. Is the interest calculation logic in the code complete, or are there rate tables, external feeds, or manual adjustments that supplement it?"
- **Validates:** BUSINESS-RULES.md BR-C001, BATCH-WORKFLOWS.md
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** Description of the full interest calculation process including any external inputs.
- **Concerning answer looks like:** "The rate tables were loaded from an external system daily" — means a critical dependency is undocumented.
- **Priority:** Tier 1

**Q-B02: Top 5 Business Rule Spot-Check** (Targeted Probe)
- **Question:** Present the following 5 rules and ask "Are these correctly described?"
  - BR-V019: SSN must be 9-digit numeric, not starting with 000, 666, or 900-999 (source: `COACTUPC.cbl:117-146`)
  - BR-L001: Transaction cannot exceed credit limit (source: `CBTRN02C.cbl:407-413`)
  - BR-C006: Bill payment subtracts from current balance (source: `COBIL00C.cbl:234`)
  - BR-R003: Overlimit transactions rejected with code 102 (source: `CBTRN02C.cbl:410-412`)
  - BR-T001: Transaction date must be before account expiration (source: `CBTRN02C.cbl:414-420`)
- **Validates:** BUSINESS-RULES.md (5 specific rules)
- **Method:** Targeted Probe
- **Correct answer looks like:** Confirmation of each rule, possibly with additional context.
- **Concerning answer looks like:** "BR-V019 is incomplete — there were additional SSN validation rules" or "BR-L001 had an overlimit tolerance percentage."
- **Priority:** Tier 1

**Q-B03: Rules Enforced Outside Code** (Tacit Knowledge)
- **Question:** "The AI extracted 58 business rules from the COBOL source code. What business rules were enforced *outside* the application — by operations staff, by policy, by downstream systems, or by manual review?"
- **Validates:** BUSINESS-RULES.md completeness
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** List of external rules (e.g., "Fraud review for transactions over $X," "Daily cutoff time for batch processing," "Manual approval for credit limit increases").
- **Concerning answer looks like:** "I can't think of any" — either the code was self-contained or the SME doesn't recall.
- **Priority:** Tier 1

**Q-B04: Rejection Code Completeness** (Targeted Probe)
- **Question:** "The batch posting program (CBTRN02C) uses rejection codes 100 (invalid card), 101 (account not found), 102 (overlimit), 103 (expired account), and 109 (update failed). Is this the complete set? Are there other rejection codes used elsewhere in the system?"
- **Validates:** BUSINESS-RULES.md BR-R001 through BR-R005
- **Method:** Targeted Probe
- **Correct answer looks like:** Confirmation that 100–109 is complete, or identification of additional codes.
- **Concerning answer looks like:** "There were additional codes in the range 104–108 for specific conditions" — means the AI missed rules.
- **Priority:** Tier 1

**Q-B05: Bill Payment Flow** (Code Walkthrough)
- **Question:** "Walk me through what happens when a user makes a bill payment. The documentation says: verify balance > 0, confirm Y/N, compute new balance, generate transaction ID, write to TRANSACT, update ACCTDAT. Is this complete? Are there additional steps?"
- **Validates:** BUSINESS-RULES.md BR-C006, BR-C008, BR-V018, BR-L002
- **Method:** Code Walkthrough
- **Correct answer looks like:** Confirmation of the flow with any additional steps (e.g., "There's also a memo posting step" or "The minimum payment amount check is missing").
- **Concerning answer looks like:** Major discrepancy in the flow.
- **Priority:** Tier 1

#### Security (5 questions)

**Q-S01: SEC-005 Exploitability in CICS Context** (Contradiction Detection)
- **Question:** "SEC-005 states that programs don't check user authorization at entry — only the menu checks. In a production CICS environment, can a user invoke a transaction directly (e.g., type 'CAUP' to reach COACTUPC) without going through the menu? Or does CICS/RACF prevent this?"
- **Validates:** SECURITY-MODEL.md SEC-005, Access Control Matrix
- **Method:** Contradiction Detection
- **Correct answer looks like:** Definitive statement about whether RACF transaction security was configured.
- **Concerning answer looks like:** Ambiguity — means SEC-005 severity is unknown.
- **Priority:** Tier 1

**Q-S02: COMMAREA Tamperability** (Contradiction Detection)
- **Question:** "SEC-008 says the unsigned COMMAREA could be tampered with to escalate privileges (changing USER-TYPE from 'U' to 'A'). In practice, can a 3270 terminal user modify COMMAREA contents between transactions, or is this a theoretical risk only?"
- **Validates:** SECURITY-MODEL.md SEC-008
- **Method:** Contradiction Detection
- **Correct answer looks like:** "In standard CICS, COMMAREA is managed by the system — users can't modify it directly" (reduces severity) or "With CEDF or terminal emulators, it's possible" (confirms severity).
- **Concerning answer looks like:** "I'm not sure" — means we can't adjust the CVSS score.
- **Priority:** Tier 1

**Q-S03: PCI-DSS Score Accuracy** (Targeted Probe)
- **Question:** "The security model assigns a PCI-DSS compliance score of 15%. Given that this is a mainframe application with RACF, network isolation, and physical security controls that the application code doesn't show, is 15% accurate? Or should the score be higher when considering infrastructure-level controls?"
- **Validates:** SECURITY-MODEL.md Section 6
- **Method:** Targeted Probe
- **Correct answer looks like:** Adjusted score with justification (e.g., "Infrastructure gets you to maybe 35–40%, but application-level gaps are still critical").
- **Concerning answer looks like:** "15% is about right even with RACF" — confirms the assessment.
- **Priority:** Tier 1

**Q-S04: Default Credentials** (Tacit Knowledge)
- **Question:** "DUSRSECJ.jcl shows 10 default users all with password 'PASSWORD'. In production, were these defaults changed? Was there a credential management process?"
- **Validates:** SECURITY-MODEL.md Section 2.4
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** "Of course — those are demo defaults. Production had unique passwords managed through [process]."
- **Concerning answer looks like:** "Those were the actual production passwords" — critical security finding.
- **Priority:** Tier 1

**Q-S05: Audit Logging** (Tacit Knowledge)
- **Question:** "SEC-007 states there's no audit logging in the application. Was audit logging handled externally — SMF records, CICS journal, DB2 audit, or RACF event logging?"
- **Validates:** SECURITY-MODEL.md SEC-007
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** "CICS journals captured transaction activity" or "SMF type 110 records were used."
- **Concerning answer looks like:** "No, we didn't have audit logging" — confirms the finding.
- **Priority:** Tier 1

#### Batch & Operations (4 questions)

**Q-O01: Six Missing JCL Files** (Targeted Probe)
- **Question:** "VL-006 found 6 JCL files not documented: CREASTMT.JCL, FTPJCL.JCL, INTRDRJ1.JCL, INTRDRJ2.JCL, TXT2PDF1.JCL, and DEFCUST.jcl. Are these production jobs, development utilities, or something else? What does the CREASTMT statement generation workflow do?"
- **Validates:** VL-006 CRT-001 (scored 73.8, weakest area)
- **Method:** Targeted Probe
- **Correct answer looks like:** Explanation of each missing job's role and whether it runs in production.
- **Concerning answer looks like:** "I don't recognize those" — they may be post-tenure additions.
- **Priority:** Tier 1

**Q-O02: Phantom MNTTRDB2** (Targeted Probe)
- **Question:** "The batch workflow documentation includes a job called MNTTRDB2 in the WEEKLY Control-M workflow, but no JCL file exists for it. Is MNTTRDB2 a real job from an external system, a DB2 utility, or an error in the documentation?"
- **Validates:** VL-006 MAJ-001 (hallucination finding)
- **Method:** Targeted Probe
- **Correct answer looks like:** "MNTTRDB2 was a DB2 maintenance job defined in [other system]" (explains the phantom) or "That doesn't exist" (confirms hallucination).
- **Concerning answer looks like:** Ambiguity.
- **Priority:** Tier 1

**Q-O03: Batch Restart Procedures** (Tacit Knowledge)
- **Question:** "If the daily batch posting job (POSTTRAN) fails mid-run, what's the restart procedure? Does it restart from the beginning, from a checkpoint, or require manual intervention? What about the interest calculation (INTCALC)?"
- **Validates:** BATCH-WORKFLOWS.md recovery procedures
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** Specific restart procedures including checkpoint/restart, data recovery steps, and escalation paths.
- **Concerning answer looks like:** "We'd just rerun from the beginning" — indicates no checkpoint/restart, which is a modernization risk.
- **Priority:** Tier 1

**Q-O04: Batch Scheduling and SLAs** (Tacit Knowledge)
- **Question:** "What were the batch scheduling windows? When did the daily cycle start and end? Were there SLAs for job completion? What happened if the batch window was exceeded?"
- **Validates:** BATCH-WORKFLOWS.md (no SLA documentation exists)
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** Specific times, SLAs, and escalation procedures.
- **Concerning answer looks like:** "I don't remember specific SLAs" — knowledge is at risk of being lost.
- **Priority:** Tier 1

#### Methodology (3 questions)

**Q-M01: Overall Quality Assessment** (Open-Ended)
- **Question:** "Having reviewed the AI-generated documentation, on a scale of 1–5, how would you rate its overall quality compared to what a human reverse engineering team would produce?"
- **Validates:** Methodology effectiveness
- **Method:** Open-ended assessment
- **Correct answer looks like:** Numeric rating with specific justification.
- **Priority:** Tier 1

**Q-M02: What Did AI Miss?** (Tacit Knowledge)
- **Question:** "What is the single most important thing about this application that the AI documentation does NOT capture?"
- **Validates:** Methodology completeness
- **Method:** Tacit Knowledge Extraction
- **Correct answer looks like:** Identification of a critical gap (e.g., "The relationship with the authorization system," "The daily reconciliation process," "The reason we chose this VSAM key structure").
- **Priority:** Tier 1

**Q-M03: Trust Level** (Targeted Probe)
- **Question:** "What percentage of this AI-generated documentation would you trust for production use without manual verification?"
- **Validates:** Methodology reliability for real systems
- **Method:** Targeted Probe
- **Correct answer looks like:** A percentage with justification (e.g., "80% for the technical docs, 50% for the business rules").
- **Concerning answer looks like:** Below 50% — indicates the methodology needs significant improvement.
- **Priority:** Tier 1

### 6.2 Tier 2: Extended Questions (~50)

These provide deeper validation for additional sessions or follow-up.

#### Architecture & CICS — Extended (8 questions)

**Q-A09:** "Is the CICS HANDLE ABEND pattern used throughout the application standard practice, or specific to this application's error handling approach?"
- Validates: C4-L4-CODE-PATTERNS.md
- Method: Targeted Probe

**Q-A10:** "The context model shows Bill Payment (COBIL00C) writing across bounded context boundaries — it writes to both ACCTDAT and TRANSACT. Is this cross-context write pattern typical? Should it have been decomposed differently?"
- Validates: CONTEXT-MAP.md cross-context data access
- Method: Contradiction Detection

**Q-A11:** "The navigation model shows all PF3 keys return to the menu via XCTL with CDEMO-TO-PROGRAM. Were there any direct program-to-program transfers that bypass the menu?"
- Validates: NAVIGATION-FLOWS.md, VL-003 MAJ-004
- Method: Targeted Probe

**Q-A12:** "COMEN01C has special handling for COPAUS0C (Pending Authorization View) — it checks with CICS INQUIRE before transferring. Was this feature ever implemented, or was it always a placeholder?"
- Validates: VL-001 MAJ-003, CONTEXT-MAP.md
- Method: Tacit Knowledge

**Q-A13:** "The C4 Level 3 component diagram groups programs by functional area. Does this grouping reflect how the development team organized their work, or is it a post-hoc categorization?"
- Validates: C4-L3-COMPONENT.md
- Method: Tacit Knowledge

**Q-A14:** "Were there any external systems that interacted with CardDemo — authorization networks, statement printing systems, fraud detection, or reporting tools?"
- Validates: C4-L1-SYSTEM-CONTEXT.md external actors
- Method: Tacit Knowledge

**Q-A15:** "The COMMAREA stores navigation state (FROM-TRANID, FROM-PROGRAM, TO-TRANID, TO-PROGRAM). Were there cases where this navigation tracking was used for something beyond screen flow — auditing, debugging, or operational monitoring?"
- Validates: COMMAREA-SPECIFICATION.md
- Method: Tacit Knowledge

**Q-A16:** "VL-003 found 5 BMS map name errors (COACTVWA should be CACTVWA, COACTU1A should be CACTUPA, etc.). The pattern is consistently using CO-prefixed names instead of C-prefixed short forms. Was there a naming convention change during the application's history?"
- Validates: VL-003 MAJ-001
- Method: Tacit Knowledge

#### Data & VSAM — Extended (8 questions)

**Q-D06:** "The ACCTDAT record is 300 bytes but the documented fields sum to significantly less. What fills the remaining space — FILLER, unused fields, or fields we haven't identified?"
- Validates: DATA-DICTIONARY.md, CVACT01Y.cpy
- Method: Targeted Probe

**Q-D07:** "The TRANSACT file has a composite key of 16 bytes at offset 0 (TRAN-ID). What determined the key structure — business requirements, performance, or VSAM constraints?"
- Validates: DATA-MODEL.md VSAM specifications
- Method: Tacit Knowledge

**Q-D08:** "CARDXREF maps card numbers to account IDs. Was this cross-reference always up to date, or were there reconciliation issues?"
- Validates: DATA-LINEAGE.md
- Method: Tacit Knowledge

**Q-D09:** "The EXPIRAION field name (in CVACT01Y.cpy) appears to be misspelled (missing 'T'). Was this a known issue? Were there other field naming inconsistencies?"
- Validates: VL-002 INFO-001 (preserved typo)
- Method: Targeted Probe

**Q-D10:** "The data model shows DISCLOSURE_GROUP connected to TRAN_CAT_BAL. VL-002 noted this is an indirect relationship through CBACT04C processing logic. Is this how the business relationship actually works?"
- Validates: VL-002 MINOR-003
- Method: Contradiction Detection

**Q-D11:** "Were there ever data migration or conversion issues between the EBCDIC data files and the ASCII reference data? The repository contains both formats."
- Validates: DATA-MODEL.md, CBEXPORT/CBIMPORT programs
- Method: Tacit Knowledge

**Q-D12:** "What was the typical data volume? How many accounts, cards, and daily transactions was the system designed to handle?"
- Validates: Modernization sizing
- Method: Tacit Knowledge

**Q-D13:** "The CUSTDAT record has CUST-FICO-CREDIT-SCORE PIC 9(03). Was FICO score actively used in processing decisions, or was it informational only?"
- Validates: BUSINESS-RULES.md BR-L003
- Method: Targeted Probe

#### Business Rules — Extended (8 questions)

**Q-B06:** "BR-V011 says card name can only contain alphabets and spaces. Were there customers with hyphens, apostrophes, or accented characters in their names? How were those handled?"
- Validates: BUSINESS-RULES.md BR-V011
- Method: Tacit Knowledge

**Q-B07:** "The bill payment decision table shows 5 scenarios. Were there additional scenarios — partial payments, scheduled payments, minimum payments, or payment reversals?"
- Validates: BUSINESS-RULES.md bill payment decision table
- Method: Tacit Knowledge

**Q-B08:** "BR-C004 and BR-C005 track cycle credits and debits separately. What was the business purpose of tracking these separately rather than just the net balance?"
- Validates: BUSINESS-RULES.md BR-C004, BR-C005
- Method: Tacit Knowledge

**Q-B09:** "The SSN validation (BR-V019) checks for 000, 666, and 900-999 in the first three digits. Were there additional SSN validation rules enforced by policy or downstream systems?"
- Validates: BUSINESS-RULES.md BR-V019
- Method: Targeted Probe

**Q-B10:** "The phone validation (BR-V020) checks NANPA area codes. Were there international customer phone numbers? How were those handled?"
- Validates: BUSINESS-RULES.md BR-V020
- Method: Tacit Knowledge

**Q-B11:** "The state/ZIP validation (BR-V022) uses a lookup table in CSLKPCDY.cpy. How often was this table updated? Were there known gaps?"
- Validates: BUSINESS-RULES.md BR-V022
- Method: Tacit Knowledge

**Q-B12:** "Were there any business rules that differed between account types, card types, or customer segments? The current documentation treats all accounts uniformly."
- Validates: BUSINESS-RULES.md completeness
- Method: Tacit Knowledge

**Q-B13:** "Transaction type and transaction category are separate entities with their own VSAM files. What's the business distinction between type and category?"
- Validates: DOMAIN-MODEL.md entity relationships
- Method: Tacit Knowledge

#### Batch & Operations — Extended (8 questions)

**Q-O05:** "The run_full_batch.sh script runs: Data Refresh → Close CICS Files → Account Processing → Transaction Processing → Reporting → Reopen CICS Files. Was this the actual production sequence? Were there additional steps?"
- Validates: BATCH-WORKFLOWS.md Section 3.3
- Method: Targeted Probe

**Q-O06:** "The CLOSEFIL/OPENFIL jobs use SDSF CICS commands to close and reopen files for batch processing. How long was the online system unavailable during batch? Was there a batch window notification process?"
- Validates: BATCH-WORKFLOWS.md Section 2.5
- Method: Tacit Knowledge

**Q-O07:** "VL-006 found the MONTHLY Interest Calculation Control-M workflow is missing TRANBKP and TRANIDX steps. Were these always part of the interest calculation, or were they added later?"
- Validates: VL-006 MAJ-002
- Method: Targeted Probe

**Q-O08:** "The CBEXPORT/CBIMPORT programs handle EBCDIC-to-ASCII conversion. Were these used for regular data feeds, or only for ad-hoc data extraction?"
- Validates: BATCH-WORKFLOWS.md Section 2.7
- Method: Tacit Knowledge

**Q-O09:** "Were there monitoring or alerting tools for batch job failures? What was the escalation path when a job failed?"
- Validates: Operational procedures (not documented)
- Method: Tacit Knowledge

**Q-O10:** "The REPTFILE GDG has limit 10 (per REPTFILE.jcl) but DEFGDGB defines the same GDG with limit 5. Which is correct? Does this cause issues?"
- Validates: VL-006 MIN-004
- Method: Targeted Probe

**Q-O11:** "Were there seasonal or period-end processing differences — month-end close, quarter-end reporting, year-end adjustments?"
- Validates: BATCH-WORKFLOWS.md completeness
- Method: Tacit Knowledge

**Q-O12:** "The CREASTMT.JCL job (missing from documentation) appears to generate statements per card. Was this a monthly statement cycle? What was the output format and distribution method?"
- Validates: VL-006 CRT-001 (CREASTMT specifically)
- Method: Tacit Knowledge

#### Security — Extended (8 questions)

**Q-S06:** "SEC-001 rates plain-text password storage at CVSS 9.8. In the mainframe context with RACF dataset protection and network isolation, is 9.8 an appropriate severity? Or should mainframe-specific mitigations reduce the score?"
- Validates: SECURITY-MODEL.md SEC-001 CVSS
- Method: Contradiction Detection

**Q-S07:** "SEC-002 (no account lockout) — was there an external lockout mechanism through RACF or a security product that would lock user IDs after failed attempts?"
- Validates: SECURITY-MODEL.md SEC-002
- Method: Tacit Knowledge

**Q-S08:** "SEC-006 identifies CVV storage in CVACT02Y.cpy (CARD-CVV-CD PIC 9(03)). Was CVV actually populated in production, or was this a data structure placeholder that was never used?"
- Validates: SECURITY-MODEL.md SEC-006
- Method: Tacit Knowledge

**Q-S09:** "SEC-010 (batch programs without security context) — in production, was batch job submission restricted by RACF to specific user IDs or groups?"
- Validates: SECURITY-MODEL.md SEC-010
- Method: Tacit Knowledge

**Q-S10:** "Were there any security incidents or audit findings during the application's history that revealed vulnerabilities not captured in the documentation?"
- Validates: SECURITY-MODEL.md completeness
- Method: Tacit Knowledge

**Q-S11:** "The User Delete function (COUSR03C) — does it perform a hard delete or soft delete of the USRSEC record? Is there a way to recover a deleted user?"
- Validates: SECURITY-MODEL.md user management
- Method: Targeted Probe

**Q-S12:** "Were there any encryption products or appliances in the infrastructure that provided data protection at the network or storage level, even though the application code doesn't encrypt?"
- Validates: SECURITY-MODEL.md SEC-006
- Method: Tacit Knowledge

**Q-S13:** "The admin menu includes 'Transaction Type List/Update' and 'Transaction Type Maintenance' using DB2 extension programs. Were these DB2 programs actually deployed? Did they introduce additional security considerations?"
- Validates: SECURITY-MODEL.md Section 3.4 admin menu
- Method: Tacit Knowledge

#### Screen Flows — Extended (5 questions)

**Q-F01:** "The screen flow documentation identifies 17 BMS mapsets. Were all 17 screens actively used in production, or were some development/test screens?"
- Validates: SCREEN-FLOWS.md, VL-005
- Method: Targeted Probe

**Q-F02:** "VL-005 found PF key discrepancies for COUSR01 and COUSR03. Were PF key assignments standardized across all screens, or did different developers use different conventions?"
- Validates: VL-005 Factual Accuracy
- Method: Tacit Knowledge

**Q-F03:** "Were there any undocumented keyboard shortcuts, clear-screen behaviors, or terminal-specific features that users relied on?"
- Validates: SCREEN-FLOWS.md completeness
- Method: Tacit Knowledge

**Q-F04:** "The documentation shows 6 user journeys. What was the most common user journey in production? What percentage of activity was inquiry vs. update?"
- Validates: SCREEN-FLOWS.md user journeys
- Method: Tacit Knowledge

**Q-F05:** "Were there any screen customizations or site-specific modifications to the BMS maps?"
- Validates: SCREEN-FLOWS.md
- Method: Tacit Knowledge

#### Modernization — Extended (5 questions)

**Q-X01:** "The bounded contexts (Authentication, Account Mgmt, Card Mgmt, etc.) are proposed as microservice boundaries. Do you see any contexts that should be merged or split for a modern architecture?"
- Validates: Modernization readiness
- Method: Contradiction Detection

**Q-X02:** "What would be the biggest technical challenge in migrating this application from COBOL/CICS to a modern stack?"
- Validates: Modernization risk assessment
- Method: Tacit Knowledge

**Q-X03:** "Are there any undocumented external dependencies — mainframe subsystems, network services, or manual processes — that would need to be replicated in a modernized version?"
- Validates: C4-L1-SYSTEM-CONTEXT.md completeness
- Method: Tacit Knowledge

**Q-X04:** "The API candidates document proposes REST endpoints for each bounded context. Are there operations that wouldn't map well to a request/response API pattern?"
- Validates: API candidates
- Method: Contradiction Detection

**Q-X05:** "If you were modernizing this application, what would you do differently from what the AI documentation recommends?"
- Validates: Modernization strategy
- Method: Open-ended

---

## 7. Methodology Validation

Since CardDemo proves the AI reverse engineering approach for the client's real mainframe systems, dedicated time must be allocated to validate the **methodology itself**, not just the documentation.

### 7.1 Methodology Assessment Questions

| # | Question | Expected Response Type |
|---|----------|----------------------|
| M-01 | "On 1–5, how does the AI documentation compare to what a human reverse engineering team would produce?" | Numeric rating + justification |
| M-02 | "What would you do differently? What did the AI approach miss?" | List of gaps and improvements |
| M-03 | "What percentage would you trust for production use without manual verification?" | Percentage + areas of concern |
| M-04 | "Would you use this methodology for production mainframe systems? What additions would you require?" | Go/no-go + conditions |
| M-05 | "What's the biggest risk of relying on AI-generated documentation for a real modernization project?" | Risk identification |
| M-06 | "Were there any findings that surprised you — things the AI caught that you wouldn't have expected?" | Positive validation |
| M-07 | "How much SME time would be needed to validate AI-generated docs for a system 10x this size?" | Scaling estimate |
| M-08 | "What additional documentation would you want that the AI approach didn't produce?" | Gap identification |

### 7.2 Assessment Criteria

The methodology is considered **validated for production use** if:

1. SME overall quality rating is ≥ 3.5 / 5.0
2. SME trust percentage is ≥ 70% for technical documentation
3. SME confirms "would use with additions" (not "would not use")
4. No critical gaps are identified that the methodology cannot address
5. Tacit knowledge captured fills identified gaps to a useful degree

The methodology is considered **needs improvement** if:

1. SME quality rating is 2.5–3.4 / 5.0
2. Trust percentage is 50–69%
3. Specific, addressable improvements are identified

The methodology is considered **not ready** if:

1. SME quality rating is < 2.5 / 5.0
2. Trust percentage is < 50%
3. Fundamental approach gaps are identified

---

## 8. Risk Assessment

### 8.1 Documentation Areas That Cannot Be Validated Without Specific SME Roles

| Area | Missing SME Role | Impact on Modernization | Impact on RFP | Impact on Compliance | Mitigation |
|------|-----------------|------------------------|---------------|---------------------|------------|
| Business rules (meaning) | Domain Expert | High — rules may be misinterpreted or incomplete | Medium — vendors add discovery | Critical — regulators need rule accuracy | Add business rules discovery phase to modernization plan |
| Security exploitability | Security Specialist | Medium — assume worst case | Medium — require vendor security assessment | Critical — compliance requires verified findings | Commission independent penetration test |
| Batch scheduling SLAs | Operations Specialist | Medium — batch windows unknown | Low — can be established during transition | Medium — SLA compliance unclear | Instrument batch jobs during parallel run period |
| Cloud architecture fit | Cloud Architect | Medium — bounded context validity unconfirmed | Low — vendors propose their own architecture | N/A | Engage cloud architect during modernization design phase |
| External system interfaces | Multiple roles | High — undocumented dependencies | High — scope uncertainty | Medium | Conduct interface discovery as modernization Phase 0 |

### 8.2 Go/No-Go Decision Matrix

This matrix guides decisions when specific documentation areas CANNOT be validated due to SME unavailability.

| Use Case | Can't Validate Domain | Can't Validate Security | Can't Validate Data | Can't Validate Batch |
|----------|----------------------|------------------------|--------------------|--------------------|
| **Modernization** | GO — with parallel run and business rule discovery phase | GO — assume worst case for all SEC findings | NO-GO — data model errors cause data corruption | GO — add batch discovery phase |
| **RFP** | GO — add 15% contingency for business rule discovery | GO — require vendor to conduct independent security assessment | GO — add data validation phase as RFP requirement | GO — separate batch scope as optional work package |
| **Compliance** | CONDITIONAL — acceptable only with documented limitations and planned remediation | NO-GO — security findings must be verified before compliance assertion | NO-GO — data handling accuracy is non-negotiable for PCI-DSS | CONDITIONAL — batch controls can be verified during audit |
| **Knowledge Preservation** | GO — label all business rule interpretations as "AI-inferred, unverified by SME" | GO — label security findings as "code-based analysis, external controls unverified" | GO — label data model as "structurally verified, semantics unverified" | GO — label batch workflows as "code-derived, operational context unverified" |

### 8.3 Residual Risk by Scenario

**Scenario A (1 SME):**
- Architecture, Data, Context: **Low residual risk** — SME can validate
- Security: **Medium residual risk** — SME may lack RACF/compliance depth
- Business Rules: **High residual risk** — technical SME may not know business intent
- Batch Operations: **Medium residual risk** — knows JCL but may not know scheduling

**Scenario B (2 SMEs):**
- Architecture, Data, Context: **Low residual risk** — SME #1 covers
- Security: **Medium residual risk** — better than Scenario A but still no dedicated security SME
- Business Rules: **Low–Medium residual risk** — SME #2 covers domain knowledge
- Batch Operations: **Low residual risk** — SME #2 covers

---

## 9. Full Interview Playbook

This section is designed so that a **project manager or analyst with NO mainframe knowledge** can facilitate the SME interviews effectively.

### 9.1 Pre-Interview Prep Packet

**Distribute to each SME at least 5 business days before their first session.**

#### Cover Letter Template

> Dear [SME Name],
>
> Thank you for agreeing to participate in our mainframe documentation validation project. We've used an AI-driven approach to reverse-engineer the documentation for a COBOL/CICS credit card processing application called CardDemo. The AI has produced detailed technical documentation, and we need your expertise to validate that the documentation accurately captures what the system does and — just as importantly — to share knowledge that isn't captured in the code.
>
> **What we're asking:**
> - Review the attached documentation excerpts before our sessions
> - Participate in [3/4/5] working sessions of 3–4 hours each
> - Share your professional judgment on the documentation accuracy
> - Help us capture operational knowledge that exists outside the code
>
> **What to expect:**
> - Some questions will ask you to describe expected behavior *before* seeing our documentation (this tests our docs, not you)
> - Some questions will be specific technical probes (correct/incorrect)
> - Some questions will be open-ended about your experience
> - There are no wrong answers — "I don't know" and "I'm not sure" are valuable responses
>
> **Attached for your review:**
> - CardDemo Application Summary (1 page)
> - Selected documentation excerpts relevant to your expertise
>
> **Please do NOT review until our session:**
> - [List of specific sections withheld for blind validation]
>
> We greatly value your expertise and time. Your input is critical for preserving mainframe knowledge that would otherwise be lost.
>
> Best regards,
> [Facilitator Name]

#### Pre-Read Materials (Include)

| Document | Sections to Send | Purpose |
|----------|-----------------|---------|
| This playbook's CardDemo summary (Section 9.1.1) | All | Application context |
| DATA-MODEL.md | Entity list, VSAM specifications | Refresh memory on record layouts |
| BUSINESS-RULES.md | Full document | Review for spot-check questions |
| SECURITY-MODEL.md | Sections 2–4 only | Authentication and access control |
| BATCH-WORKFLOWS.md | Section 1 (Executive Summary) only | High-level batch overview |

#### Materials to WITHHOLD (for Blind Validation)

| Document | Reason |
|----------|--------|
| CONTEXT-MAP.md | Q-A01 requires blind bounded context identification |
| C4-L2-CONTAINER.md | Q-A06 tests against SME's independent architecture model |
| NAVIGATION-FLOWS.md | SME should describe navigation from memory first |

#### 9.1.1 CardDemo Application Summary (1 Page — Include in Prep Packet)

> **CardDemo** is an IBM mainframe demonstration application for credit card account management. It uses CICS for online transaction processing and VSAM KSDS files for data storage.
>
> **Online functions:** User sign-on, account inquiry, account update, card list/view/update, transaction list/view/add, reports, bill payment, and user management (admin only).
>
> **Batch functions:** Transaction posting (daily), interest calculation (monthly), data refresh, reporting, export/import, and VSAM file maintenance.
>
> **Data files:** ACCTDAT (accounts), CARDDAT (cards), CUSTDAT (customers), TRANSACT (transactions), CCXREF (card-to-account cross-reference), USRSEC (user security), plus reference data files for transaction types, categories, and disclosure groups.
>
> **Technology:** COBOL (31 programs), BMS (17 mapsets), JCL (38 jobs), VSAM KSDS with alternate indexes, optional DB2/IMS/MQ extensions.

### 9.2 Session Agendas

#### Scenario A: 1 SME, 3 Sessions

**Session 1: Architecture + Data Model (4 hours)**

| Time | Activity | Questions | Method | Notes for Facilitator |
|------|----------|-----------|--------|----------------------|
| 0:00–0:15 | Welcome and context setting | — | — | Explain the project, set expectations, confirm recording consent |
| 0:15–1:00 | Blind validation: architecture | Q-A01, Q-A08 | Blind | Do NOT show any documentation yet. Ask SME to describe expected architecture from the 1-page summary only. Record verbatim. |
| 1:00–1:45 | C4 architecture review | Q-A06, Q-A07 | Cross-Reference, Targeted Probe | Now show C4 diagrams and CONTEXT-MAP.md. Compare against blind responses. |
| 1:45–2:15 | **Break** | — | — | Use break to note divergences between blind and documented architecture |
| 2:15–3:15 | Data model targeted probes | Q-D01, Q-D02, Q-D03, Q-D04, Q-D05 | Targeted Probe, Contradiction | Work through VSAM layouts, FILLER, indexes. Have copybook printouts available. |
| 3:15–3:45 | Tacit knowledge: design rationale | Q-A02, Q-A15 | Tacit Knowledge | "Why was it built this way?" questions. Record everything. |
| 3:45–4:00 | Wrap-up | — | — | Summarize key findings, note items for Session 2, confirm next session date |

**Session 2: Security + Batch + Business Rules (4 hours)**

| Time | Activity | Questions | Method | Notes for Facilitator |
|------|----------|-----------|--------|----------------------|
| 0:00–0:15 | Recap Session 1, address follow-ups | — | — | Review any items flagged for follow-up |
| 0:15–1:15 | Security contradiction detection | Q-S01, Q-S02, Q-S03, Q-A03, Q-A04 | Contradiction, Targeted Probe | Present SEC findings one at a time. For each: "Is this exploitable in a real mainframe environment?" |
| 1:15–2:00 | Batch workflow review | Q-O01, Q-O02, Q-O03 | Targeted Probe | Focus on 6 missing JCL files and phantom MNTTRDB2 |
| 2:00–2:30 | **Break** | — | — | |
| 2:30–3:15 | Business rule spot-checks | Q-B01, Q-B02, Q-B03, Q-B04 | Targeted Probe, Tacit Knowledge | Present rules one at a time. Ask "Correct?" then "Complete?" |
| 3:15–4:00 | Tacit knowledge: operations & security | Q-S04, Q-S05, Q-O04, Q-A05 | Tacit Knowledge | Operational procedures, SLAs, external controls. This is the highest-value tacit knowledge capture time. |

**Session 3: Methodology + Follow-ups + Open Questions (3 hours)**

| Time | Activity | Questions | Method | Notes for Facilitator |
|------|----------|-----------|--------|----------------------|
| 0:00–0:15 | Recap Sessions 1–2, priority follow-ups | — | — | Start with the most critical open items |
| 0:15–1:00 | Methodology validation | Q-M01, Q-M02, Q-M03, M-04, M-05 | Mixed | This validates the AI approach for the client's real systems |
| 1:00–1:30 | Follow-up items from Sessions 1–2 | Varies | Mixed | Work through flagged items from prior sessions |
| 1:30–2:00 | **Break** | — | — | |
| 2:00–2:45 | Open-ended: "What did we miss?" | Q-B05, Q-B03 (extended) | Tacit Knowledge | Unstructured time for the SME to share anything the questions didn't cover |
| 2:45–3:00 | Final wrap-up | — | — | Thank the SME, confirm any follow-up (email/phone) availability |

#### Scenario B: 2 SMEs, 5 Sessions

**Sessions 1–2 (SME #1: Application Developer)**

| Session | Focus | Duration | Key Questions |
|---------|-------|----------|---------------|
| 1 | Architecture + Data Model + Context | 4 hours | Q-A01 through Q-A08, Q-D01 through Q-D05 |
| 2 | Security + Screen Flows + CICS patterns | 4 hours | Q-S01 through Q-S05, Q-A03, Q-A04, Q-F01 through Q-F05 |

**Sessions 3–4 (SME #2: Operations/Business)**

| Session | Focus | Duration | Key Questions |
|---------|-------|----------|---------------|
| 3 | Batch Workflows + Business Rules | 4 hours | Q-O01 through Q-O04, Q-B01 through Q-B05 |
| 4 | Domain Model + Operational Procedures | 3 hours | Q-B06 through Q-B13, Q-O05 through Q-O12 |

**Session 5 (Both SMEs: Joint)**

| Time | Activity | Duration |
|------|----------|----------|
| 0:00–1:00 | Cross-validation: Items where SME #1 and #2 provided different perspectives | 1 hour |
| 1:00–1:30 | Methodology validation (both perspectives) | 30 min |
| 1:30–2:00 | Break | 30 min |
| 2:00–3:00 | Open-ended: Combined tacit knowledge capture | 1 hour |

### 9.3 Facilitator Guide

#### Before the Session

1. Print or prepare digital copies of all documentation sections needed for the session
2. Have the question bank ready with space for notes next to each question
3. Prepare recording equipment (with SME consent) or assign a dedicated note-taker
4. Review the "Materials to Withhold" list — do not display these until after blind validation questions

#### Asking Questions

**DO:**
- Read questions exactly as written — they've been carefully worded to avoid leading the SME
- Allow silence — SMEs need time to recall details from years ago
- Use follow-up probes: "Can you say more about that?" "How did you know that?" "Was that documented anywhere?"
- Ask "What else?" after the SME's initial response — tacit knowledge often comes in the second or third response

**DON'T:**
- Don't fill silences by suggesting answers
- Don't show the documentation before blind validation questions
- Don't say "The documentation says X — is that right?" (this anchors the SME). Instead say "What would you expect here?"
- Don't argue with the SME — record disagreements for later analysis

#### Handling "I Don't Know" Responses

| SME Says | Facilitator Action |
|----------|-------------------|
| "I don't know" | Ask: "Would you have known this during your active tenure? Or is this something that was always unclear?" This distinguishes **knowledge loss** from **never-known**. |
| "I'm not sure" | Ask: "What's your best guess? And how confident are you — very, somewhat, or not at all?" Record the confidence level. |
| "It depends" | Ask: "What does it depend on? Can you describe the different scenarios?" This often reveals conditional business rules. |
| "That was someone else's area" | Record as "Outside SME expertise." Ask: "Who would have known this? What role?" This helps with future SME sourcing. |
| "I don't remember the specifics" | Ask: "Do you remember the general approach? Even a directional answer is helpful." |

#### Distinguishing Documentation Issues

| SME Reaction | Likely Meaning | Action |
|-------------|----------------|--------|
| "That's wrong" | Documentation error | Record correct value, ask for source/evidence |
| "That's incomplete" | Missing context | Ask what's missing, record as tacit knowledge |
| "That's technically correct but misleading" | Interpretation issue | Ask how it should be framed, record nuance |
| "I wouldn't say it that way" | Terminology issue | Record preferred terminology, note for domain model |
| "That was true then, but it changed" | Historical context | Record timeline of changes, note which version the docs reflect |
| "I've never seen that" | Possible hallucination OR knowledge gap | Probe further — "Could this have been added after your time?" |

#### Red Flags

- SME consistently says "that's wrong" for items AI validation confirmed as factually correct → SME may be recalling a different version of the system
- SME says "I don't recognize any of this" → Verify the SME worked on this specific system (not a different mainframe application)
- SME provides answers that contradict the source code → May indicate the code was modified after the SME's tenure; note for investigation

### 9.4 Recording and Note-Taking

#### Real-Time Note Template

Use this template for each question asked:

```
─────────────────────────────────────────────────
Question ID: [Q-xxx]
Time: [HH:MM]
Question Asked: [Exact wording used]
─────────────────────────────────────────────────
SME Response (verbatim or close paraphrase):




─────────────────────────────────────────────────
Confidence Rating:  □ 5-Verified  □ 4-High  □ 3-Moderate  □ 2-Low  □ 1-Unverified
SME Self-Reported Confidence: □ Very sure  □ Somewhat sure  □ Not sure  □ Don't know
─────────────────────────────────────────────────
Documentation Impact:
□ Confirms documentation    □ Contradicts documentation
□ Adds missing context      □ No impact (informational)
─────────────────────────────────────────────────
Follow-up Needed: □ Yes  □ No
If yes, describe:

─────────────────────────────────────────────────
Tacit Knowledge Captured: □ Yes  □ No
If yes, category: □ Business rule  □ Operational procedure
                  □ Design rationale  □ Performance workaround
                  □ Historical context  □ External dependency
─────────────────────────────────────────────────
```

#### Recording Guidance

- **Preferred:** Audio recording with SME consent, supplemented by written notes
- **Fallback:** Two note-takers (one verbatim, one structural) if recording is declined
- **Minimum:** Single note-taker using the template above
- **Post-session:** Transcribe or review recordings within 24 hours while context is fresh

#### Post-Session Debrief Template

Complete within 24 hours of each session:

```
═══════════════════════════════════════════════════
POST-SESSION DEBRIEF
═══════════════════════════════════════════════════
Session: [#] of [total]
Date: [YYYY-MM-DD]
SME: [Name, role, years of experience]
Facilitator: [Name]
Note-taker: [Name]
Duration: [actual hours]
═══════════════════════════════════════════════════

1. KEY FINDINGS (top 3-5)
   a.
   b.
   c.

2. SURPRISES (things we didn't expect)
   a.
   b.

3. DOCUMENTATION CONFIRMED AS ACCURATE
   - [List specific sections/claims]

4. DOCUMENTATION CONTRADICTED
   - [List with correct information]

5. TACIT KNOWLEDGE CAPTURED
   - [Categorized list]

6. ITEMS REQUIRING FURTHER INVESTIGATION
   - [List with assigned owner]

7. CONFIDENCE RATINGS ASSIGNED
   | Area | Rating | Notes |
   |------|--------|-------|

8. ITEMS FOR NEXT SESSION
   - [List with priority]

9. METHODOLOGY FEEDBACK (if discussed)
   - Quality rating: [1-5]
   - Trust percentage: [%]
   - Key concerns:

═══════════════════════════════════════════════════
```

### 9.5 Post-Interview Deliverable Template

After all sessions with an SME are complete, produce this deliverable:

```
═══════════════════════════════════════════════════
SME VALIDATION REPORT
═══════════════════════════════════════════════════

1. SME PROFILE
   - Name: [Full name]
   - Background: [Role, company, years in mainframe]
   - Specific expertise: [COBOL/CICS/VSAM/JCL/etc.]
   - Tenure with this system: [dates]
   - Sessions completed: [N of N planned]

2. QUESTIONS ASKED WITH RESPONSES
   [Complete log from all sessions, organized by topic]

3. CONFIDENCE RATINGS PER DOCUMENTATION AREA

   | Document Area | Confidence (1-5) | Notes |
   |--------------|-------------------|-------|
   | Domain Model | | |
   | Data Model | | |
   | Context Model | | |
   | C4 Architecture | | |
   | Screen Flows | | |
   | Batch Workflows | | |
   | Security Model | | |
   | Business Rules | | |

4. TACIT KNOWLEDGE CAPTURED

   4a. Business Rules Enforced Outside Application
       [Categorized list]

   4b. Operational Procedures
       [Categorized list]

   4c. Design Rationale
       [Categorized list]

   4d. Performance Considerations
       [Categorized list]

   4e. Historical Context
       [Categorized list]

   4f. External Dependencies
       [Categorized list]

5. DIRECT QUOTES (with permission)
   - "[Quote]" — Context: [when/why said]

6. DISCREPANCIES FOUND
   | # | Doc Section | Doc Claims | SME Says | Severity |
   |---|------------|------------|----------|----------|

7. ITEMS REQUIRING FURTHER INVESTIGATION
   | # | Item | Owner | Status |
   |---|------|-------|--------|

8. RECOMMENDATIONS FOR DOCUMENTATION REMEDIATION
   | # | Document | Section | Action | Priority |
   |---|----------|---------|--------|----------|

9. METHODOLOGY ASSESSMENT
   - Quality rating: [1-5]
   - Trust percentage: [%]
   - Would use for production: [Yes/No/With conditions]
   - Key feedback: [verbatim]

═══════════════════════════════════════════════════
```

---

## 10. Tacit Knowledge Capture Framework

Tacit knowledge is the most perishable asset in mainframe modernization. This framework ensures systematic capture of knowledge that exists *outside* the code.

### 10.1 Knowledge Categories

#### Category A: Business Rules Enforced Outside the Application

Knowledge that complements the 58 rules documented in BUSINESS-RULES.md.

| Capture Prompt | Example | Impact on Modernization |
|---------------|---------|------------------------|
| "What rules were enforced by operations staff?" | Daily cutoff times, manual fraud review thresholds | Must be codified in the modernized system |
| "What rules came from policy, not code?" | Credit limit approval processes, account closure procedures | Must be documented as business requirements |
| "What rules were enforced by downstream systems?" | Authorization network responses, statement formatting rules | Must be replicated or replaced |
| "What rules existed only as tribal knowledge?" | "We never processed transactions over $X without manager approval" | At highest risk of being lost |

#### Category B: Operational Procedures

| Capture Prompt | Example | Impact on Modernization |
|---------------|---------|------------------------|
| "What was the restart/recovery procedure for [job]?" | Specific steps for each batch job failure scenario | Must be automated or documented for new operations team |
| "What monitoring alerts existed?" | JES2 alerts, CICS region health checks, VSAM space monitoring | Must be replicated in cloud monitoring |
| "What was the escalation path?" | Operator → systems programmer → on-call developer → manager | Must be defined for the new operations model |
| "What were the batch scheduling windows?" | Batch starts at 23:00, must complete by 05:00 | Defines SLA requirements for modernized batch |

#### Category C: Performance Bottlenecks and Workarounds

| Capture Prompt | Example | Impact on Modernization |
|---------------|---------|------------------------|
| "Which programs were known to be slow?" | "COACTUPC was slow because it reads 4 files" | Informs optimization priorities |
| "Were there VSAM tuning workarounds?" | CI/CA split ratios, buffer pool assignments, SHAREOPTION settings | May not apply to new data store but indicates performance-sensitive areas |
| "Were there memory constraints?" | Working storage limits, COMMAREA size constraints | Explains design decisions |

#### Category D: Design Rationale and Historical Context

| Capture Prompt | Example | Impact on Modernization |
|---------------|---------|------------------------|
| "Why was it built this way?" | "The original developer wanted to..." | Distinguishes intentional design from accidental complexity |
| "What changed over time?" | "Card validation was added in version 2 after a fraud incident" | Reveals evolution and technical debt |
| "What was considered but rejected?" | "We looked at DB2 for transactions but VSAM was faster" | Prevents repeating rejected approaches |

#### Category E: Known Issues and Technical Debt

| Capture Prompt | Example | Impact on Modernization |
|---------------|---------|------------------------|
| "What bugs did you live with?" | "The card list screen sometimes shows duplicates" | Must be fixed, not replicated |
| "What was the deferred fix list?" | "We planned to add password expiration but never got to it" | Confirms SEC findings as known issues |
| "What workarounds were in production?" | "We restarted the CICS region every night to prevent memory leaks" | Reveals stability issues to address |

#### Category F: External Dependencies

| Capture Prompt | Example | Impact on Modernization |
|---------------|---------|------------------------|
| "What RACF resources were defined?" | Transaction-level security, dataset-level access control | Must be replicated in IAM |
| "What network topology existed?" | VTAM definitions, SNA network, terminal configurations | Informs network modernization |
| "What downstream systems consumed this data?" | Statements, reporting, regulatory feeds | Must be maintained during migration |
| "What manual processes surrounded the system?" | Daily reconciliation reports, monthly account reviews | Must be automated or documented |

### 10.2 Tacit Knowledge Item Template

For each piece of tacit knowledge captured, record:

```
─────────────────────────────────────────────────
TACIT KNOWLEDGE ITEM: TK-[NNN]
─────────────────────────────────────────────────
Category: [A/B/C/D/E/F]
Source SME: [Name]
Session: [#]
Confidence: [1-5]
─────────────────────────────────────────────────
Knowledge Statement:
[Clear, specific description of what was learned]

─────────────────────────────────────────────────
Context:
[When/why this was shared, what question prompted it]

─────────────────────────────────────────────────
Impact on Modernization: □ High  □ Medium  □ Low
Explain:

─────────────────────────────────────────────────
Impact on Documentation: □ Contradicts  □ Supplements  □ No impact
Which document:

─────────────────────────────────────────────────
CardDemo-Specific or Methodology-General?
□ CardDemo-specific (only relevant to this application)
□ Methodology-general (applies to all mainframe reverse engineering)

─────────────────────────────────────────────────
Action Required: □ Update documentation  □ Add to modernization plan
                 □ Flag for compliance  □ Informational only
─────────────────────────────────────────────────
```

### 10.3 Knowledge Capture Priority Matrix

Given limited SME time, prioritize capture of knowledge with the highest combination of perishability and impact:

| Knowledge Type | Perishability | Impact | Priority |
|---------------|--------------|--------|----------|
| Restart/recovery procedures for batch jobs | Very High — only in SME's memory | High — needed for operations | **P0** |
| Business rules enforced outside code | Very High — tribal knowledge | Critical — affects correctness | **P0** |
| External system dependencies | High — may be undocumented | High — affects scope | **P0** |
| Design rationale for key decisions | Very High — original developers retired | Medium — informs architecture | **P1** |
| Performance bottlenecks and workarounds | High — operational experience | Medium — informs optimization | **P1** |
| Known bugs and technical debt | Medium — may be in issue trackers | Medium — avoid replicating bugs | **P2** |
| Historical context and evolution | Medium — some may be in change logs | Low — informational | **P2** |
| Terminal/screen shortcuts | Low — users may still know | Low — UI will change | **P3** |

---

## Appendix A: Quick Reference Card

**For the facilitator — print and keep at hand during sessions.**

### Confidence Scale (Quick Reference)

| Level | Shorthand | When to Assign |
|-------|-----------|----------------|
| 5 | "SME says 'yes, I can see it in the code'" | Verified against source |
| 4 | "SME says 'yes, that's standard'" | Expertise-based confirmation |
| 3 | "SME says 'probably, but...'" | Qualified agreement |
| 2 | "SME says 'I see issues with this'" | Partial disagreement |
| 1 | "SME says 'that's wrong' or no SME reviewed" | Unverified or rejected |

### Session Checklist

- [ ] Recording consent obtained
- [ ] Blind validation materials withheld
- [ ] Note-taking template printed
- [ ] Code printouts prepared (if code walkthrough planned)
- [ ] Previous session follow-up items reviewed
- [ ] Documentation sections loaded for reference
- [ ] Post-session debrief scheduled within 24 hours

### Emergency Questions (If Running Short on Time)

If a session is running over and you must prioritize, ask these three questions:

1. **Q-A01** (Blind Validation — bounded contexts) — highest signal-to-noise ratio
2. **Q-M03** (Trust percentage) — single most important methodology metric
3. **Q-B03** (Rules outside code) — highest-value tacit knowledge question

---

## Appendix B: Document Cross-Reference Index

All references in this strategy document mapped to source files:

| Reference | Source File | Verified |
|-----------|-----------|----------|
| VL-001 Score: 88.0 | `docs/reverse-engineering/validation/VL-001-domain-model-report.md:12` | Yes |
| VL-002 Score: 96.8 | `docs/reverse-engineering/validation/VL-002-data-model-report.md:13` | Yes |
| VL-003 Score: 80.6 | `docs/reverse-engineering/validation/VL-003-context-model-report.md:14` | Yes |
| VL-004 Score: 90.0 | `docs/reverse-engineering/validation/VL-004-c4-architecture-report.md:14` | Yes |
| VL-005 Score: 90.6 | `docs/reverse-engineering/validation/VL-005-screen-flow-report.md:17` | Yes |
| VL-006 Score: 73.8 | `docs/reverse-engineering/validation/VL-006-batch-workflow-report.md:14` | Yes |
| VL-007 Score: 90.0 | `docs/reverse-engineering/validation/VL-007-security-model-report.md:14` | Yes |
| VL-006 HAL-001: MNTTRDB2 | `VL-006-batch-workflow-report.md:101-109` | Yes |
| VL-006 CRT-001: 6 missing JCL | `VL-006-batch-workflow-report.md:39-59` | Yes |
| VL-003 MAJ-001: 5 BMS errors | `VL-003-context-model-report.md:47-64` | Yes |
| VL-007 M-001: COMMAREA ~130 vs 160 | `VL-007-security-model-report.md:49-74` | Yes |
| VL-007 M-003: SET CDEMO-USRTYP-USER | `VL-007-security-model-report.md:104-126` | Yes |
| SEC-001 through SEC-010 | `docs/reverse-engineering/05-specialized/SECURITY-MODEL.md:354-699` | Yes |
| BR-V001 through BR-R005 | `docs/reverse-engineering/01-domain-model/BUSINESS-RULES.md` | Yes |
| 58 business rules total | `BUSINESS-RULES.md` (25 validation + 8 calc + 6 state + 5 auth + 4 temporal + 3 limit + 3 sequence + 5 rejection = 59*) | Yes |

*Note: The count of 58 referenced in the plan is approximate; the catalog contains ~59 individually identified rules depending on counting method.

---

## Document Control

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-02-06 | Claude Code | Initial SME validation strategy |

---

*This document was generated as part of the CardDemo reverse engineering validation strategy. It is designed to be used by a non-expert facilitator to conduct SME validation interviews for AI-generated mainframe documentation.*
