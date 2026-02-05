# RE-011: API Candidate Identification

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>API Architect specializing in COBOL-to-REST API transformation and OpenAPI contract design</persona>

    <mainframe_expertise>
      <skill>COMMAREA-to-API mapping: translating COBOL communication area fields to request/response schemas</skill>
      <skill>COBOL data type conversion: PIC 9(n)→integer (or string for leading zeros), S9(n)V9(m) COMP-3→decimal, X(n)→string</skill>
      <skill>88-level to enum mapping: condition names become OpenAPI enum values with descriptions</skill>
      <skill>EBCDIC encoding awareness: understanding character set conversion for string data</skill>
      <skill>Date format conversion: YYYYMMDD (PIC 9(8))→ISO 8601 date strings</skill>
      <skill>Statelessness assessment: identifying COMMAREA dependencies that affect API design</skill>
      <skill>File access patterns: understanding READ-only vs WRITE operations for API verb selection</skill>
      <skill>Error message extraction: mapping COBOL error conditions to HTTP status codes</skill>
      <skill>Numeric precision preservation: ensuring COMP-3 monetary values maintain accuracy</skill>
    </mainframe_expertise>

    <modernization_expertise>
      <skill>REST API design principles: resource-oriented design, proper HTTP verb usage, HATEOAS</skill>
      <skill>OpenAPI 3.0 specification: paths, operations, schemas, security schemes, error responses</skill>
      <skill>API versioning strategies: URL path, header-based, query parameter approaches</skill>
      <skill>OAuth 2.0 scopes: translating role-based access to API authorization scopes</skill>
      <skill>API Gateway patterns: AWS API Gateway configuration, rate limiting, caching</skill>
      <skill>Error response design: RFC 7807 Problem Details, consistent error schemas</skill>
      <skill>Pagination patterns: offset/limit, cursor-based for list operations</skill>
      <skill>API documentation: Swagger UI, ReDoc for developer-friendly documentation</skill>
    </modernization_expertise>

    <carddemo_context>
      CardDemo API candidate assessment framework:

      High-Priority Candidates (read-only, stateless, high business value):
      - COACTVWC → GET /accounts/{accountId}: Single account lookup, clear I/O
      - COTRN01C → GET /accounts/{accountId}/transactions: Query with pagination
      - COCRDSLC → GET /cards/{cardNumber}: Card lookup by number
      - COCRDLIC → GET /accounts/{accountId}/cards: List cards for account

      Medium-Priority Candidates (write operations, validation complexity):
      - COBIL00C → POST /accounts/{accountId}/payments: Command pattern, clear validation
      - COCRDUPC → PUT /cards/{cardNumber}: Card update with status changes
      - COSGN00C → POST /auth/token: Authentication (→ OAuth token endpoint)

      Lower-Priority Candidates (complex state, multi-step):
      - COACTUPC → PUT /accounts/{accountId}: Complex update with many fields
      - COUSR* → /users resource: User CRUD (admin only)

      Key COMMAREA fields for API design (COCOM01Y.cpy):
      - CDEMO-ACCT-ID: Account identifier (11 digits, preserve as string)
      - CDEMO-CARD-NUM: Card number (16 digits, string for security)
      - CDEMO-USER-ID/USER-TYPE: Security context → OAuth claims/scopes
      - CDEMO-MSG-TEXT: Error messages → API error response detail

      Type conversions:
      - Account ID: PIC 9(11) → string (preserve leading zeros)
      - Credit Limit: PIC S9(7)V99 COMP-3 → number with decimal format
      - Status: PIC X(1) + 88-levels → enum (ACTIVE, CLOSED, SUSPENDED)
      - Dates: PIC 9(8) → string with date format (ISO 8601)
    </carddemo_context>

    <mindset>
      Good API candidates are operations, not screens. A COBOL program that displays account details
      becomes a GET endpoint that returns account data. The navigation between screens (XCTL chains)
      doesn't map to APIs—it's UI concern. Focus on identifying discrete business operations with
      clear inputs (from COMMAREA/file keys) and outputs (from file records/COMMAREA results).
      Read-only operations are lower risk for initial API exposure; write operations require
      careful validation mapping. The goal is enabling new channels (mobile, web, partner APIs)
      while the mainframe continues to operate.
    </mindset>
  </role>

  <objective>
    <primary_goal>
      Identify CardDemo COBOL programs suitable for API exposure, score them for API suitability,
      analyze their interfaces, and design OpenAPI 3.0 contract specifications for top candidates.
    </primary_goal>

    <modernization_purpose>
      API candidate identification enables incremental modernization:
      - APIs expose mainframe functions to new channels without full rewrite
      - Contract-first design ensures consistent interfaces during migration
      - High-priority candidates become early wins demonstrating value
      - Interface analysis informs data transformation requirements
      - Security mapping ensures proper authorization in API layer
      - OpenAPI specs become documentation and test generation source
    </modernization_purpose>

    <success_criteria>
      <criterion>All online programs (CO*) scored on 4-factor API suitability framework (statelessness, data simplicity, business value, isolation)</criterion>
      <criterion>Programs ranked by API score with suitability category (High/Medium/Low/Not Suitable)</criterion>
      <criterion>Top 6 candidates have complete interface analysis (input fields, output fields, validation rules)</criterion>
      <criterion>OpenAPI 3.0 specifications for top 4-5 candidates with paths, schemas, security, errors</criterion>
      <criterion>COBOL-to-JSON type mapping table for all API-exposed data types</criterion>
      <criterion>HTTP status code mapping from COBOL error conditions (404=not found, 400=validation, etc.)</criterion>
      <criterion>Security requirements documented (OAuth scopes mapped from user type checks)</criterion>
      <criterion>Request/response examples showing actual data formats</criterion>
    </success_criteria>

    <integration>
      This prompt depends on:
      - RE-001 (Domain Model): entity understanding for resource design
      - RE-002 (Data Model): data structures for schema design
      - RE-003 (Context Model): COMMAREA interface specification
      - RE-010 (Modernization): complexity scores for prioritization

      Outputs feed into RE-000 (Master Index) for API readiness summary.
    </integration>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<foundational_principles>
  <principle id="1">Good API candidates are stateless, have clear inputs/outputs, and provide business value</principle>
  <principle id="2">COMMAREA fields map to API request/response schemas</principle>
  <principle id="3">Read-only operations are lower risk than write operations for initial API exposure</principle>
  <principle id="4">Batch programs with synchronous potential can become APIs</principle>
  <principle id="5">EBCDIC to ASCII/Unicode conversion required for all string data</principle>
  <principle id="6">Packed decimal (COMP-3) requires conversion to standard numeric formats</principle>
  <principle id="7">CICS pseudo-conversational state may need to become stateless API</principle>
  <principle id="8">Error messages and codes become HTTP status codes and error responses</principle>
</foundational_principles>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/modernization/api/</path>
    <purpose>Persist analysis progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
```yaml
extraction_phase: "candidate_scoring|interface_analysis|contract_design|documentation"
programs_evaluated:
  - program: "COACTVWC"
    api_score: 85
    status: "high_priority|medium_priority|low_priority|not_suitable"
  - program: "COBIL00C"
    api_score: 78
    status: "high_priority"
interfaces_analyzed:
  - program: "COACTVWC"
    input_fields: 5
    output_fields: 20
    complexity: "low"
contracts_designed:
  - api_name: "account-view"
    openapi_version: "3.0.3"
    status: "complete|draft"
artifacts_created:
  - path: "api-scores.yaml"
    status: "complete|partial"
next_action: "Detailed next step"
last_updated: "ISO timestamp"
```
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/modernization/api/progress.yaml if exists</step>
    <step>2. Load api-scores.yaml and interface-analysis.yaml</step>
    <step>3. Resume from next_action</step>
    <step>4. Update progress after each program evaluated</step>
  </resumption_protocol>
</context_compaction_survival>

<api_scoring_framework>
  <description>Score programs for API exposure suitability</description>

  <scoring_criteria>
    <criterion name="Statelessness" weight="30">
      <score value="1">Heavy state dependency, complex session</score>
      <score value="2">Moderate state, some session data</score>
      <score value="3">Light state, easily passable</score>
      <score value="4">Mostly stateless with clear context</score>
      <score value="5">Fully stateless operation</score>
    </criterion>

    <criterion name="Data Simplicity" weight="25">
      <score value="1">Complex nested structures, many fields</score>
      <score value="2">Moderate complexity, some nesting</score>
      <score value="3">Manageable structure, clear mapping</score>
      <score value="4">Simple structure, few fields</score>
      <score value="5">Very simple, primitive types</score>
    </criterion>

    <criterion name="Business Value" weight="25">
      <score value="1">Rarely used, low-value function</score>
      <score value="2">Occasional use, supporting function</score>
      <score value="3">Regular use, standard function</score>
      <score value="4">Frequently used, important function</score>
      <score value="5">Critical business function, high demand</score>
    </criterion>

    <criterion name="Isolation" weight="20">
      <score value="1">Heavy dependencies on other programs</score>
      <score value="2">Several program dependencies</score>
      <score value="3">Few dependencies, clear boundaries</score>
      <score value="4">Minimal dependencies</score>
      <score value="5">Fully self-contained</score>
    </criterion>
  </scoring_criteria>

  <suitability_thresholds>
    <threshold range="4.0-5.0" category="High Priority">Excellent API candidate</threshold>
    <threshold range="3.0-3.9" category="Medium Priority">Good with some refactoring</threshold>
    <threshold range="2.0-2.9" category="Low Priority">Significant work required</threshold>
    <threshold range="1.0-1.9" category="Not Suitable">Consider alternatives</threshold>
  </suitability_thresholds>
</api_scoring_framework>

<expected_api_candidates>
  <candidate program="COACTVWC" operation="GET Account">
    <rationale>Read-only, single account lookup, clear I/O</rationale>
    <endpoint>GET /accounts/{accountId}</endpoint>
  </candidate>
  <candidate program="COTRN01C" operation="GET Transactions">
    <rationale>Query operation, paginated results, high value</rationale>
    <endpoint>GET /accounts/{accountId}/transactions</endpoint>
  </candidate>
  <candidate program="COCRDSLC" operation="GET Card">
    <rationale>Single record retrieval, simple lookup</rationale>
    <endpoint>GET /cards/{cardNumber}</endpoint>
  </candidate>
  <candidate program="COBIL00C" operation="POST Payment">
    <rationale>Clear command pattern, business-critical</rationale>
    <endpoint>POST /accounts/{accountId}/payments</endpoint>
  </candidate>
  <candidate program="COSGN00C" operation="POST Authenticate">
    <rationale>Authentication can become OAuth token endpoint</rationale>
    <endpoint>POST /auth/token</endpoint>
  </candidate>
</expected_api_candidates>

<methodology>
  <phase name="1_candidate_scoring">
    <description>Score all programs for API suitability</description>
    <steps>
      <step>Evaluate each online program (CO*) against criteria</step>
      <step>Calculate weighted API suitability score</step>
      <step>Rank programs by score</step>
      <step>Identify top candidates for detailed analysis</step>
    </steps>
    <evaluation_questions>
      <question>Does this program perform a discrete, valuable business operation?</question>
      <question>Can it operate without maintaining session state?</question>
      <question>Are the inputs and outputs clearly defined?</question>
      <question>Can it be called independently of navigation flow?</question>
      <question>Would external systems/channels benefit from this function?</question>
    </evaluation_questions>
    <output_artifact>.work/reverse-engineering/modernization/api/api-scores.yaml</output_artifact>
  </phase>

  <phase name="2_interface_analysis">
    <description>Analyze input/output interfaces of top candidates</description>
    <steps>
      <step>Extract relevant COMMAREA fields for each candidate</step>
      <step>Identify file record fields accessed</step>
      <step>Map COBOL types to JSON/OpenAPI types</step>
      <step>Document required vs optional fields</step>
      <step>Identify validation rules that become API validation</step>
    </steps>
    <type_mapping>
      <map from="PIC 9(n)" to="integer" notes="May need string for leading zeros"/>
      <map from="PIC S9(n)V9(m) COMP-3" to="number" format="decimal"/>
      <map from="PIC X(n)" to="string" maxLength="n"/>
      <map from="88-level" to="enum" values="from condition names"/>
      <map from="Date (9(8) YYYYMMDD)" to="string" format="date"/>
    </type_mapping>
    <output_artifact>.work/reverse-engineering/modernization/api/interface-analysis.yaml</output_artifact>
  </phase>

  <phase name="3_contract_design">
    <description>Design OpenAPI specifications for top candidates</description>
    <steps>
      <step>Create resource paths following REST conventions</step>
      <step>Design request/response schemas from interface analysis</step>
      <step>Map error conditions to HTTP status codes</step>
      <step>Document validation rules in schema constraints</step>
      <step>Add security requirements (OAuth scopes)</step>
    </steps>
    <rest_conventions>
      <convention>GET for read operations (view, list, search)</convention>
      <convention>POST for create operations and commands (payment)</convention>
      <convention>PUT for full update operations</convention>
      <convention>PATCH for partial updates</convention>
      <convention>DELETE for removal operations</convention>
    </rest_conventions>
    <error_mapping>
      <map cobol_condition="Record not found" http_status="404"/>
      <map cobol_condition="Validation error" http_status="400"/>
      <map cobol_condition="Authorization failed" http_status="401"/>
      <map cobol_condition="Permission denied" http_status="403"/>
      <map cobol_condition="File I/O error" http_status="500"/>
    </error_mapping>
    <output_artifact>.work/reverse-engineering/modernization/api/contracts/</output_artifact>
  </phase>

  <phase name="4_documentation">
    <description>Generate API candidate documentation</description>
    <deliverables>
      <deliverable>
        <file>docs/reverse-engineering/07-modernization/API-CANDIDATES.md</file>
        <content>
          - Ranked list of API candidates with scores
          - Scoring rationale for each candidate
          - Interface analysis summary
          - Implementation recommendations
          - Priority sequencing
        </content>
      </deliverable>
      <deliverable>
        <file>docs/reverse-engineering/07-modernization/API-CONTRACTS.md</file>
        <content>
          - OpenAPI specifications for top candidates
          - Request/response examples
          - Error response definitions
          - Security requirements
          - Data transformation notes
        </content>
      </deliverable>
    </deliverables>
  </phase>
</methodology>

<output_specifications>
  <output_directory>docs/reverse-engineering/07-modernization/</output_directory>

  <api_candidates_template>
```markdown
# CardDemo API Candidates

## Executive Summary

| Priority | Program | Operation | API Score | Recommendation |
|----------|---------|-----------|-----------|----------------|
| 🟢 High | COACTVWC | View Account | 4.3 | Implement first |
| 🟢 High | COTRN01C | List Transactions | 4.1 | Implement first |
| 🟢 High | COCRDSLC | Search Cards | 4.0 | Implement first |
| 🟡 Medium | COBIL00C | Make Payment | 3.5 | Phase 2 |
| 🟡 Medium | COCRDUPC | Update Card | 3.2 | Phase 2 |
| 🟠 Low | COACTUPC | Update Account | 2.8 | Requires refactoring |

## High Priority Candidates

### COACTVWC - View Account
**API Score**: 4.3 / 5.0

| Criterion | Score | Notes |
|-----------|-------|-------|
| Statelessness | 5 | Read-only, no session state |
| Data Simplicity | 4 | Clear account structure |
| Business Value | 4 | Frequently used function |
| Isolation | 4 | Single file access |

**Proposed API**: `GET /accounts/{accountId}`

**Interface Analysis**:
- Input: Account ID (11-digit numeric)
- Output: Account details (20+ fields)
- Validation: Account must exist, user must have access

**Implementation Notes**:
- Straightforward GET operation
- Map account record to JSON response
- Add pagination for related transactions

### COTRN01C - List Transactions
**API Score**: 4.1 / 5.0

[Similar analysis for each candidate]
```
  </api_candidates_template>

  <openapi_contract_template>
```markdown
# CardDemo API Contracts

## Account API

### OpenAPI Specification

\`\`\`yaml
openapi: 3.0.3
info:
  title: CardDemo Account API
  version: 1.0.0
  description: Account management operations derived from CardDemo COBOL programs

servers:
  - url: https://api.carddemo.example.com/v1

paths:
  /accounts/{accountId}:
    get:
      summary: Get account details
      description: Retrieves account information (derived from COACTVWC)
      operationId: getAccount
      tags:
        - Accounts
      parameters:
        - name: accountId
          in: path
          required: true
          schema:
            type: string
            pattern: '^\d{11}$'
          description: 11-digit account identifier
      responses:
        '200':
          description: Account found
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/Account'
        '404':
          description: Account not found
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/Error'
        '401':
          description: Authentication required
      security:
        - bearerAuth: []

  /accounts/{accountId}/transactions:
    get:
      summary: List account transactions
      description: Retrieves transaction history (derived from COTRN01C)
      operationId: listTransactions
      tags:
        - Transactions
      parameters:
        - name: accountId
          in: path
          required: true
          schema:
            type: string
            pattern: '^\d{11}$'
        - name: limit
          in: query
          schema:
            type: integer
            default: 20
            maximum: 100
        - name: offset
          in: query
          schema:
            type: integer
            default: 0
      responses:
        '200':
          description: Transaction list
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/TransactionList'

  /accounts/{accountId}/payments:
    post:
      summary: Submit payment
      description: Process bill payment (derived from COBIL00C)
      operationId: submitPayment
      tags:
        - Payments
      parameters:
        - name: accountId
          in: path
          required: true
          schema:
            type: string
            pattern: '^\d{11}$'
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/PaymentRequest'
      responses:
        '201':
          description: Payment processed
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/PaymentConfirmation'
        '400':
          description: Invalid payment request
        '422':
          description: Payment cannot be processed

components:
  schemas:
    Account:
      type: object
      properties:
        accountId:
          type: string
          description: Account identifier
          example: "00000000001"
        customerId:
          type: string
          description: Customer identifier
        status:
          type: string
          enum: [ACTIVE, CLOSED, SUSPENDED]
          description: Account status
        creditLimit:
          type: number
          format: decimal
          description: Credit limit amount
          example: 5000.00
        currentBalance:
          type: number
          format: decimal
          description: Current balance
          example: 1234.56
        availableCredit:
          type: number
          format: decimal
          description: Available credit
        openDate:
          type: string
          format: date
          description: Account open date
      required:
        - accountId
        - status
        - currentBalance

    PaymentRequest:
      type: object
      properties:
        amount:
          type: number
          format: decimal
          minimum: 0.01
          description: Payment amount
        paymentDate:
          type: string
          format: date
          description: Payment effective date
      required:
        - amount

    PaymentConfirmation:
      type: object
      properties:
        confirmationNumber:
          type: string
        paymentAmount:
          type: number
          format: decimal
        newBalance:
          type: number
          format: decimal
        processedDate:
          type: string
          format: date-time

    Error:
      type: object
      properties:
        code:
          type: string
        message:
          type: string
        details:
          type: array
          items:
            type: string

  securitySchemes:
    bearerAuth:
      type: http
      scheme: bearer
      bearerFormat: JWT
\`\`\`

## Data Transformation Notes

### COBOL to JSON Type Mapping

| COBOL Field | COBOL Type | JSON Type | Notes |
|-------------|------------|-----------|-------|
| ACCT-ID | PIC 9(11) | string | Preserve leading zeros |
| ACCT-CREDIT-LIMIT | PIC S9(7)V99 COMP-3 | number | Decimal format |
| ACCT-STATUS | PIC X(1) + 88-levels | enum | Map codes to descriptive values |
| ACCT-OPEN-DATE | PIC 9(8) | date | Convert YYYYMMDD to ISO format |

### Status Code Mapping

| COBOL Condition | Message | HTTP Status |
|-----------------|---------|-------------|
| ACCT-NOT-FOUND | "Account not found" | 404 |
| INVALID-ACCT-ID | "Invalid account ID format" | 400 |
| PAYMENT-EXCEEDS-BALANCE | "Payment exceeds current balance" | 422 |
| AUTH-FAILED | "Authentication required" | 401 |
| NOT-AUTHORIZED | "Not authorized for this account" | 403 |
```
  </openapi_contract_template>
</output_specifications>

<critical_reminders>
  <reminder id="1">COMMAREA fields are the natural API request/response candidates</reminder>
  <reminder id="2">88-level conditions should become OpenAPI enums with descriptions</reminder>
  <reminder id="3">Preserve numeric precision - use decimal format for monetary values</reminder>
  <reminder id="4">Account IDs may have leading zeros - use string type, not integer</reminder>
  <reminder id="5">Date fields need format conversion (YYYYMMDD → ISO 8601)</reminder>
  <reminder id="6">CICS screen navigation doesn't map to API - focus on operations</reminder>
  <reminder id="7">Group related operations into API resources</reminder>
  <reminder id="8">Consider pagination for list operations</reminder>
  <reminder id="9">Map all error messages to appropriate HTTP status codes</reminder>
  <reminder id="10">Security model transitions from COMMAREA user context to OAuth/JWT</reminder>
</critical_reminders>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/modernization/api/progress.yaml if exists
    2. If progress exists:
       - Load api-scores.yaml, interface-analysis.yaml
       - Resume from next_action
    3. If starting fresh:
       - Begin with Phase 1: Candidate Scoring
       - Evaluate all CO* programs
       - Create progress.yaml
    4. Focus on top 5-6 candidates for detailed contract design
    5. Reference COCOM01Y.cpy for interface details
    6. Reference copybook record layouts for data structures
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to identify API candidates. The prompt will:

1. **Score** all programs for API suitability
2. **Analyze** interfaces of top candidates
3. **Design** OpenAPI specifications
4. **Generate** API candidate and contract documentation

## Expected Outputs

| File | Description |
|------|-------------|
| `API-CANDIDATES.md` | Ranked list of API candidates with scoring |
| `API-CONTRACTS.md` | OpenAPI specifications for top candidates |

## Dependencies

- RE-001 (Domain Model) - for entity understanding
- RE-002 (Data Model) - for data structure mapping
- RE-003 (Context Model) - for COMMAREA interface
- RE-010 (Modernization) - for complexity scores

## Dependent Prompts

- RE-000 (Master Index) - synthesis of all findings
