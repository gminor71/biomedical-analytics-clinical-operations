# Template Column Definitions

## Query Log

| Column            | Meaning                                                                   |
| ----------------- | ------------------------------------------------------------------------- |
| **query_id**      | Unique query identifier (e.g., Q001, Q002).                               |
| **subject_id**    | Subject associated with the query; may be blank if site-level.            |
| **site_id**       | Site where the issue originated.                                          |
| **domain**        | Data category affected (e.g., Visits, Labs, AE, Dose, Imaging).           |
| **issue_type**    | Type of issue (Missing, Inconsistent, Clarification, Out-of-range, etc.). |
| **description**   | Brief explanation of the data concern or requested clarification.         |
| **opened_date**   | Date the query was created.                                               |
| **resolved_date** | Date the site or team resolved the query.                                 |
| **status**        | Current state (Open, Answered, Closed).                                   |
| **aging_days**    | Number of days the query has been open; used for monitoring trends.       |
| **notes**         | Optional comments or context (follow-up details, CRA notes, etc.).        |


## Deviation Log

| Column         | Meaning                                       |
| -------------- | --------------------------------------------- |
| deviation_id   | Unique ID (D001, D002…)                       |
| subject_id     | Related patient                               |
| site_id        | Related site                                  |
| deviation_date | When it occurred                              |
| deviation_type | Visit Window / Eligibility / Safety / Imaging |
| description    | Short explanation                             |
| severity       | Minor / Major / Critical                      |
| detected_by    | CRA / Site / Data Review                      |
| action_taken   | Training / Query / CAPA                       |
| status         | Open / Resolved                               |
| notes          | Additional context                            |


## Action Item Tracker

| Column          | Meaning                                 |
| --------------- | --------------------------------------- |
| action_id       | Unique identifier (A001, A002…)         |
| site_id         | Related site                            |
| source          | MVR / Risk Assessment / Sponsor Request |
| description     | What must be done                       |
| owner           | Site staff or CRA responsible           |
| due_date        | Expected completion                     |
| status          | Open / In Progress / Completed          |
| priority        | Low / Medium / High                     |
| completion_date | Date finished                           |
| notes           | Optional context                        |
