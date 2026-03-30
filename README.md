# AI Productivity Complementarity  
**AI as a Capability-Contingent Amplifier of Knowledge Worker Performance**

## Overview

This repository contains materials for an ongoing research program examining how generative AI use relates to knowledge worker performance.

A central puzzle in the emerging literature on AI and work is that its productivity effects are highly uneven: AI substantially improves performance in some contexts while producing limited or even negative effects in others.

This project develops and tests a **complementarity-based explanation** of this heterogeneity.

---

## Core Idea

Rather than treating AI as a uniform productivity enhancer, this research conceptualizes AI as a **capability-contingent amplifier**:

> The performance effects of AI depend on the alignment between  
> (1) AI usage,  
> (2) worker capability, and  
> (3) task structure.

Under this perspective, AI does not independently improve performance. Instead, its returns emerge from complementarities between human capability and task conditions.

---

## Current Study: Pilot

The `pilot-study/` directory contains a preregistered cross-sectional survey of knowledge workers:

- Total sample: N = 102  
- Formal evaluation subsample: N = 69  

### Key Features

- **Performance outcomes**
  - Formal evaluation outcome (ordinal)
  - Self-reported relative performance  

- **Core constructs**
  - AI usage intensity  
  - Digital capability  
  - Task standardization  

- **Methods**
  - Ordinal logistic regression  
  - Interaction models  
  - Predicted probability analysis  

---

## Main Findings

- AI usage is **not associated with a uniform main effect** on performance  
- Instead, performance effects vary systematically across:
  - worker capability  
  - task structure  

- The marginal returns to AI usage are:
  - higher for workers with greater digital capability  
  - more stable under structured task conditions  

These results provide initial evidence that AI functions as a **capability-contingent amplifier**, producing heterogeneous performance effects.

---

## Theoretical Contribution

This project contributes to research on AI and work by:

- Moving beyond average treatment effects to explain **heterogeneity in AI productivity**
- Integrating:
  - human capital (capability)  
  - task design (structure)  
  - technology use (AI)  

- Developing a **complementarity framework** for human–AI interaction  

---

## Repository Structure

- `pilot-study/` — Original pilot data, scripts, and analysis  
- `paper/` — Working paper and manuscript materials  
- `docs/` — Supplementary documentation  

---

## Working Paper

📄 The current working paper is available here:  
`paper/paper.pdf`

---

## Reproducibility

This repository is structured to support reproducible research, including:

- transparent data preprocessing  
- clearly defined model specifications  
- reproducible tables and figures  

---

## Ongoing Work

This project is being extended beyond survey-based analysis toward **behavioral trace data**.

Specifically, I am developing an LLM-based workflow system that:

- captures real-time AI usage behavior  
- logs task-level decisions (e.g., reply, archive, ignore)  
- enables measurement of productivity in naturalistic work settings  

This system is designed to generate richer empirical data for testing complementarity-based theories of AI and performance.

---

## Status

This repository contains pilot-stage results and is actively being extended into a full research pipeline combining:

- theory development  
- empirical modeling  
- system-based data collection  

---

## Data and Code Availability

Due to ongoing research and planned submission for academic publication, the full dataset and analysis code are not publicly released at this time.

A reproducible version of the analysis will be made available upon publication.

---

## Author

Masaki Higashida  
University of California, Berkeley