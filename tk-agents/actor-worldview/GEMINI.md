# GEMINI - Actor Worldview Project Guide

This directory contains the **Actor Worldview Framework**, a design-thinking methodology that positions the Actor Model as a tool for system design rather than a strict implementation dogma. It is part of the `tk-agents` project.

## Project Overview

The Actor Worldview framework provides a structured approach to thinking about, designing, and optimizing complex agentic systems. It emphasizes a **Design → Fitness → Optimize → Validate** workflow.

- **Core Philosophy:** Model systems as actors for clear thinking, then optimize pragmatically to meet performance and resource goals (the "fitness function").
- **Key Concepts:** Graph-based addressing, location transparency, virtual actors, supervision trees, and effect actors (for external system boundaries).

## Directory Structure

The repository is organized into functional categories to support different levels of engagement:

- **`WORLDVIEW.md`**: The authoritative source of truth for the framework's philosophy and its seven core principles.
- **`INDEX.md`**: A comprehensive navigation guide and reading path index.
- **`guides/`**: Practical how-to documentation (e.g., `IMPLEMENTATION_GUIDE.md`, `ADDRESSING_CONVENTIONS.md`).
- **`architectures/`**: Detailed system designs (e.g., `PURE_ACTOR_MODEL.md`, `CLI_ACTOR_ARCHITECTURE.md`).
- **`patterns/`**: A catalog of reusable design patterns (e.g., `ENTANGLED_ACTORS.md`, `PATTERN_CATALOG.md`).
- **`migration/`**: Strategies and reports for transitioning existing code to the actor model.
- **`topics/`**: Specialized research, including actor compilation and optimization techniques.
- **`archive/`**: Historical versions and research papers preserved for context.

## Core Mandates for Agents

When working within this directory or applying this worldview to code:

1.  **Design First:** Always start by modeling entities as actors and defining their message protocols. Refer to `WORLDVIEW.md` for the seven key principles.
2.  **Fitness-Driven Optimization:** Do not over-engineer for "actor purity." Optimization (like actor fusion or selective bypasses) should be driven by a defined fitness function (latency, throughput, etc.).
3.  **Consistency:** Adhere to the naming and addressing conventions defined in `guides/ADDRESSING_CONVENTIONS.md`.
4.  **Documentation:** New designs or patterns should be documented following the existing structure (Design → Fitness → Optimize → Validate).

## Recommended Reading Paths

- **Quick Start (30 min):** Read `README.md` → `WORLDVIEW.md` (Executive Summary & Principles) → `Application to Primer`.
- **Implementation (2 hours):** Read `guides/IMPLEMENTATION_GUIDE.md` → `guides/ADDRESSING_CONVENTIONS.md` → `patterns/`.
- **Deep Dive (4-6 hours):** Full review of `WORLDVIEW.md`, `architectures/`, and `topics/COMPILATION_RESEARCH.md`.

## Key Commands/Usage

This is a non-code project focused on documentation and design. 
- Use `INDEX.md` to find specific documentation.
- Use the **Design → Fitness → Optimize → Validate** framework when proposing new architectural changes.
