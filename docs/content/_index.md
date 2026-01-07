---
title: "Superwell"
description: "Physics-based hydro-economic model for estimating groundwater cost and supply globally"
---

<section id="overview">

# Superwell

**Physics-based hydro-economic model for estimating groundwater cost and supply globally**

Superwell provides estimates of global extractable volumes and unit-costs of accessible groundwater production under user-specified extraction scenarios. The model integrates hydrogeological and economic principles to assess groundwater resource sustainability and economic feasibility.

![Superwell Conceptual Framework](/concept.png)

## Key Capabilities

- **Spatially explicit** - Grid-based analysis from local to global scales
- **Temporally flexible** - Analysis periods from years to centuries  
- **Economics integrated** - Cost-supply relationships with extraction scenarios
- **Robust methodology** - Physics-based calculations using established hydrogeological principles

</section>

<section id="installation">

## Installation

### Requirements
- Python 3.7+
- NumPy, Pandas, SciPy

### Quick Start

```bash
git clone https://github.com/JGCRI/superwell.git
cd superwell/python
pip install -r requirements.txt
python superwell_deepening.py
```

### Configuration

The model accepts CSV input files with hydrogeological parameters:
- Aquifer properties (transmissivity, storativity)
- Well specifications (depth, radius)
- Economic parameters (discount rates, electricity costs)

<a href="/docs/" class="btn">View Documentation</a>

</section>

<section id="dashboard">

## Data Exploration

Interactive tools for exploring model outputs and input datasets.

<div class="dashboard-container">
  <div class="dashboard-grid">
    <div class="dashboard-panel">
      <h3>Global Cost Maps</h3>
      <p>Explore spatially explicit groundwater extraction costs across different scenarios and time periods.</p>
    </div>
    <div class="dashboard-panel">
      <h3>Supply Curves</h3>
      <p>Analyze cost-supply relationships for different regions and extraction scenarios.</p>
    </div>
    <div class="dashboard-panel">
      <h3>Scenario Comparison</h3>
      <p>Compare outcomes across different technological and economic assumptions.</p>
    </div>
  </div>
</div>

<a href="/dashboard/" class="btn">Launch Dashboard</a>

</section>

<section id="documentation">

## Documentation

### Model Description
Detailed technical documentation of the hydrogeological and economic methodologies implemented in Superwell.

### Input Specifications  
Complete reference for model input parameters, data formats, and configuration options.

### Python API
Function reference and usage examples for the Python implementation.

<a href="/docs/" class="btn">View Full Documentation</a>

</section>

<section id="publications">

## Publications & Research

### Primary Reference
Niazi, H., et al. (2025). Long-term hydro-economic analysis tool for evaluating global groundwater cost and supply: Superwell v1.1. *Geoscientific Model Development*, 18(5), 1737-1767.

### Related Studies
- Regional groundwater sustainability assessments
- Economic analysis of groundwater depletion
- Integrated water-energy modeling applications

<a href="/publications/" class="btn">View All Publications</a>

</section>
