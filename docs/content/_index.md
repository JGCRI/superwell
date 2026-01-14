---
title: "Superwell"
description: "Physics-based hydro-economic model for estimating groundwater cost and supply globally"
---

<section id="overview" class="section-content active">
  <div class="compact-header">
    <h1>Superwell <span class="version">v1.1</span></h1>
    <p class="tagline">Physics-based hydro-economic model for global groundwater cost and supply estimation</p>
  </div>
  
  <div class="info-cards">
    <div class="info-card">
      <i class="fas fa-globe"></i>
      <h3>Global Scale</h3>
      <p>72,548 grid cells worldwide</p>
    </div>
    <div class="info-card">
      <i class="fas fa-layer-group"></i>
      <h3>Multi-temporal</h3>
      <p>Years to centuries analysis</p>
    </div>
    <div class="info-card">
      <i class="fas fa-calculator"></i>
      <h3>Physics-Based</h3>
      <p>Hydrogeological principles</p>
    </div>
    <div class="info-card">
      <i class="fas fa-chart-line"></i>
      <h3>Economic Integration</h3>
      <p>Cost-supply relationships</p>
    </div>
  </div>
  
  <div class="concept-image-wrapper">
    <div class="concept-image">
      <img src="./concept.png" alt="Superwell Conceptual Framework" />
      <div class="concept-overlay">
        <div class="overlay-content">
          <h3>Hydro-Economic Framework</h3>
          <p>Superwell integrates hydrogeological physics with economic modeling to estimate groundwater extraction costs. The model combines aquifer properties, well hydraulics, and energy economics to provide spatially-explicit cost-supply relationships across global groundwater basins. Superwell calculates how much groundwater can be extracted and at what cost, using detailed maps and data of the Earth's properties. Through these estimates, and by using them with other models, Superwell facilitates exploration of coupled human–environmental system challenges, such as future water supply sustainability or multi-sectoral energy–water–land feedbacks.</p>
        </div>
      </div>
    </div>
  </div>
  
  <div class="cta-section">
    <div class="cta-card primary-cta" data-section="dashboard">
      <div class="cta-icon">
        <i class="fas fa-chart-area"></i>
      </div>
      <div class="cta-content">
        <h3>Explore Dashboard</h3>
        <p>Interactive visualizations and data analysis tools</p>
      </div>
      <div class="cta-arrow">
        <i class="fas fa-arrow-right"></i>
      </div>
    </div>
    <div class="cta-card" data-external="https://doi.org/10.5194/gmd-18-1737-2025">
      <div class="cta-icon">
        <i class="fas fa-book-open"></i>
      </div>
      <div class="cta-content">
        <h3>Read the Science</h3>
        <p>Published in Geoscientific Model Development & Nature Sustainability</p>
      </div>
      <div class="cta-arrow">
        <i class="fas fa-external-link-alt"></i>
      </div>
    </div>
    <div class="cta-card" data-section="documentation">
      <div class="cta-icon">
        <i class="fas fa-book"></i>
      </div>
      <div class="cta-content">
        <h3>View Documentation</h3>
        <p>Installation guides, API reference, and examples</p>
      </div>
      <div class="cta-arrow">
        <i class="fas fa-arrow-right"></i>
      </div>
    </div>
  </div>
</section>

<section id="dashboard" class="section-content">
  <div class="section-header">
    <h2><i class="fas fa-chart-line"></i> Interactive Dashboard</h2>
    <div class="header-actions">
      <button class="btn-sm" id="exportDashboard"><i class="fas fa-download"></i> Export</button>
      <button class="btn-sm" id="resetDashboard"><i class="fas fa-redo"></i> Reset</button>
    </div>
  </div>
  
  <!-- Key Metrics Bar -->
  <div class="metrics-bar">
    <div class="metric">
      <span class="metric-label">Total Scenarios</span>
      <span class="metric-value" id="totalScenarios">-</span>
    </div>
    <div class="metric">
      <span class="metric-label">Grid Cells</span>
      <span class="metric-value" id="totalCells">72,548</span>
    </div>
    <div class="metric">
      <span class="metric-label">Variables</span>
      <span class="metric-value" id="totalVars">-</span>
    </div>
    <div class="metric">
      <span class="metric-label">Time Range</span>
      <span class="metric-value" id="timeRange">-</span>
    </div>
  </div>
  
  <!-- Control Panel -->
  <div class="control-panel">
    <div class="control-group">
      <label>Variable</label>
      <select id="variableSelect" class="select-input">
        <option value="">Loading...</option>
      </select>
    </div>
    <div class="control-group">
      <label>Time Period</label>
      <select id="timeSelect" class="select-input">
        <option value="">Loading...</option>
      </select>
    </div>
    <div class="control-group">
      <label>Region Filter</label>
      <select id="regionSelect" class="select-input">
        <option value="all">All Regions</option>
      </select>
    </div>
    <div class="control-group">
      <label>Chart Type</label>
      <select id="chartType" class="select-input">
        <option value="map">Spatial Map</option>
        <option value="timeseries">Time Series</option>
        <option value="distribution">Distribution</option>
        <option value="scatter">Scatter Plot</option>
      </select>
    </div>
    <button class="btn-primary" id="updateViz"><i class="fas fa-sync"></i> Update Visualization</button>
  </div>
  
  <!-- Visualization Area -->
  <div class="viz-container">
    <div class="viz-main" id="mainVisualization">
      <div class="viz-placeholder">
        <i class="fas fa-chart-area fa-3x"></i>
        <p>Select variables and click "Update Visualization" to begin</p>
      </div>
    </div>
  </div>
</section>

<section id="data-explorer" class="section-content">
  <div class="section-header">
    <h2><i class="fas fa-database"></i> Data Explorer</h2>
    <div class="header-actions">
      <button class="btn-sm" id="loadSample"><i class="fas fa-file-csv"></i> Load Sample</button>
      <button class="btn-sm" id="loadFull"><i class="fas fa-database"></i> Load Full Dataset</button>
    </div>
  </div>
  
  <div class="data-tabs">
    <button class="tab-btn active" data-tab="inputs">Input Data</button>
    <button class="tab-btn" data-tab="outputs">Output Data</button>
    <button class="tab-btn" data-tab="stats">Statistics</button>
  </div>
  
  <div class="tab-content active" id="inputs-tab">
    <div class="table-controls">
      <input type="text" id="searchInput" placeholder="Search..." class="search-input">
      <select id="columnFilter" class="select-input-sm">
        <option value="all">All Columns</option>
      </select>
      <span class="record-count">Rows: <span id="rowCount">0</span></span>
    </div>
    <div class="table-container" id="inputTableContainer">
      <div class="loading-state">
        <i class="fas fa-spinner fa-spin"></i>
        <p>Click "Load Sample" or "Load Full Dataset" to view data</p>
      </div>
    </div>
  </div>
  
  <div class="tab-content" id="outputs-tab">
    <div class="table-controls">
      <input type="text" id="searchOutput" placeholder="Search..." class="search-input">
      <select id="outputColumnFilter" class="select-input-sm">
        <option value="all">All Columns</option>
      </select>
      <span class="record-count">Rows: <span id="outputRowCount">0</span></span>
    </div>
    <div class="table-container" id="outputTableContainer">
      <div class="loading-state">
        <i class="fas fa-spinner fa-spin"></i>
        <p>Click "Load Sample" or "Load Full Dataset" to view data</p>
      </div>
    </div>
  </div>
  
  <div class="tab-content" id="stats-tab">
    <div class="stats-grid" id="statsContainer">
      <div class="loading-state">
        <p>Load data to see statistics</p>
      </div>
    </div>
  </div>
</section>

<section id="visualization" class="section-content">
  <div class="section-header">
    <h2><i class="fas fa-map"></i> Advanced Visualization</h2>
  </div>
  
  <div class="viz-layout">
    <div class="viz-sidebar">
      <h3>Configuration</h3>
      <div class="viz-config-group">
        <label>X-Axis Variable</label>
        <select id="xAxisVar" class="select-input"></select>
      </div>
      <div class="viz-config-group">
        <label>Y-Axis Variable</label>
        <select id="yAxisVar" class="select-input"></select>
      </div>
      <div class="viz-config-group">
        <label>Color By</label>
        <select id="colorVar" class="select-input">
          <option value="none">None</option>
        </select>
      </div>
      <div class="viz-config-group">
        <label>Size By</label>
        <select id="sizeVar" class="select-input">
          <option value="none">None</option>
        </select>
      </div>
      <div class="viz-config-group">
        <label class="checkbox-label">
          <input type="checkbox" id="showGrid"> Show Grid
        </label>
        <label class="checkbox-label">
          <input type="checkbox" id="logScale"> Log Scale
        </label>
      </div>
      <button class="btn-primary full-width" id="generateViz">
        <i class="fas fa-chart-bar"></i> Generate
      </button>
    </div>
    <div class="viz-main-area">
      <div id="customVisualization" class="custom-viz">
        <div class="viz-placeholder">
          <i class="fas fa-map-marked-alt fa-3x"></i>
          <p>Configure and generate custom visualizations</p>
        </div>
      </div>
    </div>
  </div>
</section>

<section id="documentation" class="section-content">
  <div class="section-header">
    <h2><i class="fas fa-book"></i> Documentation</h2>
  </div>
  
  <div class="doc-grid">
    <div class="doc-card">
      <div class="doc-icon"><i class="fas fa-rocket"></i></div>
      <h3>Quick Start</h3>
      <p>Get up and running with Superwell in minutes</p>
      <div class="code-block">
        <div class="code-line">git clone https://github.com/JGCRI/superwell.git</div>
        <div class="code-line">cd superwell/python</div>
        <div class="code-line">pip install -r requirements.txt</div>
        <div class="code-line">python superwell_deepening.py</div>
      </div>
    </div>
    <div class="doc-card">
      <div class="doc-icon"><i class="fas fa-cog"></i></div>
      <h3>Model Parameters</h3>
      <div class="param-list">
        <div class="param-item">
          <strong>Transmissivity (T)</strong>
          <span>Aquifer hydraulic conductivity</span>
        </div>
        <div class="param-item">
          <strong>Storativity (S)</strong>
          <span>Water storage coefficient</span>
        </div>
        <div class="param-item">
          <strong>Well Depth</strong>
          <span>Extraction depth and radius</span>
        </div>
        <div class="param-item">
          <strong>Recharge Rate</strong>
          <span>Natural aquifer replenishment</span>
        </div>
        <div class="param-item">
          <strong>Economic Factors</strong>
          <span>Discount rate, electricity cost</span>
        </div>
      </div>
    </div>
    <div class="doc-card">
      <div class="doc-icon"><i class="fas fa-file-csv"></i></div>
      <h3>Data Format</h3>
      <p>Required CSV columns for model inputs:</p>
      <div class="data-format-list">
        <div class="format-item"><code>lat, lon</code> <span>Geographic coordinates</span></div>
        <div class="format-item"><code>transmissivity, storativity</code> <span>Aquifer properties</span></div>
        <div class="format-item"><code>recharge_rate, depth_to_water</code> <span>Water levels</span></div>
        <div class="format-item"><code>electricity_rate, discount_rate</code> <span>Economic factors</span></div>
      </div>
    </div>
    <div class="doc-card citation-card">
      <div class="doc-icon"><i class="fas fa-graduation-cap"></i></div>
      <h3>Citation</h3>
      <div class="citation-text">
        <p>Niazi, H., Ferencz, S. B., Graham, N. T., Yoon, J., Wild, T. B., Hejazi, M., Watson, D. J., & Vernon, C. R. (2025).</p>
        <p><em>Long-term hydro-economic analysis tool for evaluating global groundwater cost and supply: Superwell v1.1.</em></p>
        <p>Geoscientific Model Development, 18(5), 1737-1767.</p>
      </div>
      <a href="https://doi.org/10.5194/gmd-18-1737-2025" target="_blank" class="doc-link">
        <i class="fas fa-external-link-alt"></i> View Publication
      </a>
    </div>
  </div>
  
  <!-- Terminal Bar for Documentation -->
  <div class="doc-terminal">
    <div class="terminal-minimal">
      <span class="terminal-label">Get Superwell</span>
      <span class="terminal-prompt">$</span>
      <span class="terminal-cmd">git clone https://github.com/JGCRI/superwell.git</span>
      <button class="terminal-copy-btn" onclick="navigator.clipboard.writeText('git clone https://github.com/JGCRI/superwell.git')" title="Copy command">
        <i class="fas fa-copy"></i>
      </button>
    </div>
  </div>
</section>
