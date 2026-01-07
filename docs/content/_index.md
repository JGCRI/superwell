---
title: "Superwell"
description: "Physics-based hydro-economic model for estimating groundwater cost and supply globally"
---

<section id="overview" class="section-content">
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
</section>

<section id="dashboard" class="section-content active">
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
      <pre><code>git clone https://github.com/JGCRI/superwell.git
cd superwell/python
pip install -r requirements.txt
python superwell_deepening.py</code></pre>
    </div>
    
    <div class="doc-card">
      <div class="doc-icon"><i class="fas fa-cog"></i></div>
      <h3>Model Parameters</h3>
      <ul class="doc-list">
        <li><strong>Transmissivity (T)</strong> - Aquifer hydraulic conductivity</li>
        <li><strong>Storativity (S)</strong> - Water storage coefficient</li>
        <li><strong>Well Depth</strong> - Extraction depth and radius</li>
        <li><strong>Recharge Rate</strong> - Natural aquifer replenishment</li>
        <li><strong>Economic Factors</strong> - Discount rate, electricity cost</li>
      </ul>
    </div>
    
    <div class="doc-card">
      <div class="doc-icon"><i class="fas fa-file-csv"></i></div>
      <h3>Data Format</h3>
      <p>Required CSV columns for model inputs:</p>
      <ul class="doc-list">
        <li>lat, lon - Geographic coordinates</li>
        <li>transmissivity, storativity</li>
        <li>recharge_rate, depth_to_water</li>
        <li>electricity_rate, discount_rate</li>
      </ul>
    </div>
    
    <div class="doc-card">
      <div class="doc-icon"><i class="fas fa-graduation-cap"></i></div>
      <h3>Citation</h3>
      <p>Niazi, H., et al. (2025). <em>Long-term hydro-economic analysis tool for evaluating global groundwater cost and supply: Superwell v1.1.</em> Geoscientific Model Development, 18(5), 1737-1767.</p>
      <a href="https://doi.org/10.5194/gmd-18-1737-2025" class="doc-link">
        <i class="fas fa-external-link-alt"></i> View Publication
      </a>
    </div>
  </div>
</section>
