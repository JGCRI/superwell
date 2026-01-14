// Superwell Dashboard - Interactive Features

// Global State
const state = {
  currentSection: 'overview',
  sidebarCollapsed: false,
  currentData: null,
  inputData: null,
  outputData: null,
  selectedVariable: null,
  selectedTime: null,
  chartType: 'map'
};

// Initialize on DOM load
document.addEventListener('DOMContentLoaded', function() {
  initializeDashboard();
  setupEventListeners();
  loadInitialData();
});

// Initialize Dashboard
function initializeDashboard() {
  // Set initial sidebar state
  const sidebar = document.getElementById('sidebar');
  if (sidebar) {
    sidebar.classList.add('collapsed');
    state.sidebarCollapsed = true;
  }
  
  // Check URL hash and show appropriate section
  const hash = window.location.hash.substring(1); // Remove # from hash
  const initialSection = hash || 'overview';
  showSection(initialSection);
  
  // Initialize navigation
  updateActiveNav();
  
  // Handle browser back/forward buttons
  window.addEventListener('hashchange', handleHashChange);
}

// Setup Event Listeners
function setupEventListeners() {
  // Sidebar toggle from top bar
  const toggleBtn = document.getElementById('toggleSidebar');
  if (toggleBtn) {
    toggleBtn.addEventListener('click', toggleSidebar);
  }
  
  // Sidebar toggle from sidebar button
  const sidebarToggleBtn = document.getElementById('sidebarToggleBtn');
  if (sidebarToggleBtn) {
    sidebarToggleBtn.addEventListener('click', toggleSidebar);
  }
  
  // Refresh data
  const refreshBtn = document.getElementById('refreshData');
  if (refreshBtn) {
    refreshBtn.addEventListener('click', refreshData);
  }
  
  // Download data
  const downloadBtn = document.getElementById('downloadData');
  if (downloadBtn) {
    downloadBtn.addEventListener('click', downloadData);
  }
  
  // Terminal bar copy command
  const copyCommand = document.getElementById('copyCommand');
  if (copyCommand) {
    copyCommand.addEventListener('click', () => {
      const command = document.getElementById('cloneCommand').textContent;
      navigator.clipboard.writeText(command).then(() => {
        const icon = copyCommand.querySelector('i');
        const originalClass = icon.className;
        icon.className = 'fas fa-check';
        setTimeout(() => {
          icon.className = originalClass;
        }, 2000);
      });
    });
  }
  
  // CTA cards navigation
  const ctaCards = document.querySelectorAll('.cta-card[data-section]');
  ctaCards.forEach(card => {
    card.addEventListener('click', function() {
      const section = this.getAttribute('data-section');
      if (section) {
        showSection(section);
        const navLink = document.querySelector(`.nav-link[data-section="${section}"]`);
        if (navLink) {
          updateActiveNav(navLink);
        }
        // Update URL hash
        window.history.pushState(null, null, `#${section}`);
      }
    });
  });
  
  // Navigation links (sidebar)
  const navLinks = document.querySelectorAll('.nav-link');
  navLinks.forEach(link => {
    link.addEventListener('click', handleNavigation);
  });
  
  // Top navigation links
  const topNavLinks = document.querySelectorAll('.top-nav-link');
  topNavLinks.forEach(link => {
    link.addEventListener('click', handleNavigation);
  });
    
  // CTA cards with external links
  const externalCtaCards = document.querySelectorAll('.cta-card[data-external]');
  externalCtaCards.forEach(card => {
    card.addEventListener('click', function() {
      const url = this.getAttribute('data-external');
      if (url) {
        window.open(url, '_blank');
      }
    });
  });
  
  // Dashboard controls
  const updateVizBtn = document.getElementById('updateViz');
  if (updateVizBtn) {
    updateVizBtn.addEventListener('click', updateVisualization);
  }
  
  // Data explorer buttons
  const loadSampleBtn = document.getElementById('loadSample');
  if (loadSampleBtn) {
    loadSampleBtn.addEventListener('click', () => loadData('sample'));
  }
  
  const loadFullBtn = document.getElementById('loadFull');
  if (loadFullBtn) {
    loadFullBtn.addEventListener('click', () => loadData('full'));
  }
  
  // Tab buttons
  const tabBtns = document.querySelectorAll('.tab-btn');
  tabBtns.forEach(btn => {
    btn.addEventListener('click', handleTabSwitch);
  });
  
  // Advanced visualization
  const generateVizBtn = document.getElementById('generateViz');
  if (generateVizBtn) {
    generateVizBtn.addEventListener('click', generateCustomVisualization);
  }
  
  // Search inputs
  const searchInput = document.getElementById('searchInput');
  if (searchInput) {
    searchInput.addEventListener('input', debounce(filterInputTable, 300));
  }
  
  const searchOutput = document.getElementById('searchOutput');
  if (searchOutput) {
    searchOutput.addEventListener('input', debounce(filterOutputTable, 300));
  }
}

// Toggle Sidebar
function toggleSidebar() {
  const sidebar = document.getElementById('sidebar');
  if (sidebar) {
    sidebar.classList.toggle('collapsed');
    state.sidebarCollapsed = !state.sidebarCollapsed;
  }
}

// Handle Navigation
function handleNavigation(e) {
  e.preventDefault();
  const section = this.getAttribute('data-section');
  if (section) {
    showSection(section);
    updateActiveNav(this);
    // Update URL hash
    window.history.pushState(null, null, `#${section}`);
  }
}

// Handle Hash Change (browser back/forward)
function handleHashChange() {
  const hash = window.location.hash.substring(1);
  const section = hash || 'overview';
  showSection(section);
  // Find any nav link with this section
  const anyNavLink = document.querySelector(`[data-section="${section}"]`);
  if (anyNavLink) {
    updateActiveNav(anyNavLink);
  }
}

// Show Section
function showSection(sectionId) {
  // Hide all sections
  const sections = document.querySelectorAll('.section-content');
  sections.forEach(section => section.classList.remove('active'));
  
  // Show selected section
  const activeSection = document.getElementById(sectionId);
  if (activeSection) {
    activeSection.classList.add('active');
    state.currentSection = sectionId;
  }
  
  // Show/hide terminal bar - only show on overview page
  const terminalBar = document.getElementById('terminalBar');
  if (terminalBar) {
    if (sectionId === 'overview') {
      terminalBar.classList.remove('hidden');
    } else {
      terminalBar.classList.add('hidden');
    }
  }
}

// Update Active Navigation
function updateActiveNav(activeLink) {
  // Remove active class from all nav links (sidebar and top)
  const navLinks = document.querySelectorAll('.nav-link');
  const topNavLinks = document.querySelectorAll('.top-nav-link');
  navLinks.forEach(link => link.classList.remove('active'));
  topNavLinks.forEach(link => link.classList.remove('active'));
  
  if (activeLink) {
    activeLink.classList.add('active');
    // Also activate the corresponding link in the other nav
    const section = activeLink.getAttribute('data-section');
    if (section) {
      const otherLinks = activeLink.classList.contains('nav-link') 
        ? document.querySelectorAll(`.top-nav-link[data-section="${section}"]`)
        : document.querySelectorAll(`.nav-link[data-section="${section}"]`);
      otherLinks.forEach(link => link.classList.add('active'));
    }
  } else {
    // Set default active based on current section
    const currentSection = state.currentSection || 'overview';
    const defaultLinks = document.querySelectorAll(`[data-section="${currentSection}"]`);
    defaultLinks.forEach(link => link.classList.add('active'));
  }
}

// Load Initial Data
async function loadInitialData() {
  try {
    // Simulate loading data - in real implementation, fetch from CSV files
    const response = await fetch('/outputs/superwell_py_deep_C_all_B_all_G_72548_0.3PD_0.25DL_0.2RR.csv');
    const text = await response.text();
    
    // Parse CSV
    const data = parseCSV(text);
    state.currentData = data;
    
    // Update UI with data info
    updateMetrics(data);
    populateControls(data);
    
  } catch (error) {
    console.error('Error loading data:', error);
    showNotification('Unable to load data. Using demo mode.', 'warning');
    loadDemoData();
  }
}

// Parse CSV
function parseCSV(text) {
  const lines = text.trim().split('\n');
  const headers = lines[0].split(',').map(h => h.trim());
  
  const data = [];
  for (let i = 1; i < Math.min(lines.length, 1001); i++) { // Limit to first 1000 rows
    const values = lines[i].split(',');
    const row = {};
    headers.forEach((header, index) => {
      row[header] = values[index] ? values[index].trim() : '';
    });
    data.push(row);
  }
  
  return { headers, rows: data };
}

// Load Demo Data
function loadDemoData() {
  const demoHeaders = ['basin_id', 'region', 'lat', 'lon', 'year_number', 'cost_usd_m3', 
                       'extraction_m3', 'depth_m', 'transmissivity', 'storativity'];
  const demoRows = [];
  
  for (let i = 0; i < 100; i++) {
    demoRows.push({
      'basin_id': `B${i}`,
      'region': ['Asia', 'Africa', 'Americas', 'Europe'][i % 4],
      'lat': (Math.random() * 180 - 90).toFixed(4),
      'lon': (Math.random() * 360 - 180).toFixed(4),
      'year_number': Math.floor(Math.random() * 50),
      'cost_usd_m3': (Math.random() * 2).toFixed(3),
      'extraction_m3': (Math.random() * 1e6).toFixed(0),
      'depth_m': (Math.random() * 300 + 50).toFixed(1),
      'transmissivity': (Math.random() * 1000).toFixed(2),
      'storativity': (Math.random() * 0.3).toFixed(4)
    });
  }
  
  state.currentData = { headers: demoHeaders, rows: demoRows };
  updateMetrics(state.currentData);
  populateControls(state.currentData);
}

// Update Metrics
function updateMetrics(data) {
  if (!data || !data.rows) return;
  
  const totalCells = document.getElementById('totalCells');
  const totalVars = document.getElementById('totalVars');
  const timeRange = document.getElementById('timeRange');
  
  if (totalVars) {
    totalVars.textContent = data.headers.length;
  }
  
  if (data.rows.length > 0 && data.rows[0].year_number !== undefined) {
    const years = data.rows.map(r => parseFloat(r.year_number)).filter(y => !isNaN(y));
    if (years.length > 0 && timeRange) {
      const minYear = Math.min(...years);
      const maxYear = Math.max(...years);
      timeRange.textContent = `${minYear}-${maxYear}`;
    }
  }
}

// Populate Controls
function populateControls(data) {
  if (!data || !data.headers) return;
  
  // Variable select
  const variableSelect = document.getElementById('variableSelect');
  if (variableSelect) {
    variableSelect.innerHTML = data.headers
      .filter(h => !['basin_id', 'lat', 'lon', 'region'].includes(h))
      .map(h => `<option value="${h}">${formatHeader(h)}</option>`)
      .join('');
  }
  
  // Time select
  const timeSelect = document.getElementById('timeSelect');
  if (timeSelect && data.rows.length > 0 && data.rows[0].year_number !== undefined) {
    const years = [...new Set(data.rows.map(r => r.year_number))].sort();
    timeSelect.innerHTML = years.map(y => `<option value="${y}">Year ${y}</option>`).join('');
  }
  
  // Region select
  const regionSelect = document.getElementById('regionSelect');
  if (regionSelect && data.rows.length > 0 && data.rows[0].region !== undefined) {
    const regions = [...new Set(data.rows.map(r => r.region))].sort();
    regionSelect.innerHTML = '<option value="all">All Regions</option>' + 
      regions.map(r => `<option value="${r}">${r}</option>`).join('');
  }
  
  // Populate visualization axis selects
  const xAxisVar = document.getElementById('xAxisVar');
  const yAxisVar = document.getElementById('yAxisVar');
  const colorVar = document.getElementById('colorVar');
  const sizeVar = document.getElementById('sizeVar');
  
  const numericHeaders = data.headers.filter(h => {
    return !['basin_id', 'region'].includes(h) && 
           data.rows.length > 0 && 
           !isNaN(parseFloat(data.rows[0][h]));
  });
  
  const optionsHTML = numericHeaders.map(h => `<option value="${h}">${formatHeader(h)}</option>`).join('');
  
  if (xAxisVar) xAxisVar.innerHTML = optionsHTML;
  if (yAxisVar) yAxisVar.innerHTML = optionsHTML;
  if (colorVar) colorVar.innerHTML = '<option value="none">None</option>' + optionsHTML;
  if (sizeVar) sizeVar.innerHTML = '<option value="none">None</option>' + optionsHTML;
}

// Format Header
function formatHeader(header) {
  return header.split('_').map(word => 
    word.charAt(0).toUpperCase() + word.slice(1)
  ).join(' ');
}

// Update Visualization
function updateVisualization() {
  const variable = document.getElementById('variableSelect')?.value;
  const time = document.getElementById('timeSelect')?.value;
  const region = document.getElementById('regionSelect')?.value;
  const chartType = document.getElementById('chartType')?.value;
  
  if (!variable) {
    showNotification('Please select a variable', 'warning');
    return;
  }
  
  state.selectedVariable = variable;
  state.selectedTime = time;
  state.chartType = chartType;
  
  const vizContainer = document.getElementById('mainVisualization');
  if (!vizContainer) return;
  
  // Filter data
  let filteredData = state.currentData.rows;
  if (time) {
    filteredData = filteredData.filter(row => row.year_number == time);
  }
  if (region && region !== 'all') {
    filteredData = filteredData.filter(row => row.region === region);
  }
  
  // Create visualization based on type
  switch(chartType) {
    case 'map':
      createMapVisualization(vizContainer, filteredData, variable);
      break;
    case 'timeseries':
      createTimeSeriesVisualization(vizContainer, filteredData, variable);
      break;
    case 'distribution':
      createDistributionVisualization(vizContainer, filteredData, variable);
      break;
    case 'scatter':
      createScatterVisualization(vizContainer, filteredData, variable);
      break;
  }
}

// Create Map Visualization
function createMapVisualization(container, data, variable) {
  if (!data || data.length === 0) {
    container.innerHTML = '<div class="viz-placeholder"><p>No data available for selected filters</p></div>';
    return;
  }
  
  const lats = data.map(d => parseFloat(d.lat)).filter(v => !isNaN(v));
  const lons = data.map(d => parseFloat(d.lon)).filter(v => !isNaN(v));
  const values = data.map(d => parseFloat(d[variable])).filter(v => !isNaN(v));
  
  const trace = {
    type: 'scattergeo',
    mode: 'markers',
    lon: lons,
    lat: lats,
    marker: {
      size: 6,
      color: values,
      colorscale: 'Viridis',
      showscale: true,
      colorbar: {
        title: formatHeader(variable)
      }
    },
    text: values.map((v, i) => `${formatHeader(variable)}: ${v.toFixed(2)}`),
    hovertemplate: 'Lat: %{lat}<br>Lon: %{lon}<br>%{text}<extra></extra>'
  };
  
  const layout = {
    title: `${formatHeader(variable)} - Global Distribution`,
    geo: {
      projection: { type: 'natural earth' },
      showland: true,
      landcolor: 'rgb(243, 243, 243)',
      coastlinecolor: 'rgb(204, 204, 204)',
      showlakes: true,
      lakecolor: 'rgb(255, 255, 255)',
    },
    margin: { t: 40, b: 20, l: 20, r: 20 },
    height: 500
  };
  
  Plotly.newPlot(container, [trace], layout, {responsive: true});
}

// Create Time Series Visualization
function createTimeSeriesVisualization(container, data, variable) {
  if (!data || data.length === 0 || !data[0].year_number) {
    container.innerHTML = '<div class="viz-placeholder"><p>Time series data not available</p></div>';
    return;
  }
  
  // Group by region if available
  const grouped = {};
  data.forEach(row => {
    const region = row.region || 'Global';
    if (!grouped[region]) grouped[region] = [];
    grouped[region].push(row);
  });
  
  const traces = Object.keys(grouped).map(region => {
    const regionData = grouped[region].sort((a, b) => a.year_number - b.year_number);
    return {
      x: regionData.map(d => d.year_number),
      y: regionData.map(d => parseFloat(d[variable])),
      mode: 'lines+markers',
      name: region,
      type: 'scatter'
    };
  });
  
  const layout = {
    title: `${formatHeader(variable)} Over Time`,
    xaxis: { title: 'Year' },
    yaxis: { title: formatHeader(variable) },
    margin: { t: 40, b: 60, l: 60, r: 20 },
    height: 500,
    showlegend: true
  };
  
  Plotly.newPlot(container, traces, layout, {responsive: true});
}

// Create Distribution Visualization
function createDistributionVisualization(container, data, variable) {
  if (!data || data.length === 0) {
    container.innerHTML = '<div class="viz-placeholder"><p>No data available</p></div>';
    return;
  }
  
  const values = data.map(d => parseFloat(d[variable])).filter(v => !isNaN(v));
  
  const trace = {
    x: values,
    type: 'histogram',
    marker: { color: 'rgba(3, 105, 161, 0.7)' },
    nbinsx: 30
  };
  
  const layout = {
    title: `Distribution of ${formatHeader(variable)}`,
    xaxis: { title: formatHeader(variable) },
    yaxis: { title: 'Frequency' },
    margin: { t: 40, b: 60, l: 60, r: 20 },
    height: 500
  };
  
  Plotly.newPlot(container, [trace], layout, {responsive: true});
}

// Create Scatter Visualization
function createScatterVisualization(container, data, variable) {
  if (!data || data.length === 0) {
    container.innerHTML = '<div class="viz-placeholder"><p>No data available</p></div>';
    return;
  }
  
  // Use depth_m as x-axis if available, otherwise use first numeric column
  const xVar = data[0].depth_m !== undefined ? 'depth_m' : Object.keys(data[0])[0];
  
  const trace = {
    x: data.map(d => parseFloat(d[xVar])).filter(v => !isNaN(v)),
    y: data.map(d => parseFloat(d[variable])).filter(v => !isNaN(v)),
    mode: 'markers',
    type: 'scatter',
    marker: {
      size: 8,
      color: 'rgba(3, 105, 161, 0.7)',
      line: { color: 'white', width: 0.5 }
    }
  };
  
  const layout = {
    title: `${formatHeader(variable)} vs ${formatHeader(xVar)}`,
    xaxis: { title: formatHeader(xVar) },
    yaxis: { title: formatHeader(variable) },
    margin: { t: 40, b: 60, l: 60, r: 20 },
    height: 500
  };
  
  Plotly.newPlot(container, [trace], layout, {responsive: true});
}

// Generate Custom Visualization
function generateCustomVisualization() {
  const xVar = document.getElementById('xAxisVar')?.value;
  const yVar = document.getElementById('yAxisVar')?.value;
  const colorVar = document.getElementById('colorVar')?.value;
  const sizeVar = document.getElementById('sizeVar')?.value;
  const showGrid = document.getElementById('showGrid')?.checked;
  const logScale = document.getElementById('logScale')?.checked;
  
  if (!xVar || !yVar || !state.currentData) {
    showNotification('Please select variables and load data', 'warning');
    return;
  }
  
  const container = document.getElementById('customVisualization');
  const data = state.currentData.rows;
  
  const trace = {
    x: data.map(d => parseFloat(d[xVar])).filter(v => !isNaN(v)),
    y: data.map(d => parseFloat(d[yVar])).filter(v => !isNaN(v)),
    mode: 'markers',
    type: 'scatter',
    marker: {
      size: sizeVar !== 'none' ? 
        data.map(d => Math.max(5, Math.min(20, parseFloat(d[sizeVar]) / 10))) : 
        8,
      color: colorVar !== 'none' ? 
        data.map(d => parseFloat(d[colorVar])) : 
        'rgba(3, 105, 161, 0.7)',
      colorscale: colorVar !== 'none' ? 'Viridis' : undefined,
      showscale: colorVar !== 'none',
      line: { color: 'white', width: 0.5 }
    }
  };
  
  const layout = {
    title: `${formatHeader(yVar)} vs ${formatHeader(xVar)}`,
    xaxis: { 
      title: formatHeader(xVar),
      type: logScale ? 'log' : 'linear',
      showgrid: showGrid
    },
    yaxis: { 
      title: formatHeader(yVar),
      type: logScale ? 'log' : 'linear',
      showgrid: showGrid
    },
    margin: { t: 40, b: 60, l: 60, r: 20 },
    height: 560
  };
  
  if (colorVar !== 'none') {
    trace.marker.colorbar = { title: formatHeader(colorVar) };
  }
  
  Plotly.newPlot(container, [trace], layout, {responsive: true});
  showNotification('Visualization generated successfully', 'success');
}

// Load Data (Sample or Full)
async function loadData(type) {
  const inputContainer = document.getElementById('inputTableContainer');
  const outputContainer = document.getElementById('outputTableContainer');
  
  if (inputContainer) {
    inputContainer.innerHTML = '<div class="loading-state"><i class="fas fa-spinner fa-spin"></i><p>Loading data...</p></div>';
  }
  
  try {
    let data;
    if (type === 'sample') {
      // Load sample data (first 100 rows)
      data = {
        headers: state.currentData.headers,
        rows: state.currentData.rows.slice(0, 100)
      };
    } else {
      // Use current loaded data
      data = state.currentData;
    }
    
    state.inputData = data;
    state.outputData = data; // In real app, load separate output file
    
    renderDataTable(inputContainer, data, 'searchInput');
    renderDataTable(outputContainer, data, 'searchOutput');
    updateStats(data);
    
    document.getElementById('rowCount').textContent = data.rows.length;
    document.getElementById('outputRowCount').textContent = data.rows.length;
    
    showNotification(`Loaded ${data.rows.length} rows`, 'success');
  } catch (error) {
    console.error('Error loading data:', error);
    showNotification('Error loading data', 'error');
  }
}

// Render Data Table
function renderDataTable(container, data, searchId) {
  if (!container || !data) return;
  
  const html = `
    <table class="data-table">
      <thead>
        <tr>
          ${data.headers.map(h => `<th>${formatHeader(h)}</th>`).join('')}
        </tr>
      </thead>
      <tbody>
        ${data.rows.map(row => `
          <tr>
            ${data.headers.map(h => `<td>${row[h]}</td>`).join('')}
          </tr>
        `).join('')}
      </tbody>
    </table>
  `;
  
  container.innerHTML = html;
}

// Update Stats
function updateStats(data) {
  const statsContainer = document.getElementById('statsContainer');
  if (!statsContainer || !data) return;
  
  const numericCols = data.headers.filter(h => {
    return data.rows.length > 0 && !isNaN(parseFloat(data.rows[0][h]));
  });
  
  const stats = numericCols.slice(0, 6).map(col => {
    const values = data.rows.map(r => parseFloat(r[col])).filter(v => !isNaN(v));
    const mean = values.reduce((a, b) => a + b, 0) / values.length;
    const min = Math.min(...values);
    const max = Math.max(...values);
    
    return `
      <div class="stat-card">
        <h4>${formatHeader(col)}</h4>
        <div class="stat-value">${mean.toFixed(2)}</div>
        <div class="stat-details">
          Min: ${min.toFixed(2)} | Max: ${max.toFixed(2)}
        </div>
      </div>
    `;
  }).join('');
  
  statsContainer.innerHTML = stats;
}

// Handle Tab Switch
function handleTabSwitch(e) {
  const tab = this.getAttribute('data-tab');
  
  // Update tab buttons
  document.querySelectorAll('.tab-btn').forEach(btn => btn.classList.remove('active'));
  this.classList.add('active');
  
  // Update tab content
  document.querySelectorAll('.tab-content').forEach(content => content.classList.remove('active'));
  const activeTab = document.getElementById(`${tab}-tab`);
  if (activeTab) {
    activeTab.classList.add('active');
  }
}

// Filter Tables
function filterInputTable() {
  filterTable('searchInput', 'inputTableContainer');
}

function filterOutputTable() {
  filterTable('searchOutput', 'outputTableContainer');
}

function filterTable(searchId, containerId) {
  const searchValue = document.getElementById(searchId)?.value.toLowerCase();
  const table = document.querySelector(`#${containerId} .data-table`);
  
  if (!table || !searchValue) return;
  
  const rows = table.querySelectorAll('tbody tr');
  rows.forEach(row => {
    const text = row.textContent.toLowerCase();
    row.style.display = text.includes(searchValue) ? '' : 'none';
  });
}

// Refresh Data
function refreshData() {
  showNotification('Refreshing data...', 'info');
  loadInitialData();
}

// Download Data
function downloadData() {
  if (!state.currentData) {
    showNotification('No data to download', 'warning');
    return;
  }
  
  // Create CSV content
  const csv = [
    state.currentData.headers.join(','),
    ...state.currentData.rows.map(row => 
      state.currentData.headers.map(h => row[h]).join(',')
    )
  ].join('\n');
  
  // Create download link
  const blob = new Blob([csv], { type: 'text/csv' });
  const url = window.URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = 'superwell_data.csv';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  window.URL.revokeObjectURL(url);
  
  showNotification('Data downloaded successfully', 'success');
}

// Show Notification
function showNotification(message, type = 'info') {
  // Simple console notification - can be enhanced with toast/modal
  console.log(`[${type.toUpperCase()}] ${message}`);
  
  // You can add a toast notification library here for better UX
  // For now, using browser alert for critical messages
  if (type === 'error') {
    alert(message);
  }
}

// Debounce utility
function debounce(func, wait) {
  let timeout;
  return function executedFunction(...args) {
    const later = () => {
      clearTimeout(timeout);
      func(...args);
    };
    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
  };
}