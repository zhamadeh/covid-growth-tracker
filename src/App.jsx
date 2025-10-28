import React, { useState, useEffect, useMemo, useRef } from 'react';
import Plot from 'react-plotly.js';
import DatePicker from 'react-datepicker';
import 'react-datepicker/dist/react-datepicker.css';
import './App.css';

// Better color palette - professional and distinct colors
const COLOR_PALETTE = [
  '#2563eb', // Blue
  '#dc2626', // Red
  '#16a34a', // Green
  '#ea580c', // Orange
  '#7c3aed', // Purple
  '#0891b2', // Cyan
  '#db2777', // Pink
  '#84cc16', // Lime
  '#f59e0b', // Amber
  '#8b5cf6', // Violet
];

function App() {
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  
  // State for filters
  const [viewType, setViewType] = useState('countries');
  const [selectedCountries, setSelectedCountries] = useState(['Canada', 'United States']);
  const [countryInput, setCountryInput] = useState('');
  const [metric, setMetric] = useState('new_deaths');
  const [perCapita, setPerCapita] = useState(true);
  const [logScale, setLogScale] = useState(true);
  const [smoothing, setSmoothing] = useState(14);
  const [dateRange, setDateRange] = useState({
    start: new Date('2020-03-01'),
    end: new Date()
  });
  const [showBackdrop, setShowBackdrop] = useState(false);
  const [highlightEnabled, setHighlightEnabled] = useState(false);
  const [highlightThreshold, setHighlightThreshold] = useState(40);
  const [highlightLookback, setHighlightLookback] = useState(90);
  const [showR0, setShowR0] = useState(false);
  const [r0Window, setR0Window] = useState(21);
  const [r0SI, setR0SI] = useState(4.8);
  const [showAbout, setShowAbout] = useState(false);
  
  const sidebarRef = useRef(null);
  const [sidebarWidth, setSidebarWidth] = useState(280);
  
  // Sidebar resize
  useEffect(() => {
    let isResizing = false;
    let startX = 0;
    let startWidth = 0;

    const handleMouseMove = (e) => {
      if (!isResizing) return;
      const newWidth = startWidth + (e.clientX - startX);
      setSidebarWidth(Math.max(240, Math.min(500, newWidth)));
    };

    const handleMouseUp = () => {
      isResizing = false;
      document.body.style.cursor = 'default';
      document.body.style.userSelect = 'auto';
    };

    const handleMouseDown = (e) => {
      if (e.target.classList.contains('resize-handle')) {
        isResizing = true;
        startX = e.clientX;
        startWidth = sidebarWidth;
        document.body.style.cursor = 'ew-resize';
        document.body.style.userSelect = 'none';
      }
    };

    document.addEventListener('mousedown', handleMouseDown);
    document.addEventListener('mousemove', handleMouseMove);
    document.addEventListener('mouseup', handleMouseUp);

    return () => {
      document.removeEventListener('mousedown', handleMouseDown);
      document.removeEventListener('mousemove', handleMouseMove);
      document.removeEventListener('mouseup', handleMouseUp);
    };
  }, [sidebarWidth]);

  // Merge overlapping or nearby highlight periods
  const mergeHighlightPeriods = (highlights) => {
    if (highlights.length === 0) return highlights;
    
    // Group by country
    const byCountry = {};
    highlights.forEach(h => {
      if (!byCountry[h.country]) byCountry[h.country] = [];
      byCountry[h.country].push(h);
    });
    
    // Merge for each country
    const merged = [];
    Object.keys(byCountry).forEach(country => {
      const periods = byCountry[country].sort((a, b) => 
        new Date(a.startDate) - new Date(b.startDate)
      );
      
      let current = periods[0];
      for (let i = 1; i < periods.length; i++) {
        const next = periods[i];
        const currentEnd = new Date(current.endDate);
        const nextStart = new Date(next.startDate);
        const daysDiff = (nextStart - currentEnd) / (1000 * 60 * 60 * 24);
        
        // Merge if gap is <= 7 days
        if (daysDiff <= 7) {
          current.endDate = next.endDate;
          current.growth = Math.max(parseFloat(current.growth), parseFloat(next.growth)).toFixed(1);
        } else {
          merged.push(current);
          current = next;
        }
      }
      merged.push(current);
    });
    
    return merged;
  };

  // Fetch data
  useEffect(() => {
    const fetchData = async () => {
      try {
        setLoading(true);
        const response = await fetch(
          'https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv'
        );
        const text = await response.text();
        const parsed = parseCSV(text);
        setData(parsed);
        setLoading(false);
      } catch (err) {
        setError(err.message);
        setLoading(false);
      }
    };
    fetchData();
  }, []);

  // Parse CSV
  const parseCSV = (text) => {
    const lines = text.trim().split('\n');
    const headers = lines[0].split(',');
    
    const result = [];
    for (let i = 1; i < lines.length; i++) {
      const values = lines[i].split(',');
      const row = {};
      headers.forEach((header, index) => {
        row[header] = values[index];
      });
      result.push(row);
    }
    return result;
  };

  // Get unique countries
  const countries = useMemo(() => {
    if (!data) return [];
    const unique = [...new Set(data.map(d => d.location || d.country).filter(Boolean))];
    return unique.sort();
  }, [data]);

  // Filtered countries for dropdown
  const filteredCountries = useMemo(() => {
    if (!countryInput) return countries;
    const search = countryInput.toLowerCase();
    return countries.filter(c => c.toLowerCase().includes(search));
  }, [countries, countryInput]);

  // Add country to selection
  const addCountry = (country) => {
    if (!selectedCountries.includes(country)) {
      setSelectedCountries([...selectedCountries, country]);
    }
    setCountryInput('');
  };

  // Remove country from selection
  const removeCountry = (country) => {
    setSelectedCountries(selectedCountries.filter(c => c !== country));
  };

  // Rolling average function
  const rollingAverage = (arr, window) => {
    const result = [];
    for (let i = 0; i < arr.length; i++) {
      const start = Math.max(0, i - window + 1);
      const slice = arr.slice(start, i + 1);
      const sum = slice.reduce((a, b) => a + (parseFloat(b) || 0), 0);
      result.push(sum / slice.length);
    }
    return result;
  };

  // Process data for plotting
  const plotData = useMemo(() => {
    if (!data) return { traces: [], layout: {}, shapes: [], r0Data: [], highlightData: [] };

    let filtered = data.filter(d => {
      const date = new Date(d.date);
      return date >= dateRange.start && date <= dateRange.end;
    });

    let traces = [];
    let shapes = [];
    let r0Data = [];
    let highlightData = [];
    
    if (viewType === 'global') {
      // Aggregate global data
      const dateMap = new Map();
      filtered.forEach(row => {
        const date = row.date;
        const value = parseFloat(row[metric]) || 0;
        const pop = parseFloat(row.population) || 1;
        
        if (!dateMap.has(date)) {
          dateMap.set(date, { value: 0, pop: 0 });
        }
        const current = dateMap.get(date);
        current.value += value;
        current.pop += pop;
      });

      const dates = Array.from(dateMap.keys()).sort();
      let values = dates.map(d => dateMap.get(d).value);
      
      if (metric.includes('new_')) {
        values = rollingAverage(values, smoothing);
      }
      
      if (perCapita) {
        const worldPop = dates.map(d => dateMap.get(d).pop);
        values = values.map((v, i) => (v / worldPop[i]) * 1e6);
      }

      // Filter out zeros and negatives for log scale
      const filteredData = dates.map((d, i) => ({ date: d, value: values[i] }))
        .filter(d => d.value > 0 || !logScale);

      traces.push({
        x: filteredData.map(d => d.date),
        y: filteredData.map(d => d.value),
        type: 'scatter',
        mode: 'lines',
        name: 'Global',
        line: { color: COLOR_PALETTE[0], width: 3 },
        hovertemplate: '<b>Global</b><br>Date: %{x}<br>Value: %{y:,.0f}<extra></extra>'
      });

    } else if (viewType === 'countries' && selectedCountries.length > 0) {
      // Individual country traces
      selectedCountries.forEach((country, idx) => {
        const countryData = filtered.filter(d => 
          (d.location || d.country) === country
        ).sort((a, b) => a.date.localeCompare(b.date));

        if (countryData.length === 0) return;

        let values = countryData.map(d => parseFloat(d[metric]) || 0);
        const pop = parseFloat(countryData[0].population) || 1;

        if (metric.includes('new_')) {
          values = rollingAverage(values, smoothing);
        }

        if (perCapita) {
          values = values.map(v => (v / pop) * 1e6);
        }

        const dates = countryData.map(d => d.date);
        
        // Calculate highlights for this country
        if (highlightEnabled && metric.includes('new_')) {
          const allDates = dates;
          const allValues = values;
          
          // Calculate lookback start date
          const endDate = new Date(Math.max(...allDates.map(d => new Date(d))));
          const lookbackStart = new Date(endDate.getTime() - highlightLookback * 24 * 60 * 60 * 1000);
          
          const recentData = allDates.map((d, i) => ({ 
            date: d, 
            value: allValues[i],
            index: i 
          })).filter(d => new Date(d.date) >= lookbackStart);
          
          for (let i = 7; i < recentData.length; i++) {
            const current = recentData[i].value;
            const weekAgo = recentData[i - 7].value;
            if (weekAgo > 0 && current > 0) {
              const growth = ((current / weekAgo - 1) * 100);
              if (growth >= highlightThreshold) {
                const startIdx = Math.max(0, i - 3);
                const endIdx = Math.min(recentData.length - 1, i + 3);
                const startDate = recentData[startIdx].date;
                const endDate = recentData[endIdx].date;
                
                // Add to shapes (check if overlaps with existing shape for this period)
                const overlaps = shapes.some(s => 
                  s.x0 === startDate || s.x1 === endDate
                );
                
                if (!overlaps) {
                  shapes.push({
                    type: 'rect',
                    xref: 'x',
                    yref: 'paper',
                    x0: startDate,
                    x1: endDate,
                    y0: 0,
                    y1: 1,
                    fillcolor: 'rgba(255, 107, 107, 0.15)',
                    line: { width: 0 },
                    layer: 'below'
                  });
                }
                
                // Add to highlight data table
                highlightData.push({
                  country,
                  startDate,
                  endDate,
                  growth: growth.toFixed(1)
                });
              }
            }
          }
        }
        
        // Calculate R0 if enabled
        if (showR0) {
          const recentDates = dates.slice(-r0Window);
          const recentValues = values.slice(-r0Window).filter(v => v > 0);
          
          if (recentValues.length >= 5) {
            const logValues = recentValues.map(v => Math.log(v));
            const n = logValues.length;
            const xValues = Array.from({ length: n }, (_, i) => i);
            const xMean = xValues.reduce((a, b) => a + b) / n;
            const yMean = logValues.reduce((a, b) => a + b) / n;
            
            const numerator = xValues.reduce((sum, x, i) => sum + (x - xMean) * (logValues[i] - yMean), 0);
            const denominator = xValues.reduce((sum, x) => sum + Math.pow(x - xMean, 2), 0);
            
            if (denominator !== 0) {
              const slope = numerator / denominator;
              const r0 = Math.exp(slope * r0SI);
              const doublingTime = slope > 0 ? Math.log(2) / slope : slope < 0 ? -Math.log(2) / slope : null;
              
              r0Data.push({
                country,
                r0: r0.toFixed(2),
                doublingHalving: doublingTime ? doublingTime.toFixed(1) : '—'
              });
            }
          }
        }
        
        // Filter out zeros and negatives for log scale
        const filteredData = dates.map((d, i) => ({ date: d, value: values[i] }))
          .filter(d => d.value > 0 || !logScale);

        traces.push({
          x: filteredData.map(d => d.date),
          y: filteredData.map(d => d.value),
          type: 'scatter',
          mode: 'lines',
          name: country,
          line: { color: COLOR_PALETTE[idx % COLOR_PALETTE.length], width: 2.5 },
          hovertemplate: `<b>${country}</b><br>Date: %{x}<br>Value: %{y:,.0f}<extra></extra>`
        });
      });

      // Merge overlapping highlight periods
      if (highlightData.length > 0) {
        const mergedHighlights = mergeHighlightPeriods(highlightData);
        highlightData = mergedHighlights;
        
        // Recreate shapes from merged highlights
        shapes = [];
        mergedHighlights.forEach(h => {
          shapes.push({
            type: 'rect',
            xref: 'x',
            yref: 'paper',
            x0: h.startDate,
            x1: h.endDate,
            y0: 0,
            y1: 1,
            fillcolor: 'rgba(255, 107, 107, 0.15)',
            line: { width: 0 },
            layer: 'below'
          });
        });
      }

      // Add backdrop if enabled
      if (showBackdrop) {
        // First, calculate the max value from selected countries to use as reference
        const selectedMax = Math.max(...traces.flatMap(t => t.y.filter(v => isFinite(v))));
        const selectedMin = Math.min(...traces.flatMap(t => t.y.filter(v => isFinite(v) && v > 0)));
        
        const allCountries = countries.filter(c => !selectedCountries.includes(c)).slice(0, 100);
        const backdropTraces = [];
        
        allCountries.forEach(country => {
          const countryData = filtered.filter(d => 
            (d.location || d.country) === country
          ).sort((a, b) => a.date.localeCompare(b.date));

          if (countryData.length === 0) return;

          let values = countryData.map(d => parseFloat(d[metric]) || 0);
          const pop = parseFloat(countryData[0].population) || 1;

          if (metric.includes('new_')) {
            values = rollingAverage(values, smoothing);
          }

          if (perCapita) {
            values = values.map(v => (v / pop) * 1e6);
          }

          const dates = countryData.map(d => d.date);
          const filteredData = dates.map((d, i) => ({ date: d, value: values[i] }))
            .filter(d => d.value > 0 || !logScale);

          // Calculate max for this country
          const countryMax = Math.max(...filteredData.map(d => d.value).filter(v => isFinite(v)));
          
          // Only include backdrop countries within 100x of selected countries' range
          // This prevents extreme outliers from warping the scale
          if (countryMax <= selectedMax * 100 && countryMax >= selectedMin / 100) {
            backdropTraces.push({
              x: filteredData.map(d => d.date),
              y: filteredData.map(d => d.value),
              type: 'scatter',
              mode: 'lines',
              name: country,
              line: { color: 'rgba(0,0,0,0.03)', width: 0.5 },
              showlegend: false,
              hoverinfo: 'skip'
            });
          }
        });
        
        // Add backdrop traces first (so they appear behind)
        traces = [...backdropTraces, ...traces];
      }
    }

    const metricLabel = {
      'total_cases': 'Total cases',
      'new_cases': 'New cases',
      'total_deaths': 'Total deaths',
      'new_deaths': 'New deaths'
    }[metric] || 'Value';

    const yAxisTitle = perCapita ? `${metricLabel} (per million)` : metricLabel;
    
    // Create plot title
    let plotTitle = '';
    if (viewType === 'global') {
      plotTitle = `Global ${metricLabel}`;
    } else if (selectedCountries.length > 0) {
      plotTitle = `${metricLabel} - ${selectedCountries.join(', ')}`;
    }
    if (metric.includes('new_') && smoothing > 1) {
      plotTitle += ` (${smoothing}-day average)`;
    }
    if (perCapita) {
      plotTitle += ' [per million]';
    }
    if (logScale) {
      plotTitle += ' [log scale]';
    }

    // Better y-axis configuration
    const yAxisConfig = {
      title: yAxisTitle,
      type: logScale ? 'log' : 'linear',
      gridcolor: '#e5e7e5',
      showgrid: true,
      automargin: true,
      side: 'left'
    };

    if (logScale) {
      // For log scale, force specific tick values for clean display
      yAxisConfig.tickmode = 'array';
      yAxisConfig.tickformat = '.3s';
      yAxisConfig.exponentformat = 'none';
      // Will show: 1k, 10k, 100k, 1M, 10M, etc.
    } else {
      // For linear scale
      yAxisConfig.tickformat = '.3s';
      yAxisConfig.separatethousands = true;
    }

    const layout = {
      title: {
        text: plotTitle,
        font: { size: 14, color: '#374151' },
        x: 0.5,
        xanchor: 'center'
      },
      xaxis: {
        title: 'Date',
        gridcolor: '#e5e7e5',
        showgrid: true,
        automargin: true
      },
      yaxis: yAxisConfig,
      hovermode: 'closest',
      plot_bgcolor: '#ffffff',
      paper_bgcolor: '#ffffff',
      font: { family: 'Inter, sans-serif', size: 12, color: '#333' },
      margin: { l: 60, r: 20, t: 20, b: 60 },
      autosize: true,
      legend: {
        orientation: 'h',
        y: 1.05,
        x: 0,
        bgcolor: 'rgba(255,255,255,0.8)',
        bordercolor: '#ddd',
        borderwidth: 1
      },
      hoverlabel: {
        bgcolor: 'white',
        bordercolor: '#ddd',
        font: { family: 'Inter', size: 12, color: 'black' }
      },
      shapes: shapes
    };

    return { traces, layout, r0Data, highlightData };
  }, [data, viewType, selectedCountries, metric, perCapita, logScale, smoothing, dateRange, showBackdrop, countries, highlightEnabled, highlightThreshold, highlightLookback, showR0, r0Window, r0SI]);

  if (loading) {
    return (
      <div className="loading-container">
        <div className="spinner"></div>
        <p>Loading COVID-19 data...</p>
      </div>
    );
  }

  if (error) {
    return (
      <div className="error-container">
        <h2>Error loading data</h2>
        <p>{error}</p>
      </div>
    );
  }

  return (
    <div className="app">
      <header className="header">
        <div className="header-content">
          <div className="header-left">
            <h1>COVID-19 Trend Tracker</h1>
            <p className="subtitle">Interactive visualization of global pandemic trends</p>
          </div>
          <button className="about-button" onClick={() => setShowAbout(true)}>
            About
          </button>
        </div>
      </header>

      {showAbout && (
        <div className="modal-overlay" onClick={() => setShowAbout(false)}>
          <div className="modal-content" onClick={(e) => e.stopPropagation()}>
            <button className="modal-close" onClick={() => setShowAbout(false)}>×</button>
            <h2>About COVID-19 Trend Tracker</h2>
            
            <h3>Purpose</h3>
            <p>This tool provides interactive visualization of COVID-19 pandemic trends across countries and time periods. It helps researchers, journalists, and the public understand the progression and patterns of the pandemic through data analysis.</p>
            
            <h3>Data Source</h3>
            <p>All data is sourced from <a href="https://ourworldindata.org/coronavirus" target="_blank" rel="noopener noreferrer">Our World in Data (OWID)</a>, which compiles COVID-19 statistics from official government sources worldwide. The dataset includes daily updates on cases, deaths, testing, and vaccinations.</p>
            
            <h3>Features</h3>
            <ul>
              <li><strong>Multi-country comparison:</strong> Compare trends across multiple countries simultaneously</li>
              <li><strong>Per-capita normalization:</strong> Adjust values by population to enable fair comparisons between countries of different sizes</li>
              <li><strong>Rolling averages:</strong> Smooth out daily reporting variations (weekends, holidays) to reveal underlying trends</li>
              <li><strong>Log/linear scales:</strong> View exponential growth (log) or absolute numbers (linear)</li>
              <li><strong>Highlight rapid growth:</strong> Automatically identify and highlight periods of steep increases</li>
              <li><strong>R₀ estimation:</strong> Experimental calculation of reproduction number based on recent growth trends</li>
              <li><strong>Date range selection:</strong> Focus on specific time periods of interest</li>
            </ul>
            
            <h3>Technical Notes</h3>
            <p><strong>Data Quality:</strong> Many countries reduced or stopped COVID-19 reporting after 2023. Data completeness and accuracy varies significantly by country and time period. Flat or zero segments typically indicate reporting changes rather than absence of cases.</p>
            
            <p><strong>Rolling Averages:</strong> These smooth out day-to-day variations caused by reporting delays, weekend effects, and holidays. A 7-day or 14-day average is recommended for most analyses.</p>
            
            <p><strong>Highlighting:</strong> Periods are highlighted when the weekly growth rate exceeds your specified threshold. This helps identify rapid surge periods that may require attention.</p>
            
            <p><strong>R₀ Estimation:</strong> The reproduction number (R₀) represents the average number of secondary infections from one infected person. This tool estimates R₀ from recent case growth using exponential fitting. This is a simplified approach and should be considered experimental. Professional epidemiological analyses use more sophisticated methods.</p>
            
            <h3>Created By</h3>
            <p>Made by <a href="https://zhamadeh.github.io" target="_blank" rel="noopener noreferrer">Zeid Hamadeh</a></p>
            
            <h3>Open Source</h3>
            <p>This project is open source. Feel free to use, modify, and share.</p>
          </div>
        </div>
      )}

      <div className="main-content">
        <aside className="sidebar" ref={sidebarRef} style={{ width: `${sidebarWidth}px` }}>
          <div className="control-section">
            <h3>View Settings</h3>
            
            <div className="control-group">
              <label>View Type</label>
              <div className="radio-group">
                <label className="radio-label">
                  <input
                    type="radio"
                    value="global"
                    checked={viewType === 'global'}
                    onChange={(e) => setViewType(e.target.value)}
                  />
                  <span>Global</span>
                </label>
                <label className="radio-label">
                  <input
                    type="radio"
                    value="countries"
                    checked={viewType === 'countries'}
                    onChange={(e) => setViewType(e.target.value)}
                  />
                  <span>Countries</span>
                </label>
              </div>
            </div>

            {viewType === 'countries' && (
              <div className="control-group">
                <label htmlFor="country-input">Countries</label>
                <div className="country-tags">
                  {selectedCountries.map(country => (
                    <span key={country} className="country-tag">
                      {country}
                      <button onClick={() => removeCountry(country)} className="remove-tag">×</button>
                    </span>
                  ))}
                </div>
                <input
                  id="country-input"
                  type="text"
                  value={countryInput}
                  onChange={(e) => setCountryInput(e.target.value)}
                  placeholder="Type to search countries..."
                  className="country-input"
                />
                {countryInput && filteredCountries.length > 0 && (
                  <div className="country-dropdown">
                    {filteredCountries.slice(0, 10).map(country => (
                      <div
                        key={country}
                        className="country-option"
                        onClick={() => addCountry(country)}
                      >
                        {country}
                      </div>
                    ))}
                  </div>
                )}
                
                <label className="checkbox-label">
                  <input
                    type="checkbox"
                    checked={showBackdrop}
                    onChange={(e) => setShowBackdrop(e.target.checked)}
                  />
                  <span>Show all countries as backdrop</span>
                </label>
              </div>
            )}
          </div>

          <div className="control-section">
            <h3>Metric</h3>
            
            <div className="control-group">
              <div className="radio-group">
                <label className="radio-label">
                  <input
                    type="radio"
                    value="total_cases"
                    checked={metric === 'total_cases'}
                    onChange={(e) => setMetric(e.target.value)}
                  />
                  <span>Total cases</span>
                </label>
                <label className="radio-label">
                  <input
                    type="radio"
                    value="new_cases"
                    checked={metric === 'new_cases'}
                    onChange={(e) => setMetric(e.target.value)}
                  />
                  <span>New cases</span>
                </label>
                <label className="radio-label">
                  <input
                    type="radio"
                    value="total_deaths"
                    checked={metric === 'total_deaths'}
                    onChange={(e) => setMetric(e.target.value)}
                  />
                  <span>Total deaths</span>
                </label>
                <label className="radio-label">
                  <input
                    type="radio"
                    value="new_deaths"
                    checked={metric === 'new_deaths'}
                    onChange={(e) => setMetric(e.target.value)}
                  />
                  <span>New deaths</span>
                </label>
              </div>
            </div>

            <div className="control-group">
              <label className="checkbox-label">
                <input
                  type="checkbox"
                  checked={perCapita}
                  onChange={(e) => setPerCapita(e.target.checked)}
                />
                <span>Per million (normalize by population)</span>
              </label>
              
              <label className="checkbox-label">
                <input
                  type="checkbox"
                  checked={logScale}
                  onChange={(e) => setLogScale(e.target.checked)}
                />
                <span>Log scale</span>
              </label>
            </div>

            {(metric === 'new_cases' || metric === 'new_deaths') && (
              <div className="control-group">
                <label htmlFor="smoothing">Rolling average: {smoothing} days</label>
                <small>Smooths out daily reporting variations</small>
                <input
                  id="smoothing"
                  type="range"
                  min="1"
                  max="28"
                  value={smoothing}
                  onChange={(e) => setSmoothing(parseInt(e.target.value))}
                  className="slider"
                />
              </div>
            )}
          </div>

          {viewType === 'countries' && (
            <div className="control-section">
              <h3>Date Range</h3>
              
              <div className="control-group">
                <label>Start Date</label>
                <DatePicker
                  selected={dateRange.start}
                  onChange={(date) => setDateRange({ ...dateRange, start: date })}
                  selectsStart
                  startDate={dateRange.start}
                  endDate={dateRange.end}
                  maxDate={dateRange.end}
                  dateFormat="yyyy-MM-dd"
                  className="date-picker"
                  showMonthYearPicker={false}
                  showYearDropdown
                  scrollableYearDropdown
                  yearDropdownItemNumber={10}
                />
                
                <label>End Date</label>
                <DatePicker
                  selected={dateRange.end}
                  onChange={(date) => setDateRange({ ...dateRange, end: date })}
                  selectsEnd
                  startDate={dateRange.start}
                  endDate={dateRange.end}
                  minDate={dateRange.start}
                  maxDate={new Date()}
                  dateFormat="yyyy-MM-dd"
                  className="date-picker"
                  showMonthYearPicker={false}
                  showYearDropdown
                  scrollableYearDropdown
                  yearDropdownItemNumber={10}
                />
              </div>
            </div>
          )}

          <div className="control-section">
            <h3>Highlight Rises</h3>
            <div className="control-group">
              <label className="checkbox-label">
                <input
                  type="checkbox"
                  checked={highlightEnabled}
                  onChange={(e) => setHighlightEnabled(e.target.checked)}
                />
                <span>Enable highlighting</span>
              </label>
              <small>Highlights periods with rapid increases</small>
              
              {highlightEnabled && (
                <>
                  <label htmlFor="hl-lookback">Lookback: {highlightLookback} days</label>
                  <small>Time window to scan for surges</small>
                  <input
                    id="hl-lookback"
                    type="range"
                    min="30"
                    max="365"
                    step="10"
                    value={highlightLookback}
                    onChange={(e) => setHighlightLookback(parseInt(e.target.value))}
                    className="slider"
                  />
                  
                  <label htmlFor="hl-threshold">Threshold: {highlightThreshold}% growth/week</label>
                  <small>Minimum weekly growth rate to highlight</small>
                  <input
                    id="hl-threshold"
                    type="range"
                    min="10"
                    max="200"
                    step="5"
                    value={highlightThreshold}
                    onChange={(e) => setHighlightThreshold(parseInt(e.target.value))}
                    className="slider"
                  />
                </>
              )}
            </div>
          </div>

          <div className="control-section">
            <h3>R₀ Estimation</h3>
            <div className="control-group">
              <label className="checkbox-label">
                <input
                  type="checkbox"
                  checked={showR0}
                  onChange={(e) => setShowR0(e.target.checked)}
                />
                <span>Show R₀ calculator</span>
              </label>
              <small>Experimental. Based on recent growth trends.</small>
              
              {showR0 && (
                <>
                  <label htmlFor="r0-window">Lookback: {r0Window} days</label>
                  <small>Recent period used to estimate growth rate</small>
                  <input
                    id="r0-window"
                    type="range"
                    min="7"
                    max="90"
                    value={r0Window}
                    onChange={(e) => setR0Window(parseInt(e.target.value))}
                    className="slider"
                  />
                  
                  <label htmlFor="r0-si">Serial interval: {r0SI} days</label>
                  <small>Time between successive infections</small>
                  <input
                    id="r0-si"
                    type="range"
                    min="1"
                    max="14"
                    step="0.1"
                    value={r0SI}
                    onChange={(e) => setR0SI(parseFloat(e.target.value))}
                    className="slider"
                  />
                </>
              )}
            </div>
          </div>
          
          <div className="resize-handle"></div>
        </aside>

        <main className="chart-area">
          <div className="chart-container">
            {plotData.traces.length > 0 ? (
              <Plot
                data={plotData.traces}
                layout={plotData.layout}
                config={{
                  responsive: true,
                  displayModeBar: true,
                  displaylogo: false,
                  modeBarButtonsToRemove: ['lasso2d', 'select2d']
                }}
                style={{ width: '100%', height: '100%' }}
                useResizeHandler={true}
              />
            ) : (
              <div className="no-data">
                <p>No data to display. {viewType === 'countries' && 'Please select countries.'}</p>
              </div>
            )}
          </div>
          
          {showR0 && plotData.r0Data && plotData.r0Data.length > 0 && (
            <div className="data-table-container">
              <h4>R₀ Estimates (Last {r0Window} days)</h4>
              <table className="data-table">
                <thead>
                  <tr>
                    <th>Country</th>
                    <th>R₀</th>
                    <th>Doubling/Halving (days)</th>
                  </tr>
                </thead>
                <tbody>
                  {plotData.r0Data.map(row => (
                    <tr key={row.country}>
                      <td><strong>{row.country}</strong></td>
                      <td>{row.r0}</td>
                      <td>{row.doublingHalving}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
          
          {highlightEnabled && plotData.highlightData && plotData.highlightData.length > 0 && (
            <div className="data-table-container">
              <h4>Highlighted Growth Periods (≥{highlightThreshold}% weekly growth)</h4>
              
              {/* Country tabs */}
              <div className="country-tabs">
                {selectedCountries.map(country => {
                  const countryHighlights = plotData.highlightData.filter(h => h.country === country);
                  if (countryHighlights.length === 0) return null;
                  
                  return (
                    <details key={country} className="country-tab">
                      <summary>
                        <strong>{country}</strong> 
                        <span className="badge">{countryHighlights.length}</span>
                      </summary>
                      <table className="data-table">
                        <thead>
                          <tr>
                            <th>Start Date</th>
                            <th>End Date</th>
                            <th>Max Growth Rate</th>
                          </tr>
                        </thead>
                        <tbody>
                          {countryHighlights.map((row, idx) => (
                            <tr key={idx}>
                              <td>{row.startDate}</td>
                              <td>{row.endDate}</td>
                              <td>{row.growth}%</td>
                            </tr>
                          ))}
                        </tbody>
                      </table>
                    </details>
                  );
                })}
              </div>
            </div>
          )}
        </main>
      </div>

      <footer className="footer">
        <p>
          Made by <a href="https://zhamadeh.github.io" target="_blank" rel="noopener noreferrer">Zeid Hamadeh</a>. 
          Data: <a href="https://ourworldindata.org/coronavirus" target="_blank" rel="noopener noreferrer">Our World in Data</a>
        </p>
      </footer>
    </div>
  );
}

export default App;
