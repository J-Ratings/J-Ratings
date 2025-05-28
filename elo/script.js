// script.js

// Load CSV and plot
Plotly.d3.csv("plot_data.csv", function(err, rows) {
  if (err) {
    console.error("Failed to load CSV:", err);
    return;
  }

  // Get unique player names sorted alphabetically
  const players = [...new Set(rows.map(r => r.Name))].sort((a, b) =>
    a.toLowerCase().localeCompare(b.toLowerCase())
  );

  // Group players by their latest ELO
  const playerBands = {
    "<1600": [],
    "1600-1800": [],
    "1800-2000": [],
    "2000-2200": [],
    ">2200": []
  };

  players.forEach(name => {
    const data = rows.filter(r => r.Name === name);
    const lastELO = +data[data.length - 1].ELO_smooth;
    if (lastELO > 2200) {
      playerBands[">2200"].push(name);
    } else if (lastELO >= 2000) {
      playerBands["2000-2200"].push(name);
    } else if (lastELO >= 1800) {
      playerBands["1800-2000"].push(name);
    } else if (lastELO >= 1600) {
      playerBands["1600-1800"].push(name);
    } else {
      playerBands["<1600"].push(name);
    }
  });

  // Custom colour palette
  const customColors = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#e7ba52", "#e7969c",
    "#d6616b", "#ad494a", "#a55194", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#393b79", "#637939", "#8c6d31", "#843c39", "#7b4173",
    "#5254a3", "#6b6ecf", "#9c9ede", "#8ca252", "#bd9e39",
    "#ce6dbd", "#de9ed6", "#6b6ecf", "#9c9ede", "#ff9896",
    "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5",
    "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "#ff7f0e",
    "#ff9896", "#9467bd", "#c5b0d5", "#8c564b", "#c49c94"
  ];

  // Create one trace per player
  const traces = players.map((name, i) => {
    const data = rows.filter(r => r.Name === name);
    return {
      x: data.map(r => r.Date),
      y: data.map(r => +r.ELO_smooth),
      mode: 'lines',
      name: name,
      line: { color: customColors[i % customColors.length] },
      hovertemplate: '%{y:.0f}<extra>%{fullData.name}</extra>'
    };
  });

  // Generate vertical dashed lines every 6 months from 2020-01-01
  const dateExtent = d3.extent(rows.map(r => new Date(r.Date)));
  const shapes = [];
  let current = new Date("2020-01-01");
  while (current <= new Date(dateExtent[1].getTime() + 1000 * 3600 * 24 * 50)) {
    const iso = current.toISOString().split('T')[0];
    shapes.push({
      type: 'line',
      x0: iso,
      x1: iso,
      y0: 0,
      y1: 1,
      yref: 'paper',
      line: { color: 'grey', width: 1.5, dash: 'dash' }
    });
    current.setMonth(current.getMonth() + 6);
  }

  // Layout
  const layout = {
    xaxis: { title: 'Date', type: 'date' },
    yaxis: { title: 'ELO', autorange: true },
    legend: { orientation: 'v', x: 1, xanchor: 'left' },
    margin: { t: 50, r: 150 },
    shapes: shapes
  };

  // Plot and initially hide all traces
  Plotly.newPlot('plot', traces, layout, { responsive: true }).then(() => {
    const idx = traces.map((_, i) => i);
    Plotly.restyle('plot', { visible: 'legendonly' }, idx);
  });

  // Expose band data for button handlers
  window.playerBands = playerBands;
});

// Show all traces
function showAllPlayers() {
  const count = document.getElementById('plot').data.length;
  Plotly.restyle('plot', { visible: true }, [...Array(count).keys()]);
}

// Hide all traces
function hideAllPlayers() {
  const count = document.getElementById('plot').data.length;
  Plotly.restyle('plot', { visible: 'legendonly' }, [...Array(count).keys()]);
}

// Show only players in a specific rating band
function showBand(band) {
  const plot = document.getElementById('plot');
  const names = plot.data.map(trace => trace.name);
  const visibility = names.map(n =>
    window.playerBands[band].includes(n) ? true : 'legendonly'
  );
  Plotly.restyle('plot', { visible: visibility }, [...Array(names.length).keys()]);
}
