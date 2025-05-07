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

  // Group players by their first rating
  const playerBands = {
    "<1600": [],
    "1600-1800": [],
    "1800-2000": [],
    "2000-2200": [],
    ">2200": []
  };

  players.forEach(name => {
    const playerData = rows.filter(r => r.Name === name);
    const lastELO = +playerData[playerData.length - 1].ELO_smooth;

    if (firstELO > 2200) {
      playerBands[">2200"].push(name);
    } else if (firstELO >= 2000) {
      playerBands["2000-2200"].push(name);
    } else if (firstELO >= 1800) {
      playerBands["1800-2000"].push(name);
    } else if (firstELO >= 1600) {
      playerBands["1600-1800"].push(name);
    } else {
      playerBands["<1600"].push(name);
    }
  });

  // Extended colour palette (not too light)
  const customColors = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#393b79", "#637939", "#8c6d31", "#843c39", "#7b4173",
    "#5254a3", "#6b6ecf", "#9c9ede", "#8ca252", "#bd9e39",
    "#e7ba52", "#e7969c", "#d6616b", "#ad494a", "#a55194",
    "#ce6dbd", "#de9ed6", "#6b6ecf", "#9c9ede", "#ff9896",
    "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5",
    "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "#ff7f0e",
    "#ff9896", "#9467bd", "#c5b0d5", "#8c564b", "#c49c94"
  ];

  // Create one trace per player
  const traces = players.map((name, index) => {
    const playerData = rows.filter(r => r.Name === name);
    return {
      x: playerData.map(r => r.Date),
      y: playerData.map(r => +r.ELO_smooth),
      mode: 'lines',
      name: name,
      line: { color: customColors[index % customColors.length] },
      hovertemplate: '%{y:.0f}<extra>%{fullData.name}</extra>'
    };
  });

  // Layout
  const layout = {
    xaxis: {
      title: 'Date',
      type: 'date'
    },
    yaxis: {
      title: 'ELO',
      autorange: true
    },
    legend: {
      orientation: "v",
      x: 1,
      xanchor: "left"
    },
    margin: { t: 50 }
  };

  // Plot and initially hide all traces
  Plotly.newPlot('plot', traces, layout, { responsive: true }).then(function() {
    const traceIndices = traces.map((_, i) => i);
    Plotly.restyle('plot', { visible: 'legendonly' }, traceIndices);
  });

  // Make bands accessible globally
  window.playerBands = playerBands;
});

// Show all traces
function showAllPlayers() {
  const traceCount = document.getElementById('plot').data.length;
  const update = { visible: true };
  const traceIndices = Array.from({ length: traceCount }, (_, i) => i);
  Plotly.restyle('plot', update, traceIndices);
}

// Hide all traces
function hideAllPlayers() {
  const traceCount = document.getElementById('plot').data.length;
  const update = { visible: 'legendonly' };
  const traceIndices = Array.from({ length: traceCount }, (_, i) => i);
  Plotly.restyle('plot', update, traceIndices);
}

// Show only players in a specific rating band
function showBand(band) {
  const plot = document.getElementById('plot');
  const allNames = plot.data.map(trace => trace.name);
  const visibleIndices = allNames.map((name, i) =>
    playerBands[band].includes(name) ? i : null
  ).filter(i => i !== null);

  const traceCount = plot.data.length;
  const visibility = Array(traceCount).fill("legendonly");
  visibleIndices.forEach(i => visibility[i] = true);

  Plotly.restyle('plot', { visible: visibility });
}
