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
    const firstELO = +playerData[0].ELO_smooth;

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

  // Create one trace per player
  const traces = players.map(name => {
    const playerData = rows.filter(r => r.Name === name);
    return {
      x: playerData.map(r => r.Date),
      y: playerData.map(r => +r.ELO_smooth),
      mode: 'lines',
      name: name,
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

  // Expose bands globally
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

// Show only players in the selected band
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
