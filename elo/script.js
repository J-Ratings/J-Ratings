// Load CSV and plot
Plotly.d3.csv("plot_data.csv", function(err, rows) {
  if (err) {
    console.error("Failed to load CSV:", err);
    return;
  }

  // Get unique player names in alphabetical order (case-insensitive)
  const players = [...new Set(rows.map(r => r.Name))].sort((a, b) =>
    a.toLowerCase().localeCompare(b.toLowerCase())
  );

  // Create one trace per player
  const traces = players.map(name => {
    const playerData = rows.filter(r => r.Name === name);
    return {
      x: playerData.map(r => r.Date),
      y: playerData.map(r => +r.ELO_smooth),
      mode: 'lines',
      name: name
    };
  });

  // Define layout with Y-axis autoranging
  const layout = {
    title: 'Interactive Rating Graph',
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

  // Create the plot, then hide all traces to preserve legend order
  Plotly.newPlot('plot', traces, layout, { responsive: true }).then(function() {
    const traceIndices = traces.map((_, i) => i);
    Plotly.restyle('plot', { visible: 'legendonly' }, traceIndices);
  });
});

// Show all players
function showAllPlayers() {
  const traceCount = document.getElementById('plot').data.length;
  const update = { visible: true };
  const traceIndices = Array.from({ length: traceCount }, (_, i) => i);
  Plotly.restyle('plot', update, traceIndices);
}

// Hide all players
function hideAllPlayers() {
  const traceCount = document.getElementById('plot').data.length;
  const update = { visible: 'legendonly' };
  const traceIndices = Array.from({ length: traceCount }, (_, i) => i);
  Plotly.restyle('plot', update, traceIndices);
}
