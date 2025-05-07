Plotly.d3.csv("plot_data.csv", function(err, rows) {
  if (err) {
    console.error("Failed to load CSV:", err);
    return;
  }

  const players = [...new Set(rows.map(r => r.Name))].sort((a, b) =>
    a.toLowerCase().localeCompare(b.toLowerCase())
  );

  const traces = players.map(name => {
    const playerData = rows.filter(r => r.Name === name);
    return {
      x: playerData.map(r => r.Date),
      y: playerData.map(r => +r.ELO_smooth),
      mode: 'lines',
      name: name
      // Don't set visible here!
    };
  });

  const layout = {
    title: 'Interactive Rating Graph',
    xaxis: { title: 'Date' },
    yaxis: { title: 'ELO', range: [1250, 2350] },
    legend: { orientation: "v", x: 1, xanchor: "left" },
    margin: { t: 50 }
  };

  Plotly.newPlot('plot', traces, layout, { responsive: true }).then(function() {
    // Immediately hide all traces
    const update = { visible: 'legendonly' };
    const traceIndices = traces.map((_, i) => i);
    Plotly.restyle('plot', update, traceIndices);
  });
});
