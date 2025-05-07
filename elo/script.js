// Load CSV and plot
Plotly.d3.csv("plot_data.csv", function(err, rows) {
    if (err) {
      console.error("Failed to load CSV:", err);
      return;
    }
  
    const players = [...new Set(rows.map(r => r.Name))].sort();
    const traces = players.map(name => {
      const playerData = rows.filter(r => r.Name === name);
      return {
        x: playerData.map(r => r.Date),
        y: playerData.map(r => +r.ELO_smooth),
        mode: 'lines',
        name: name,
        visible: 'legendonly'
      };
    });
  
    const layout = {
      title: 'Interactive Rating Graph',
      xaxis: { title: 'Date' },
      yaxis: { title: 'ELO', range: [1250, 2350] },
      legend: { orientation: "v", x: 1, xanchor: "left" },
      margin: { t: 50 }
    };
  
    Plotly.newPlot('plot', traces, layout, {responsive: true});
  });
  