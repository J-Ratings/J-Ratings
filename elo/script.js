// Load CSV and plot
Plotly.d3.csv("plot_data.csv", function(err, rows) {
  if (err) {
    console.error("Failed to load CSV:", err);
    return;
  }

  // 1. Get unique player names sorted alphabetically
  const players = [...new Set(rows.map(r => r.Name))].sort((a, b) =>
    a.toLowerCase().localeCompare(b.toLowerCase())
  );

  // 2. Group players by their *latest* rating
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

  // 3. Define an extended colour palette
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

  // 4. Build Plotly traces
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

  // 5. Layout with autorange on Y
  const layout = {
    xaxis: { title: 'Date', type: 'date' },
    yaxis: { title: 'ELO', autorange: true },
    legend: { orientation: "v", x: 1, xanchor: "left" },
    margin: { t: 50 }
  };

  // 6. Plot & hide all traces initially
  Plotly.newPlot('plot', traces, layout, { responsive: true }).then(() => {
    const idx = traces.map((_, i) => i);
    Plotly.restyle('plot', { visible: 'legendonly' }, idx);
  });

  // 7. Expose bands globally
  window.playerBands = playerBands;
});

// Show all traces
function showAllPlayers() {
  const count = plot.data.length;
  Plotly.restyle('plot', { visible: true }, [...Array(count).keys()]);
}

// Hide all traces
function hideAllPlayers() {
  const count = plot.data.length;
  Plotly.restyle('plot', { visible: 'legendonly' }, [...Array(count).keys()]);
}

// Show only one rating band
function showBand(band) {
  const all = plot.data.map(t => t.name);
  const visible = all.map((name, i) =>
    window.playerBands[band].includes(name) ? true : 'legendonly'
  );
  Plotly.restyle('plot', { visible }, [...Array(all.length).keys()]);
}
