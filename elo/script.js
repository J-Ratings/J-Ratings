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
  const traces = players.map((name, index) => {
    const playerData = rows.filter(r => r.Name === name);
    return {
      x: playerData.map(r => r.Date),
      y: playerData.map(r => +r.ELO_smooth),
      mode: 'lines',
      name: name,
      line: { color: customColors[index % customColors.length] },
      hovertemplate:
        'Date: ELO: %{y:.0f}<extra>%{fullData.name}</extra>'
    };
  });

  // Layout with vertical lines and hoverformat on x-axis
  const layout = {
    xaxis: {
      title: 'Date',
      type: 'date',
      tickformat: "%Y",
      tickvals: [
        '2015-01-01', '2016-01-01', '2017-01-01', '2018-01-01', '2019-01-01',
        '2020-01-01', '2021-01-01', '2022-01-01', '2023-01-01', '2024-01-01',
        '2025-01-01', '2026-01-01'
      ],
      hoverformat: '%Y-%m-%d'
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
    margin: { t: 50 },
    shapes: [
      { type:'line', x0:'2015-01-01', x1:'2015-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2015-07-01', x1:'2015-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2016-01-01', x1:'2016-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2016-07-01', x1:'2016-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2017-01-01', x1:'2017-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2017-07-01', x1:'2017-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2018-01-01', x1:'2018-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2018-07-01', x1:'2018-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2019-01-01', x1:'2019-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2019-07-01', x1:'2019-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2020-01-01', x1:'2020-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2020-07-01', x1:'2020-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2021-01-01', x1:'2021-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2021-07-01', x1:'2021-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2022-01-01', x1:'2022-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2022-07-01', x1:'2022-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2023-01-01', x1:'2023-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2023-07-01', x1:'2023-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2024-01-01', x1:'2024-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2024-07-01', x1:'2024-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2025-01-01', x1:'2025-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2025-07-01', x1:'2025-07-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} },
      { type:'line', x0:'2026-01-01', x1:'2026-01-01', y0:0, y1:1, yref:'paper', line:{color:'grey',width:1,dash:'dash'} }
    ]
  };

  // Create the plot, then hide all traces initially
  Plotly.newPlot('plot', traces, layout, { responsive: true }).then(() => {
    const traceIndices = traces.map((_, i) => i);
    Plotly.restyle('plot', { visible: 'legendonly' }, traceIndices);
  });

  // Store bands for access by buttons
  window.playerBands = playerBands;
});

// Show all traces
function showAllPlayers() {
  const traceCount = document.getElementById('plot').data.length;
  Plotly.restyle('plot', { visible: true }, [...Array(traceCount).keys()]);
}

// Hide all traces
function hideAllPlayers() {
  const traceCount = document.getElementById('plot').data.length;
  Plotly.restyle('plot', { visible: 'legendonly' }, [...Array(traceCount).keys()]);
}

// Show only players in a specific rating band
function showBand(band) {
  const plot = document.getElementById('plot');
  const allNames = plot.data.map(trace => trace.name);
  const visibility = allNames.map(name =>
    playerBands[band].includes(name) ? true : 'legendonly'
  );
  Plotly.restyle('plot', { visible: visibility }, [...Array(allNames.length).keys()]);
}
