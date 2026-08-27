(function () {
  const DEFAULTS = {
    defaultRange: '2010',
    lineWidth: 4
  };

  const SERIES_COLOURS = [
    '#1f77b4',
    '#ff7f0e',
    '#2ca02c',
    '#d62728',
    '#9467bd',
    '#8c564b',
    '#e377c2',
    '#7f7f7f',
    '#bcbd22',
    '#17becf'
  ];

  function iso(d) {
    return new Date(d).toISOString().slice(0, 10);
  }

  function normalise(series, dedupeByDay = false) {
    const s = (Array.isArray(series) ? series : [])
      .filter(p => p && p.x && Number.isFinite(+p.y))
      .map(p => ({
        ...p,
        x: p.x,
        y: +p.y
      }))
      .sort((a, b) => new Date(a.x) - new Date(b.x));

    if (!dedupeByDay) return s;

    const out = [];
    let lastKey = null;

    for (const p of s) {
      const key = iso(p.x);

      if (key !== lastKey) out.push(p);
      else out[out.length - 1] = p;

      lastKey = key;
    }

    return out;
  }

  function aggregate(series, mode) {
    if (mode === 'daily') return normalise(series, true);

    const buckets = Object.create(null);

    for (const p of series) {
      const d = new Date(p.x);
      let key;

      if (mode === 'weekly') {
        const tmp = new Date(Date.UTC(d.getFullYear(), d.getMonth(), d.getDate()));
        const dayNum = tmp.getUTCDay() || 7;

        tmp.setUTCDate(tmp.getUTCDate() + 4 - dayNum);

        const yearStart = new Date(Date.UTC(tmp.getUTCFullYear(), 0, 1));
        const weekNo = Math.ceil((((tmp - yearStart) / 86400000) + 1) / 7);

        key = `${tmp.getUTCFullYear()}-W${String(weekNo).padStart(2, '0')}`;
      } else if (mode === 'monthly') {
        key = `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, '0')}`;
      } else if (mode === 'quarterly') {
        const q = Math.floor(d.getMonth() / 3) + 1;
        key = `${d.getFullYear()}-Q${q}`;
      } else if (mode === 'decade') {
        key = `${Math.floor(d.getFullYear() / 10) * 10}-D`;
      } else {
        key = `${d.getFullYear()}`;
      }

      (buckets[key] ||= []).push(p);
    }

    const out = Object.keys(buckets).sort().map(k => {
      const pts = buckets[k].filter(p => Number.isFinite(p.y));
      const avg = pts.reduce((a, b) => a + b.y, 0) / pts.length;
      const last = pts.length ? pts[pts.length - 1] : null;

      let x;

      if (k.includes('W')) {
        const [y, w] = k.split('-W');
        const simple = new Date(Date.UTC(+y, 0, 1 + (+w - 1) * 7));
        const dow = simple.getUTCDay();

        if (dow <= 4) simple.setUTCDate(simple.getUTCDate() - dow + 1);
        else simple.setUTCDate(simple.getUTCDate() + 8 - dow);

        x = iso(simple);
      } else if (k.includes('Q')) {
        const [y, q] = k.split('-Q');
        x = iso(new Date(Date.UTC(+y, (+q - 1) * 3, 1)));
      } else if (k.endsWith('-D')) {
        const y = +k.slice(0, 4);
        x = iso(new Date(Date.UTC(y, 0, 1)));
      } else if (k.length === 7) {
        const [y, m] = k.split('-');
        x = iso(new Date(Date.UTC(+y, +m - 1, 1)));
      } else {
        x = iso(new Date(Date.UTC(+k, 0, 1)));
      }

      return {
        ...(last || {}),
        x,
        y: avg
      };
    });

    return normalise(out);
  }

  function labelStepForRange(y0, y1) {
    const span = Math.abs(y1 - y0);
    if (span < 100) return 25;
    if (span < 300) return 50;
    return 100;
  }

  function makeYGridShapes(yMin, yMax, step) {
    if (!Number.isFinite(yMin) || !Number.isFinite(yMax) || !step) return [];

    const lo = Math.floor((yMin - 1) / step) * step;
    const hi = Math.ceil((yMax + 1) / step) * step;
    const shapes = [];

    for (let y = lo; y <= hi; y += step) {
      let width;
      let color;

      if (step === 100) {
        width = 2.2;
        color = 'rgba(255,255,255,0.28)';
      } else if (step === 50) {
        width = 1.4;
        color = 'rgba(255,255,255,0.16)';
      } else {
        width = 1.0;
        color = 'rgba(255,255,255,0.08)';
      }

      shapes.push({
        type: 'line',
        xref: 'paper',
        x0: 0,
        x1: 1,
        yref: 'y',
        y0: y,
        y1: y,
        line: { width, color }
      });
    }

    return shapes;
  }

  function setTickLabelsEvery(chartId, yMin, yMax, step) {
    const lo = Math.floor(yMin / step) * step;
    const hi = Math.ceil(yMax / step) * step;

    const vals = [];
    const text = [];

    for (let y = lo; y <= hi; y += step) {
      vals.push(y);
      text.push(String(y));
    }

    return Plotly.relayout(chartId, {
      'yaxis.tickmode': 'array',
      'yaxis.tickvals': vals,
      'yaxis.ticktext': text
    });
  }

  function extentFrom(seriesList) {
    const xs = [];

    for (const s of seriesList) {
      for (const p of s || []) {
        if (p && p.x) xs.push(new Date(p.x));
      }
    }

    if (!xs.length) return { min: null, max: null };

    return {
      min: new Date(Math.min(...xs)),
      max: new Date(Math.max(...xs))
    };
  }

  function makePaddedEnd(start, endData) {
    const oneDay = 86400000;
    const spanMs = Math.max(oneDay, +endData - +start);
    const padMs = Math.max(3 * oneDay, Math.min(14 * oneDay, spanMs * 0.02));

    return new Date(+endData + padMs);
  }

  function rangeFromKey(key, dataMin, dataMax) {
    if (!dataMin || !dataMax) return null;

    let start;

    if (key === 'ALL') {
      start = new Date(dataMin);
    } else if (/^\d{4}$/.test(String(key))) {
      start = new Date(`${key}-01-01`);
    } else if (key === 'LY') {
      start = new Date(dataMax);
      start.setFullYear(start.getFullYear() - 1);
    } else if (key === 'YTD') {
      start = new Date(dataMax.getFullYear() + '-01-01');
    } else {
      start = new Date(dataMin);
    }

    start = new Date(Math.max(+start, +dataMin));

    return [iso(start), iso(makePaddedEnd(start, dataMax))];
  }

  function rangeFromYears(startYear, endYear, dataMin, dataMax) {
    if (!dataMin || !dataMax) return null;

    let y0 = Number(startYear);
    let y1 = Number(endYear);

    if (!Number.isFinite(y0) || !Number.isFinite(y1)) return null;
    if (y0 > y1) [y0, y1] = [y1, y0];

    const start = new Date(Math.max(
      +new Date(`${Math.trunc(y0)}-01-01`),
      +dataMin
    ));

    const endOfYear = new Date(Date.UTC(Math.trunc(y1) + 1, 0, 1));
    endOfYear.setUTCDate(endOfYear.getUTCDate() - 1);

    const endData = new Date(Math.min(+endOfYear, +dataMax));

    return [iso(start), iso(makePaddedEnd(start, endData))];
  }

  function updateActiveButtons(container, activeKey) {
    if (!container) return;

    container.querySelectorAll('.range-buttons .seg').forEach(btn => {
      const on = btn.dataset.range === activeKey;
      btn.classList.toggle('active', on);
      btn.classList.toggle('is-active', on);
      btn.setAttribute('aria-pressed', on ? 'true' : 'false');
    });
  }

  function createChart(options) {
    const chartId = options.chartId;
    const chartEl = document.getElementById(chartId);
    const aggEl = document.getElementById(options.aggId);
    const controlsEl = options.controlsEl || document;

    let activeRangeKey = options.defaultRange || DEFAULTS.defaultRange;
    let rememberedXRange = null;
    let renderedSeries = [];
    let dataMin = null;
    let dataMax = null;
    let initialRangeApplied = false;
    let fromYearEl = null;
    let toYearEl = null;
    let yearControlsEl = null;

    function styleYearSelect(select) {
      select.style.padding = '8px 10px';
      select.style.border = '1px solid #2b2f36';
      select.style.borderRadius = '10px';
      select.style.background = 'rgba(255,255,255,0.03)';
      select.style.color = 'inherit';
      select.style.fontSize = '0.95rem';
    }

    function ensureYearControls() {
      if (!controlsEl || yearControlsEl) return;

      const ranges = controlsEl.querySelector('.range-buttons');
      if (!ranges) return;

      yearControlsEl = document.createElement('div');
      yearControlsEl.className = 'jr-year-range-controls';
      yearControlsEl.style.display = 'flex';
      yearControlsEl.style.alignItems = 'center';
      yearControlsEl.style.gap = '8px';
      yearControlsEl.style.flexWrap = 'wrap';

      const fromLabel = document.createElement('label');
      fromLabel.textContent = 'From:';
      fromLabel.style.fontSize = '0.95rem';

      fromYearEl = document.createElement('select');
      fromYearEl.setAttribute('aria-label', 'Start year');
      styleYearSelect(fromYearEl);

      const toLabel = document.createElement('label');
      toLabel.textContent = 'To:';
      toLabel.style.fontSize = '0.95rem';

      toYearEl = document.createElement('select');
      toYearEl.setAttribute('aria-label', 'End year');
      styleYearSelect(toYearEl);

      fromLabel.appendChild(fromYearEl);
      toLabel.appendChild(toYearEl);
      yearControlsEl.appendChild(fromLabel);
      yearControlsEl.appendChild(toLabel);

      ranges.insertAdjacentElement('afterend', yearControlsEl);

      const applyCustom = () => {
        if (!dataMin || !dataMax || !fromYearEl || !toYearEl) return;

        let y0 = Number(fromYearEl.value);
        let y1 = Number(toYearEl.value);

        if (y0 > y1) {
          if (document.activeElement === fromYearEl) {
            toYearEl.value = String(y0);
            y1 = y0;
          } else {
            fromYearEl.value = String(y1);
            y0 = y1;
          }
        }

        const range = rangeFromYears(y0, y1, dataMin, dataMax);
        if (!range) return;

        activeRangeKey = 'CUSTOM';
        rememberedXRange = range;
        updateActiveButtons(controlsEl, activeRangeKey);

        Plotly.relayout(chartId, { 'xaxis.range': range })
          .then(() => adjustYToVisible(range[0], range[1]));
      };

      fromYearEl.addEventListener('change', applyCustom);
      toYearEl.addEventListener('change', applyCustom);
    }

    function populateYearControls() {
      if (!dataMin || !dataMax) return;

      ensureYearControls();
      if (!fromYearEl || !toYearEl) return;

      const minYear = dataMin.getFullYear();
      const maxYear = dataMax.getFullYear();

      const oldFrom = Number(fromYearEl.value);
      const oldTo = Number(toYearEl.value);

      fromYearEl.innerHTML = '';
      toYearEl.innerHTML = '';

      for (let y = minYear; y <= maxYear; y += 1) {
        const a = document.createElement('option');
        a.value = String(y);
        a.textContent = String(y);
        fromYearEl.appendChild(a);

        const b = document.createElement('option');
        b.value = String(y);
        b.textContent = String(y);
        toYearEl.appendChild(b);
      }

      const presetYear = /^\d{4}$/.test(String(activeRangeKey))
        ? Number(activeRangeKey)
        : minYear;

      const defaultFrom = Number.isFinite(oldFrom) && oldFrom >= minYear && oldFrom <= maxYear
        ? oldFrom
        : Math.max(minYear, Math.min(maxYear, presetYear));

      const defaultTo = Number.isFinite(oldTo) && oldTo >= minYear && oldTo <= maxYear
        ? oldTo
        : maxYear;

      fromYearEl.value = String(defaultFrom);
      toYearEl.value = String(defaultTo);
    }

    function syncYearControlsToPreset(key) {
      if (!fromYearEl || !toYearEl || !dataMin || !dataMax) return;

      const minYear = dataMin.getFullYear();
      const maxYear = dataMax.getFullYear();

      let startYear = minYear;
      if (/^\d{4}$/.test(String(key))) {
        startYear = Math.max(minYear, Math.min(maxYear, Number(key)));
      } else if (key === 'YTD') {
        startYear = maxYear;
      } else if (key === 'LY') {
        startYear = Math.max(minYear, maxYear - 1);
      }

      fromYearEl.value = String(startYear);
      toYearEl.value = String(maxYear);
    }

    function makeTrace(item, index) {
      const series = item.series;
      const colour = item.colour || SERIES_COLOURS[index % SERIES_COLOURS.length];

      return {
        type: 'scatter',
        name: item.name || '',
        x: series.map(p => p.x),
        y: series.map(p => p.y),
        customdata: series.map(p => item.customData ? item.customData(p) : null),
        mode: 'lines',
        line: {
          width: options.lineWidth || DEFAULTS.lineWidth,
          shape: 'linear',
          color: colour
        },
        hovertemplate: item.hovertemplate ||
          '%{x|%d %b %Y}<br>%{y:.0f} Rating<extra>' + (item.name || '') + '</extra>',
        connectgaps: true
      };
    }

    function adjustYToVisible(x0, x1) {
      const lo = new Date(x0);
      const hi = new Date(x1);
      const values = [];

      for (const item of renderedSeries) {
        for (const p of item.series) {
          const d = new Date(p.x);

          if (d >= lo && d <= hi && Number.isFinite(p.y)) {
            values.push(p.y);
          }
        }
      }

      if (!values.length) return;

      const min = Math.min(...values);
      const max = Math.max(...values);
      const pad = (max - min) * 0.12 || 10;
      const y0 = min - pad;
      const y1 = max + pad;
      const step = labelStepForRange(y0, y1);

      Plotly.relayout(chartId, {
        'yaxis.range': [y0, y1],
        'shapes': makeYGridShapes(y0, y1, step)
      }).then(() => {
        setTickLabelsEvery(chartId, y0, y1, step);
      });
    }


    function renderHtmlLegend(items) {
      const legendId = `${chartId}-html-legend`;
      let legendEl = document.getElementById(legendId);

      if (items.length <= 1) {
        if (legendEl) legendEl.remove();
        return;
      }

      if (!legendEl) {
        legendEl = document.createElement('div');
        legendEl.id = legendId;
        chartEl.insertAdjacentElement('afterend', legendEl);
      }

      legendEl.style.display = 'flex';
      legendEl.style.flexWrap = 'wrap';
      legendEl.style.alignItems = 'center';
      legendEl.style.justifyContent = 'flex-start';
      legendEl.style.gap = '10px 18px';
      legendEl.style.padding = '12px 8px 4px';
      legendEl.style.fontSize = window.matchMedia('(max-width: 700px)').matches ? '15px' : '16px';
      legendEl.style.lineHeight = '1.25';
      legendEl.style.color = getComputedStyle(document.body).color;

      legendEl.innerHTML = '';

      items.forEach((item, index) => {
        const row = document.createElement('div');
        row.style.display = 'inline-flex';
        row.style.alignItems = 'center';
        row.style.gap = '7px';
        row.style.minWidth = '0';
        row.style.maxWidth = '100%';

        const swatch = document.createElement('span');
        swatch.style.display = 'inline-block';
        swatch.style.width = '28px';
        swatch.style.height = '4px';
        swatch.style.flex = '0 0 28px';
        swatch.style.background =
          item.colour || SERIES_COLOURS[index % SERIES_COLOURS.length];

        const label = document.createElement('span');
        label.textContent = item.name || '';
        label.style.whiteSpace = 'normal';
        label.style.overflowWrap = 'anywhere';

        row.appendChild(swatch);
        row.appendChild(label);
        legendEl.appendChild(row);
      });
    }

    async function render(rawItems) {
      const mode = aggEl ? aggEl.value : 'weekly';

      if (chartEl.textContent && chartEl.textContent.includes('Loading')) {
        chartEl.textContent = '';
      }

      const gd = chartEl;
      const liveLayout = gd && gd._fullLayout ? gd._fullLayout.xaxis : null;

      if (liveLayout && liveLayout.range) {
        rememberedXRange = [liveLayout.range[0], liveLayout.range[1]];
      }

      renderedSeries = rawItems
        .map(item => ({
          ...item,
          series: aggregate(item.raw || item.series || [], mode)
        }))
        .filter(item => item.series.length);

      if (!renderedSeries.length) {
        chartEl.textContent = 'No history found.';
        return;
      }

      const ext = extentFrom(renderedSeries.map(x => x.series));
      dataMin = ext.min;
      dataMax = ext.max;
      populateYearControls();

      const allY = renderedSeries
        .flatMap(item => item.series.map(p => p.y))
        .filter(Number.isFinite);

      const yMin0 = allY.length ? Math.min(...allY) : 0;
      const yMax0 = allY.length ? Math.max(...allY) : 1;
      const bg = getComputedStyle(document.body).backgroundColor;

      const traces = renderedSeries.map((item, index) => makeTrace(item, index));
      const isMobile = window.matchMedia('(max-width: 700px)').matches;

      const layout = {
        margin: isMobile
          ? { l: 48, r: 18, t: 10, b: 44 }
          : { l: 50, r: 28, t: 10, b: 40 },

        xaxis: {
          type: 'date',
          fixedrange: true,
          showgrid: false,
          rangeslider: { visible: false }
        },

        yaxis: {
          title: '',
          fixedrange: true,
          showgrid: false,
          zeroline: false,
          tick0: 0,
          dtick: 25,
          ticks: 'outside',
          ticklen: 4,
          tickmode: 'array',
          tickvals: [],
          ticktext: []
        },

        paper_bgcolor: bg,
        plot_bgcolor: bg,
        font: { color: getComputedStyle(document.body).color },
        showlegend: false,
        shapes: makeYGridShapes(yMin0, yMax0, 100)
      };

      if (!initialRangeApplied && dataMin && dataMax) {
        rememberedXRange = rangeFromKey(activeRangeKey, dataMin, dataMax);
        initialRangeApplied = true;
      }

      await Plotly.react(chartId, traces, layout, {
        staticPlot: false,
        scrollZoom: false,
        displayModeBar: false,
        responsive: true
      });

      renderHtmlLegend(renderedSeries);

      if (rememberedXRange) {
        const [x0, x1] = rememberedXRange;
        await Plotly.relayout(chartId, { 'xaxis.range': [x0, x1] });
        adjustYToVisible(x0, x1);
      }

      updateActiveButtons(controlsEl, activeRangeKey);

      if (!chartEl._jrHandlersAttached) {
        chartEl.on('plotly_relayout', ev => {
          const x0 = ev['xaxis.range[0]'] ?? (ev['xaxis.range'] && ev['xaxis.range'][0]);
          const x1 = ev['xaxis.range[1]'] ?? (ev['xaxis.range'] && ev['xaxis.range'][1]);

          if (x0 && x1) {
            rememberedXRange = [x0, x1];
            adjustYToVisible(x0, x1);
          }
        });

        chartEl._jrHandlersAttached = true;
      }
    }

    function setRange(key) {
      const range = rangeFromKey(key, dataMin, dataMax);
      if (!range) return;

      activeRangeKey = key;
      rememberedXRange = range;

      updateActiveButtons(controlsEl, activeRangeKey);
      syncYearControlsToPreset(key);

      Plotly.relayout(chartId, { 'xaxis.range': range })
        .then(() => adjustYToVisible(range[0], range[1]));
    }

    controlsEl.querySelectorAll('.range-buttons .seg').forEach(btn => {
      btn.addEventListener('click', () => setRange(btn.dataset.range));
    });

    if (aggEl) {
      aggEl.addEventListener('change', () => {
        if (typeof options.getItems === 'function') {
          render(options.getItems());
        }
      });
    }

    updateActiveButtons(controlsEl, activeRangeKey);

    return {
      render,
      setRange,
      getItems: () => renderedSeries
    };
  }

  window.JRGraph = {
    normalise,
    aggregate,
    createChart
  };
})();