/* Link known event labels and confirmed games to a catalogue edition. */
(function (root) {
  const key = value => String(value || '').split('·')[0].normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '').toLowerCase()
    .replace(/\bkorean\b/g, 'korea').replace(/\bmeijin[ -]sen\b/g, 'meijin')
    .replace(/\bwomen['’]?s\b|\bfemale\b/g, 'women')
    .replace(/\b(?:china|chinese) (?:a league|weiqi league,? division a)\b/g, 'chinese a league')
    .replace(/[^a-z0-9]/g, '');
  const href = file => ({'wr-quzhou-lanke-3-.json': '../tournaments/lanke/?year=2025',
    'wr-quzhou-lanke-4-.json': '../tournaments/lanke/?year=2026'}[file] ||
    '../tournaments/event/?file=' + encodeURIComponent(file));
  function enrich(games, catalogue, gameLinks) {
    const byFile = new Map(catalogue.map(e => [e.file, e]));
    const byLabel = new Map();
    const remember = (label, file) => {
      const k = key(label); if (!k || k === 'goratings') return;
      if (!byLabel.has(k)) byLabel.set(k, new Set());
      byLabel.get(k).add(file);
    };
    for (const e of catalogue) remember(e.name, e.file);
    const matchGame = g => {
      const winner = g.result === '1-0' ? g.black : g.result === '0-1' ? g.white : '';
      if (!winner) return null;
      const pair = [g.black, g.white].sort();
      const file = gameLinks[[g.date, pair[0], pair[1], winner].join('|')];
      return typeof file === 'string' && byFile.has(file) ? file : null;
    };
    // A confirmed edition can teach the alternate label used in this profile.
    // Conflicting mappings stay a set and are never resolved by list order.
    for (const g of games) {
      const file = matchGame(g);
      if (file) remember(g.event || g.tournament, file);
    }
    for (const g of games) {
      const label = g.event || g.tournament || '';
      let file = matchGame(g);
      if (!file) {
        const matches = new Set(byLabel.get(key(label)) || []);
        if (/quzhou[ -]lanke/i.test(label)) {
          const international = /international/i.test(label);
          const canonical = key(label.replace(/\binternational\s*/i, ''));
          for (const e of catalogue) {
            const family = international ? '/news/wr/quzhou-lanke/' : '/news/cn/qc/';
            if (e.family === family && key(e.name) === canonical) matches.add(e.file);
          }
          // Domestic and international competitions share edition numbers.
          for (const candidate of matches) {
            const family = byFile.get(candidate)?.family;
            if (international && family === '/news/cn/qc/') matches.delete(candidate);
          }
        }
        if (matches.size === 1) file = [...matches][0];
      }
      if (file) {
        g.eventUrl = href(file);
        if (!label || label === 'GoRatings') g.event = byFile.get(file).name;
      }
    }
    return games;
  }
  root.JRTournamentLinks = {enrich};
  if (typeof module !== 'undefined' && module.exports) module.exports = {enrich};
})(typeof globalThis !== 'undefined' ? globalThis : this);
