function renderLeagueStandings(root, leagues) {
  const style=document.createElement('style');style.textContent='.league-table-wrap{overflow:auto;max-width:1000px}.league-table{width:100%;border-collapse:collapse;font-size:17px}.league-table th,.league-table td{padding:10px 14px;border-bottom:1px solid #394252;text-align:center}.league-table td:nth-child(2),.league-table th:nth-child(2){text-align:left}.league-table tbody tr:nth-child(even){background:#171e29}.league-table td:first-child{font-weight:bold}.league-round-picker{background:#202735;color:#dce2eb;border:1px solid #536174;border-radius:6px;font:inherit;padding:9px 12px;margin:10px 0}.league-games .rounds{display:block}.league-games .round{width:auto;padding:0;background:none}.league-games .round h3{display:none}.league-games .round:not([hidden]){display:grid;grid-template-columns:repeat(auto-fill,minmax(320px,1fr));gap:14px}.league-games .game{margin:0}.league-games [hidden]{display:none!important}';document.head.append(style);
  leagues.forEach(league=>{
    const section=document.createElement('section');section.className='stage';
    const heading=document.createElement('h2');heading.textContent=league.name+' standings';section.append(heading);
    const wrap=document.createElement('div');wrap.className='league-table-wrap';
    const table=document.createElement('table');table.className='league-table';const head=document.createElement('thead'),tr=document.createElement('tr');
    ['Position','Team','Points','Board wins','W–L–D'].forEach(label=>{const th=document.createElement('th');th.scope='col';th.textContent=label;tr.append(th)});head.append(tr);table.append(head);
    const body=document.createElement('tbody');[...league.teams].sort((a,b)=>(a.rank??Infinity)-(b.rank??Infinity)).forEach(team=>{
      const row=document.createElement('tr');[team.rank??'–',team.name.replace(/^Team\s+/,''),team.points,team.boardWins,team.record].forEach(value=>{const td=document.createElement('td');td.textContent=value||'–';row.append(td)});body.append(row);
    });table.append(body);wrap.append(table);section.append(wrap);root.append(section);
  });
}
function simplifyLeagueGames(section, columns) {
  const style=document.createElement('style');style.textContent='.league-games .round:not([hidden]){display:block}.league-fixture{border:1px solid #394252;border-radius:8px;background:#171e29;margin:12px 0}.league-fixture>summary{cursor:pointer;font-size:18px;font-weight:600;padding:16px}.fixture-boards{display:grid;grid-template-columns:repeat(auto-fill,minmax(320px,1fr));gap:12px;padding:12px}.fixture-score{color:#b7d7ff;margin:0 12px}';document.head.append(style);
  section.classList.add('league-games');section.open=false;
  const picker=document.createElement('select');picker.className='league-round-picker';picker.setAttribute('aria-label','Select round');
  const rounds=[...columns.children];rounds.sort((a,b)=>(parseInt(b.querySelector('h3').textContent)||0)-(parseInt(a.querySelector('h3').textContent)||0));
  rounds.forEach(round=>{
    const fixtures=new Map();
    [...round.querySelectorAll(':scope > .game')].forEach(card=>{
      const f=card.fixture;if(!f)return;
      if(!fixtures.has(f.id)) {
        const fixture=document.createElement('details');fixture.className='league-fixture';
        const summary=document.createElement('summary');summary.append(document.createTextNode(f.teamA.replace(/^Team\s+/,'')));
        const score=document.createElement('span');score.className='fixture-score';score.textContent=f.score.replace('-','–');summary.append(score,document.createTextNode(f.teamB.replace(/^Team\s+/,'')));
        const boards=document.createElement('div');boards.className='fixture-boards';fixture.append(summary,boards);round.append(fixture);fixtures.set(f.id,boards);
      }
      fixtures.get(f.id).append(card);
    });
  });
  rounds.forEach((round,i)=>{const option=document.createElement('option');option.value=i;option.textContent=round.querySelector('h3').textContent;picker.append(option)});
  const choose=()=>rounds.forEach((round,i)=>round.hidden=i!==Number(picker.value));picker.addEventListener('change',choose);choose();section.insertBefore(picker,columns);
}
