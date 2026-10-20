/* Tournament-only history, shared by dedicated and generic tournament pages. */
function setupTournamentPlayerHistory(data) {
  const style=document.createElement('style');
  style.textContent=`.tournament-history{text-align:center;background:#171e29;color:#dce2eb;border:1px solid #536174;border-radius:12px;padding:24px;width:min(1100px,calc(100vw - 64px));font-size:20px;max-height:80vh}.tournament-history::backdrop{background:rgba(0,0,0,.7)}.history-header{position:relative;display:block;padding:0 90px;min-height:44px;text-align:center}.history-header h2{margin:0;text-align:center;font-size:1.8rem}.history-close{position:absolute;top:0;right:0;background:#202735;color:#dce2eb;border:1px solid #536174;border-radius:6px;padding:8px 12px;cursor:pointer;font:inherit}.history-subtitle{color:#b0bdce}.history-table-scroll{overflow:auto}.history-table{width:100%;border-collapse:collapse;font-size:22px}.history-table th,.history-table td{text-align:center;padding:10px 8px;border-bottom:1px solid #394252;vertical-align:top}.history-table th{color:#b0bdce}.history-table .history-number{white-space:nowrap;font-variant-numeric:tabular-nums}.history-table .won{color:#32d583;font-weight:700}.history-table .lost{color:#ff6464;font-weight:700}.history-profile{display:inline-block;margin-top:16px}.history-profile[hidden]{display:none}.final-positions-button{display:block;background:#30291c;color:#ead39a;border:1px solid #8e7541;border-radius:6px;padding:10px 16px;margin:10px auto;font:inherit;font-weight:600;cursor:pointer}.final-positions-button:hover{background:#403521}.position-rank{font-weight:800}.medal-rank{display:inline-block;background:linear-gradient(160deg,var(--metal-dark) 0%,var(--metal-mid) 32%,var(--metal-light) 46%,var(--metal-mid) 57%,var(--metal-dark) 100%);background-clip:text;-webkit-background-clip:text;color:transparent;filter:drop-shadow(0 1px 1px rgba(0,0,0,.55))}.medal-gold{--metal-dark:#aa7413;--metal-mid:#e2ac32;--metal-light:#fff0b0}.medal-silver{--metal-dark:#7d8c9f;--metal-mid:#b7c4d4;--metal-light:#f8fcff}.medal-bronze{--metal-dark:#9a4e23;--metal-mid:#cf8445;--metal-light:#ffcf99}.name,.player-name{cursor:pointer}`;
  document.head.append(style);
  const games=[];
  const add=(game,stage,round)=>{
    if(game.bye||(game.players||[]).length!==2)return;
    games.push({...game,stage,round});
  };
  if(data.matches) {
    data.matches.forEach(m=>{
      const round=data.rounds?.[m.round-1]||m.round;
      if(m.series?.length)m.series.forEach((g,i)=>add(g,'',round+' · Game '+(i+1)));
      else add(m,'',round);
    });
  } else {
    const consumed=new Set(data.bracketGameIndices||[]);
    (data.games||[]).forEach((g,i)=>{if(!consumed.has(i))add(g,g.stage,g.round)});
    (data.brackets||[]).forEach(b=>b.matches.forEach(m=>{
      if(m.series?.length)m.series.forEach((g,i)=>add(g,b.name,m.round+' · Game '+(i+1)));
      else add(m,b.name,m.round);
    }));
  }
  const players=new Map();games.forEach(g=>g.players.forEach(p=>{
    if(!/^(Winner of|Loser of|Draw pending|To be determined)/i.test(p.name))players.set(p.name,p);
  }));
  (data.sourceStandings||[]).forEach(s=>s.entries.forEach(e=>players.set(e.player.name,e.player)));
  const dialog=document.createElement('dialog');dialog.className='tournament-history';dialog.setAttribute('aria-labelledby','history-player-title');
  const header=document.createElement('div');header.className='history-header';
  const title=document.createElement('h2');title.id='history-player-title';
  const close=document.createElement('button');close.type='button';close.className='history-close';close.textContent='Close';close.addEventListener('click',()=>dialog.close());header.append(title,close);
  const subtitle=document.createElement('p');subtitle.className='history-subtitle';subtitle.textContent=data.name;
  const content=document.createElement('div');content.className='history-table-scroll';
  const profile=document.createElement('a');profile.className='history-profile';profile.textContent='Full player profile';
  dialog.append(header,subtitle,content,profile);document.body.append(dialog);
  dialog.addEventListener('click',e=>{const r=dialog.getBoundingClientRect();if(e.target===dialog&&(e.clientX<r.left||e.clientX>r.right||e.clientY<r.top||e.clientY>r.bottom))dialog.close()});
  const positionsButton=document.createElement('button');positionsButton.type='button';positionsButton.className='final-positions-button';positionsButton.textContent='Final positions';
  const positionAnchor=document.getElementById('round-nav')||document.getElementById('stages');
  positionAnchor?.before(positionsButton);
  if(!positionsButton.isConnected)document.getElementById('title').after(positionsButton);
  positionsButton.addEventListener('click',()=>{
    title.textContent='Final positions';profile.hidden=true;content.replaceChildren();
    if(data.leagueTables?.length&&typeof renderLeagueStandings==='function') {
      title.textContent='Team standings';renderLeagueStandings(content,data.leagueTables);dialog.showModal();return;
    }
    const groups=data.matches?[{name:data.name,matches:data.matches.map(m=>({...m,round:data.rounds[m.round-1],column:m.round-1}))}]:(data.brackets||[]);
    const all=groups.flatMap(b=>b.matches);
    const multiplePaths=all.some(m=>(m.feedsFrom||[]).some(f=>f.outcome==='loser'));
    const overallFinal=groups.find(b=>b.name==='Championship final');
    const note=document.createElement('p');note.className='history-subtitle';note.textContent='Only supported positions are shown. Unfinished stages are provisional; tied positions share a rank.';content.append(note);
    // Count individual games, never aggregate series cards.
    const record=(name,matches)=>{
      const completed=matches.flatMap(m=>m.series?.length?m.series:[m]).filter(g=>g.winner&&g.players.some(p=>p.name===name));
      return [completed.filter(g=>g.winner===name).length,completed.filter(g=>g.winner!==name).length];
    };
    const combined=new Map();
    const ordinal=n=>n+((n%100>=11&&n%100<=13)?'th':({1:'st',2:'nd',3:'rd'}[n%10]||'th'));
    const collect=(label,matches,simple,published=[])=>{
      const known=new Map();matches.forEach(m=>(m.players||[]).forEach(p=>{if(players.has(p.name))known.set(p.name,p)}));
      published.forEach(e=>known.set(e.player.name,e.player));
      const standings=[...known.values()].map(p=>{
        const official=published.find(e=>e.player.name===p.name);
        if(official)return {p,rank:/Champion|Runner-up/.test(official.status)?official.rank:null,status:/Champion|Runner-up/.test(official.status)?official.status:'Completed league stage',active:false};
        const appearances=matches.filter(m=>m.players?.some(x=>x.name===p.name));
        const last=appearances.sort((a,b)=>b.column-a.column||(b.row??0)-(a.row??0))[0];
        let rank=null,status='Position unconfirmed';
        if(last?.winner) {
          const won=last.winner===p.name;
          if(/^final$/i.test(last.round)) {
            if(simple){rank=won?1:2;status=won?'Champion':'Runner-up'}
            else status=won?(/prelim|qualif/i.test(label)?'Qualified':'Stage winner'):'Eliminated in '+last.round;
          } else if(!won) {
            status='Eliminated in '+last.round;
            if(simple){rank=2+matches.filter(m=>!m.bye&&m.column>last.column).length}
          } else status='Advanced · '+last.round;
        } else if(last)status='Pending · '+last.round;
        if(last?.winner&&last.winner!==p.name&&all.some(m=>(m.feedsFrom||[]).some(f=>f.id===last.id&&f.outcome==='loser')))status='Moved to losers’ bracket';
        const placement=label.match(/^(\d+)(?:st|nd|rd|th) place decision match$/i);
        if(placement&&last?.winner){rank=last.winner===p.name?Number(placement[1]):null;status=rank!=null?ordinal(rank)+' place':'Placement match lost'}
        return {p,rank,status,active:/^(Pending|Advanced|Moved to)/.test(status),column:last?.column??-1};
      }).sort((a,b)=>Number(b.active)-Number(a.active)||(a.active&&b.active?b.column-a.column:0)||(a.rank??Infinity)-(b.rank??Infinity)||a.p.name.localeCompare(b.p.name));
      const priority=published.length?(/title/i.test(label)?4:3):/prelim|qualif/i.test(label)?0:/place decision/i.test(label)?1:2;
      standings.forEach(s=>{const previous=combined.get(s.p.name);if(!previous||priority>=previous.priority)combined.set(s.p.name,{...s,priority})});
    };
    const render=()=>{
      const standings=[...combined.values()].sort((a,b)=>Number(b.active)-Number(a.active)||(a.rank??Infinity)-(b.rank??Infinity)||b.priority-a.priority||a.p.name.localeCompare(b.p.name));
      const table=document.createElement('table');table.className='history-table';
      const head=document.createElement('thead'),hr=document.createElement('tr');
      ['Position','Player','Status','Wins','Losses','End Elo'].forEach(t=>{const th=document.createElement('th');th.scope='col';th.textContent=t;hr.append(th)});head.append(hr);table.append(head);
      const body=document.createElement('tbody');standings.forEach(s=>{
        const publishedStages=new Set((data.sourceStandings||[]).map(stage=>stage.name));
        const countable=groups.length?groups.filter(b=>!publishedStages.has(b.name)).flatMap(b=>b.matches):games.filter(g=>!publishedStages.has(g.stage));
        const row=document.createElement('tr'),counts=record(s.p.name,countable);
        (data.sourceStandings||[]).forEach(stage=>{const e=stage.entries.find(e=>e.player.name===s.p.name);if(e){if(Number.isFinite(e.wins))counts[0]+=e.wins;if(Number.isFinite(e.losses))counts[1]+=e.losses}});
        [s.rank==null?'–':ordinal(s.rank),s.p.name,s.status,...counts].forEach((value,i)=>{
          const cell=document.createElement('td');
          if(i===0)cell.className='position-rank';
          if(i===1){if(s.p.flag){const flag=document.createElement('img');flag.src='../../../flags/'+encodeURIComponent(s.p.flag)+'.svg';flag.alt=s.p.flag.toUpperCase();flag.style.cssText='width:20px;height:14px;vertical-align:middle;margin-right:7px';cell.append(flag)}const link=document.createElement(s.p.id?'a':'span');link.textContent=value;if(s.p.id)link.href='../../player/?id='+encodeURIComponent(s.p.id);link.style.cursor='pointer';link.addEventListener('click',e=>{if(e.ctrlKey||e.metaKey)return;e.preventDefault();dialog.close();open(s.p)});cell.append(link)}else if(i===0&&s.rank!=null&&s.rank<=3){const rank=document.createElement('span');rank.className='medal-rank medal-'+{1:'gold',2:'silver',3:'bronze'}[s.rank];rank.textContent=value;cell.append(rank)}else cell.textContent=value;
          row.append(cell);
        });const rating=document.createElement('td');rating.textContent='–';row.append(rating);body.append(row);
        const dates=[...games.map(g=>g.date),...(data.sourceStandings||[]).map(s=>s.asOf)].filter(d=>/^\d{4}-\d{2}-\d{2}$/.test(d||''));
        const end=dates.sort().at(-1);
        if(s.p.id&&end)fetch('../../data/games/'+encodeURIComponent(s.p.id)+'.json').then(r=>r.ok?r.json():[]).then(history=>{
          const played=history.filter(g=>g.date<=end).sort((a,b)=>b.date.localeCompare(a.date));
          if(played.length){const g=played[0];const elo=g.black===s.p.name?g.blackElo:g.white===s.p.name?g.whiteElo:null;const delta=Number(g.delta);if(elo!=null)rating.textContent=Math.round(Number(elo)+(Number.isFinite(delta)?delta:0))}
        }).catch(()=>{});
      });table.append(body);content.append(table);
    };
    (data.sourceStandings||[]).forEach(s=>collect(s.name,[],false,s.entries));
    if(groups.length)groups.forEach(b=>{
      const simple=(!multiplePaths||b===overallFinal)&&b.matches.filter(m=>/^final$/i.test(m.round)).length===1;
      collect(b.name,b.matches,simple);
    });
    else {
      collect('Tournament results',games.map((g,i)=>({...g,column:i})),false);
    }
    render();dialog.showModal();
  });
  function open(player) {
    title.textContent=player.name;content.replaceChildren();
    const table=document.createElement('table');table.className='history-table';
    const head=document.createElement('thead'),tr=document.createElement('tr');
    ['Date','Stage / round','Opponent','Opponent Elo','Win %','Result'].forEach(label=>{const th=document.createElement('th');th.scope='col';th.textContent=label;tr.append(th)});head.append(tr);table.append(head);
    const body=document.createElement('tbody');
    games.filter(g=>g.players.some(p=>p.name===player.name)).sort((a,b)=>{
      const key=g=>g.date|| (g.result?'9998':'9999');return key(a).localeCompare(key(b));
    }).forEach(g=>{
      const own=g.players.find(p=>p.name===player.name),opponent=g.players.find(p=>p.name!==player.name);
      const row=document.createElement('tr');
      const result=g.winner?(g.winner===player.name?'Win':'Loss')+(g.result?' · '+g.result:''):'Pending';
      const values=[g.date||'–',[g.stage,g.round].filter(Boolean).join(' · '),opponent?.name||'To be determined',opponent?.rating??'–',own.winPct!=null?own.winPct+'%':'–',result];
      values.forEach((value,i)=>{const cell=document.createElement('td');cell.textContent=value;if(i===3||i===4)cell.className='history-number';if(i===5&&g.winner)cell.className=g.winner===player.name?'won':'lost';row.append(cell)});body.append(row);
    });
    table.append(body);content.append(table);profile.hidden=!player.id;
    if(player.id)profile.href='../../player/?id='+encodeURIComponent(player.id);
    dialog.showModal();
  }
  document.querySelector('main').addEventListener('click',e=>{
    if(e.ctrlKey||e.metaKey||e.shiftKey||e.altKey||e.button!==0)return;
    const name=e.target.closest('.name,.player-name');if(!name)return;
    const player=players.get(name.textContent);if(!player)return;
    e.preventDefault();open(player);
  });
  // Unresolved professionals also have histories; make their name keyboard usable.
  document.querySelectorAll('main span.name,main span.player-name').forEach(name=>{
    if(!players.has(name.textContent))return;
    name.tabIndex=0;name.setAttribute('role','button');
    name.addEventListener('keydown',e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();open(players.get(name.textContent))}});
  });
}





