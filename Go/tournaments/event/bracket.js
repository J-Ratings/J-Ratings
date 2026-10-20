/* A source-defined graph: no round-size, seeding or tournament-name assumptions. */
function createTournamentSection(root, name, open) {
  const section=document.createElement('details');section.className='stage';section.open=open;
  const heading=document.createElement('summary');heading.textContent=name;section.append(heading);root.append(section);
  return section;
}
function renderTournamentBrackets(root, brackets, appearances, highlight, openByDefault=brackets.length===1) {
  const style=document.createElement('style');
  style.textContent=`.bracket-scroll{overflow:auto;border:1px solid #303947;border-radius:10px}.bracket-canvas{position:relative}.bracket-band{position:absolute;top:0;bottom:0;background:#171e29;border-radius:8px}.bracket-band:nth-child(even){background:#202735}.bracket-label{position:absolute;top:14px;width:370px;text-align:center;font-weight:bold}.bracket-card{position:absolute;width:350px;margin:0;z-index:2;box-sizing:border-box}.bracket-lines{position:absolute;inset:0;pointer-events:none}.bracket-lines path{fill:none;stroke:#56657a;stroke-width:2}.series-game{border-top:1px solid #394252}.series-label,.feed-note{padding:6px 10px;color:#b0bdce;font-size:12px}.series-total{font-size:12px;padding:6px 10px}.pending-score{color:#b0bdce}`;
  root.appendChild(style);
  const allMatches=new Map(brackets.flatMap(b=>b.matches.map(m=>[m.id,m])));
  const matchBrackets=new Map(brackets.flatMap(b=>b.matches.map(m=>[m.id,b.name])));
  const playerKey=p=>p.id||p.name;
  function rows(card,g,scores) {
    (g.players||[]).forEach((p,i)=>{
      const row=document.createElement('div');row.className='player';
      if(p.colour==='B'||p.colour==='W') {const stone=document.createElement('span');stone.className='stone '+(p.colour==='B'?'black':'white');stone.title=p.colour==='B'?'Black':'White';row.append(stone)}
      if(p.flag){const flag=document.createElement('img');flag.className='flag';flag.src='../../../flags/'+encodeURIComponent(p.flag)+'.svg';flag.alt=p.flag.toUpperCase();row.append(flag)}
      const name=document.createElement(p.id?'a':'span');name.className='name';name.textContent=p.name;
      if(p.id)name.href='../../player/?id='+encodeURIComponent(p.id);row.append(name);
      if(p.rating!=null){const rating=document.createElement('span');rating.className='rating';rating.textContent=p.rating;row.append(rating)}
      if(p.winPct!=null){const chance=document.createElement('span');chance.className='chance';chance.textContent=p.winPct+'%';row.append(chance)}
      const score=document.createElement('span');
      const completed=Boolean(g.winner)&&g.players.some(p=>p.name===g.winner);
      score.className='score '+(completed?(p.name===g.winner?'win':'loss'):'pending-score');
      score.textContent=scores?scores[i]:(completed?(p.name===g.winner?'1':'0'):'–');row.append(score);
      const key=playerKey(p);appearances.push({row,key});
      row.addEventListener('mouseenter',()=>highlight(key));row.addEventListener('mouseleave',()=>highlight(null));
      row.addEventListener('focusin',()=>highlight(key));row.addEventListener('focusout',()=>highlight(null));card.append(row);
    });
    const note=document.createElement('div');note.className='note';note.textContent=[g.date,g.result].filter(Boolean).join(' · ');if(note.textContent)card.append(note);
  }
  brackets.forEach(bracket=>{
    const section=createTournamentSection(root,bracket.name,openByDefault);
    const description=document.createElement('div');description.className='bracket-description';section.append(description);
    const badge=text=>{const pill=document.createElement('span');pill.className='pill';pill.textContent=text;description.append(pill)};
    badge('Knockout');
    const entrants=new Set(bracket.matches.flatMap(m=>m.directEntrants||[]).filter(n=>n&&!n.includes('previous match')));
    if(entrants.size)badge(entrants.size+' entrants');
    const lateEntrants=bracket.matches.filter(m=>m.column>Math.min(...bracket.matches.map(n=>n.column))).flatMap(m=>m.directEntrants||[]);
    if(lateEntrants.length)badge(lateEntrants.length+' late entrants / byes');
    const seriesFormats=[...new Set(bracket.matches.filter(m=>m.series?.length>1).map(m=>m.series.length+'|'+m.round))];
    seriesFormats.forEach(format=>{const [n,round]=format.split('|');badge((n==='3'?'Best-of-three':n==='5'?'Best-of-five':n+'-game series')+' '+round.toLowerCase())});
    const nav=document.createElement('nav');nav.className='bracket-nav';nav.setAttribute('aria-label',bracket.name+' rounds');section.append(nav);
    const scroll=document.createElement('div');scroll.className='bracket-scroll';
    const canvas=document.createElement('div');canvas.className='bracket-canvas';scroll.append(canvas);section.append(scroll);
    const columns=[...new Set(bracket.matches.map(m=>m.column))].sort((a,b)=>a-b);
    const cards=new Map();const positions=new Map();const local=new Set(bracket.matches.map(m=>m.id));
    const width=columns.length*400+24;canvas.style.width=width+'px';
    columns.forEach((col,i)=>{
      const band=document.createElement('div');band.className='bracket-band';band.style.backgroundColor=i%2?'#202735':'#171e29';band.style.left=(i*400+8)+'px';band.style.width='400px';canvas.append(band);
      const label=document.createElement('div');label.className='bracket-label';label.style.left=(i*400+24)+'px';
      label.textContent=[...new Set(bracket.matches.filter(m=>m.column===col).map(m=>m.round))].join(' / ');canvas.append(label);
      if(bracket.matches.some(m=>m.column===col&&m.redrawBefore)) {
        const boundary=document.createElement('span');boundary.className='pill';boundary.textContent='Redraw';nav.append(boundary);
      }
      const button=document.createElement('button');button.type='button';button.textContent=label.textContent;
      button.addEventListener('click',()=>scroll.scrollTo({left:i*400,behavior:'smooth'}));nav.append(button);
    });
    bracket.matches.forEach(m=>{
      const card=document.createElement('div');card.className='game bracket-card';card.dataset.matchId=m.id;card.id='match-'+m.id;
      rows(card,m,m.seriesScores);
      if(m.redrawBefore){const note=document.createElement('div');note.className='feed-note';note.textContent=m.drawPending?'New draw pending':'Pairings from new draw';card.append(note)}
      (m.feedsFrom||[]).filter(f=>!local.has(f.id)).forEach(f=>{
        const source=allMatches.get(f.id);if(!source)return;
        const note=document.createElement('div');note.className='feed-note';
        note.textContent=(f.player?f.player+' · ':'')+(f.outcome==='loser'?'Loser':'Winner')+' of '+matchBrackets.get(f.id)+' · '+source.round;
        card.append(note);
      });
      (m.series||[]).forEach((g,i)=>{const game=document.createElement('div');game.className='series-game';const label=document.createElement('div');label.className='series-label';label.textContent='Game '+(i+1);game.append(label);rows(game,g);card.append(game)});
      canvas.append(card);cards.set(m.id,card);
    });
    const svg=document.createElementNS('http://www.w3.org/2000/svg','svg');svg.classList.add('bracket-lines');canvas.append(svg);
    function layout(){
      if(!section.open)return;
      positions.clear();let maxBottom=0;
      columns.forEach((col,ci)=>{
        let bottom=60;
        bracket.matches.filter(m=>m.column===col).sort((a,b)=>a.row-b.row).forEach(m=>{
          const card=cards.get(m.id),height=card.offsetHeight;
          const sources=(m.feedsFrom||[]).map(f=>positions.get(f.id)).filter(Boolean);
          // Row geometry also positions direct entrants and late-round byes.
          let centre=bracket.matches.length===1?60+height/2:sources.length?sources.reduce((v,p)=>v+p.y+p.h/2,0)/sources.length:60+(m.row-Math.min(...bracket.matches.map(n=>n.row))+m.span/2)*85;
          const y=Math.max(bottom,centre-height/2),x=ci*400+24;
          card.style.left=x+'px';card.style.top=y+'px';positions.set(m.id,{x,y,h:height});bottom=y+height+18;maxBottom=Math.max(maxBottom,bottom);
        });
      });
      canvas.style.height=(maxBottom+20)+'px';svg.setAttribute('width',width);svg.setAttribute('height',maxBottom+20);svg.replaceChildren();
      bracket.matches.forEach(m=>(m.feedsFrom||[]).forEach(f=>{
        const a=positions.get(f.id),b=positions.get(m.id);if(!a||!b)return;
        const path=document.createElementNS(svg.namespaceURI,'path');const x=a.x+350,y=a.y+a.h/2,end=b.y+b.h/2,mid=(x+b.x)/2;
        path.setAttribute('d',`M ${x} ${y} H ${mid} V ${end} H ${b.x}`);svg.append(path);
      }));
    }
    section.addEventListener('toggle',layout);layout();new ResizeObserver(layout).observe(scroll);
  });
}
