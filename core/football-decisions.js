/* Shared competition outcome. Match scores and Elo results remain separate. */
(function(root){
  function scores(match){
    if(match.homeScore!=null&&match.awayScore!=null)return [Number(match.homeScore),Number(match.awayScore)];
    const pair=String(match.score||'').match(/^(\d+)\s*[-–]\s*(\d+)/);
    return pair?[Number(pair[1]),Number(pair[2])]:null;
  }
  function shootoutWinner(match){
    const pair=scores(match);
    if(!pair||pair[0]!==pair[1]||match.played===false||match.upcoming||match.status==='scheduled')return null;
    if(match.homePenalties!=null&&match.awayPenalties!=null){
      const h=Number(match.homePenalties),a=Number(match.awayPenalties);
      if(Number.isFinite(h)&&Number.isFinite(a)&&h!==a)return h>a?match.home:match.away;
    }
    return match.decision==='penalties'&&[match.home,match.away].includes(match.winner)?match.winner:null;
  }
  function winner(match){
    if(!match||match.played===false||match.upcoming||match.status==='scheduled')return null;
    const pair=scores(match);if(!pair)return null;
    if(pair[0]>pair[1])return match.home;
    if(pair[1]>pair[0])return match.away;
    return shootoutWinner(match);
  }
  function renderScore(element,match,separator='–'){
    const pair=scores(match);if(!pair)return;
    const decided=shootoutWinner(match);element.replaceChildren();
    pair.forEach((goals,index)=>{
      if(index)element.append(document.createTextNode(separator));
      const score=document.createElement('span');score.textContent=goals;
      if(decided)score.style.color=decided===(index?match.away:match.home)?'#32d583':'#ff6464';
      element.append(score);
    });
    if(decided)element.title=match.homePenalties!=null?`Penalties ${match.homePenalties}–${match.awayPenalties}`:'Decided on penalties';
  }
  root.JRFootballDecisions={winner,shootoutWinner,renderScore};
})(typeof window==='undefined'?globalThis:window);
