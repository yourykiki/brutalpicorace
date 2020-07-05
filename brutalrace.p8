pico-8 cartridge // http://www.pico-8.com
version 29
__lua__
-- brutal pico race 1.0.4
-- @yourykiki

local scale,
      rdz--rb length
      =48,64
local frm,endfrm,nb_finish,
      nb_player,nb_ai,
      nb_player_and_ai,
      num_trk,
      spxmax,
      trklen,lastnss,nss,
      racepos,rd_info,
      nb_rb,--rb number
      road
      
-- player infos
local cam,
      ss,sss,--ship,shadow
      zz,zzz,--rot ss,sss
      ship,ctrl,ssp,sssp,rot,
      ird,--pl road indice
      spd,sdx,--,skyx
      height,yoffset,ctr,csy,
      ai_skill
      ={},
       {},{},
       {},{},
       {},{},{},{},{},
       {},
       {},{},
       128,{},{},128,
       0

local ship_info={
{name="aura  ",accel=2},
{name="blitz ",accel=2},
{name="astral",accel=3},
{name="surge ",accel=2},
{name="storm ",accel=3},
{name="titan ",accel=3},
{name="viper ",accel=2},
{name="blaze ",accel=2},
{name="tiger ",accel=1},
{name="raven ",accel=1}
}

function _init()
 cartdata"brutalpicorace"
	menuitem(1,"reset time",
  function()
   for i=0,24 do
    dset(i,21600)
   end
  end)
 scn_intro={
  init=init_intro,
  update=update_intro,
  draw=draw_intro
 }
 scn_game={
  init=init_game,
  update=update_game,
  draw=draw_game
 }
 start_scene(scn_intro)
end
function _update()
 scn.update()
end
function _draw()
 scn.draw()
end
function init_game()
 if nb_player==1 then
  music(10,0,12)
 else
  music"-1"
 end

 sfx"42"
 cls""
 frm,endfrm,nb_finish,racepos=
  0,0,0,{}
 init_all_road(
  parse_trk(num_trk))
 -- half-pipe
 rd_info={}
 for i=0,6 do
  local lof7,at=(24+i)*128,0
  local stp={
   dx=peek(0x2000+lof7),
   dy=peek(0x2001+lof7),
   sx=peek(0x2002+lof7),
   sy=peek(0x2003+lof7),
   fl=peek(0x2004+lof7)
  }
  stp.dst=sqrt(sqrdist(0,0,stp.dx,stp.dy))
  if i>2 then
   stp.dy,stp.sx,stp.fl,at=
    -stp.dy,-stp.sx,-stp.fl,1
  end
  stp.rotz=at+atan2(stp.dx,stp.dy)
  add(rd_info,stp)
 end 

 if nb_player==2 then
  height,nb_rb,cdz,
  rd_grd1,rd_grd2,rd_grd3=
   64,9,48,
   0.5,0.62,0.75
 else
  height,nb_rb,cdz,
  rd_grd1,rd_grd2,rd_grd3=
   128,17,32,
   0.58,0.68,0.75
 end
 csy=height
 --loading
 for p=1,nb_player_and_ai do
  loadship(p,nss[p]-1)
  initshipaccel(ship[p])
  -- human
  if p<=nb_player then
   cam[p],sdx[p],
   yoffset[p],ctr[p]=
    {x=0,y=128,z=0,flw=p},0,
    height*(p-1),
    {x=64,y=height/4}
  end
  --start on segment 1
  add(road[1].pl,p)
 end
end

function initshipaccel(shipp)
 val0,val1,
 shipp.accurv,
 shipp.r_acc,
 absc,coef=
  0,0,{},{},{0,25,50,100},1
 if (ai_skill>1) coef=1-(3-ai_skill)*0.2
  
 for j=1,#absc-1 do
  val1=peek_spr(0x2c+64*(shipp.accel),j*2)
  from,to=absc[j],absc[j+1]
  for i=from,to do
   shipp.accurv[i]=lerp(
    val0,val1,
    (i-from)/(to-from))*coef
   if not shipp.r_acc[
    shipp.accurv[i]\1] then
    shipp.r_acc[
     shipp.accurv[i]\1]=i
   end   
  end
  val0=val1
 end
 shipp.spxmax=9-shipp.accel*1.5
end

-- loading a ship
function loadss(nbss,ss,sss)
 local m,xmin,xmax,zmin,zmax=
  0,8,0,8,0
 for k=7,0,-1 do-- y
  for j=0,7 do-- x
   local shad=false
   for i=2,0,-1 do
    c=sget(8*i+j,k+nbss*8)
    if c~=0 then
     v,shad=
      {x=j-4,y=i-1,z=4-k,
       oz=4-k,c=c,pat=0},
      c~=10
     m+=1
     add(ss,v)
    end
   end
   --half z
   if shad and k%2==0 then
    v={x=j-4,y=0,z=4-k,
     oz=0,c=9,pat=1}
    add(sss,v)
    xmin,xmax,zmin,zmax=
     min(v.x,xmin),max(v.x,xmax),
     min(v.z,zmin),max(v.z,zmax)
   end
  end
 end
 -- /5 because half_width*scale
 local si=ship_info[nbss+1]
 si.mass,si.width,si.height=
  m,(xmax-xmin)/5,(zmax-zmin)/5
end

function update_cntdn(p,ctrlp,spdp,sspp)
 if frm>150 then
  if p<=nb_player then
   sspp.update=update_player
  else
   sspp.update=update_ai
  end
 end
end

function update_end(p,ctrlp,spd,ssp)
 ctrlp.left,ctrlp.right,ctrlp.boost,
  ctrlp.accel,ctrlp.brake,pbtn=
 false,false,false,false,false,p-1
 if p<=nb_player then
  if frm>endfrm+45 then
   local flw=cam[p].flw
   if (btnp(2,pbtn)) flw+=1
   if (btnp(3,pbtn)) flw-=1
   cam[p].flw=mid(1,flw,nb_player_and_ai)
  end
  snd_engine(pbtn,spd)
 end
 if ssp.hp<=0 then
   create_pspark(ssp,spd.z,0,0)
 end
end

function update_game()
 frm+=1
 for p=1,nb_player_and_ai do
  -- where is the spaceship ?
  local sspp,backcam,spdp
   =ssp[p],false,spd[p]
  if (sspp.z-4)>=trklen then
   sspp.z-=trklen
   backcam=true
   sspp.rlap+=1
  end
  --old ship location on road
  -- +1 for array access
  local oird=ird[p]+1
  local oldpl=road[oird].pl
  --new ship location
  ird[p]=get_ird(sspp)
  local irdp=ird[p]+1
  v=road[irdp]
  vn=road[irdp%#road+1]
  -- update road link
  if oird~=irdp then
   del(oldpl,p)
   add(v.pl,p)
  end
  -- update a player/ai
  spxmax=ship[p].spxmax
  --handle control
  sspp.update(p,ctrl[p],spdp,sspp)
  updateship(p,v,vn,sspp,rot[p],spdp,ctrl[p],ship[p])
  -- camera tracking
  for i=1,nb_player do
   local camp=cam[i]
   if camp and camp.flw==p then
    if (backcam) camp.z-=trklen
    --sky
    sdx[i]-=spdp.z*v.fx/32
    --set with 30fps 10/30
    camp.x,camp.y,camp.z=
     lerp(camp.x,ssp[p].x,0.3333333),
     14+lerp(camp.y,ssp[p].y,0.3333333),
     lerp(camp.z,ssp[p].z-cdz,0.5)
   end
  end
 end
 --if (frm%15==0)
  calcrank()
 update_particles(p_spark)
 update_particles(p_boost)
 update_particles(p_trail)
 race_end()
end

function calcrank()
 local ranks={}
 for p=1,nb_player_and_ai do
  ranks[p]={
   key=ssp[p].z+ssp[p].rlap*trklen,
   p=p
  }
 end
 shellsort(ranks)
 for p=1,nb_player_and_ai do
  ssp[ranks[p].p].rank=
   nb_player_and_ai-p+1
 end
end

function race_end()
 if nb_finish>=nb_player then
  if endfrm==0 then
   endfrm=frm
  elseif frm>endfrm+150 
   and btnp(4) then
    start_scene(scn_intro)
  end
 end
end

-- road index
function get_ird(pos)
 -- 4 offset for shadow
 return ((pos.z-4)\rdz)%#road
end

function update_ai(p,ctrlp,spd,sspp)
 -- +skill=+decision
 if rnd"10">6-ai_skill then
  -- where to go ?
  local xmin,xmax=
   sspp.xmin,sspp.xmax
  local v=road[ird[p]+1]
  if v.t&2==2 
   and v.fx<-2 then
   xmin,xmax=58,70
  elseif v.t&8==8
   and v.fx>2 then
   xmin,xmax=-70,-58
  end
  -- avoid other ships
  for pn=1,nb_player_and_ai do
   local irdist=ird[pn]-ird[p]
   if p~=pn and irdist>=0
     and irdist<=2
     and ssp[pn].z>sspp.z
     and is_collide(sspp,ssp[pn],
      ship[p],ship[pn],0) then
    local dx=sgn(sspp.xflat
     -ssp[pn].xflat)*10
    xmin,xmax=xmin+dx,xmax+dx
    sspp.xmin,sspp.xmax=xmin,xmax
   end
  end
  -- correcting position
  if sspp.xflat<xmin then
   ctrlp.right,ctrlp.left=
    true,false 
  elseif sspp.xflat>xmax then
   ctrlp.right,ctrlp.left=
    false,true
  else
   ctrlp.right,ctrlp.left=
    false,false
  end
  --adaptive boost among 1st player
  if rnd"32">30-ai_skill+ssp[1].rank
    and abs(v.fx)<8 then
   if ctrlp.cdn_bst==10 then
    ctrlp.boost=false
   elseif ctrlp.can_bst then
    ctrlp.boost=true
   end
  end
 end
 -- accel,decel
 if spd.prc<=55+ai_skill*10 then
  ctrlp.accel=true
 else
  ctrlp.accel=rnd"10">5-ai_skill
 end
 
 -- race finished
 if is_race_ended(sspp) then
  sspp.update=update_end
  if (sspp.hp>0) add(racepos,p)
 end
 sspp.frm+=1
end


function update_player(p,ctrlp,spd,ssp)
 -- left / right
 local pbtn=p-1
 ctrlp.left,ctrlp.right,ctrlp.boost,
 ctrlp.accel,ctrlp.brake=
  btn(0,pbtn),btn(1,pbtn),
  btn(2,pbtn) and ctrlp.can_bst,
  btn(4,pbtn),btn(5,pbtn)
 -- sfx
 if ctrlp.cdn_bst>0 then
  sfx(46,pbtn*2,mid(0,ctrlp.cdn_bst,9)*2,2)
 else
  snd_engine(pbtn,spd)
 end
 ssp.frm+=1
 
 -- race finished
 if is_race_ended(ssp) then
  ssp.update=update_end
  nb_finish+=1
  if ssp.hp>0 then
   add(racepos,p)
   -- update time
   local iscr=num_trk*4+ai_skill-1
   if ssp.frm<dget(iscr) 
    or dget(iscr)==0 then
    dset(iscr,ssp.frm)
   end
  else
   sfx"63"
  end
 end
end

function is_race_ended(sspp)
 return sspp.rlap>2 
    and sspp.z>256
     or sspp.hp<=0
end

function updateship(p,v,vn,sspp,
  rot,spdp,ctrlp,shipp)
 if ctrlp.left then
  spdp.x=lerp(spdp.x,-spxmax,0.2)
 end
 if ctrlp.right then
  spdp.x=lerp(spdp.x,spxmax,0.2)
 end
 if not ctrlp.left and
    not ctrlp.right then
  spdp.x=lerp(spdp.x,0,0.2) 
 end
 if ctrlp.boost then
  -- charging
  ctrlp.cdn_bst=min(10,
    ctrlp.cdn_bst+recover_cdn(sspp))
 elseif not ctrlp.boost
  and ctrlp.cdn_bst>0 then
  -- applying
  if (p<=nb_player) sfx(45,(p-1)*2)
  spdp.bz+=ctrlp.cdn_bst*0.5
  ctrlp.cdn_bst,ctrlp.can_bst=
   -ctrlp.cdn_bst*2,
   false
 else 
  local bef=ctrlp.can_bst
  ctrlp.cdn_bst=min(0,
   ctrlp.cdn_bst+recover_cdn(sspp))
  ctrlp.can_bst=not btn(2,p-1) and ctrlp.cdn_bst>=0
  if not bef and ctrlp.can_bst
    and p<=nb_player then sfx"49"
  end
 end
 spdp.bz=max(0,spdp.bz-0.025)
 local dy=(v.y-vn.y)/8
 if ctrlp.accel then
  spdp.prc,spdp.gz=
   min(spdp.prc+0.25,100),
   lerp(spdp.gz,dy,0.1)
 end
 if ctrlp.brake then
  spdp.prc,spdp.gz=
   max(spdp.prc-1,0),
   lerp(spdp.gz,0,0.1)
 end
 if not ctrlp.accel and
    not ctrlp.brake then
  spdp.z=sgn(spdp.z)
   *max(0,abs(spdp.z)-0.2)
  spdp.prc=get_r_accel(
   shipp.r_acc,spdp.z)
  spdp.gz=lerp(spdp.gz,dy,0.05)
 else
  spdp.z=getaccel(shipp.accurv,spdp.prc)
 end

 -- epsilon
 if (abs(spdp.gz)<0.1) spdp.gz=0
 -- new pos,spd gz and boost z
 local spdpz=spdp.z+spdp.gz+spdp.bz
 sspp.z=sspp.z
  +sin(atan2(spdp.x,spdpz))
   *spdpz*sgn(spdpz)
 sssp[p].z=sspp.z
 -- speed fx
 if spdpz>12 and frm%10==0 then
  p_boost.nb=flr(spdpz-11)*1.5
  create_particles(p_boost,
   sspp,spdp,p)
 end
 -- move ship left/right
 -- phx pseudo phy
 local phx=
  -(1*spdpz/getaccel(shipp.accurv,100)
     *v.fx*0.78)
 sspp.xflat+=spdp.x+phx+spdp.gx
 -- ship col
 for pn=1,nb_player_and_ai do
  if p~=pn and
    abs(ird[p]-ird[pn])<=1 then
   --close enough for testing
   local shippn,ssppn,spdpn=
    ship[pn],ssp[pn],spd[pn]
   -- collision ?
   if is_collide(sspp,ssppn,
     shipp,shippn,1) then
    -- response
    local resx,resz,sgnx,mn_m=
     spdp.x+phx/2,
     (spdp.z+spdpn.z)/2*0.9,
     sgn(sspp.xflat-ssppn.xflat),
     shippn.mass/shipp.mass
    -- ship mass
    sspp.xflat-=resx
    sspp.z-=resz
    spdp.x+=abs(spxmax)*sgnx*mn_m
    spdp.z=resz*mn_m
    spdp.prc=get_r_accel(
     shipp.r_acc,spdp.z)
    --ship pn
    ssppn.xflat+=resx
    ssppn.z+=resz
    spdpn.x-=abs(shippn.spxmax)*sgnx/mn_m
    spdpn.z=resz/mn_m
    spdpn.prc=get_r_accel(
     shippn.r_acc,spdpn.z)
    -- fx with cooldown
    if frm-spdp.cdn>10 then 
     sspp.hp-=getdamage()
     ssppn.hp-=getdamage()
    end
    -- fx
    if not isnotvisible(sspp,cam[1],0) then
     sfx"43"
    end
    create_pspark(
     {x=(sspp.x+ssppn.x)/2,
      y=(sspp.y+ssppn.y)/2,
      z=(sspp.z+ssppn.z)/2},
     (spdp.z+spdpn.z)/2,
     0,0
    )
   end
  end
 end
 --progress on rb
 local prc=(sspp.z-v.z)/rdz
 local xtmp,ytmp=
  lerp(v.x,vn.x,prc),
  lerp(v.y,vn.y,prc)
 handle_road_collision(sspp,v,vn,xtmp,spdp,shipp,prc)
 --prepare for rendering
 sssp[p].y=ytmp
 -- convert flat to "real" x,y
 convert_xflat_to_x(sspp,xtmp,ytmp,rot,spdp,shipp)
 rot.y=spdp.x/64
 
 local mat=calcrotmat(rot)
 zz[p],zzz[p]=
  applyrot(ss[p],mat,p<=nb_player),
  applyrot(sss[p],mat,false)
 -- shadow
 -- token sssp[p] ==> ssspp en param
 sssp[p].xflat=sspp.xflat
 convert_xflat_to_x(sssp[p],xtmp,ytmp,rot)
 sssp[p].y+=ytmp
end

function handle_road_collision(
  sspp,v,vn,xtmp,spdp,shipp,prc)

 local xl,xr=v.x-56,v.x+56
 local l1,l2,l3,r1,r2,r3=
  xl-35.7,xl-80.9,xl-162.4,
  xr+35.7,xr+80.9,xr+162.4

 local lborx,rborx=l1,r1
 
 if v.t&4==4 then
  if vn.t&8==8 
   and v.t&8==0 then
   lborx=lerp(l1,l2,prc)
  end
  if vn.t&2==2 
   and v.t&2==0 then
   rborx=lerp(r1,r2,prc)
  end
 end
 
 lborx=road_part_collision(
  l1,l2,l3,prc,8,16,lborx)
 rborx=road_part_collision(
  r1,r2,r3,prc,2,1,rborx)
 lborx=road_part2_collision(
  l2,l3,prc,16,lborx)
 rborx=road_part2_collision(
  r2,r3,prc,1,rborx)

 lborx+=shipp.width*8
 rborx-=shipp.width*8
 
 --handle collision
 sspp.xflat=mid(lborx,sspp.xflat,rborx)
 local coll=0
 if sspp.xflat==lborx then
  spdp.x=abs(spdp.x)/2
  coll=-1
 end
 if sspp.xflat==rborx then
  spdp.x=-abs(spdp.x)/2
  coll=1
 end
 -- collision common
 if coll~=0 then
  if frm-spdp.cdn>10 then 
   spdp.z*=0.85
   sspp.hp-=getdamage()
   spdp.prc,spdp.cdn,
   sspp.xmin,sspp.xmax=
    get_r_accel(
     shipp.r_acc,spdp.z),
    frm,-5,5
   if not isnotvisible(sspp,cam[1],0) then
    sfx"44"
   end
  end
  -- fx
  create_pspark(sspp,spdp.z,
   rot.z,coll)
 end
end

function create_pspark(sspp,
  spdpz,rz,coll)
 create_particles(p_spark,
  {x=sspp.x+coll*cos(rz)*8,
   y=sspp.y+sin(rz)*8,
   z=sspp.z},{z=spdpz*0.8})
end

function applyrot(ss,m,sort)
 -- faster 0.02cpu but +30 tokens
 local xx,xy,xz,
       yx,yy,yz,
       zx,zy,zz,rr
          =m.xx,m.xy,m.xz,
           m.yx,m.yy,m.yz,
           m.zx,m.zy,m.zz,
           {}

 for v in all(ss) do
   local r={}
   r.x,r.y,r.z,
   r.c,r.pat=
    xx*v.x+xy*v.y+xz*v.z,
    yx*v.x+yy*v.y+yz*v.z,
    zx*v.x+zy*v.y+zz*v.z,
    v.c,v.pat
   r.key=r.z
   add(rr,r)
 end
 -- z sort
 if (sort) shellsort(rr)
 return rr
end

function init_all_road(trk)
 local i,h=0,0
 trklen,road=0,{}
 for t in all(trk) do
  local len,hgt,hln,
    alp,last_h=
   t.len or 16,
   t.hgt or 0,
   t.hln or 1,
   0,h
  for j=1,len do
   v={x=0,y=0,z=i*rdz,
    fx=t.curv,c=13,t=4,
    pl={},parts={}}
   --curvy road
   local fx=v.fx
   if i==4 then
    v.t=0x0104 --start+flat
   elseif fx>4 then
    v.t=0x001c --border left
   elseif fx<-4 then
    v.t=0x0007 --border right
   elseif fx>2 then
    v.t=0x000c --li'l border left
   elseif fx<-2 then
    v.t=0x0006 --li'l border right
   end 
   -- force road type
   if (t.frc) v.t=t.frc
   -- height
   if hgt~=0 then
    alp+=1
    h=last_h+(1-sin(-alp/(len*hln)-0.25))*hgt
   end
   v.y=h
   -- colors
   if i%2==0 then 
    v.c=5
   end
   add(road,v)
   trklen+=rdz
   i+=1
  end
 end
end

function draw_game()
 for p=1,nb_player do
  -- clip/camera
  local h=height*(p-1)
  clip(0,h+(p-1),128,height)
  camera(0,-h)
  -- sky
  local flw=cam[p].flw
  local sdy=(ctr[p].y+ssp[flw].y/16)\1
  rectfill(0,0,128,sdy-1,skyc1)
  rectfill(0,sdy,128,sdy,skyc2)
  rectfill(0,sdy+1,128,sdy+1,skyc3)
  rectfill(0,sdy+2,128,128,skyc4)
  local b=true
  for i=0,128,32 do
   b=not b
  	spr(ofsky,(sdx[p]+i)%160-32,sdy-16,4,2,b)
  end
  -- draw game
  cx,cy=ctr[p].x,ctr[p].y
  drawroad(cam[p],ird[flw],flw)
  -- speed
  local spz,sspp=
   spd[flw].z+spd[flw].gz+spd[flw].bz,
   ssp[p]
  -- hud
  drawbar(7,spz,12,9)
  drawbar(5,ctrl[flw].cdn_bst,12,
   ctrl[flw].can_bst and 11 or 8)
  drawbar(3,ssp[flw].hp,100,8)
  rectfill(95,0,123,18,1)
  print("lap "..
    min(sspp.rlap+1,3)
   .."/3",96,1,7)
  print("pos "..sspp.rank.."/"
   ..nb_player_and_ai,96,7)
  print(formattime(sspp.frm),96,13)
  rectfill(4,12,4,height-4,6)
  for i=1,nb_player_and_ai do
   local h=lerp(height-4,12,
    ssp[i].z/trklen) 
   rectfill(4,h,7,h,i==flw and 7 or 6)
  end
  if endfrm>0 then
   spr_grd(racepos[1]==p 
     and 195 or 227,
    48,24,32,15,0x1b0,offset)
  end
 end
 --restore
 clip()
 camera(0,0)
 -- countdown
 if frm<150 then
  local start=107+frm\30
  offset+=1
  spr_grd(start,60,32,8,15,0x1b0,offset)
  print("press ⬆️ to charge boost",
   16,height-6,5)
 end
 -- endrace
 if endfrm>0 then
  offset+=1
  local w,h=66,#racepos*6+2
  local by=(124-h)/2
  rectfill(31,by,31+w,by+h,1)
  rect(31,by,31+w,by+h,0)
  for i=1,#racepos do
   print(i..
    " "..ship[racepos[i]].name..
    " "..formattime(
      ssp[racepos[i]].frm),
    33,(i-1)*6+by+2,7)
  end
 end
 if nb_player==1 then
  palt(0, false)
  rectfill(60,16,91,16,6)
  for i=0,3 do
   pal(peek_spr(0x02b0,i),
    ((ssp[1].z/32-i)\1)%4==0
     and 10 or 0)
  end
  spr(7,60,0,4,2)
  local hps=3-(ssp[1].hp\25)
  if hps>0 then
   hps=min(hps,4)
   spr(161+2*hps,68,0,2,2)
  end
  palt()
  pal()
 end
 -- debug
 print(stat(1),0,0,6)
-- print("cpu="..stat(1))
--  .." fps="..stat(7)
--  .." ram="..stat(0)
--  ,0,height-5,1)
end

function drawroad(cam,ird,p)
 -- calculating road
 local r,rn,cfx,v,vn,vz=
  nil,nil--render info
 -- first partial offset
 local irdn=ird+1
 local irdlast=ird+nb_rb-1
 cfx=-(ssp[p].z-road[irdn%#road+1].z)
  /rdz*road[ird+1].fx

 -- all other
 for i=irdn,irdlast do
  cfx+=road[i%#road+1].fx
 end

 -- ready to draw road
 for i=irdlast,ird-1,-1 do
  v,vn=road[i%#road+1],
   road[(i+1)%#road+1]
  vz,xl,xr,iy,ylgt=
   v.z+(i\#road)*trklen,
   v.x-56,v.x+56,v.y,
   lerp(v.y,vn.y,0.5)
  -- clip
  dz=max(vz-cam.z,7)

  prc=(cam.z+7-vz)/rdz
  if dz==7 then
   --interpolation
   xl=lerp(xl,vn.x-56,prc)
   xr=lerp(xr,vn.x+56,prc)
   iy=lerp(v.y,vn.y,prc)
  end
  -- current part
  fct,cfx=scale/dz,cfx-v.fx

  r={
   x1=proj_fct_x(xl-cam.x,cfx),
   x2=proj_fct_x(xr-cam.x,cfx),
   y=proj_fct_y(iy-cam.y),

   xl1=proj_fct_x(xl-32-cam.x,cfx),
   xr1=proj_fct_x(xr+32-cam.x,cfx),
   y2=proj_fct_y(iy+16-cam.y),
   xl2=proj_fct_x(xl-64-cam.x,cfx),
   xr2=proj_fct_x(xr+64-cam.x,cfx),
   xl3=proj_fct_x(xl-80-cam.x,cfx),
   xr3=proj_fct_x(xr+80-cam.x,cfx),
   y3=proj_fct_y(iy+48-cam.y),
   y4=proj_fct_y(iy+128-cam.y),
  }
  -- drawing road
  if rn then 
   --color fade (-2 for lights)
   local tx,even,odd,flp=
    (i-ird)/(nb_rb-2),
    v.c,vn.c,nil
   
   if tx>=rd_grd3 then
--    even,odd=1,1
      flp=0b1111111111111111.1
--      even,odd=0,0
   elseif tx>=rd_grd2 then
--    flp=0xa5a5
      flp=0b1010010110100101.1
   elseif tx>=rd_grd1 then
--    flp=0xa0a0
      flp=0b1010000010100000.1
   end
   -- colors are the same
   if tx>=rd_grd1 
     and tx<rd_grd3 
     and v.c~=5 then
    even,odd=odd,even
   end

   if v.t&256==256 then
    drawstartlane(r,rn,cfx,
     odd,flp,cam,vz)
   end
   -- other ? (if possible)
   local lborx,lbory,
    rborx,rbory,lcol=
    xl-32,ylgt+16,xr+32,ylgt+16,
    peek_spr(0x0170,irdlast-i-1)
   if v.t&4==4 then
    --main road
    pfill(r.x1,r.y, r.x2,r.y, rn.x2,rn.y, rn.x1,rn.y, even,flp)
    -- tokens left1=right1
    -- but see perfs
    -- left 1
    pfill(r.xl1,r.y2, rn.xl1,rn.y2, rn.x1,rn.y, r.x1,r.y, even,flp)
    if vn.t&8==8 
     and v.t&8==0
     then --left1 to left2
     pfill(r.xl1,r.y2,rn.xl2,rn.y3,rn.xl1,rn.y2,nil,nil,even,flp)
     lborx,lbory=xl-48,ylgt+32
    end
    -- right 1
    pfill(r.xr1,r.y2,rn.xr1,rn.y2,rn.x2,rn.y,r.x2,r.y,even,flp)
    if vn.t&2==2 
     and v.t&2==0
     then --right1 to right2
     pfill(r.xr1,r.y2,rn.xr2,rn.y3,rn.xr1,rn.y2,nil,nil,even,flp)
     rborx,rbory=xr+48,ylgt+32
    end
   end
   if v.t&8==8 then
  	 -- left 2
   	lborx,lbory=xl-64,ylgt+48
    if vn.t&8==8
  	  or v.t&16==16 then
   	 pfill(rn.xl2,rn.y3,rn.xl1,rn.y2,r.xl1,r.y2,r.xl2,r.y3,even,flp)
    else
     --left2 to left1
     lborx,lbory=xl-48,ylgt+32
     pfill(r.xl2,r.y3,rn.xl1,rn.y2,r.xl1,r.y2,nil,nil,even,flp)
    end
    if vn.t&16==16
     and v.t&16==0 then
     --left2 to left3
     pfill(r.xl2,r.y3,rn.xl3,rn.y4,rn.xl2,rn.y3,nil,nil,even,flp)
     lborx,lbory=xl-72,ylgt+88
    end
   end
   if v.t&2==2 then
    -- right 2
   	rborx,rbory=xr+64,ylgt+48
    if vn.t&2==2
     or v.t&1==1
     then
     pfill(rn.xr2,rn.y3,rn.xr1,rn.y2,r.xr1,r.y2,r.xr2,r.y3,even,flp)
    else
     rborx,rbory=xr+48,ylgt+32
     pfill(r.xr2,r.y3,rn.xr1,rn.y2,r.xr1,r.y2,nil,nil,even,flp)
    end
    if vn.t&1==1
     and v.t&1==0 then
     --right2 to right3
     pfill(r.xr2,r.y3,rn.xr3,rn.y4,rn.xr2,rn.y3,nil,nil,even,flp)
     rborx,rbory=xr+72,ylgt+88
    end
   end
   if v.t&16==16 then
   	--left 3
    if vn.t&16==16 then
     lborx,lbory=xl-80,ylgt+128
     pfill(rn.xl3,rn.y4,rn.xl2,rn.y3,r.xl2,r.y3,r.xl3,r.y4,even,flp)
    else
     --left 3 to left 2
     lborx,lbory=xl-72,ylgt+88
     pfill(r.xl3,r.y4,rn.xl2,rn.y3,r.xl2,r.y3,nil,nil,even,flp)
    end
   end
   if v.t&1==1 then
    --right 3
    if vn.t&1==1 then
     rborx,rbory=xr+80,ylgt+128
     pfill(rn.xr3,rn.y4,rn.xr2,rn.y3,r.xr2,r.y3,r.xr3,r.y4,even,flp)
   	else 
     rborx,rbory=xr+72,ylgt+88
     pfill(r.xr3,r.y4,rn.xr2,rn.y3,r.xr2,r.y3,nil,nil,even,flp)
   	end
   end
  	-- lights
   lz=vz-cam.z+rdz/2
   if lz>7 then   	
    local mfct=scale/lz
    local lsize=max(0.8,mfct*4)
    circfill(cx+mfct*(lborx-cam.x)+cfx+v.fx/2,
     cy-mfct*(lbory-cam.y),lsize,lcol)
    circfill(cx+mfct*(rborx-cam.x)+cfx+v.fx/2,
     cy-mfct*(rbory-cam.y),lsize,lcol)
   end
  end
  -- players,ai ships
  -- ntrk=>lapsloop
  local pl,pcfx,ntrk,pls=
   v.pl,0,(i\#road)*trklen,{}
  -- sort on z
  if #pl>1 then
   for inp=1,#pl do
    add(pls,
     {np=pl[inp],key=ssp[pl[inp]].z})
   end
   shellsort(pls)
   for inp=1,#pls do
    pl[inp]=pls[#pls-inp+1].np
   end
  end
  for inp=1,#pl do
   np=pl[inp]
   pcfx=(ssp[np].z-vz)/rdz*v.fx+cfx
   local shake=spd[np].z+spd[np].gz+spd[np].bz>=14
   if shake then
    ox,oy=cam.x,cam.y
    cam.x+=rnd"2"-1
    cam.y+=rnd"2"-1
   end
   -- shadow
   drawvoxel(zzz[np],sssp[np],cam,pcfx,ntrk)
   -- spaceship
   drawvoxel(zz[np],ssp[np],cam,pcfx,ntrk,spd[np],ctrl[np])
   if shake then
    cam.x,cam.y=ox,oy
   end
  end
  --particles
  for inp=1,#v.parts do
   part=v.parts[inp]
   pcfx=(part.z-vz)/rdz*v.fx+cfx
   part.draw(part,cam,pcfx,ntrk,p)
  end
  rn=r
 end
end

function drawstartlane(r,rn,cfx,col,flp,cam,vz)
 pfill(rn.xl3,rn.y4,rn.xl1,rn.y2,r.xl1,r.y2,r.xl3,r.y4,col)
 pfill(rn.xr3,rn.y4,rn.xr1,rn.y2,r.xr1,r.y2,r.xr3,r.y4,col)
 --laser
 if cam.z+7<vz then
  local lsrc=frm%2==0 and 12 or 7
  lsrc=frm<150 and 9 or lsrc
  lsrc=frm<120 and 8 or lsrc
  local mfct=scale/(vz-cam.z+rdz/2)
 	local stxl1,stxr1,sty=
 	 cx+mfct*(xl-48-cam.x)+cfx,
 	 cx+mfct*(xr+48-cam.x)+cfx,
   cy-mfct*(iy+53-cam.y)
  drawthunder(stxl1,sty,stxr1,sty,mfct,lsrc)
 	stxl1,stxr1,sty=
 	 cx+mfct*(xl-64-cam.x)+cfx,
 	 cx+mfct*(xr+64-cam.x)+cfx,
   cy-mfct*(iy+90-cam.y)
  drawthunder(stxl1,sty,stxr1,sty,mfct,lsrc)
 end
end

function drawthunder(x1,y1,x2,y2,zfct,col)
 x,y=x1,y1
 local zrnd=max(0.1,zfct*8) 
 for i=1,4 do
  xn,yn=x1+(x2-x1)/5*i+rnd(zrnd),
   y1+(y2-y1)/5*i+rnd(zrnd)-zrnd/2
  line(x,y,xn,yn,col)
  x,y=xn,yn 
 end
 line(xn,yn,x2,y2)
end

function isnotvisible(pos,cam,ntrk)
 return pos.z+ntrk<cam.z+7 or
  pos.z+ntrk>cam.z+10*rdz
end

function brilar()
 return max((rnd"8")\1-6,0)
end

function drawvoxel(zz,ssp,cam,
 cfx,ntrk,spd,ctrl)
 if isnotvisible(ssp,cam,ntrk) then
  return
 end
 local brighter,larger=
  brilar(),brilar()

 local pre,spcx,spcy,ofsx,ofsy=
  init_draw_voxel(ssp,cam,ntrk,cfx)

 -- draw reverse
 for i=#zz,1,-1 do
  r=zz[i]
  fct=scale/(r.z+pre)--flatten
  vfct=2.5*fct--zoom model
  if r.c==10 or r.c==9 and r.pat==1 then
   x,y=
    ofsx+fct*spcx+vfct*(r.x+0.5),
    ofsy-fct*spcy-vfct*(r.y-0.5)
   if r.c==10 then
    --engine
    if spd then
     col=(spd.prc*0.03)\1+brighter
     if ctrl.cdn_bst>0 then col+=5
     elseif spd.bz>0 then col+=1
     end
     larger=spd.prc<50 and 0 or larger
     -- trail
     if (larger~=0 and ctrl.accel)
       or ctrl.cdn_bst<-6 then
      create_particles(
       p_trail,ssp,spd,r)
     end
    else
     col,larger=5,0
    end
    drawengine(x,y,vfct,larger,col)
   else
    --shadow
    circfill(x,y,vfct,0x50)
   end
  else
   x,y=
    ofsx+fct*spcx+vfct*r.x,
    ofsy-fct*spcy-vfct*r.y
   rectfill(x,y,x+vfct-1,y+vfct-1,r.c)
  end
 end
end

function drawengine(x,y,vfct,
 larger,col)
 local col1,col2=
  peek_spr(0x0070,col),
  peek_spr(0x00b0,col)
 circfill(x,y,vfct,col1)
 circfill(x,y,vfct-.5,col2)
 if larger~=0 then
  fillp(0b1010010110100101.1)
  circfill(x,y,vfct-.5+larger*1.5,col1)
  fillp()
 elseif vfct>1 then
  circ(x,y,vfct,6)
 end
end

function convert_xflat_to_x(pos,decx,ytmp,rot,spdp,shipp)
 
 local xflat,cumx,cumy,newz=
  pos.xflat-decx,0,0,1
 pos.x,pos.y=pos.xflat,0 

 for stp in all(rd_info) do
  if xflat > stp.fl then
   -- startx,y
   stx,sty=stp.sx,stp.sy
   cumx,cumy=stx+stp.dx,sty+stp.dy
   pos.x=decx+lerp(stx,cumx,(xflat-stp.fl)/stp.dst)
   pos.y=lerp(sty,cumy,(xflat-stp.fl)/stp.dst)
   newz=stp.rotz
   break
  end
 end
 if rot then
  rot.z=lerp(rot.z,newz,0.25)
  newz=rot.z
 end
 if pos.yflat then
  pos.x=pos.x
   +sin(1-newz)*pos.yflat
  pos.y=ytmp+pos.y
   +cos(newz)*pos.yflat
 end
 if spdp then 
  local gx=sin(newz)
  -- meme sens, speed bonus
  -- sens oppose, speed malus
  if spdp.x*gx<0 then
   spdp.gz=min(spdp.gz+abs(gx)/2,
                4-shipp.accel)
  elseif spdp.x*gx>0 then
   spdp.gz=max(spdp.gz-abs(gx)/2,
               -4+shipp.accel)
  end
  spdp.x-=gx
 end
end

-- intro
local nb_ship,nb_choice
local intro_mnu={
 {name="player   ",values={1,2},maxval=2},
 {name="ai skill ",values={"no","easy","normal","hard"},maxval=4,val=3},
 {name="track    ",maxval=6},
 {name="ship 1   ",maxval=#ship_info},
 {name="ship 2   ",maxval=#ship_info}
}
function init_intro()
 nb_choice,nb_player,track_id,
  cam_int,cy,rot_intro,offset,
  lastnss,nss=
  1,1,1,
  {x=0,y=72,z=40},
  32,{x=0,y=0,z=0},0,
  {},{}
 music"0"
end

function update_intro()
 --handle controls
 if btnp"2" then
  nb_choice=max(1,nb_choice-1)
  sfx"40"
 elseif btnp"3" then
  nb_choice=min(#intro_mnu,nb_choice+1)
  sfx"40"
 end
 local mnuitem=intro_mnu[nb_choice]
 if (mnuitem.val==nil) mnuitem.val=1
 --
 if btnp"0" then
  mnuitem.val=max(1,mnuitem.val-1)
  sfx"40"
 elseif btnp"1" then
  mnuitem.val=min(mnuitem.maxval,mnuitem.val+1)
  sfx"40"
 end
 local ship2=intro_mnu[5]
 if btnp(0,1) then
  ship2.val=max(1,ship2.val-1)
  sfx"40"
 elseif btnp(1,1) then
  ship2.val=min(ship2.maxval,ship2.val+1)
  sfx"40"
 end
 -- start game
 if offset>0 and btnp"4" then
  sfx"41"
  nb_ai,ai_skill,num_trk=
   0,intro_mnu[2].val,
   intro_mnu[3].val-1
  if ai_skill>1 then
   nb_ai=4-nb_player
   nss[4]=3
   nss[3]=2
   if (nb_player<2) nss[2]=10
  end
  nb_player_and_ai=nb_player+nb_ai
  offset,ofsky,skyadr=
   0,3+32*(num_trk%2),
   0x0230+64*(num_trk%2)
  skyc1,skyc2,skyc3,skyc4=
   peek_spr(skyadr,0),
   peek_spr(skyadr,1),
   peek_spr(skyadr,2),
   peek_spr(skyadr,3)
  start_scene(scn_game)
 end
 --updating
 rot_intro.y+=0.015
 for p=1,2 do
  nss[p]=intro_mnu[3+p].val or 1
  --loading ship
  if lastnss[p]~=nss[p] then
   lastnss[p]=nss[p]
   loadship(p,nss[p]-1)
   initshipaccel(ship[p])
   ssp[p].xflat,ssp[p].z,
    sssp[p].z=0,88,88
  end
 
  local mat=calcrotmat(rot_intro)
  zz[p]=applyrot(ss[p],mat,true)
  zzz[p]=applyrot(sss[p],mat,false)
 end
end
function draw_intro()
 cls(0)
 offset+=1
 spr_grd(67,32,4,64,31,0x1b0,offset)

 --choice
 nb_player=intro_mnu[1].val
 local y=39
 for yitem=1,#intro_mnu do
  entry=intro_mnu[yitem]
  col=7
  if (entry.val==nil) entry.val=1
  -- 2nd ship
  if entry.name~="ship 2   " 
    or nb_player~=1 then
   if entry.values then
    entry.value=entry.values[entry.val]
   else
    entry.value=entry.val
   end

   local msg=entry.name..entry.value
   if nb_choice==yitem then
    col=8
    print("⬅️             "
     .."        ➡️",16,y,col)
   end
   print(msg,46,y,col)
  end
  y+=6
 end
 spr(39,60,122,2,1)
 print("start ❎🅾️    yourykiki",
  16,122,1)
 if nb_choice==3 then
  local itrk=intro_mnu[3].val-1
  spr(204,16,72,4,4)
  spr(131+2*itrk,24,80,2,2)
  print("best race time",56,72,5)
  print(formattime(
          dget(itrk*4+
             intro_mnu[2].val-1)),
        64,78,5)
 else
  -- ships
  for p=1,nb_player do
   cx=-32*(nb_player-1)+p*64
   spr(44,cx-15,96,4,4)
   drawvoxel(zzz[p],sssp[p],cam_int,0,0)
   drawvoxel(zz[p],ssp[p],cam_int,0,0)
   -- stats
   cx=cx-14
   print(ship[p].name,cx,70,5)
   print("   m:"..ship[p].mass,cx,82)
   local j,k=0
   for i=1,28 do
    k=ship[p].accurv[(i*100/28)\1]
    line(cx+i-1,86-j,cx+i,86-k,7)
    j=k
   end
  end
 end
-- print("cpu:"..stat(1),0,0,1)
end

function formattime(tm)
 local mm="0"..((tm\30)%60)
 return (tm\1800)..":"
  ..sub(mm,#mm-1).."."
  ..(tm\3)%10
end

-->8
-- rasterization @fsouchu
function polyfill(p,col,flp)
	color(col)
 if (flp) fillp(flp)
	local p0,nodes=p[#p],{}
	local x0,y0=p0[1],p0[2]
	for i=1,#p do
		local p1=p[i]
		local x1,y1=p1[1],p1[2]
		local _x1,_y1=x1,y1
		if(y0>y1) x1=x0 y1=y0 x0=_x1 y0=_y1
		local dx=(x1-x0)/(y1-y0)
		if(y0<0) x0-=y0*dx y0=-1
		local cy0=y0\1+1
		x0+=(cy0-y0)*dx
		for y=cy0,min(y1\1,127) do
			local x=nodes[y]
			if x then
				rectfill(x,y,x0,y)
			else
				nodes[y]=x0
			end
			x0+=dx
		end
		x0=_x1
		y0=_y1
	end
	fillp()
end
--helper
function pfill(x1,y1,x2,y2,x3,y3,x4,y4,c,flp)
 local q={}
 add(q,{x1,y1})
 add(q,{x2,y2})
 add(q,{x3,y3})
 if (x4 and y4) add(q,{x4,y4})
 polyfill(q,c,flp)
end

-->8
--triplefox with ciura's sequence
--https://www.lexaloffle.com/bbs/?tid=2477
shell_gaps={701,301,132,57,23,10,4,1} 
function shellsort(a)
 for gap in all(shell_gaps) do
  if gap<=#a then
   for i=gap,#a do
    local t=a[i]
    local j=i
    while j>gap and
       a[j-gap].key>t.key do 
     a[j]=a[j-gap]
     j-=gap
    end
    a[j]=t
   end
  end
 end
end

-->8
-- math lerp
function lerp(v0,v1,prc)
 return (1-prc)*v0+prc*v1
end

function sqrdist(x1,y1,x2,y2)
 return (x2-x1)^2+(y2-y1)^2
end

function getaccel(accurv,prc)
 return accurv[mid(0,prc\1,100)]
end

function get_r_accel(r_accurv,val)
 local prc=r_accurv[val\1]
 if (prc) return prc
 if (val>0) return 100
 return 0 
end

function calcrotmat(rot)
 --pitch x yaw y roll z
 local cosa,sina,cosb,sinb,cosc,sinc=
  cos(rot.z),sin(rot.z),
  cos(-rot.y),sin(-rot.y),
  cos(rot.x),sin(rot.x)
 
 local axx,axy,axz=
  cosa*cosb,
  cosa*sinb*sinc-sina*cosc,
  cosa*sinb*cosc+sina*sinc

 local ayx,ayy,ayz=
  sina*cosb,
  sina*sinb*sinc+cosa*cosc,
  sina*sinb*cosc-cosa*sinc

 local azx,azy,azz=
  -sinb,cosb*sinc,cosb*cosc 

 return {xx=axx,xy=axy,xz=axz,
  yx=ayx,yy=ayy,yz=ayz,
  zx=azx,zy=azy,zz=azz}
end

function recover_cdn(ssp)
 return 0.3/(6-ssp.hp\25.0001)
end
-->8
--particles
function create_particles(part,
 pos,sp,obj)
 for i=1,part.nb do
  local new_part=part.create(pos,sp,obj)
  local v=road[get_ird(new_part)+1]
  add(v.parts,new_part)
  add(part.parts,new_part)
 end
end
function update_particles(part)
 for p in all(part.parts) do
  local oird=get_ird(p)
  if not part.update(p) then
   -- dead particle
   del(road[oird+1].parts,p)
   del(part.parts,p)
  else
   -- placing in draw state
   local nird=get_ird(p)
   if oird~=nird then
    del(road[oird+1].parts,p)
    add(road[nird+1].parts,p)
   end
  end
  p.frm-=1
 end
end
function draw_part_line(
 part,cam,cfx,ntrk,col)
 if isnotvisible(part,cam,ntrk) then
  return
 end
 local pre,ofsx,ofsy,wz2=
  ntrk-cam.z,
  cx+cfx,cy,
  part.z+part.spz
 local fct1,fct2=
   scale/(part.z+pre),
   scale/(wz2+pre)
 local pos1,pos2=
  {x=part.x,y=part.y,z=part.z},
  {x=part.x+part.spx,
   y=part.y+part.spy,z=wz2}
 local x1,y1,x2,y2=
  ofsx+(pos1.x-cam.x)*fct1,
  ofsy-(pos1.y-cam.y)*fct1,
  ofsx+(pos2.x-cam.x)*fct2,
  ofsy-(pos2.y-cam.y)*fct2
	line(x1,y1,x2,y2,col)
end
function init_draw_voxel(pos,cam,ntrk,cfx)
 --pre,spcx,spcy,ofsx,ofsy
 return
  pos.z-cam.z+ntrk,
  pos.x-cam.x,pos.y-cam.y,
  cx+cfx,cy
end
-- sparks 
function create_spark(pos,sp)
	local ang,speed=
	 rnd(),.1+rnd"1"
	return {
		x=pos.x,y=pos.y,z=pos.z,
		spx=sin(ang)*speed,
		spy=cos(ang)*speed,
		spz=sp.z,
		frm=10+(rnd"20")\1,
		draw=draw_spark
	}
end
function update_spark(part)
	part.x+=part.spx
	part.y+=part.spy
	part.z+=part.spz
	part.spy-=0.1
	return part.frm>0
end
function draw_spark(part,cam,cfx,ntrk)
 draw_part_line(part,cam,cfx,ntrk,
  peek_spr(0x0030,part.frm))
end
p_spark={
	create=create_spark,
	update=update_spark,
	draw=draw_spark,
	parts={},
	nb=2
}
-- trails
function create_trail(pos,spd,obj)
	return {
		x=pos.x,y=pos.y,z=pos.z,
		spz=spd.z*0.98,
		frm=4,
		draw=draw_trail,
		trail=obj
	}
end
function update_trail(part)
	if part.frm <= 0 then
		return false
	end
	part.spz*=0.96
	part.z+=part.spz
	return true
end
function draw_trail(part,cam,cfx,ntrk)
 if isnotvisible(part,cam,ntrk) then
  return
 end
 local pre,spcx,spcy,ofsx,ofsy=
  init_draw_voxel(part,cam,ntrk,cfx)
 local r=part.trail
 local fct=scale/(r.z+pre)
 local vfct=2.5*fct
 local x,y=
  ofsx+fct*spcx+vfct*(r.x+0.5),
  ofsy-fct*spcy-vfct*(r.y-0.5)
 fillp(0b1010010110100101.1)
 circfill(x,y,min(vfct,2),
  peek_spr(0x00f0,part.frm))
 fillp()
end

p_trail={
 create=create_trail,
 update=update_trail,
 draw=draw_trail,
 parts={},
 nb=1
}
-- boost 
function create_boost(pos,sp,obj)
 local col=
  peek_spr(0x0130,(rnd"3")\1)
 return {
  x=rnd"256"-128,
  y=pos.y+rnd"56",
  z=pos.z+64+rnd"384",
  spx=0,spy=0,spz=-sp.z*0.75,
  frm=32+(rnd"32")\1,
  draw=draw_boost,
  col=col,
  player=obj
 }
end
function update_boost(part)
 if part.frm <=0 then
  return false
 end
 part.z+=part.spz
 part.spz*=0.9
 return true
end
function draw_boost(part,cam,cfx,ntrk,p)
 if (part.player~=p) return
 draw_part_line(part,cam,cfx,ntrk,
  part.col)
end
p_boost={
	create=create_boost,
	update=update_boost,
	draw=draw_boost,
	parts={},
	nb=2
}
-->8
-- token quest
function peek_spr(addr,offset)
 return peek(addr+offset/2) >>
  offset%2*4
end

function spr_grd(nbs,x,y,w,h,col_addr,offset)
 local sprx,spry,scol=
  nbs%16 *8,(nbs\16)*8,0
 for i=0,w do
  for j=0,h do
   scol=sget(sprx+i,spry+j)
   if scol==3 then
    pset(x+i,y+j,
     peek_spr(col_addr,(offset+(j\3))%30)+128)
   elseif scol!=0 then
    pset(x+i,y+j,scol)
   end
  end
  offset+=1
 end
end

function drawbar(yp,val,vmax,col)
 	rectfill(4,yp-1,
 	 lerp(4,36,abs(val/vmax)),
 	 yp,col)
end

function proj_fct_x(worldx,cfx)
 return cx+fct*worldx+cfx
end

function proj_fct_y(worldy)
 return cy-fct*worldy
end

function start_scene(scene)
  scn=scene
  scn.init()
  scn.update()
end

function loadship(p,nbss)
 ss[p],sss[p],xflat={},{},
  (p%2)*32-16
 loadss(nbss,ss[p],sss[p])
 ssp[p],rot[p],spd[p],
 ctrl[p],ird[p]=
  {x=0,y=8,z=72+24*p,
   xflat=xflat,yflat=9,
   rlap=0,rank=0,update=update_cntdn,
   frm=0},
  {x=0,y=0,z=1},
  {x=0,z=0,gx=0,gz=0,prc=0,cdn=0,bz=0},
  {cdn_bst=0,can_bst=true},1
 sssp[p]={x=ssp[p].x,y=0,z=56}
 ship[p]=ship_info[nbss+1]
 ssp[p].hp=100
 ssp[p].xmin,ssp[p].xmax=
  xflat-5,xflat+5   
end

 
-- b1,b2,b3 border
-- prc on rb
-- flg1 flg2, rb type
-- borx
function road_part_collision(
  b1,b2,b3,prc,flg1,flg2,borx)
 if v.t&flg1==flg1 then
  --right 2
  if vn.t&flg1==flg1
   and v.t&flg2==0 then
   borx=b2
  else
   --right 2 to right 1
   borx=lerp(b2,b1,prc)
  end
  if vn.t&flg2==flg2
   and v.t&flg2==0 then
   --right 2 to right 3
   borx=lerp(b2,b3,prc)
  end
 end
 return borx
end
-- test coll 2
function road_part2_collision(
  b2,b3,prc,flg,borx)
 if v.t&flg==flg then
 	--left 3
 	if vn.t&flg==flg then
  	borx=b3
  else 
   borx=lerp(b3,b2,prc)
  end
 end
 return borx
end

function parse_trk()
 local trk={}
 byte_offset=num_trk*256
 for i=1,read_byte() do
  trk[i]={curv=read_byte(),
   len=read_byte(),
   frc=read_byte(),
   hgt=read_byte(),
   hln=read_byte()}
 end
 return trk
end

function read_byte()
 local val=peek(0x2000+byte_offset)-128
 byte_offset += 1
 if (val==-128) return nil
 return val
end

function is_collide(sspp,ssppn,
 shipp,shippn,dz)
 --/8 /4 scale
 return
  abs(sspp.xflat/8
   -ssppn.xflat/8)
   <=shipp.width+shippn.width
  and abs(sspp.z/4
   -ssppn.z/4)*dz
   <=shipp.height+shippn.height
end

function snd_engine(p,spd)
 sfx(47+p,p*2+1,mid(0,spd.z\1,11)*2,2)
end

function getdamage()
 return max(2,ai_skill)
end
__gfx__
000880000000000000000000000000000000000000500000000000006505505503c3333333333c3055055056000000002229999999aaaaa77777777777777700
002882000000000000000000111111111111111111551111111111116050505503133366663331305505050600a0b0b099aa769aa77000000000000000000000
00088000000000000000000000500000000000000055500000000000650550503133333333333313050550560080a0c09aa7c6ccccc000000000000000000000
00088000000770000000000015511111115111111151555111111111655505503e333000000333e305505556005080d029aa7700000000000000000000000000
200880020007700000077000115111111151511111115a5111111111605505cdde000aaaaaa000eddc50550600000000c7600000000000000000000000000000
2228822280088008000880005551111115555111111555511111111165011c00009a222aa222a90000c110560080a0b0499aaaaaaaaaaaaaaaaaaaaa00000000
2008800280088008000880005552222222555222225555522222222265505550309a65aaaa56a90305550556006090c0128e9999aaaaaaa77777766666ccd100
a000000a00000000000000005551111151595111115555511111155565550550309aaaaaaaaaa90305505556003060d0128999aa000000000000000000000000
000bb00000000000000000005952522255555222225aa55252225599655550503309aaff9faa903305055556000000000e210000000000000000000000000000
003bb3000007700000000000555255225555522222555552522225556055505033300fff9ff003330505550600000000cfd10000000000000000000000000000
033bb3300007700000077000aa52555255555225225599525252255560005050333300f44f00333305050006000000001cde0000000000000000000000000000
330bb033000bb000000bb000555e555e5aa55e555e55555e555ee55560000054443330ffff033344450000060000000000000000000000000000000000000000
300bb003b00bb00b000bb000a5525952555552252255aa5259522555650000544449907777099444450000560000000000000000000000000000000000000000
30000003b000000b00000000555e555e55aa5e555e55555e555ee55565555000049990f99f099940000555560000000000000000000000000000000000000000
30000003b000000b000000005a5e595e55555555ee59955e555ee555600000555000000ff0000005550000060000000000000000000000000000000000000000
a000000a0000000000000000555e555e555555555e55555e55555555655555555550000000500555555555560000000000000000000000000000000000000000
000000000000000000000000cccccccccccccccccccccccccccccccc000000cc0c00000000000000000000000000000000000000000000000000000000000000
100000010000000000000000ffffffffffffffffffffffffffffffffcc000cccc000000000000000000000000000000000000000000666666666d00000000000
100000010000000000000000cccccccccccccccccccccccccccccccc0cc11ccccc00000000000000000000000000000000000000766111111111166d00000000
100cc0010007700000000000ccccc6776cccccccccccccccc6776ccccccccccc000000000000000000000000000000000000007611111111111111116d000000
110cc011c007700c00077000cccc777777cccccccccccccc677776cc0cccccc100000000000000000000000000000000000076111111111111111111116d0000
110cc011c00cc00c000cc000ccc67777776ccccccccccc67777777cc01cccc10000000000000000000000000000000000007111111111111111111111111d000
11111111cc0cc0cc000cc000ff77777777776ffffff77777777777ff000000000000000000000000000000000000000000711111111111111111111111111d00
000000000a0000a000000000c677777777777776cccc77777777777c0000000000000000000000000000000000000000071111111111111111111111111111d0
00022000000000000000000077777777777777777ffff677777777ff0000000000000000000000000000000000000000071111111111111111111111111111d0
000220000000000000000000fff6777776fffffffffffffff6776fff00000000000000000000000000000000000000007111111111111111111111111111111d
000220000007700000000000ffffffffffffffffffffffffffffffff00000000000000000000000000000000000000007111111111111111111111111111111d
000220000027720000077000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa00000000000000000000000000000000000000007111111111111111111111111111111d
00222200002ee200000ee000ffffffffffffffffffffffffffffffff00000000000000000000000000000000000000007111111111111111111111111111111d
0022220000eeee00000ee000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0000000000000000000000000000000000000000771111111111111111111111111111d1
022002200eeeeee0000ee000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa000000000000000000000000000000000000000077711111111111111111111111111d11
0a0000a00000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa000000000000000000000000000000000000000077711111111111111111111111111d51
0400004000000000000000005550555555505555555555555550555555555555555555555555555555505555000000007777111111111111111111111111d111
04000040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000077766611111111111111111111555151
04400440090000900000000000000000000000000000000000000000000000000000000000000000000000000000000007767666611111111111111555555110
44000044090000900900077000000000033333300333333003300033033333300333333003300000000000000000000000666666666611111111dd5d55555100
44044044099009900900099000000000003333330033333300330003303333330033333300330000000000000000000000067666666ddddddddddd5555551000
04455440099999900990099000000000000330033003300330033003300033000003300330033000000000000000000000006666666d6ddddddddd5d55550000
05055050050550500000000000000000000330033003300330033003300033000003300330033000000000000000000000000066666ddddddddddd5555000000
000000000a0000a0000000000000000000033333000333330003300330003300000333330003300000000000000000000000000006dd6dddddddd55000000000
000000000000000000000000000000000033333000333330003300330003300000333333003300000000000000000000000000000000dddddddd000000000000
0000000000c00c000000000000000000003333330033333000330033000330000033003300330000000000000040040000000000000000000000000000000000
0000000000c00c000000000000000000003300330033003300330033000330000033003300330000000000000404040000000000000000000000000000000000
c000000c0c9cc9c0c007700c00000000003300330033003300330033000330000033003300330000000000000044040000000000000000000000000000000000
c011110c009cc900c09cc90c00000000033003330330003303300333003300000330033003300000000000000004040000000000000000000000000000000000
c110011c0c9cc9c0c09cc90c00000000033333300330000333333330003300000330033003333330000000000040040000000000000000000000000000000000
c000000c009cc900c000000c00000000033333000330000333333300003300000330033003333333000000000000000000000000000000000000000000000000
0000000000a00a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00300300000000000000000000000000000000000000000033333300333333003333333003333330000000000000000000000000000000000000000000000000
033003300b0000b00000000000000000077007007007000003333330033333300333333300333333000000000333330000033300033333000033330000033000
033003300b0000b00000000000000000070700070070700000330033003300330033000000033000000000000333333000003310003333300333333003333100
00300300000770000007700000000000077007070070700000330033003300330033000000033000000000000331133100003310000113310331133103333100
003bb30000bbbb00000bb00000000000070007007007000000333330003333300033000000033000000000000331001100033110000003310311033100133100
003bb30000bbbb00000bb00000000000000000000000000003333300033333300330000000333300000000000331000000033100000003310010331100033100
000bb00000a00a000bbbbbb000000000000000000000000003333300033003300330000000333330000000000331000000331100000003310000331000033100
00000000000000000000000000000000000000000000000003300330033003300330000000330000000000000333330000331000000033110003311000033100
00000000000000000000000000000000000000080000000003300330033003300330000000330000000000000333333003311000000033100003310000033100
0000000000077000888778880000000000000097f000000033000330330033003300000003330000000000000011133103313300000003300033110000033100
0002200000288200008778000000000000000a777e00000033000033330033003333330003333300000000000000033103313310000003310033100000033100
00022000000880000008800000000000000000b7d000000033000033330033003333333003333330000000000330033103333330033003310331100000033100
000220000008800000088000000000000000000c0000000000000000000000000000000000000000000000000331033103333331033103310331000000033100
00000000002882000888888000000000000000000000000000000000000000000000000000000000000000000333333100113311033333310333333000033100
00000000000000000a0880a000000000000000000000000000000000000000000000000000000000000000000033331100003310003333110333333100033100
00000000000000000000000055505555555055555555555555505555555555555555555555555555555055550001111000000110000111100011111100001100
00000000000000000000000006660660060600600666066006060660066606600606066006660660060600600666066006060666066606600606066000000000
00000000000990000000000000600606066006600060060606600006006006060660006600600606066006000060060606600600006006060660060000000000
00444400000990000000000000600660066600600060066006660060006006600666000600600660066606660060066006660066006006600666066600000000
00000000000990000000000000600606060606660060060606060666006006060606066600600606060600600060060606060666006006060606066600000000
04400440009999000007700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000990000007700000666666666666000000000000066600000000066000000000006666666600000000000000000000000000000000000000000000
44400444009999000009900006000000000000600000000000600060000000600600000000060000000060000000000066666600000000000000000000000000
0a0000a0000000000009900060000000000000060000000006000006000000600600000000600000000006000000000600000060006600006675600000000000
00000000000000000000000060000000000000060000000006000006000000600600000000600006660006000000000600000060060060060000060000000000
00066000000000000000000006000000000000060066000006000006000000600600066000600060006006000066000066600070060060060000006000000000
00066000000000000000000000666600000000060600600006000006000666000600600600600060006006000600600000060050060006600000006000000000
60066006000770000000000000000060000000066000060060000006006000000600600600060000006006000600066666600060060000000000006000000000
60066006000660000000000000000006000000066000006600000006005000000066000600006675660006000600000000000060006600000000006000000000
65566556600660060000000000000006000000060600000000600060007000000000000600000000000006000600000000000060000066660000060000000000
0a0660a0600000060000000000000000600000600066675606066600006000000000006000000060000060000066666660666600000000006666600000000000
00000000000000000000000000000000067566000000000060000000000666666666660000000006666600000000000006000000000000000000000000000000
00600600006006000000000003c3333333333c3003c3333333333c3003c3333333333c3003c3333333333c300010000000000000000000000000000000000000
00600600006006000000000003133366663331300313336666333130031333666633313003133366663331300050000060666060000000000000000000000000
066006600660065000000000313333333333331331333333333333133133333333333313313333333333331300d0000060006006000000000000000000000000
0666666006666560000000773e333000000333e33e333000000333e33e333000000333e33e333000000333e300c0000060060060000000000000000000000000
660000666665566600000066de000aaaaaa000edde000a9aaaa000edde000a9aaaa000edde000a9aaaa000ed0000000060060600000000000000000000000000
660000666656656600000060009a22aaaa22a900009a22a9aa22a900009a22a9aa22a900009a2229a222a9000040000060060666000000000000000000000000
005555000566665000000000309a652aa256a903309a652aa256a903309a652a9256a903309a66afaa66a9030040000000000000000000000000000000000000
0000000000a00a0000000000309aaaaaaaaaa903309a8aaaaaa8a903309a8a9aaaa8a903309a8affafa8f9030090000000000000000000000000000000000000
0000000000000000000000003309aaff9faa90333308aaff9fa8903333098aff9fa8903383098aff9ff8f0880090000000000000000000000000000000000000
00066000000000000000000033300fff9ff0033333300fff9ff0033333308ff44f80033338808ff44f80883300a0000000000000000000006066606000000000
000660000007700000000000333300f44f003333333300f44f003333333300f8ff003333333800f77800383300a0000000000000000000006060660600000000
600660060007700060077006443330ffff033344443330f8ff033344443330877f033344448330855f0333840060000000000000000000006066660600000000
60066006000770006007700644499077770994444449907778099444444990855f099444484890855f0994840060000000000000000000006000660600000000
600660060006600060666606049990f77f099940049990f778099940049990f588099940089898f5880999400070000000000000000000006066006000000000
6056650606666660605665065000000ff00000055000000990000005500000077800000550000805580000050070000000000000000000000000000000000000
60a00a060000000060a00a0655500000005005555550050000500555555005000050055555508507708005550000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00066000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009999999999999999999999999999990
00066000000770000000000000003300033003300033000330000000000000000000000000000000000000000000000009aa0000aaaa0000aaaa0000aaaa0090
60066006000770006007700600000330003300330003300033000000000000000000000000000000000000000000000009a0000aaaa0000aaaa0000aaaa00090
600660060007700060077006000000330003300330003300033000000000000000000000000000000000000000000000090000aaaa0000aaaa0000aaaa000090
60066006000660006066660600000033100331033100331003310000000000000000000000000000000000000000000009000aaaa0000aaaa0000aaaa0000a90
6056650606666660605665060000003310033103310033100331000000000000000000000000000000000000000000000900aa9999999999999999999900aa90
60a00a060000000060a00a06000003311033113311033130331100000000000000000000000000000000000000000000090aaa900000000000000000090aaa90
00000000000000000000000000000331303310331003313133100000000000000000000000000000000000000000000009aaaa90000000000000000009aaaa90
00000000000000000000000000000331313310331003313133100000000000000000000000000000000000000000000009aaa090000000000000000009aaa090
00000000600000060007700000000331313310331003313133100000000000000000000000000000000000000500505009aa0090000000000000000009aa0090
50066005606776060007700000003313133113311033110331100000000000000000000000000000000000005050550509a00090000000000000000009a00090
60066006566666650007700000003333333103310033100331000000000000000000000000000000000000000050505009000090000000000000000009000090
50066005606666060006600000000331331103310033100331000000000000000000000000000000000000000500550509000a90000000000000000009000a90
0000000060800806066666600000001101100011000110001100000000000000000000000000000000000000555050500900aa9000000000000000000900aa90
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000090aaa900000000000000000090aaa90
00600600006006000000000000000000000000000000000000000000000000000000000000000000000000000000000009aaaa90000000000000000009aaaa90
00600600006006000000000000000000000000000000000000000000000000000000000000000000000000000000000009aaa090000000000000000009aaa090
06600660066006500000000033000000333333003333330033333300000000000000000000000000000000000000000009aa0090000000000000000009aa0090
06666660066665600000007703300000033333300333333003333330000000000000000000000000000000000000000009a00090000000000000000009a00090
66000066666556660000006600330000003311330033111100331111000000000000000000000000000000000000000009000090000000000000000009000090
66000066665665660000006000331000003310331331100000331000000000000000000000000000000000000000000009000a90000000000000000009000a90
0055550005666650000000000033100000331033133100000033100000000000000000000000000000000000000000000900aa9000000000000000000900aa90
0000000000a00a0000000000033110000331033113333300033330000000000000000000000000000000000000000000090aaa900000000000000000090aaa90
00000000000000000000000003310000033103310033333003333300000000000000000000000000000000000000000009aaaa90000000000000000009aaaa90
00000000000000000000000003310000033103310001133103311110000000000000000000000000000000000000000009aaa099999999999999999999aaa090
00000000000000000000000003310000033103310000033103310000000000000000000000000000000000000000000009aa0000aaaa0000aaaa0000aaaa0090
00000000000000000000000033110000331133310000333133310000000000000000000000000000000000000000000009a0000aaaa0000aaaa0000aaaa00090
000000000000000000000000333333003333331133333311333330000000000000000000000000000000000000000000090000aaaa0000aaaa0000aaaa000090
00000000000000000000000033333330033331100333311033333300000000000000000000000000000000000000000009000aaaa0000aaaa0000aaaa0000a90
00000000000000000000000001111111001111000011110001111110000000000000000000000000000000000000000009999999999999999999999999999990
__label__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000555055555550555555555555555055555555555555555555555555555550555500000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000006666cc00128e9900aa000aa07777760066ccd1008e000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000666ccd0028e99900aa000a70777766006ccd1100e900000000000000000000000000000000000000000000
00000000000000000000000000000000000000000006600d1008e009900aa00a70007700000cc001200990000000000000000000000000000000000000000000
00000000000000000000000000000000000000000006c001100e9009a00aa00770007600000cd002800990000000000000000000000000000000000000000000
0000000000000000000000000000000f0000000f0006ccd1000e9999000aa00770007600000cd112000990000000000f0000000f000000000000000000000000
0000000000000000f000000f00000000000000000066ccd0008e999000aa00a7f0077000f0ccd112009900000000000000000000000000000000000000000000
0000000000000000000000000000000f0000000f006ccd1100e9999000aa00770007600000cd0028009900000000000000000000000000000000000000000000
0000000000000000000000000000000000000000006c001100e9009a00aa00770007600000cd0028009900000000000000000000000000000000000000000000
0000000000000000000000000000000000000000006c001100e9009a00aa00770007600000cd0028009900000000000000000000000000000000000000000000
000000000000000000000000000000000000000006c001120e9000aa0aa00777007600000cd00280099000000000000000000000000000000000000000000000
000000000000000000000000000000000000000006ccd1100e90000aaaaaa770007600000cd002800999aaa00000000000000000000000000000000000000000
000000000000000000000000000000000000000006ccd1000e90000aaaaaa700007600000cd002800999aaaa0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000aaaa770077666600cd1128e0099aaaa00000000000000000000000000000000000000000
000000000000000000000000000000000000000007700700700700000aaa7770076666600d1128e9009aaaaa0000000000000000000000000000000000000000
0000000000000000000000000000000000000000070700070070700000a70077006600cc00120000000aa0000000000000000000000000000000000000000000
0000000000000000000000000000000000000000077007070070700000a70077006600cc00120000000aa0000000000000000000000000000000000000000000
0000000000000000000000000000000000000000070007007007000000a77770006666c000120000000aa0000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000a77770006666cc00120000000aaaa000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000a77770006600cc00120000000aaaaa00000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000a70077006600cc00120000000aa00000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000008000000000770077006600cd00280000000aa00000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000097f0000000a70007706600cc00120000000aaa00000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000a777e000000a70000766600cc00128e99000aaaaa000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000b7d0000000770000666600cd0028e999900aaaaa700000000000000000000000000000000000000000
000f0000000000000000000000000000000000000000000c00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000555055555550555555555555555055555555555555555555555555555550555500000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000888880000000000000000000000008880800088808080888088800000000000008800000000000000000000000008888800000000000000
00000000000000008880088000000000000000000000008080800080808080800080800000000000000800000000000000000000000088008880000000000000
00000000000000008800088000000000000000000000008880800088808880880088000000000000000800000000000000000000000088000880000000000000
00000000000000008880088000000000000000000000008000800080800080800080800000000000000800000000000000000000000088008880000000000000
00000000000000000888880000000000000000000000008000888080808880888080800000000000008880000000000000000000000008888800000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000007770777000000770707077707000700000007700077077707770777070000000000000000000000000
00000000000000000000000000000000000000000000007070070000007000707007007000700000007070707070707770707070000000000000000000000000
00000000000000000000000000000000000000000000007770070000007770770007007000700000007070707077007070777070000000000000000000000000
00000000000000000000000000000000000000000000007070070000000070707007007000700000007070707070707070707070000000000000000000000000
00000000000000000000000000000000000000000000007070777000007700707077707770777000007070770070707070707077700000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000007770777077700770707000000000000000007700000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000700707070707000707000000000000000000700000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000700770077707000770000000000000000000700000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000700707070707000707000000000000000000700000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000700707070700770707000000000000000007770000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000770707077707770000077000000000000007700000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000007000707007007070000007000000000000000700000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000007770777007007770000007000000000000000700000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000070707007007000000007000000000000000700000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000007700707077707000000077700000000000007770000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000555050505550555000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000505050505050505000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000555050505500555000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000005050505050505050f0000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000050500550505050500000007777777000000000000000000f0f0000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000777777700000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000007777000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000007770000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000070000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000007000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000070000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000700000000555000005050500000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000700000000055500500505050000000000000000000000000000000000f000000000000000000
0000000000000000000000000000000000000000000000000000700000000f505000005550555000000000000000000000000000000000000000000f00000000
0000000000000000000000000000000000000000000f0f000007f00000000050500500005050500000000000000000000000000f000000f00f00000000000000
000000000000000000000000000000000000000000000000007000000000005050000000505550000000000000000000000000000000000000000ff000000000
f0000000000000000000f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000f000000000000f0000f0000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000f0000000000000000000000000000000888880000000000000000000000000000000000000000000000000000000000
0f0000000000000000f0f0f000000000000000000000000000000000008866008888880000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000f00000000000000000000000088866607777880000000000000000000000000f00000000000000000000000000000000
000000000000000000000000000000000000000f00000000000000000888666077778800008880000000000000000000000000000f0000000000000000000000
0000000000000000000000000000000000000000000000000000000008886660777788000088860000000000000000000000000000000000000000000000000f
0000000f000000000000000000000000f00000000000000000000000022222227777880000888660000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000002222222777778220008826600000000000000000000000000f00000000000000f0000000
00000000000000000000000000000000000000000000000000000000222611277777822222222660000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000007610001188777822222226600000000f00000000000000000000000000000000000000000
000000000000000000000000000000000000000000000f000000076110000018888800111222d000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000007111100000888888000112221d00000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000f711110002288888800001111000d0000000000000000000000000000000000000000000000000
00000000000000f0f00000000000000000000000000000000071111100f22888882220011100000d000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000071111100008888882220111100000d0000000000000000000000000000000000000000f0000000
f000000000000000000000000000000000000000000000f007111111100f88888822201110000001d00000000000000000000000000000000000000000000000
000000000000000f00000000000000f0000000000000000007111111111188888800001110000011d00000000000000000000000000000000000000000000000
000000000000000000000000000000000000f0000000000007111111111110000000011110000011d00f00000000000000000000000000f00000000000000000
00000000000000000000000000000000000000000000000007111111111110000000011111000111d000000000000000000f00000000f0000000000000000000
00000f0000000000000000000000000000000000000000000771111111111000000001111111111d100000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000077711111111000000001111111111d110000000000f0000000000000f0000000000000000000000
000000000000000000000000000000000000000000000000077711111111000000011111111111d510000000000000000000000000000000000000000f000000
00000000000000000000000000000000000000000000000007777111111100000001111111111d11100000000000000000000000000000000000000000000000
00000000000000000000000000000f00000000000000000007776661111110000001111111155515100000000000000f00000000000000000000000000000000
00000000000000000000000000000000000000000000000000776766661111100011111155555511000000000000000000000000000000000000000000000000
f00000000000000000000000000000000000000000000000000666666666611111111dd5d5555510000000000000000000000000000000000000000000000000
000000000000000000000000000000000f00000000f00000000067666666ddddddddddd555555100000000000000000000000000000000000000000000000000
0000000000f000000f0000000000000000000f000f000000000006666666d6ddddddddd5d55550000000000000000000000000000000000000f00f00f0000000
000000000000000000000000000000000000000000000000000000066666ddddddddddd555500000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000006dd6dddddddd550000000f0000000000f000000000000000000000000f0000f000f000
0000000000000000000000000000000000000000000000000000000000000dddddddd00000000f0000000000000000000000000000000000000000f000000000
00f00000000000000000000000000000000000000000000000000000000000000f00000000000000000000000000000000000000000000000000000000000000
000000000000000001101110111011101110000001111100011111000000000000cc0c0010100110101011101010101011101010111000000000000000000000
0000000000000000100001001010101001000000110101101100f1100f0fcc000cccc0001f1010101010101010101010010010100100000000000000000000f0
0000000000000000111001001110110001000000111011101101011000000cc11ccccc00111f10101010110011101100010011000100000000000f0000000000
000000000000000000100100101010100100000011010110110001100000cccccccc000000101010101010100010101001001010010000f00000000000000000
0000000000000000110001001010101001000000011111000111110000000cccccc10000111011000110101f111f101011101010111000000000000000000000
000000000f0000000000000000000000000000000000000000000000000001cccc10000000000000000000000000000000000000000000000000000000000000

__map__
a0800000000080000040007e840000007c8400000078880000007c840000007e84000000800000c0827e840000007c8400000078880000007c840000007e8400000080a09f500080880040827e840000007c8400000078980000007c840000007e84000000800000000082840000008484000000888800000084840000008284
0000007e840000007c8400000078880000007c840000007e8400000080840000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
a88000000000828184000084818400008882840000848184000082818400007e818400007c8184000078828400007c818400007e818400007e840000007c8400000078880000007c840000007e84000000800000d0007e840000007c840000007a980000007c840000007e84000000800000408282848e000084849f00008890
9f003884849f000082849f00007e849f00007c849f000078909f00007c848e00007e84000000808800c0827e840000007c840000007a880000007c840000007e84000000808400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ac8000000000828200000084820000008884000000848200000082828e00007e828e00007c8200000078840000007c820000007e828e000080848e0000808400000080848e6082808400000080848ea0828084000000828200000084840000008a980000008484000000828200000080848c000080848ca08280888400008084
86608280848600007e820000007c84000000769000c0827c840000007e82000000828200000084840000008a980040828484000000828200000080000000008282000000848400000088840000008484000000828200000080840000000000000000000000000000000000000000000000000000000000000000000000000000
9580000000007e828e00007c820000007898beff827c820000007e828e000080000000007e820000007c90be00007e8200000080008ec0827e828e00007c900000007e848e00007c840000007688be00007c840000007e820000008086004082808800018280008e200000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
a6800000000082828e000084840000008888000000848400000082828e0000808c00c000808c0040008000be000082828e000084840000008890000000848400000082828e00007e820000007c84be00007a8800c0007c840000007e8200000080000000007e820000007c8200000076900000007c820000007e820000008084
8e0000828200000084820000008a900000008482000000828200000080000000008282000000848800600088880000008488006000828200000080820000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
a0800000000082828e00008484000000888800000084840000008288000000848400a082888800a082848400000082820000008000000000820000c082840000c082868800c08288880000008482000000820000e08284820000008a90000000848200708282820070827e820070827c8200000076900040827c820000008282
8e7082828200708284820070828a880060828482007082828200708280888e01820000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1050783088000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
202058105b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2010380038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
7000380038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
201058105b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2020783088000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
10508880da000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
010500200c1531615316153161530c1530c1530c1530c1531615316153161530c1530c1530c153161531615316153161530c1530c1530c1531615316153161530c1530c1530c1531615316153161530c1530c153
010c001010b5413b7513b650000010b5413b7513b650000010b5413b7513b650000004b6407b7507b6504b2504b54000000000000000000000000000000000000000000000000000000000000000000000000000
010c00021807418075000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010100080c2200c2310c2410c2510c2510c2410c2310c2210f20313203162030d203142030c2030f203172031220316203162030d203122031620312203162030c2030c2030c2030c2030c2030c2030c2030c203
010c000400750077500c7500770000701007010070000700007000070000700007000070000700007000070000700007000070000700007000070000700007000070000700007000070000700007000070000700
010c002024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0024b0224b0224b0224b0224b0224b0224b0230b00
010c00200c1730c11301600000003c6333c6150c1730c1133c6333c6153c6053c6050c1730c113000003c6053c6333c6150c1730c113000000000000000000003c6333c61500000000003c6333c6150000000000
010c00200c1730c113016000000000000016000c1730c113000000000000000000000c1730c113000000000000000000000c1730c113000000000000000000003c6333c61500000000003c6333c6150000000000
010c00200c1730c11301600016003c6333c604026040000000000186050c1730c1133c6333c6043c6033c6030c1730c11300000000003c6333c6043c6000000000000186050c1730c1133c6333c6043c6033c633
010c00100c1730c11301600016003c6333c604026040000000000186050c1730c1133c63300000000000000013a00000000000000000000000000000000000000000000000000000000000000000000000000000
01060020180500c7151b0400f7151f030137101371500200180500c7151b0400f7151f030137101371500200180500c7151b0400f7151f030137101371500200180500c7551b0400f7451f030137311371522205
010600201b0500f7151f04013715220301671016715000001b0500f7151f04013715220301671016715000001b0500f7151f04013715220301671016715000001b0500f7151f0401371522030167111671529700
0106000020050147152404018715270300f7100f7150000020050147152404018715270300f7100f7150000020050147152404018715270300f7100f7150000020050147152404018715270300f7110f71529700
010600001f050137152404018715260301a7101a715000001f050137152404018715260301a7101a715000001f050137152404018715260301a7101a715000001f050137152404018715260301a7111a71529700
010600201f050137152304017715260301a7101a715000001f050137152304017715260301a7101a715000001f050137152304017715260301a7101a715000001f050137152304017715260301a7101a71529700
010c00200c1730c11301600016003c633000000c1730c1130c1730c1130000000000000000000000000000000c1730c1130000000000000000000000000000000c1730c11300000000003c6333c6033c6153c634
010c00200cb700cb700cb50000000fb5413b7513b55000000cb740cb500cb7000b520cb7200b520cb55000000fb740fb500fb7003b520fb7203b520fb550000013b7413b5013b7007b5213b7207b5213b5500000
010c00200fb740fb750fb750000013b7416b7516b75000000fb740fb500fb7003b520fb7203b520fb5507b0513b7413b5013b7007b5213b7207b5213b550000016b7416b5016b700ab5216b720ab5216b5500000
010c001014b7414b7514b750000018b741bb751bb750000014b7414b5014b7008b5214b7208b520fb550cb0514000240001800027000180002700000000000000000000000000000000000000000000000000000
010c000018b7418b5018b700cb5218b720cb5218b55000001ab741ab501ab700eb521ab720eb521ab550cb0500000000000000000000000000000000000000000000000000000000000000000000000000000000
010c002013b7413b7513b750000017b741ab751ab750000013b7413b5013b7007b5213b7207b5213b550bb0517b7417b5017b700bb5217b720bb5217b55000001ab741ab501ab700eb521ab720eb521ab5500000
010c00200cb100cb200cb300cb400cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb500cb520cb520cb520cb420cb320cb220cb120cb00
010c00200fb100fb200fb300fb400fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb500fb520fb520fb520fb420fb320fb220fb120fb00
010c001014b1014b2014b3014b4014b5014b5014b5014b5014b5214b5214b5214b4214b3214b2214b1200b0000000000000000000000000000000000000000000000000000000000000000000000000000000000
010c001013b1013b2013b3013b4013b5013b5013b5013b5013b5213b5213b5213b4213b3213b2213b120000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010c002007b1007b2007b3007b4007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5007b5207b5207b5207b4207b3207b2207b1207b00
010c002024b1024b2024b3024b4024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5024b5224b5224b5224b4224b3224b2224b1230b00
010c002027b1027b2027b3027b4027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5027b5227b5227b5227b4227b3227b2227b1227b00
010c00102cb102cb202cb302cb402cb502cb502cb502cb502cb522cb522cb522cb422cb322cb222cb1224b0024000240002400024000240002400024000240002400024000240002400024000240002400024000
010c00102bb102bb202bb302bb402bb502bb502bb502bb502bb522bb522bb522bb422bb322bb222bb122400024000240002400024000240002400024000240002400024000240002400024000240002400024000
010c00201fb101fb201fb301fb401fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb501fb521fb521fb521fb421fb321fb221fb121fb00
010c002024b7024b7024b502400027b542bb752bb552400024b7424b5024b7018b5224b7218b5224b552400027b7427b5027b701bb5227b721bb5227b55240002bb742bb502bb701fb522bb721fb522bb5500000
010c002027b7427b7527b75240002bb742eb752eb752400027b7427b5027b701bb5227b721bb5227b552bb052bb742bb502bb701fb522bb721fb522bb55240002eb742eb502eb7022b522eb7222b522eb5500000
010c00102cb742cb752cb750000030b7433b7533b75000002cb742cb502cb7020b522cb7220b5227b550cb0500000000000000000000000000000000000000000000000000000000000000000000000000000000
010c001030b7430b5030b7024b5230b7224b5230b550000032b7432b5032b7026b5232b7226b5232b550cb0500000000000000000000000000000000000000000000000000000000000000000000000000000000
010c00202bb742bb752bb75000002fb7432b7532b75000002bb742bb502bb701fb522bb721fb522bb550bb052fb742fb502fb7023b522fb7223b522fb550000032b7432b5032b7026b5232b7226b5232b5507b00
0130002018b1018b3118b5118b5118b5118b5218b3218b1218b1018b3118b5118b5118b5118b5218b3218b121fb101fb311fb511fb511fb511fb521fb321fb121bb101bb311bb511bb511bb511bb521bb321bb12
0130000018b1018b3118b5118b5118b5118b5118b5218b321bb101bb311bb511bb511bb511bb511bb521bb3227b1027b3127b5127b5124b5124b5124b5224b3222b1122b3122b5122b511fb511fb511fb521fb32
011800200c7540c7540c1730c754137540c6050c7540c7000c7540c7540c6750c75413754187000c7540c7050c7540c7540c1730c754137540c1030c1730c7050c7540c7540c6750c7540c675137040c7540c705
0118002018b5018b5018b3118b110cb5018b5018b310cb5018b5018b5018b3118b110cb5018b5018b310cb5016b5016b4015b5015b4013b5013b4016b5016b4013b5013b4011b5011b4013b5013b5113b4113b31
010800000c1760c15618136181162a100341003e1003f100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100
01080000181762415624136301162a100341003e1003f100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100
007c00000020024255242552425524255302550020000200002003520000200002000020000200002000020000200002000020000200002000020000200002000020000200002000020000200002000020000200
000c0000186750c635000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200002467324673246632466318653186430c6330c623000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00040000302642f6502d2642c650292642865025254246401f2441e63015234146200360411630016000f630016000d630016000b6300a6200962008610076100760000200002000020000200002000020000200
0004121409234086200c2340c6200f2340e620132441263017244166301b2441a6301e2441d634202441f63422254216442425423644292042860425204246041f2041e604152041460409004086040020400204
0108161800b3000b1001b3001b1002b3002b1003b4003b1004b4004b1005b4005b1006b4006b1007b4007b1008b4008b1009b4009b100ab500ab200bb500bb200000000000000000000000000000000000000000
0108161801b3001b1002b3002b1003b3003b1004b4004b1005b4005b1006b4006b1007b4007b1008b4008b1009b4009b100ab400ab100bb500bb200cb500cb220000000000000000000000000000000000000000
010800001f5560c5360c5260c51600500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500
011800200c7540c7540c1730c754137540c6050c7540c7000c7540c7540c6750c75413754187000c7540c7040c7540c7540c1730c754137540c1030c1730c7050c1730c7540c6750c754137540c1730c6750c704
011800200c1730c7540c6750c754137540c1730c6750c7540c1730c7540c6750c754137540c1730c6750c7540c1730c7540c7540c173137540c7540c1730c7040c7540c1730c7540c7540c675137540c6750c605
011800100c173187540c675187541f7540c1730c675187540c173187540c675187541f7540c1730c6751870400000000000000000000000000000000000000000000000000000000000000000000000000000000
011800000cb500fb5013b500fb5018b501bb501fb501bb500fb5013b5016b5013b501bb501fb5022b501fb500fb5013b5016b5013b501bb501fb5022b501fb5013b5017b5018b501ab501fb5023b5024b5026b50
0118000024b5024b5024b3124b1118b5024b5024b3118b5024b5024b5024b3124b1118b5024b5024b3118b5022b5022b4021b5021b401fb501fb4022b5022b401fb501fb401db501db401fb501fb511fb411fb31
0118000018b501bb501fb501bb5024b5027b502bb5027b501bb501fb5022b501fb5027b502bb502eb502bb501bb501fb5022b501fb5027b502bb502eb502bb501fb5023b5024b5026b502bb502fb5030b5032b50
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000003c64033650366502a6402e6402363027620196201e6200a61010610026000160000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 0a150f1a
00 0b160f1b
00 0c17081c
00 0d18091d
00 0e19071e
00 0a100f1f
00 0b110f20
00 0c120921
00 0d130922
02 0e140623
01 4a556624
00 4b562625
01 4c573227
01 4d583327
00 4e593435
00 41423335
00 41423436
00 4a413436
00 41423437
02 41423337
02 4e4e4844

