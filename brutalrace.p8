pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- brutal pico race 1.0.2
-- @yourykiki

local scale,
      rdz,--rb length
      nb_rb,--rb number
      road
      =
      48,64,16,{}
local frm,endfrm,nb_finish,
      nb_player,nb_ai,
      nb_player_and_ai,
      num_trk,
      spxmax,
      trklen,lastnss,nss,
      racepos,rd_info
      
-- player infos
local cam,
      ss,sss,--spaceship,shadow
      zz,zzz,--rot ss,sss
      ship,ctrl,ssp,sssp,rot,
      ird,--pl road indice
      spd,sdx,--,skyx
      height,yoffset,ctr,csy
      ={},
       {},{},
       {},{},
       {},{},{},{},{},
       {},
       {},{},
       128,{},{},128

local ship_info={
{name="rezar ",accel=2},
{name="virto ",accel=2},
{name="tridnz",accel=3},
{name="pk-12 ",accel=2},
{name="byx300",accel=3},
{name="scard ",accel=3},
{name="tw-50 ",accel=2},
{name="7rider",accel=2},
{name="ectox ",accel=1},
{name="lt-ai ",accel=1}
}

function _init()
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
 music(-1)
 sfx(42)
 cls()
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
  initshipaccel(ship[p],ai_skill)
  -- human
  if p<=nb_player then
   cam[p],sdx[p],
   yoffset[p],ctr[p]=
    {x=0,y=128,z=0,flw=p},0,
    height*(p-1),
    {x=64,y=height/4}
  else
   ssp[p].skill=ai_skill
  end
  --start on segment 1
  add(road[1].pl,p)
 end
end

function initshipaccel(shipp,ai_skill)
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
   if shipp.r_acc[
    flr(shipp.accurv[i])]==nil then
    shipp.r_acc[
     flr(shipp.accurv[i])]=i
   end   
  end
  val0=val1
 end
 shipp.spxmax=(5-shipp.accel)*2
end

-- loading a ship
function loadss(nbss,ss,sss)
 local m,xmin,xmax,zmin,zmax=
  0,8,0,8,0
 for k=7,0,-1 do--sprite y
  for j=0,7 do--sprite x
   local shad=false
   for i=2,0,-1 do--layer
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
   --half z  k%2==0
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
function update_end(p,ctrlp,spd)
 ctrlp.left,ctrlp.right,ctrlp.boost,
  ctrlp.accel,ctrlp.brake=
 false,false,false,false,false
 if p<=nb_player then
  if frm>endfrm+45 then
   local flw=cam[p].flw
   if (btnp(2,p-1)) flw+=1
   if (btnp(3,p-1)) flw-=1
   cam[p].flw=mid(1,flw,nb_player_and_ai)
  end
  snd_engine(p,spd)
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
   -- laps
   if band(v.t,256)==256 then
    sspp.lap+=1
   end
  end
  -- update a player/ai
  local shipp,ctrlp=
   ship[p],ctrl[p]
  spxmax=shipp.spxmax
  --handle control
  sspp.update(p,ctrlp,spdp,sspp)
  updateship(p,v,vn,sspp,rot[p],spdp,camp,ctrlp,shipp)
  -- camera tracking
  for i=1,nb_player do
   local camp=cam[i]
   if camp~=nil and camp.flw==p then
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
 -- calc ships rank
 if (frm%15==0) calcrank()
 -- particules
 update_particles(p_spark)
 update_particles(p_boost)
 update_particles(p_trail)
 -- end race ?
 race_end()
end

function calcrank()
 local ranks={}
 
 for p=1,nb_player_and_ai do
  ranks[p]={
   key=ssp[p].z+ssp[p].lap*trklen,
   p=p
  }
 end
-- ce_heap_sort(ranks)
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
-- f(pos.z)
function get_ird(pos)
 -- 4 offset for shadow
 return flr((pos.z-4)/rdz)%#road
end

function update_ai(p,ctrlp,spd,sspp)
 -- +skill=+decision
 local skill=sspp.skill
 if rnd(10)>6-skill then
  -- where to go ?
  local xmin,xmax=
   sspp.xmin,sspp.xmax
  local v=road[ird[p]+1]
  if band(v.t,2)==2 
   and v.fx<0 then
   xmin,xmax=60,70
  elseif band(v.t,8)==8
   and v.fx>0 then
   xmin,xmax=-60,-70
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
  --boost could be adaptive 
  --among player rank
  local pouet=rnd(15)>15-skill
  if ctrlp.cdn_bst==10
   and pouet then
   ctrlp.boost=false
  elseif ctrlp.can_bst 
   and pouet then
   ctrlp.boost=true
  end
 end
 -- accel,decel
 if spd.prc<=(6+skill)*10 then
  ctrlp.accel=true
 else
  ctrlp.accel=rnd(10)>6-skill
 end
  
 -- race finished ?
 if sspp.lap>3 then
  sspp.update=update_end
  add(racepos,p)
 end
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
  sfx(46,p*2-1,mid(0,ctrlp.cdn_bst,9)*2,2)
 else
  snd_engine(p,spd)
 end
 -- finished ?
 if ssp.lap>3 then
  nb_finish+=1
  ssp.update=update_end
  add(racepos,p)
 end
end

function updateship(p,v,vn,sspp,rot,spdp,camp,ctrlp,shipp)
 --apply
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
 -- boost
 if ctrlp.boost then
  -- charging
  ctrlp.cdn_bst=min(10,
    ctrlp.cdn_bst+recover_cdn(sspp))
 elseif not ctrlp.boost
  and ctrlp.cdn_bst>0 then
  -- applying
  if (p<=nb_player) sfx(45,p*2-1)
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
    and p<=nb_player then sfx(49)
  end
 end
 spdp.bz=max(0,spdp.bz-0.025)
 -- accel -1*...
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
 -- update position z
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
  -(1*spdp.z/getaccel(shipp.accurv,100)
     *v.fx*0.8)
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
     sfx(43)
    end
    create_particles(p_spark,
     {x=(sspp.x+ssppn.x)/2,
      y=(sspp.y+ssppn.y)/2,
      z=(sspp.z+ssppn.z)/2},
     {z=(spdp.z+spdpn.z)/2})
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

function handle_road_collision(sspp,v,vn,xtmp,spdp,shipp,prc)

 local xl,xr=v.x-56,v.x+56
 local l1,l2,l3,r1,r2,r3=
  xl-35.7,xl-80.9,xl-162.4,
  xr+35.7,xr+80.9,xr+162.4

 local lborx,rborx=l1,r1
 
 if band(v.t,4)==4 then
  if band(vn.t,8)==8 
   and band(v.t,8)==0 then
   lborx=lerp(l1,l2,prc)
  end
  if band(vn.t,2)==2 
   and band(v.t,2)==0 then
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
   spdp.z*=0.875
   spdp.prc=get_r_accel(
    shipp.r_acc,spdp.z)
   spdp.cdn=frm
   sspp.hp-=getdamage()
   sspp.xmin,sspp.xmax=-5,5
   if not isnotvisible(sspp,cam[1],0) then
    sfx(44)
   end
  end
  -- fx
  create_particles(p_spark,
   {x=sspp.x+coll*cos(rot.z)*8,
    y=sspp.y+sin(rot.z)*8,
    z=sspp.z},{z=spdp.z*0.8})
 end
end

function applyrot(ss,m,sort)
 local rr={}
 -- faster 0.02cpu but +30 tokens
 local xx,xy,xz,
       yx,yy,yz,
       zx,zy,zz
          =m.xx,m.xy,m.xz,
           m.yx,m.yy,m.yz,
           m.zx,m.zy,m.zz

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
-- if (sort) ce_heap_sort(rr)
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
    fx=t.curv,c=13,t=4,pl={},parts={}}
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
   if (t.frc~=nil) v.t=t.frc
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
  local sdy=flr(ctr[p].y+ssp[flw].y/16)
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
--  print(spz.." "..coef,7,8,1)
  drawbar(5,ctrl[flw].cdn_bst,12,
   ctrl[flw].can_bst and 11 or 8)
  drawbar(3,ssp[flw].hp,100,8)
  rectfill(95,2,123,14,1)
  print("lap "..mid(1,3,sspp.lap)
   .."/3",
   96,3,7)
  print("pos "..sspp.rank.."/"
   ..nb_player_and_ai,96,9)
  rectfill(4,12,4,height-4,6)
  for i=1,nb_player_and_ai do
   local h=lerp(height-4,12,
    ssp[i].z/trklen) 
   rectfill(4,h,7,h,i==flw and 7 or 6)
  end
  if endfrm>0 then
   local victory=racepos[1]==p 
    and 195 or 227
   spr_grd(victory,48,24,32,16,0x1b0,offset)
  end
 end
 --restore camera
 clip()
 camera(0,0)
 -- countdown
 if frm<150 then
  local start=107+flr(frm/30)
  offset+=1
  spr_grd(start,60,32,8,16,0x1b0,offset)
 end
 -- endrace
 if endfrm>0 then
  offset+=1
  local w,h=44,#racepos*6+2
  local by=(124-h)/2
  rectfill(42,by,42+w,by+h,1)
  rect(42,by,42+w,by+h,0)
  for i=1,#racepos do
   print(i..
    " "..ship[racepos[i]].name..
    " "..racepos[i],
    45,(i-1)*6+by+2,7)
  end
 end
 if nb_player==1 then
  palt(0, false)
  rectfill(60,16,91,16,6)
  for i=0,3 do
   pal(peek_spr(0x02b0,i),
    flr((ssp[1].z/32-i))%4==0
     and 10 or 0)
  end
  spr(7,60,0,4,2)
  local hps=3-flr(ssp[1].hp/25)
  if hps>0 then
   hps=min(hps,3)
   spr(161+2*hps,68,0,2,2)
  end
  palt()
  pal()
 end
 -- debug
-- print("cpu="..stat(1)
--  .." fps="..stat(7)
--  .." ram="..stat(0)
--  ,0,height-5,1)
--[[ print("camx="..cam[1].x
  .." sspx="..ssp[1].x,0,32)
]]-- 
end

function drawroad(cam,ird,p)
 -- calculating road
 local r,rn,cfx,v,vn,vz=
  nil,nil,0--render info
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
  r={}--render current
  v,vn=road[i%#road+1],
   road[(i+1)%#road+1]
  vz=v.z+flr(i/#road)*trklen
  xl,xr,iy,ylgt=
   v.x-56,v.x+56,v.y,
   lerp(v.y,vn.y,0.5)
  -- clip
  dz=max(vz-cam.z,7)

  prc=(cam.z+7-vz)/rdz
  if dz==7 then
   --interpolation
   --prc=(cam.z+7-vz)/rdz
   xl=lerp(xl,vn.x-56,prc)
   xr=lerp(xr,vn.x+56,prc)
   iy=lerp(v.y,vn.y,prc)
  end
  -- current part
  --prc=(cam.z+7-vz)/rdz
  fct=scale/dz--scale
  cfx=cfx-v.fx

  r.x1=proj_fct_x(xl-cam.x,cfx)
  r.x2=proj_fct_x(xr-cam.x,cfx)
  r.y=proj_fct_y(iy-cam.y)

  r.xl1=proj_fct_x(xl-32-cam.x,cfx)
  r.xr1=proj_fct_x(xr+32-cam.x,cfx)
  r.y2=proj_fct_y(iy+16-cam.y)
  r.xl2=proj_fct_x(xl-64-cam.x,cfx)
  r.xr2=proj_fct_x(xr+64-cam.x,cfx)
  r.xl3=proj_fct_x(xl-80-cam.x,cfx)
  r.xr3=proj_fct_x(xr+80-cam.x,cfx)
  r.y3=proj_fct_y(iy+48-cam.y)
  r.y4=proj_fct_y(iy+128-cam.y)

  -- drawing road
  if rn~=nil then 
   --color fade (-2 for lights)
   local tx=(i-ird)/(nb_rb-2)
   local even,odd,flp=
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

   if band(v.t,256)==256 then
    drawstartlane(r,rn,cfx,
     odd,flp,cam,vz)
   end
   -- other ? (if possible)
   local lborx,lbory,
    rborx,rbory,lcol=
    xl-32,ylgt+16,xr+32,ylgt+16,
    peek_spr(0x0170,irdlast-i-1)
   if band(v.t,4)==4 then
    if r.y>rn.y then
     --main road
     otri(r.x1,r.y,r.x2,r.y,rn.x1,rn.y,even,flp)
     otri(rn.x1,rn.y,rn.x2,rn.y,r.x2,r.y,even,flp)
    end
    -- tokens left1=right1
    -- but see perfs
    -- left 1
    otri(r.xl1,r.y2,rn.x1,rn.y,r.x1,r.y,even,flp)
    otri(rn.xl1,rn.y2,rn.x1,rn.y,r.xl1,r.y2,even,flp)
    if band(vn.t,8)==8 
     and band(v.t,8)==0
     then --left1 to left2
     otri(r.xl1,r.y2,rn.xl2,rn.y3,rn.xl1,rn.y2,even,flp)
     lborx,lbory=xl-48,ylgt+32
    end 
    -- right 1
    otri(r.xr1,r.y2,rn.x2,rn.y,r.x2,r.y,even,flp)
    otri(rn.xr1,rn.y2,rn.x2,rn.y,r.xr1,r.y2,even,flp)
    if band(vn.t,2)==2 
     and band(v.t,2)==0
     then --right1 to right2
     otri(r.xr1,r.y2,rn.xr2,rn.y3,rn.xr1,rn.y2,even,flp)
     rborx,rbory=xr+48,ylgt+32
    end
   end
   if band(v.t,8)==8 then
  	 -- left 2
   	lborx,lbory=xl-64,ylgt+48
    if band(vn.t,8)==8
  	  or band(v.t,16)==16 then
   	 otri(rn.xl2,rn.y3,rn.xl1,rn.y2,r.xl2,r.y3,even,flp)
    else
     --left2 to left1
     lborx,lbory=xl-48,ylgt+32
    end
    otri(r.xl2,r.y3,rn.xl1,rn.y2,r.xl1,r.y2,even,flp)
    if band(vn.t,16)==16
     and band(v.t,16)==0 then
     --left2 to left3
     otri(r.xl2,r.y3,rn.xl3,rn.y4,rn.xl2,rn.y3,even,flp)
     lborx,lbory=xl-72,ylgt+88
    end
   end
   if band(v.t,2)==2 then
    -- right 2
   	rborx,rbory=xr+64,ylgt+48
    if band(vn.t,2)==2
     or band(v.t,1)==1
     then
     otri(rn.xr2,rn.y3,rn.xr1,rn.y2,r.xr2,r.y3,even,flp)
    else
     rborx,rbory=xr+48,ylgt+32
    end
    otri(r.xr2,r.y3,rn.xr1,rn.y2,r.xr1,r.y2,even,flp)
    if band(vn.t,1)==1
     and band(v.t,1)==0 then
     --right2 to right3
     otri(r.xr2,r.y3,rn.xr3,rn.y4,rn.xr2,rn.y3,even,flp)
     rborx,rbory=xr+72,ylgt+88
    end
   end
   if band(v.t,16)==16 then
   	--left 3
    if band(vn.t,16)==16 then
     lborx,lbory=xl-80,ylgt+128
     otri(rn.xl3,rn.y4,rn.xl2,rn.y3,r.xl3,r.y4,even,flp)
    else
     --left 3 to left 2
     lborx,lbory=xl-72,ylgt+88
    end
    otri(r.xl3,r.y4,rn.xl2,rn.y3,r.xl2,r.y3,even,flp)
   end
   if band(v.t,1)==1 then
    --right 3
    if band(vn.t,1)==1 then
     rborx,rbory=xr+80,ylgt+128
     otri(rn.xr3,rn.y4,rn.xr2,rn.y3,r.xr3,r.y4,even,flp)
   	else 
     rborx,rbory=xr+72,ylgt+88
   	end
    otri(r.xr3,r.y4,rn.xr2,rn.y3,r.xr2,r.y3,even,flp)
   end
  	-- lights
   lz=vz-cam.z+rdz/2
   if lz>7 then--and i%2==1   	
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
   v.pl,0,flr(i/#road)*trklen,{}
  -- sort on z
  if #pl>1 then
   for inp=1,#pl do
    add(pls,
     {np=pl[inp],key=ssp[pl[inp]].z})
   end
--   ce_heap_sort(pls)
   shellsort(pls)
   for inp=1,#pls do
    pl[inp]=pls[#pls-inp+1].np
   end
  end
  for inp=1,#pl do
   np=pl[inp]
   pcfx=(ssp[np].z-vz)/rdz*v.fx+cfx
   --and the others ? dec on pos
   local shake=spd[np].z+spd[np].gz+spd[np].bz>=14
   if shake then
    ox,oy=cam.x,cam.y
    cam.x+=rnd(2)-1
    cam.y+=rnd(2)-1
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
  local road_parts=v.parts
  for inp=1,#road_parts do
   part=road_parts[inp]
   pcfx=(part.z-vz)/rdz*v.fx+cfx
   part.draw(part,cam,pcfx,ntrk,p)
  end
  -- next
  rn=r
 end
end

function drawstartlane(r,rn,cfx,col,flp,cam,vz)
 otri(rn.xl3,rn.y4,rn.xl1,rn.y2,r.xl1,r.y2,col,flp)
 otri(r.xl3,r.y4,rn.xl3,rn.y4,r.xl1,r.y2,col,flp)
 otri(rn.xr3,rn.y4,rn.xr1,rn.y2,r.xr1,r.y2,col,flp)
 otri(r.xr3,r.y4,rn.xr3,rn.y4,r.xr1,r.y2,col,flp)
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
 line(xn,yn,x2,y2,col)
end

function isnotvisible(pos,cam,ntrk)
 return pos.z+ntrk<cam.z+7 or
  pos.z+ntrk>cam.z+10*rdz
end

function brilar()
 return max(flr(rnd(8))-6,0)
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
    if spd~=nil then
     col=flr(spd.prc*0.03)+brighter
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
 if rot~=nil then
  rot.z=lerp(rot.z,newz,0.25)
  newz=rot.z
 end
 if pos.yflat then
  pos.x=pos.x
   +sin(1-newz)*pos.yflat
  pos.y=ytmp+pos.y
   +cos(newz)*pos.yflat
 end
 if spdp~=nil then 
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
 {name="track    ",maxval=3},
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
 music(0)
end

function update_intro()
 --handle controls
 if btnp(2) then
  nb_choice=max(1,nb_choice-1)
  sfx(40)
 elseif btnp(3) then
  nb_choice=min(#intro_mnu,nb_choice+1)
  sfx(40)
 end
 local mnuitem=intro_mnu[nb_choice]
 if (mnuitem.val==nil) mnuitem.val=1
 --
 if btnp(0) then
  mnuitem.val=max(1,mnuitem.val-1)
  sfx(40)
 elseif btnp(1) then
  mnuitem.val=min(mnuitem.maxval,mnuitem.val+1)
  sfx(40)
 end
 local ship2=intro_mnu[5]
 if btnp(0,1) then
  ship2.val=max(1,ship2.val-1)
  sfx(40)
 elseif btnp(1,1) then
  ship2.val=min(ship2.maxval,ship2.val+1)
  sfx(40)
 end
 -- start game
 if offset>0 and btnp(4) then
  sfx(41)
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
   initshipaccel(ship[p],0)
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
 spr_grd(67,32,4,64,32,0x1b0,offset)

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
   if entry.values~=nil then
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
  spr(204,48,72,4,4)
  spr(131+2*(intro_mnu[3].val-1)
   ,56,80,2,2)
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
    k=ship[p].accurv[flr(i*100/28)]
    line(cx+i-1,86-j,cx+i,86-k,7)
    j=k
   end
  end
 end
-- print("cpu:"..stat(1),0,0,1)
end
-->8
--tris
local cpy=0

-- based on p01
-- https://www.lexaloffle.com/bbs/?tid=31478

function otri(x0,y0,x1,y1,x2,y2,col,flp)
 color(col)
 if (flp~=nil) fillp(flp)
 y0,y1,y2=
  band(y0,0xffff),
  band(y1,0xffff),
  band(y2,0xffff)
 if(y1<y0)x0,x1,y0,y1=x1,x0,y1,y0
 if(y2<y0)x0,x2,y0,y2=x2,x0,y2,y0
 if(y2<y1)x1,x2,y1,y2=x2,x1,y2,y1
 col=x0+(x2-x0)/(y2-y0)*(y1-y0)
 p01_trapeze_h(x0,x0,x1,col,y0,y1)
 p01_trapeze_h(x1,col,x2,x2,y1,y2)
 fillp()
end

function p01_trapeze_h(l,r,lt,rt,y0,y1)
 lt,rt=(lt-l)/(y1-y0),(rt-r)/(y1-y0)
 if(y0<0)l,r,y0=l-y0*lt,r-y0*rt,0 
 y1=min(y1,csy)
 for y0=y0,y1 do
  rectfill(l,y0,r,y0)
  l+=lt
  r+=rt
 end
end

-->8
-- ciura's sequence
--https://www.lexaloffle.com/bbs/?tid=2477
shell_gaps={701,301,132,57,23,10,4,1} 
function shellsort(a,cmp_idx)
 for gap in all(shell_gaps) do
  if gap<=#a then
   for i=gap,#a do
    local t=a[i]
    local j=i
    while j>gap and
       a[j-gap].key
       > t.key do 
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
 return accurv[mid(0,flr(prc),100)]
end

function get_r_accel(r_accurv,val)
 local prc=r_accurv[flr(val)]
 if (prc~=nil) return prc
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
 return 0.3/(6-flr(ssp.hp/25.0001))
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
 local fct1=scale/(part.z+pre)
 local fct2=scale/(wz2+pre)
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
	local ang=rnd()
	local speed=.1+rnd(1)
	return {
		x=pos.x,y=pos.y,z=pos.z,
		spx=sin(ang)*speed,
		spy=cos(ang)*speed,
		spz=sp.z,
		frm=10+flr(rnd(20)),
		draw=draw_spark
	}
end
function update_spark(part)
	if part.frm<=0 then
		return false
	end
	part.x+=part.spx
	part.y+=part.spy
	part.z+=part.spz
	part.spy-=0.1
	return true
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
  peek_spr(0x0130,flr(rnd(3)))
 return {
  x=rnd(256)-128,
  y=pos.y+rnd(56),
  z=pos.z+64+rnd(384),
  spx=0,spy=0,spz=-sp.z*0.75,
  frm=32+flr(rnd(32)),
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
 return shr(
  peek(addr+offset/2),
  offset%2*4)
end

function spr_grd(nbs,x,y,w,h,col_addr,offset)
 local sprx,spry,scol=
  (nbs%16)*8,flr(nbs/16)*8,0
 for i=0,w do
  for j=0,h-1 do
   scol=sget(sprx+i,spry+j)
   if scol==3 then
    pset(x+i,y+j,
     peek_spr(col_addr,(offset+flr(j/3))%30))
   elseif scol!=0 then
    pset(x+i,y+j,scol)
   end
  end
  offset+=1
 end
end

function drawbar(yp,val,vmax,col)
  local hx=lerp(4,36,abs(val/vmax))
 	rectfill(4,yp-1,hx,yp,col)
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
  {x=0,y=8,z=72+16*p,
   xflat=xflat,yflat=9,
   lap=0,rank=0,update=update_cntdn},
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
 if band(v.t,flg1)==flg1 then
  --right 2
  if band(vn.t,flg1)==flg1
   and band (v.t,flg2)==0 then
   borx=b2
  else
   --right 2 to right 1
   borx=lerp(b2,b1,prc)
  end
  if band(vn.t,flg2)==flg2
   and band(v.t,flg2)==0 then
   --right 2 to right 3
   borx=lerp(b2,b3,prc)
  end
 end
 return borx
end
-- test coll 2
function road_part2_collision(
  b2,b3,prc,flg,borx)
 if band(v.t,flg)==flg then
 	--left 3
 	if band(vn.t,flg)==flg then
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
--  printh("curv="..trk[i].curv
--   .." len="..(trk[i].len or "0")
--   .." frc="..(trk[i].frc or "0")
--   .." hgt="..(trk[i].hgt or "0")
--   .." hln="..(trk[i].hln or "0"))
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
 --/8 /4 for scale
 return
  abs(sspp.xflat/8
   -ssppn.xflat/8)
   <=shipp.width+shippn.width
  and abs(sspp.z/4
   -ssppn.z/4)*dz
   <=shipp.height+shippn.height
end

function snd_engine(p,spd)
 sfx(46+p,p*2,mid(0,flr(spd.z),11)*2,2)
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
00000000000990000007700000666666666666000000000000066600000000066000000000000006600000000000000660000000000000066000000000000000
44400444009999000009900006000000000000600000000000600060000000600600000000000060060000000000006006000000000000600600000000000000
0a0000a0000000000009900060000000000000060000000006000006000000600600000000000060060000000000006006000000000000600600000000000000
00000000000000000000000060000000000000060000000006000006000000600600000000000060060000000000006006000000000000600600000000000000
00066000000000000000000006000000000000060066000006000006000000600600066000000060060006600000006006000660000000600600066000000000
00066000000000000000000000666600000000060600600006000006000666000600600600066600060060060006660006006006000666000600600600000000
60066006000770000000000000000060000000066000060060000006006000000600600600600000060060060060000006006006006000000600600600000000
60066006000660000000000000000006000000066000006600000006005000000066000600500000006600060050000000660006005000000066000600000000
65566556600660060000000000000006000000060600000000600060007000000000000600700000000000060070000000000006007000000000000600000000
0a0660a0600000060000000000000000600000600066675606066600006000000000006000600000000000600060000000000060006000000000006000000000
00000000000000000000000000000000067566000000000060000000000666666666660000066666666666000006666666666600000666666666660000000000
00600600006006000000000003c3333333333c3003c3333333333c3003c3333333333c3000000000100000000000000000000000000000000000000000000000
00600600006006000000000003133366663331300313336666333130031333666633313000000000500000000000000060666060000000000000000000000000
06600660066006500000000031333333333333133133333333333313313333333333331300000000d00000000000000060006006000000000000000000000000
0666666006666560000000773e333000000333e33e333000000333e33e333000000333e300000000c00000000000000060060060000000000000000000000000
660000666665566600000066de000aaaaaa000edde000aaaaaa000edde000aaaaaa000ed00000000000000000000000060060600000000000000000000000000
660000666656656600000060009a22aaaa22a900009a22aaaa22a900009a22aaaa22a90000000000400000000000000060060666000000000000000000000000
005555000566665000000000309a652aa256a903309a652aa256a903309a652aa256a90300000000400000000000000000000000000000000000000000000000
0000000000a00a0000000000309aaaaaaaaaa903309a8aaaaaa8a903309a8aaaaaa8a90300000000900000000000000000000000000000000000000000000000
0000000000000000000000003309aaff9faa90333308aaff9fa8903333098aff9fa8903300000000900000000000000000000000000000000000000000000000
00066000000000000000000033300fff9ff0033333300fff9ff0033333308ff44f80033300000000a00000000000000000000000000000000000000000000000
000660000007700000000000333300f44f003333333300f44f003333333300f8ff00333300000000a00000000000000000000000000000000000000000000000
600660060007700060077006443330ffff033344443330f8ff033344443330877f03334400000000600000000000000000000000000000000000000000000000
60066006000770006007700644499077770994444449907778099444444990855f09944400000000600000000000000000000000000000000000000000000000
600660060006600060666606049990f77f099940049990f778099940049990f58809994000000000700000000000000000000000000000000000000000000000
6056650606666660605665065000000ff00000055000000990000005500000077800000500000000700000000000000000000000000000000000000000000000
60a00a060000000060a00a0655500000005005555550050000500555555005000050055500000000000000000000000000000000000000000000000000000000
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
00000500000000000000000000000000000000000050000000000000000000000000050000000000000000000000000000000000005000000000000000000000
11115511111111111111111111111111111111111155111111111111111111111111551111111111111111111111111111111111115511111111111111111111
00058888888888888888888888880000000000000055500000000000000000000005550000000000000005000050000111111111111111111111111111110000
15558888888888888888888888881111115111111151555111111111111111111555151111111511111115511551111171117771777111117771117177711111
15a5b1111115151111111511115111111151511111115a51111111111111111115a5111111151511111115111151111171117171717111111171171111711111
1555b111111555511111155555511111155551111115555111111111111111111555511111155551111115555551111171117771777111111771171117711111
25559522222555222222255555522222225552222255555222222222222222222555552222255522222225555552222171117171711111111171171111712222
15559511111595151111155555511111515951111155555111111555555111111555551111159515111115555551111177717171711111117771711177711111
255aa52111255551112111911151112255511222225aa5525222559999552225255aa52222255555222525955952522111111111111111111111111111115222
25555521212555512151215151525122555512222255555252222555555222252555552222255555225525555552552177711771177111117771117177715522
2599552151255551215121a1a1511152555512252255995252522555555225252599552252255555255525aaaa52555171717171711111111171171111715552
e55555e151e55aa1e151e1515151555e5aa51e555e55555e555ee555555ee555e55555e555e55aa5e555e555555e55517771717177711111777117117771555e
25aa6521112515511191115111511152555111252255aa52595225555552259525aa5522522555552595255aa552595171117171117111117111171171115952
e55565e555e5aa55e555e555555e555e55aa5e555e55555e555ee555555ee555e55555e555e5aa55e555e555555e55517111771177111111777171117771555e
e55965ee55555555e595e5a55a5e595e55555555ee59955e555ee555555ee555e55995ee55555555e595e5a55a5e59511111111111111111111111111111595e
e55565e555555555e555e555555e555e555555555e55555e5555555555555555e55555e555555555e555e555555e555e555555555e55555e55555555555e555e
eeee6eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
22226222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
11116111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11116111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11116111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11116111111111111111111111111111111111111115d9d191111111111111111111195111111111111111111111111111111111111111111111111111111111
11116111111111111111111111111111111111d51a15151d1d41111111111111111d451ad1111111111111111111111111111111111111111111111111111111
1111611111111111111111111111111111adddddd55555555555555555555555555555ddda111111111111111111111111111111111111111111111111111111
11116111111111111111111111111111ddddddddddd5151515151515151515151515ddddddd11111111111111111111111111111111111111111111111111111
111161111111111111111111111115ddddddddddddddddddddddddddddddddddddddddddddd51111111111111111111111111111111111111111111111111111
111161111111111111111a1115555555dddddddddddddddd77ddddddcd1128dd999aaaddaa777711111111111111111111111111111111111111111111111111
11116111111111111111aaa55555555555ddddddddddddddd66dddddd1128e9dd9aaaaa557777771111111111111111111111111111111111111111111111111
111161111111111115555a555555555555577ddddddddddddd66dddddd121199ddaa111155771111111111111111111111111111111111111111111111111111
11116111111115555555555555555555b55bb5b555555555556615555512159919a1155555771555511111111111111111111111111111111111111111111111
1111611111d555555555555555555555b33bb3b55555555555661555552815991aa15555557715555d1111111111111111111111111111111111111111111111
111161ddddddd5555555555555555555955555955555555556611555512159911aaaaa555777755dddd111111111111111111111111111111111111111111111
11dd6dddddddddd555555555555555555555555555555555566155555121599155aaaaa5577777ddddddd1111111111111111111111111111111111111111111
dddd6dddddddddddd5555555555555550000000555555555566155555281599155511aa15771111ddddddd1aaa11111111111111111111111111111111111111
dddd6dddddddddddddd55555555555555555555555555555566155555281599155555aa1d771ddddddddddaaaaa1111111111111111111111111111111111111
dddd6dddddddddddddddd55555555555555555555555555566115555121199915555aaa17771ddddddddddaaaaa1111111111111111111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddd666ccddd28e99911aaaaaa1177776dddddddddaaaaa1111111111111111111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddd666ccd1dd8e9911ddaaaa11d777766dddddddddaaad1111111111111111111111111111111111111
dddd6dddddddddddddddddddddddddddddddddddddddddddd1111111dd1111dddd1111ddd111111ddddddddddddd111111111111111111111111111111111111
dddd6dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd11111111111111111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd1111111111111111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd11111111111111111111111111111111
dddd6dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd1111111111111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd111111111111111111111111111111
dddd6dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd11111111111111111111111111111
dddd6dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd111111111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd11111111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd51111111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd5555111111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd555555511111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd55555555555111111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd5555555555555511111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd555555555555555551111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd55555555555555555555111111111111111111
dddd6ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd5555555555555555555555551111111111111111
dddd6ddddddddddddddddddddddddddddddddddddd00000000000000000000000000000000000000000000055555555555555555555555555111111111111111
dddd6ddddddddddddddddddddddddddddddddddddd01111111111111111111111111111111111111111111055555555555555555555555555511111111111111
dddd66666ddddddddddddddddddddddddddddddddd01177111111717177717771777117711111111177711055555555555555555555555555551111111111111
dddd6ddddddddddddddddddddddddddddddddddddd01117111111717117117171171171711111111111711055555555555555555555555555555511111111111
dddd6ddddddddddddddddddddddddddddddddddddd01117111111717117117711171171711111111177711055555555555555555555555555555551111111111
dddd6ddddddddddddddddddddddddddddddddddddd01117111111777117117171171171711111111171111055555555555555555555555555555555111111111
55555555555555555555555555555555555555555501177711111171177717171171177111111111177711055555555555555555555555555555555511111111
55555555555555555555555555555555555555555501111111111111111111111111111111111111111111055555555555555555555555555555555555111111
55555555555555555555555555555555555555555501177711111777177717771777177711111111177111055555555555555555555555555555555555511111
00000000000000000000000000000000000000000001111711111717171111171717171711111111117111000000000000000000000000000000000000000000
55111111111111111111111111551111111111111101177711111771177111711777177111111111117111011155111111111111111111111111111111111111
55508888888888888888888888880000000000000001171111111717171117111717171711111111117111000555000111111111111111111111111111110000
51558888888888888888888888881111111511111101177711111717177717771717171711111111177711055515111171117771777111117771117177711111
115ab1111111111111111115a511111115151111110111111111111111111111111111111111111111111105a511111171117171717111111171171111711111
1555b111111111111111111555511111155551111100000000000000000000000000000000000000000000055551111171117771777111111771171117711111
55559222222222222222222555552222255522222225555552222222555222225555522222222222222222255555222171117171711111111171171111712222
55559111111555555111111555551111159515111115555551111151595111115555511111155555511111155555111177717171711111117771711177711111
5aa55251115599911121112111a11122255115222525955952522255555222225aa5525222559999552225255aa5222111111111111111111111111111112222
55555251212555515121212151552122255515225525555552552255555222225555525222255555522225255555222177711771177111117711117177712222
55995251512555515121212191511152255515255525aaaa52555255555225225599525252255555522525259955225171717171711111111711171111712522
55555e5151e5555151e151e15151e555e55a15e555e555555e555e5aa55e555e55555e555ee555555ee555e55555e5517771717177711111171117117771555e
55aa62511125155111211121115111522551112595255aa5525952555552252255aa52595225555552259525aa55225171117171117111111711171171112522
55556e555ee555555ee555e55555e555e5aa55e555e555555e555e55aa5e555e55555e555ee555555ee555e55555e5517111771177111111777171117771555e
59956e555ee555555ee555e55995ee55555555e595e5a55a5e595e55555555ee59955e555ee555555ee555e55995ee51111111111111111111111111111155ee
55556e5555555555555555e55555e555555555e555e555555e555e555555555e55555e5555555555555555e55555e555555555e555e555555e555e555555555e
eeee6eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
22226222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
11116111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11116111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11116111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11116111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1111611111111111111111111111111111111111111111111111111d11111111111111111111111111111d111111111111111111111111111111111111111111
111161111111111111111111111111111111111111111111111115addd1111111111111111111111155dddddaddd511111111111111111111111111111111111
111161111111111111111111111111111111111111111111111a555ddddd1111111111111111111595dddddddd5555551a111111111111111111111111111111
11116111111111111111111111111111111111111111111111aaa5555ddddddddddddddddddddddddddddddd55555555aaa11111111111111111111111111111
111161111111111111111111111111111111111111111111155a66555d1dd8eddd9adddaaddddddddddddd55555555555a555511111111111111111111111111
111161111111111111111111111111111111111111111111d55556c555125599555aa555a7555555555555555555555555555555d11111111111111111111111
1111611111111111111111111111111111111111111111ddddd555cc555285599555aa55577555555555555555555555555555dddddd11111111111111111111
11116111111111111111111111111111111111111111adddddddd5cc155281599155aa155771555555555555555555555555dddddddddddd11a1111111111111
1111611111111111111111111111111111111111111aaaddddddddcd1558e1599155aa1557715555555555555555555555dddddddddddddddaaa111111111111
111161111111111111111111111111111111111111ddaddddddddcc115281199115aa1a5771155555555555555555555ddddddddddddddddddadddd111111111
11116111111111111111111111111111111111111ddddddddddddcc11d281d991ddaa1a1771dddddddddddddddddddddddddddddddddddddddddddddddd11111
111161111111111111111111111111111111111ddddddddddddddcd1118e1d991ddaa1a1771dddddddddddddddddddddddddddddddddddddddddddddddddddd1
11116111111111111111111111111111111111dddddddddddddddcd1118e1d991ddaa1a1771ddddddddddddddddddddddddddddddddddddddddddddddddddddd
1111611111111111111111111111111111115dddddddddddddddcc11128119911daa11d7711ddddddddddddddddddddddddddddddddddddddddddddddddddddd
111161111111111111111111111111111115555dddddddddddddcd1128e1d991ddaa1dd771dddddddddddddddddddddddddddddddddddddddddddddddddddddd
11116111111111111111111111111111115555555ddddddddddddd112811d991ddaa1dd771dddddddddddddddddddddddddddddddddddddddddddddddddddddd
1111611111111111111111111111111155555555555ddddddddddd11d11ddd11ddd11ddd11ddddddddddddddddddddddddddddddddddddddddddddddddddddd5
111161111111111111111111111111155555555555555dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd555
11116111111111111111111111111555555555555555555dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd55555
1111611111111111111111111111555555555555555555555dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd5555555
111161111111111111111111115555555555555555555555555dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd555555555
11116111111111111111aaa115555555555555555555555555555dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd55555555555
1111611111111111111aaaaa55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
111161111111111111aaaaaaa5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
111161111111111111aaaaaaa5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
111161111111111111aaaaaaa5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
1111611111111111115aaaaa55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
11116111111111115555aaa555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
11116111111111155555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
11116111111111555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
11116111111155555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
11116111111555555555555555555555555555555555555555555555555557777755555555555555555555555555555555555555555555555555555555555555
1111611115555555555555555555555555555555555555555555555555555bbbbb55555555555555555555555555555555555555555555555555555555555555
1111611155555555555555555555555555555555555555555555555555555bbbbb55555555555555555555555555555555555555555555555555555555555555
1111615555555555555555555555555555555555555555555555555555555bbbbb55555555555555555555555555555555555555555555555555555555555555
1111777775555555555555555555555555555555555555555555555555533bbbbb33555bb5555555555555555555555555555555555555555555555555555555
11116555555555555555555555555555555555555555555555555bbb33333bbbbb33333bbb555555555555555555555555555555555555555555555555555555
11556555555555555555555555555555555555555555555555555bbb33333bbbbb33333bbb555555555555555555555555555555555555555555555555555555
15556555555555555555555555555555555555555555555555555bbb33333bbbbb33333bbb555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555bbb33355bbbbb55333bbb555555555555555555555555555555555555555555555555555555
5555555555555555555555555555555555555555555555555555566655555bb00005555366655555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555556999655500000000555699965555555555555555555555555555555555555555555555555555

__map__
a0800000000080000040007e840000007c8400000078880000007c840000007e84000000800000c0827e840000007c8400000078880000007c840000007e8400000080a09f500080880040827e840000007c8400000078980000007c840000007e84000000800000000082840000008484000000888800000084840000008284
0000007e840000007c8400000078880000007c840000007e8400000080840000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
a88000000000828184000084818400008882840000848184000082818400007e818400007c8184000078828400007c818400007e818400007e840000007c8400000078880000007c840000007e84000000800000d0007e840000007c840000007a980000007c840000007e84000000800000408282848e000084849f00008890
9f003884849f000082849f00007e849f00007c849f000078909f00007c848e00007e84000000808800c0827e840000007c840000007a880000007c840000007e84000000808400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ac8000000000828200000084820000008884000000848200000082828e00007e828e00007c8200000078840000007c820000007e828e000080848e0000808400000080848e6082808400000080848ea0828084000000828200000084840000008a980000008484000000828200000080848c000080848ca08280888400008084
86608280848600007e820000007c84000000769000c0827c840000007e82000000828200000084840000008a980040828484000000828200000080000000008282000000848400000088840000008484000000828200000080840000000000000000000000000000000000000000000000000000000000000000000000000000
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
010c00200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010c00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010800000c1760c15618136181162a100341003e1003f100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100
01080000181762415624136301162a100341003e1003f100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100
007c00000020024255242552425524255302550020000200002003520000200002000020000200002000020000200002000020000200002000020000200002000020000200002000020000200002000020000200
000c0000186750c635000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200002467324673246632466318653186430c6330c623000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00040000302642f6502d2642c650292642865025254246401f2441e63015234146200360411630016000f630016000d630016000b6300a6200962008610076100760000200002000020000200002000020000200
0004121409224086200c2340c6200f2340e620132441263017244166301b2441a6301e2441d634202441f63422254216442425423644292042860425204246041f2041e604152041460409004086040020400204
0008161800b4000b1001b4001b1002b4002b1003b5003b2004b5004b2005b5005b2006b5006b2007b5007b2008b5008b2009b5009b200ab600ab300bb600bb300000000000000000000000000000000000000000
0008161800b4000b1001b4001b1002b4002b1003b5003b2004b5004b2005b5005b2006b5006b2007b5007b2008b5008b2009b5009b200ab600ab300bb600bb300000000000000000000000000000000000000000
010800001f5560c5360c5260c51600500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500
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
00 4a504f55
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 0a010f4f
00 41424344
00 41424344
02 0e4e0844

