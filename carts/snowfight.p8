pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
-- snowfight
-- by @freds72

local _tok={
  ['true']=true,
  ['false']=false}
function nop() return true end
local _g={
  cls=cls,
  clip=clip,
  map=map,
  print=print,
  line=line,
  spr=spr,
  sspr=sspr,
  pset=pset,
  rect=rect,
  rectfill=rectfill,
  sfx=sfx}

-- json parser
-- from: https://gist.github.com/tylerneylon/59f4bcf316be525b30ab
local table_delims={['{']="}",['[']="]"}
local function match(s,tokens)
  for i=1,#tokens do
    if(s==sub(tokens,i,i)) return true
  end
  return false
end
local function skip_delim(str, pos, delim, err_if_missing)
if sub(str,pos,pos)!=delim then
  --if(err_if_missing) assert'delimiter missing'
  return pos,false
end
return pos+1,true
end

local function parse_str_val(str, pos, val)
  val=val or ''
  --[[
  if pos>#str then
    assert'end of input found while parsing string.'
  end
  ]]
  local c=sub(str,pos,pos)
  -- lookup global refs
if(c=='"') return _g[val] or val,pos+1
  return parse_str_val(str,pos+1,val..c)
end
local function parse_num_val(str,pos,val)
  val=val or ''
  --[[
  if pos>#str then
    assert'end of input found while parsing string.'
  end
  ]]
  local c=sub(str,pos,pos)
  -- support base 10, 16 and 2 numbers
  if(not match(c,"-xb0123456789abcdef.")) return tonum(val),pos
  return parse_num_val(str,pos+1,val..c)
end
-- public values and functions.

function json_parse(str, pos, end_delim)
  pos=pos or 1
  -- if(pos>#str) assert'reached unexpected end of input.'
  local first=sub(str,pos,pos)
  if match(first,"{[") then
    local obj,key,delim_found={},true,true
    pos+=1
    while true do
      key,pos=json_parse(str, pos, table_delims[first])
      if(key==nil) return obj,pos
      -- if not delim_found then assert'comma missing between table items.' end
      if first=="{" then
        pos=skip_delim(str,pos,':',true)  -- true -> error if missing.
        obj[key],pos=json_parse(str,pos)
      else
        add(obj,key)
      end
      pos,delim_found=skip_delim(str, pos, ',')
  end
  elseif first=='"' then
    -- parse a string (or a reference to a global object)
    return parse_str_val(str,pos+1)
  elseif match(first,"-0123456789") then
    -- parse a number.
    return parse_num_val(str, pos)
  elseif first==end_delim then  -- end of an object or array.
    return nil,pos+1
  else  -- parse true, false
    for lit_str,lit_val in pairs(_tok) do
      local lit_end=pos+#lit_str-1
      if sub(str,pos,lit_end)==lit_str then return lit_val,lit_end+1 end
    end
    -- assert'invalid json token'
  end
end

-- actors
local db={
	fire_log={
		model="log",
		pos={0,0,0},
		rotation={0,0,0}},
	tree1={
		model="pine",
		pos={10,0,-4},
		rotation={0,0.2,0}},
	tree2={
		model="pine",
		pos={-8,0,4},
		rotation={0,0.1,0}},
	tree3={
		model="pine",
		pos={10,0,8},
		rotation={0,0.6,0}},
	gnome={
		model="gnome",
		pos={2,0,2},
		rotation={0,0,0}}
}

-- dither pattern 4x4 kernel
local dither_pat=json_parse'[0x1000.ffff,0x1000.7fff,0x1000.7fdf,0x1000.5fdf,0x1000.5f5f,0x1000.5b5f,0x1000.5b5e,0x1000.5a5e,0x1000.5a5a,0x1000.1a5a,0x1000.1a4a,0x1000.0a4a,0x1000.0a0a,0x1000.020a,0x1000.0208,0x1000.0000]'
-- clipplanes
local clipplanes=json_parse'[[0.707,0,-0.707,0.1767],[-0.707,0,-0.707,0.1767],[0,0.707,-0.707,0.1767],[0,-0.707,-0.707,0.1767],[0,0,-1,-0.25]]'

--3d
-- world axis
local v_fwd,v_right,v_up={0,0,1},{1,0,0},{0,1,0}

-- models & actors
local all_models,actors,cam={},{}
local sun_dir={-0.4811,0.7523,-0.45}

function _init()
 	-- mouse support
	poke(0x5f2d,1)
	-- integrated fillp/color
	poke(0x5f34,1)

	-- 3d data
	unpack_models()

 	-- 3d cam
 	cam=make_cam(63.5,63.5,63.5)

 	-- reset actors & engine
 	actors={}
 	for _,o in pairs(db) do
		add(actors,make_actor(o.model,o.pos,make_m_from_euler(munpack(o.rotation))))
 	end
end

-- execute the given draw commands from a table
function exec(cmds)
  -- call native pico function from list of instructions
  for i=1,#cmds do
    local drawcmd=cmds[i]
    drawcmd.fn(munpack(drawcmd.args))
  end
end

local plyr={
  pos={0,1.6,0},
  hdg=0,
  pitch=0
}
local mousex,mousey
local mouselb=false
function _update()
	-- input
	local mx,my,lmb=stat(32),stat(33),stat(34)==1

  local dx,dz=0,0
  if(btn(0) or btn(0,1)) dx=-1
  if(btn(1) or btn(1,1)) dx=1
  if(btn(2) or btn(2,1)) dz=1
  if(btn(3) or btn(3,1)) dz=-1

  if mousex then
   if abs(mousex-64)<32 then
   	plyr.hdg+=(mx-mousex)/128
   else
   	plyr.hdg+=(mx-64)/2048
   end
  end
  if mousey then
	  plyr.pitch+=(my-mousey)/128
			plyr.pitch=mid(plyr.pitch,-0.25,0.25)
	 end
  
  local m=make_m_from_euler(0,plyr.hdg,0)
  v_add(plyr.pos,m_right(m),0.1*dx)
  v_add(plyr.pos,m_fwd(m),0.1*dz)

  cam:track(plyr.pos,make_m_from_euler(plyr.pitch,plyr.hdg,0,'yxz'))
	
	if mouselb==true and mouselb!=lmb then
	 m=cam.m
	 local v=cam:unproject(mousex,mousey)
	 v_normz(v)
	 --local v={m[3],m[7],m[11]}
	 local p=v_clone(plyr.pos)
	 v_add(p,v,0.5)
		make_bullet(p,v)
	end

	-- update actors
	-- todo: fix missed updates
	for _,a in pairs(actors) do
		if a.update then
			if(not a:update()) del(actors,a)
		end
	end

  mousex,mousey=mx,my
	mouselb=lmb
end

function _draw()
   cls(6)
   
	  draw_ground()
	  zbuf_draw()
	  

			palt(0,false)
			palt(14,true)
			spr(1,mousex,mousey)
			palt()
			
			-- hud
			printb("♥♥♥",2,2,8)
			printb("●●●●●",2,10,7)

   -- perf monitor!
   --
   local cpu=(flr(1000*stat(1))/10).."%"
   ?"∧"..cpu,2,120,2
   ?"∧"..cpu,2,120,7
   
end 

-->8
-- 3d engine @freds72
function clone(src,dst)
	-- safety checks
	if(src==dst) assert()
	if(type(src)!="table") assert()
	dst=dst or {}
	for k,v in pairs(src) do
		if(not dst[k]) dst[k]=v
	end
	-- randomize selected values
	if src.rnd then
		for k,v in pairs(src.rnd) do
			-- don't overwrite values
			if not dst[k] then
				dst[k]=v[3] and rndarray(v) or rndlerp(v[1],v[2])
			end
		end
	end
	return dst
end

-- https://github.com/morgan3d/misc/tree/master/p8sort
function sort(data)
 local n = #data 
 if(n<2) return
 
 -- form a max heap
 for i = flr(n / 2) + 1, 1, -1 do
  -- m is the index of the max child
  local parent, value, m = i, data[i], i + i
  local key = value.key 
  
  while m <= n do
   -- find the max child
   if ((m < n) and (data[m + 1].key > data[m].key)) m += 1
   local mval = data[m]
   if (key > mval.key) break
   data[parent] = mval
   parent = m
   m += m
  end
  data[parent] = value
 end 

 -- read out the values,
 -- restoring the heap property
 -- after each step
 for i = n, 2, -1 do
  -- swap root with last
  local value = data[i]
  data[i], data[1] = data[1], value

  -- restore the heap
  local parent, terminate, m = 1, i - 1, 2
  local key = value.key 
  
  while m <= terminate do
   local mval = data[m]
   local mkey = mval.key
   if (m < terminate) and (data[m + 1].key > mkey) then
    m += 1
    mval = data[m]
    mkey = mval.key
   end
   if (key > mkey) break
   data[parent] = mval
   parent = m
   m += m
  end  
  
  data[parent] = value
 end
end

-- zbuffer (kind of)
local znear_plane={0,0,-1,-0.25}
-- bbox clipping outcodes
local k_center=1
local k_right=2
local k_left=4
function zbuf_draw()
	local objs={}

	for _,d in pairs(actors) do
		d:collect_drawables(objs)
	end

	-- z-sorting
	sort(objs)

 -- actual draw
	for i=1,#objs do
		local d=objs[i]
   if d.kind==3 then
		project_poly(d.v,d.c)
   elseif d.kind==1 then
	 	circfill(d.x,d.y,d.r,d.c)
	 end
 end
 
 print(#objs,110,3,1)
 print(#objs,110,2,7) 
end

function lerp(a,b,t)
	return a*(1-t)+b*t
end

function make_v(a,b)
	return {
		b[1]-a[1],
		b[2]-a[2],
		b[3]-a[3]}
end
function v_clone(v)
	return {v[1],v[2],v[3]}
end
function v_dot(a,b)
	return a[1]*b[1]+a[2]*b[2]+a[3]*b[3]
end
function v_scale(v,scale)
	v[1]*=scale
	v[2]*=scale
	v[3]*=scale
end
function v_cross(a,b)
	local ax,ay,az=a[1],a[2],a[3]
	local bx,by,bz=b[1],b[2],b[3]
	return {ay*bz-az*by,az*bx-ax*bz,ax*by-ay*bx}
end
function v_normz(v)
	local d=v_dot(v,v)
	if d>0.001 then
		d=d^.5
		v[1]/=d
		v[2]/=d
		v[3]/=d
	end
	return d
end

function v_add(v,dv,scale)
	scale=scale or 1
	v[1]+=scale*dv[1]
	v[2]+=scale*dv[2]
	v[3]+=scale*dv[3]
end
function v_min(a,b)
	return {min(a[1],b[1]),min(a[2],b[2]),min(a[3],b[3])}
end
function v_max(a,b)
	return {max(a[1],b[1]),max(a[2],b[2]),max(a[3],b[3])}
end

-- matrix functions
function m_x_v(m,v)
	local x,y,z=v[1],v[2],v[3]
	v[1],v[2],v[3]=m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14],m[3]*x+m[7]*y+m[11]*z+m[15]
end

function make_m_from_euler(x,y,z,order)
	local a,b = cos(x),-sin(x)
	local c,d = cos(y),-sin(y)
	local e,f = cos(z),-sin(z)
 
 if order=='yxz' then
  local ce,cf,de,df=c*e,c*f,d*e,d*f
	 return {
	  ce+df*b,a*f,cf*b-de,0,
	  de*b-cf,a*e,df+ce*b,0,
	  a*d,-b,a*c,0,
	  0,0,0,1}
	end
	
 -- xyz order
 -- blender default
 local ae,af,be,bf=a*e,a*f,b*e,b*f

	return {
		c*e,af + be * d,bf - ae * d,0,
		- c * f, ae - bf * d,be + af * d,0,
  d,- b * c, a * c,0,
  0,0,0,1
	}	 
end

-- only invert 3x3 part
function m_inv(m)
	m[2],m[5]=m[5],m[2]
	m[3],m[9]=m[9],m[3]
	m[7],m[10]=m[10],m[7]
end
function m_set_pos(m,v)
	m[13],m[14],m[15]=v[1],v[2],v[3]
end
-- returns up vector from matrix
function m_up(m)
	return {m[5],m[6],m[7]}
end
-- returns right vector from matrix
function m_right(m)
	return {m[1],m[2],m[3]}
end
-- returns foward vector from matrix
function m_fwd(m)
	return {m[9],m[10],m[11]}
end

-- default function for 3d-model based actors
function collect_drawables(model,m,pos,out)
 -- vertex cache
 local p={}

 -- cam pos in object space
 local cam_pos=make_v(pos,cam.pos)
 local x,y,z=cam_pos[1],cam_pos[2],cam_pos[3]
 cam_pos={m[1]*x+m[2]*y+m[3]*z,m[5]*x+m[6]*y+m[7]*z,m[9]*x+m[10]*y+m[11]*z}

 local outcode=0 
 for _,b in pairs(model.bbox) do
	 local a=v_clone(b)
 	m_x_v(m,a)
 	v_add(a,cam.pos,-1)
 	m_x_v(cam.m,a)
 	
		-- outcode
	 -- 0: vizible
	 local acode=0
	 local ax,ay,az=a[1],a[2],a[3]
	 if az>0.25 then
 		if ax>az then acode=k_right
	 	elseif -ax>az then acode=k_left
 		else acode=k_center end
	 end
	 outcode=bor(outcode,acode)
 end
 if((outcode==6 or band(outcode,1)==1)==false) return
  			
 -- select lod
 local d=v_dot(cam_pos,cam_pos)*1.5
 
 -- lod selection
 local lodid=0
 for i=1,#model.lod_dist do
  --printh(d..">"..model.lod_dist[i])
 	if(d>model.lod_dist[i]) lodid+=1
 end
  
 -- not visible?
 if(lodid>=#model.lods) return 
 --lodid=min(lodid,#model.lods-1)
 model=model.lods[lodid+1]
 
  -- light pos in object space
 local light=make_v(pos,plyr.pos)
 local x,y,z=light[1],light[2],light[3]
 light={m[1]*x+m[2]*y+m[3]*z,m[5]*x+m[6]*y+m[7]*z,m[9]*x+m[10]*y+m[11]*z}
 
  -- faces
	for _,f in pairs(model.f) do
  -- front facing?
  if band(f.flags,0x1)>0 or v_dot(f.n,cam_pos)>f.cp then
   -- face vertices (for clipping)
   local z,vertices=0,{}
   -- project vertices
   for i=1,f.ni do
		local ak=f[i]
		local a=p[ak]
		if not a then
	    	a=v_clone(model.v[ak])
    		-- relative to world
    		m_x_v(m,a)
    		-- world to cam
    		v_add(a,cam.pos,-1)
  			m_x_v(cam.m,a)
	   		p[ak]=a  		 
  		end
		local az=a[3]
    	z+=az
		vertices[i]=a
	end
   --
	vertices=z_poly_clip(0.25,vertices)
	if #vertices>2 then
		local c=0x1100+f.c
		-- non emitting face?
		if band(f.flags,8)==0 then
			local l=make_v(light,model.v[f[1]])
			v_normz(l)
			c=max(-5*v_dot(f.n,l))
			-- get floating part
			local cf=(#dither_pat-1)*(1-c%1)
			c=bor(shl(sget(16+min(c+1,5),f.c),4),sget(16+c,f.c))--+dither_pat[flr(cf)+1]
		end
		add(out,{key=64*f.ni/z,v=vertices,c=c,kind=3})
	end
	  --print(outcode,2,12,13)
	  --return
  end
 end
end

-- sutherland-hodgman clipping
-- n.p is pre-multiplied in n[4]
function plane_poly_clip(n,v)
	local dist,allin={},0
	for i,a in pairs(v) do
		local d=n[4]-(a[1]*n[1]+a[2]*n[2]+a[3]*n[3])
		if(d>0) allin+=1
	 dist[i]=d
	end
 -- early exit
	if(allin==#v) return v
 if(allin==0) return {}

	local res={}
	local v0,d0,v1,d1,t,r=v[#v],dist[#v]
 -- use local closure
 local clip_line=function()
 	local r,t=make_v(v0,v1),d0/(d0-d1)
 	v_scale(r,t)
 	v_add(r,v0)
 	if(v0[4]) r[4]=lerp(v0[4],v1[4],t)
 	if(v0[5]) r[5]=lerp(v0[5],v1[5],t)
 	res[#res+1]=r
 end
	for i=1,#v do
		v1,d1=v[i],dist[i]
		if d1>0 then
			if(d0<=0) clip_line()
			res[#res+1]=v1
		elseif d0>0 then
   clip_line()
		end
		v0,d0=v1,d1
	end
	return res
end
function z_poly_clip(znear,v)
	local dist,allin={},0
	for i,a in pairs(v) do
		local d=-znear+a[3]
		if(d>0) allin+=1
	 dist[i]=d
	end
 -- early exit
	if(allin==#v) return v
 if(allin==0) return {}

	local res={}
	local v0,d0,v1,d1,t,r=v[#v],dist[#v]
 -- use local closure
 local clip_line=function()
 	local r,t=make_v(v0,v1),d0/(d0-d1)
 	v_scale(r,t)
 	v_add(r,v0)
 	if(v0[4]) r[4]=lerp(v0[4],v1[4],t)
 	if(v0[5]) r[5]=lerp(v0[5],v1[5],t)
 	res[#res+1]=r
 end
	for i=1,#v do
		v1,d1=v[i],dist[i]
		if d1>0 then
			if(d0<=0) clip_line()
			res[#res+1]=v1
		elseif d0>0 then
   clip_line()
		end
		v0,d0=v1,d1
	end
	return res
end

function make_actor(model,p,m)
  angle=angle and angle/360 or 0
  model=all_models[model]
	-- instance
	local a={
		pos=v_clone(p),
		m=m,
		collect_drawables=function(self,out)
			collect_drawables(model,self.m,self.pos,out)
		end
  }

	-- init position
  m_set_pos(a.m,p)
	return a
end

function make_bullet(p,v)
	local t=60+rnd(5)
	local b={
		is_shaded=true,
		pos=v_clone(p),		
		v=v_clone(v),
		update=function(self)
			t-=1
			if(t<0) return
			v_add(self.pos,self.v,0.3)
			
			if(self.pos[2]<0) return
			return true
		end,
		collect_drawables=function(self,out)
	 	local p=v_clone(self.pos)	 	
	 	v_add(p,cam.pos,-1)
 		m_x_v(cam.m,p)

			local x,y,w=cam:project2d(p)
			if(w>0) add(out,{key=w,kind=1,x=x,y=y,c=0x1107,r=max(0.5,0.5*w)})
		end
	}
	return add(actors, b)
end

function make_cam(x0,y0,focal)
	local c={
		pos={0,0,0},
		track=function(self,pos,m)
    self.pos=v_clone(pos)

		-- inverse view matrix
    self.m=m
    m_inv(self.m)
	 end,
	 -- 
	 unproject=function(self,sx,sy)
   local m=self.m
		 local x,y,z=0.25*(sx-64)/focal,0.25*(64-sy)/focal,0.25
		 -- to world
			return {m[1]*x+m[2]*y+m[3]*z,m[5]*x+m[6]*y+m[7]*z,m[9]*x+m[10]*y+m[11]*z}
	 end,
		-- project cam-space points into 2d
    project2d=function(self,v)
  	  -- view to screen
  	  local w=focal/v[3]
  	  return x0+ceil(v[1]*w),y0-ceil(v[2]*w),w
		end,
		-- project cam-space points into 2d
    -- array version
    project2da=function(self,v)
  	  -- view to screen
  	  local w=focal/v[3]
  	  return {x0+ceil(v[1]*w),y0-ceil(v[2]*w),w,v[4]*w,v[5]*w}
		end
	}
	return c
end

local sky_gradient={0,0x11ee,360,2,1440,1}
local stars={}
for i=1,12 do
 	local phi=rnd()
    local theta=rnd()/2
	add(stars,{-sin(theta) * cos(phi),abs(cos(theta)),-sin(theta) * sin(phi)})
end

function draw_ground()
	-- draw horizon
	local zfar=-128
	local farplane={
			{-zfar,zfar,zfar},
			{-zfar,-zfar,zfar},
			{zfar,-zfar,zfar},
			{zfar,zfar,zfar}}
	-- cam up in world space
	local n=m_up(cam.m)

 local y0=cam.pos[2]

 -- start alt.,color,pattern
	for i=1,#sky_gradient,2 do
		-- ground location in cam space
  -- offset by sky layer ceiling
		-- or infinite (h=0) for clear sky
		local p={0,-sky_gradient[i]/120,0}
		if(horiz) p[2]+=y0
		m_x_v(cam.m,p)
		n[4]=v_dot(p,n)
		farplane=plane_poly_clip(n,farplane)
  -- display
		project_poly(farplane,sky_gradient[i+1])
	end
 
 local cloudy=-cam.pos[2]
 -- plane coords + u/v (32x32 texture)
 local cloudplane={
		{32,cloudy,32,64,0},
		{-32,cloudy,32,0,0},
		{-32,cloudy,-32,0,64},
		{32,cloudy,-32,64,64}}
 for _,v in pairs(cloudplane) do
  m_x_v(cam.m,v)
 end
 for i=1,#clipplanes do
	 cloudplane=plane_poly_clip(clipplanes[i],cloudplane)
 end
 color(0x40)
 
 -- stars
  for _,v in pairs(stars) do
    v=v_clone(v)
    m_x_v(cam.m,v)
    local x,y,w=cam:project2d(v)
    if(w>0) pset(x,y,6)
  end

 -- moon
 local moon={-5,5,-5}
 m_x_v(cam.m,moon)
 local x0,y0,w0=cam:project2d(moon)
 if w0>0 then
	circfill(x0,y0,8,0x1076.a5a5) 
	-- mask
	moon={-5.5,5.5,-5}
	m_x_v(cam.m,moon)
 	x0,y0,w0=cam:project2d(moon)
	circfill(x0,y0,8,0x1101) 
  end
end

function project_poly(p,c)
	if #p>2 then
		local x0,y0=cam:project2d(p[1])
  local x1,y1=cam:project2d(p[2])
		for i=3,#p do
			local x2,y2=cam:project2d(p[i])
			trifill(x0,y0,x1,y1,x2,y2,c)
		  x1,y1=x2,y2
		end
	end
end

-->8
-- trifill
-- by @p01
function p01_trapeze_h(l,r,lt,rt,y0,y1)
  lt,rt=(lt-l)/(y1-y0),(rt-r)/(y1-y0)
  if(y0<0)l,r,y0=l-y0*lt,r-y0*rt,0
   for y0=y0,min(y1,128) do
   rectfill(l,y0,r,y0)
   l+=lt
   r+=rt
  end
end
function p01_trapeze_w(t,b,tt,bt,x0,x1)
 tt,bt=(tt-t)/(x1-x0),(bt-b)/(x1-x0)
 if(x0<0)t,b,x0=t-x0*tt,b-x0*bt,0
 for x0=x0,min(x1,128) do
  rectfill(x0,t,x0,b)
  t+=tt
  b+=bt
 end
end

function trifill(x0,y0,x1,y1,x2,y2,col)
 color(col)
 if(y1<y0)x0,x1,y0,y1=x1,x0,y1,y0
 if(y2<y0)x0,x2,y0,y2=x2,x0,y2,y0
 if(y2<y1)x1,x2,y1,y2=x2,x1,y2,y1
 if max(x2,max(x1,x0))-min(x2,min(x1,x0)) > y2-y0 then
  col=x0+(x2-x0)/(y2-y0)*(y1-y0)
  p01_trapeze_h(x0,x0,x1,col,y0,y1)
  p01_trapeze_h(x1,col,x2,x2,y1,y2)
 else
  if(x1<x0)x0,x1,y0,y1=x1,x0,y1,y0
  if(x2<x0)x0,x2,y0,y2=x2,x0,y2,y0
  if(x2<x1)x1,x2,y1,y2=x2,x1,y2,y1
  col=y0+(y2-y0)/(x2-x0)*(x1-x0)
  p01_trapeze_w(y0,y0,y1,col,x0,x1)
  p01_trapeze_w(y1,col,y2,y2,x1,x2)
 end
end
-->8
-- print helpers
function printb(s,x,y,c)
	for i=-1,1 do
		for j=-1,1 do
			print(s,x+i,y+j,0)
		end
	end
	print(s,x,y,c)
end

-->8
-- unpack data & models
local mem=0x1000
function mpeek()
	local v=peek(mem)
	mem+=1
	return v
end

-- unpack a list into an argument list
-- trick from: https://gist.github.com/josefnpat/bfe4aaa5bbb44f572cd0
function munpack(t, from, to)
 local from,to=from or 1,to or #t
 if(from<=to) return t[from], munpack(t, from+1, to)
end

-- w: number of bytes (1 or 2)
function unpack_int(w)
  	w=w or 1
	local i=w==1 and mpeek() or bor(shl(mpeek(),8),mpeek())
	return i
end
-- unpack 1 or 2 bytes
function unpack_variant()
	local h=mpeek()
	-- above 127?
	if band(h,0x80)>0 then
		h=bor(shl(band(h,0x7f),8),mpeek())
	end
	return h
end
-- unpack a float from 1 byte
function unpack_float(scale)
	local f=shr(unpack_int()-128,5)
	return f*(scale or 1)
end
-- unpack a double from 2 bytes
function unpack_double(scale)
	local f=(unpack_int(2)-16384)/128
	return f*(scale or 1)
end
-- unpack an array of bytes
function unpack_array(fn)
	local n=unpack_variant()
	for i=1,n do
		fn(i)
	end
end
-- unpack a vector
function unpack_v(scale)
	return {unpack_double(scale),unpack_double(scale),unpack_double(scale)}
end

-- valid chars for model names
local itoa='_0123456789abcdefghijklmnopqrstuvwxyz'
function unpack_string()
	local s=""
	unpack_array(function()
		local c=unpack_int()
		s=s..sub(itoa,c,c)
	end)
	return s
end

function unpack_face()
	local f={flags=unpack_int(),c=unpack_int()}

	-- quad?
	f.ni=band(f.flags,2)>0 and 4 or 3
	-- vertex indices
	for i=1,f.ni do
		-- using the face itself saves more than 500KB!
		f[i]=unpack_variant()
	end
	return f
end

function unpack_model(model,scale)
	-- vertices
	local v=model.v
	unpack_array(function()
		add(v,unpack_v(scale))
	end)

	-- faces
	unpack_array(function()
		local f=unpack_face()
		-- inner faces?
		if band(f.flags,4)>0 then
			f.inner={}
			unpack_array(function()
				add(f.inner,unpack_face())
			end)
		end
		-- normal
		f.n=v_cross(make_v(v[f[1]],v[f[f.ni]]),make_v(v[f[1]],v[f[2]]))
		v_normz(f.n)
		-- viz check
		f.cp=v_dot(f.n,model.v[f[1]])

		add(model.f,f)
	end)
end

function unpack_models()
	mem=
	-- for all models
	unpack_array(function()
		local model,name,scale={lods={},bbox={},lod_dist={}},unpack_string(),1/unpack_int()		

		unpack_array(function()
			local d=unpack_double()
			assert(d<127,"lod distance too large:"..d)
			-- store square distance
			add(model.lod_dist,d*d)
		end)
  
		-- level of details models
		unpack_array(function()
			local lod={v={},f={},vgroups={}}
			unpack_model(lod,scale)
			-- unpack vertex groups (as sub model)
			unpack_array(function()				
				local name=unpack_string()
				local vgroup={offset=unpack_v(scale),f={}}
				-- faces
				unpack_array(function()
					local f=unpack_face()
					-- normal
					f.n=v_cross(make_v(v[f[1]],v[f[f.ni]]),make_v(v[f[1]],v[f[2]]))
					v_normz(f.n)

					-- viz check
					f.cp=v_dot(f.n,lod.v[f[1]])

					add(vgroup.f,f)
				end)				
				lod.vgroups[name]=vgroup
			end)
		
			add(model.lods,lod)
		end)

		-- bounding box
		function one_if(cond)
			return cond>0 and 1 or 0
		end
		local vmin,vmax={32000,32000,32000},{-32000,-32000,-32000}	
		for _,v in pairs(model.lods[1].v) do
			vmin,vmax=v_min(vmin,v),v_max(vmax,v)
		end
		local size=make_v(vmin,vmax)
		-- generate vertices
		for i=0,7 do
			local v1={
				one_if(band(0x2,i))*size[1],
				one_if(band(0x4,i))*size[2],
				one_if(band(0x1,i))*size[3]}
			v_add(v1,vmin)
			add(model.bbox,v1)
		end

		-- index by name
		all_models[name]=model
	end)
end

__gfx__
00000000e00000ee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000770770e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000070e070e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000e0eee0ee013ba70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000070e070e05449a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000770770e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000e00000ee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000eeeeeeee056f770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000888880000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000b35110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000001d6660000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000efd510000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
303071a1211010050010a1f35b0400f33cf35b0462f33cf31e0400f34af31e0462f34a04f1040004c504f1046204c504b4040004d304b4046204d3f33a040004
b1f33a046204b1f33a0400f35ef33a0462f35e04d5040004b104d5046204b104d50400f35e04d50462f35e0443046204b104e00462f35e04e00400f35e044304
0004b1040004000400f3cc0400f35ef32f040004b1f32f046204b1f3cc0462f35ef3ed0462f35e11204010304020204070506080204090b0c0a02040f0d0e001
2040c0b061912040916110202040a0c0918120407190a081204050718160204060811180204081912111204020402191204001e011212040708011412040d041
11e02040f00121312040303121400040b14191011010050010b004000400044604650400f3ec0400048c0400f3aa0400f3ecf391045814601475045804540400
34040400f3190458e35bf3bd2475f329f38b2476041504f824c104d19000401030200040203040004040301020b050a0b06020b060b0908020b08090a0500070
90b0700070a090700070b0a07000502191a181011010050010d1f39e0400f39ef39e0446f39ef39e04000471f39e0446047104710400f39e04710446f39e0471
04000471047104460471f39e0438f39ef39e0438047104710438f39e047104380471040004aa040004710484f39ef39e04a5f39ef39e0484f39e047104a5f39e
04710484f3bcf39e04a5f3bcf39e0484f3bc047104a5f3bcf35f0447f38ef38f0447f38ef35f04e7f38ef38f04e7f38e04500457f38e04d00457f38e045004f7
f38e04d004f7f38e012030102040302030304080702030708060502030506020102040402090a000b090b0d020f02060b09020408040a0c020406080c0b000b0
c0a0d000b0b0c0d000b0a090d03030e01151213030f0014131a08061719181a080a1b1d1c100
