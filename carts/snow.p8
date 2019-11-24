pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
-- snow!
-- by @freds72

-- vector & tools
function lerp(a,b,t)
	return a*(1-t)+b*t
end

-- pick a random item for the given array
-- @TheTomster for method name :)
function pick(a)
	return a[flr(rnd(#a))+1]
end

-- duplicate array
function clone(src)
	local dst={}
	for k,v in pairs(src) do
		dst[k]=v
	end
	return dst
end

-- vector tools
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
function v_add(v,dv,scale)
	scale=scale or 1
	v[1]+=scale*dv[1]
	v[2]+=scale*dv[2]
	v[3]+=scale*dv[3]
end
-- safe vector length
function v_len(v)
	local x,y,z=v[1],v[2],v[3]
	local d=max(max(abs(x),abs(y)),abs(z))
	x/=d
	y/=d
	z/=d
	return d*(x*x+y*y+z*z)^0.5
end
function v_normz(v)
	local d=v_len(v)
	v[1]/=d
	v[2]/=d
	v[3]/=d
end

function v_lerp(a,b,t)
	return {
		lerp(a[1],b[1],t),
		lerp(a[2],b[2],t),
		lerp(a[3],b[3],t)
	}
end
function v_cross(a,b)
	local ax,ay,az=a[1],a[2],a[3]
	local bx,by,bz=b[1],b[2],b[3]
	return {ay*bz-az*by,az*bx-ax*bz,ax*by-ay*bx}
end

local v_up={0,1,0}

-- matrix functions
function m_x_v(m,v)
	local x,y,z=v[1],v[2],v[3]
	return {m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14],m[3]*x+m[7]*y+m[11]*z+m[15]}
end
function m_x_m(a,b)
	local a11,a12,a13,a14=a[1],a[5],a[9],a[13]
	local a21,a22,a23,a24=a[2],a[6],a[10],a[14]
	local a31,a32,a33,a34=a[3],a[7],a[11],a[15]

	local b11,b12,b13,b14=b[1],b[5],b[9],b[13]
	local b21,b22,b23,b24=b[2],b[6],b[10],b[14]
	local b31,b32,b33,b34=b[3],b[7],b[11],b[15]

	return {
			a11*b11+a12*b21+a13*b31,a21*b11+a22*b21+a23*b31,a31*b11+a32*b21+a33*b31,0,
			a11*b12+a12*b22+a13*b32,a21*b12+a22*b22+a23*b32,a31*b12+a32*b22+a33*b32,0,
			a11*b13+a12*b23+a13*b33,a21*b13+a22*b23+a23*b33,a31*b13+a32*b23+a33*b33,0,
			a11*b14+a12*b24+a13*b34+a14,a21*b14+a22*b24+a23*b34+a24,a31*b14+a32*b24+a33*b34+a34,1
		}
end
function make_m_from_v_angle(up,angle)
	local fwd={-sin(angle),0,cos(angle)}
	local right=v_cross(up,fwd)
	v_normz(right)
	fwd=v_cross(right,up)
	return {
		right[1],right[2],right[3],0,
		up[1],up[2],up[3],0,
		fwd[1],fwd[2],fwd[3],0,
		0,0,0,1
	}
end
-- only invert 3x3 part
function m_inv(m)
	m[2],m[5]=m[5],m[2]
	m[3],m[9]=m[9],m[3]
	m[7],m[10]=m[10],m[7]
end

-- returns basis vectors from matrix
function m_right(m)
	return {m[1],m[2],m[3]}
end
function m_up(m)
	return {m[5],m[6],m[7]}
end
function m_fwd(m)
	return {m[9],m[10],m[11]}
end

-- coroutine helper
function corun(f,arg0,arg1,arg2)
	local cs=costatus(f)
	if cs=="suspended" then
		assert(coresume(f,arg0,arg1,arg2))
		return f
	end
	return nil
end

-- sort
-- https://github.com/morgan3d/misc/tree/master/p8sort
-- 
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

-- fade ramp
-- see: http://kometbomb.net/pico8/fadegen.html-
local fadetable={}
-- note: must be in memory as we are destroying the spritesheet with the zoom!
for i=0,15 do
	local r={}
	fadetable[i]=r
	for j=0,4 do
		-- lookup color for c0
		r[j]=sget(24+j,i)
	end
end
function whiteout_async(delay,reverse)
	for i=1,delay do
		yield()
	end
	for i=0,4 do
		-- fade to white
		i=reverse and (4-i) or i
		for c=0,15 do
			if flr(i+1)>=5 then
				pal(c,7,1)
			else
				pal(c,fadetable[c][flr(i)],1)
			end
		end
		yield()
	end
end

-->8
-- main engine
-- global vars
local actors,ground,plyr,cam={}

-- camera clip planes
local k_far,k_near,k_right,k_left,z_near=0,2,4,8,0.2

-- camera
function make_cam()
	--
	local up={0,1,0}

	-- screen shake
	local shkx,shky=0,0
	camera()

	local clouds,clouds2={},{}
	for i=-16,16 do
		add(clouds,{i=i,r=max(8,rnd(16))})
		add(clouds2,{i=i,r=max(12,rnd(24))})
	end

	return {
		pos={0,0,0},
		angle=0,
		m=make_m_from_v_angle(v_up,0),
		shake=function()
			shkx,shkx=min(4,shkx+rnd(8)),min(4,shky+rnd(8))
		end,
		update=function()
			shkx*=-0.7-rnd(0.2)
			shky*=-0.7-rnd(0.2)
			if abs(shkx)<0.5 and abs(shky)<0.5 then
				shkx,shky=0,0
			end
			camera(shkx,shky)
		end,
		track=function(self,pos,a,u,power)
   			pos=v_clone(pos)
   			-- lerp angle
			self.angle=lerp(self.angle,a,power or 0.8)
			-- lerp orientation
			up=v_lerp(up,u,0.1)
			v_normz(up)

			-- shift cam position			
			local m=make_m_from_v_angle(up,self.angle)
			-- 1.8m player
			-- v_add(pos,v_up,64)
			v_add(pos,m_up(m),1.6)
			
			-- inverse view matrix
			m_inv(m)
			self.m=m_x_m(m,{
				1,0,0,0,
				0,1,0,0,
				0,0,1,0,
				-pos[1],-pos[2],-pos[3],1
			})
			
			self.pos=pos
		end,
		project2d=function(self,v)
			local w=63.5/v[3]
			return v.x or 63.5+flr(w*v[1]),v.y or 63.5-flr(w*v[2]),w
		end,
		project_poly=function(self,p,c0)
			local x0,y0=self:project2d(p[1])
			local x1,y1=self:project2d(p[2])
			for i=3,#p do
				local x2,y2=self:project2d(p[i])
				trifill(x0,y0,x1,y1,x2,y2,c0)
				x1,y1=x2,y2
			end
			
			--[[
			local p0=p[#p]
			local x0,y0=p0.x or 63.5+flr(shl(p0[1]/p0[3],6)),p0.y or 63.5-flr(shl(p0[2]/p0[3],6))
			for i=1,#p do
				local p1=p[i]
				local x1,y1=p1.x or 63.5+flr(shl(p1[1]/p1[3],6)),p1.y or 63.5-flr(shl(p1[2]/p1[3],6))
				line(x0,y0,x1,y1,1)
				x0,y0=x1,y1
			end
			]]
		end,
		draw_horizon=function(self,ground_color,sky_color)
			cls(sky_color)
			
			-- cam up in world space
			local n=m_up(self.m)
			-- a bit ugly
			v_scale(n,-1)

			-- intersection between camera eye and up plane (world space)
			local x0,y0=self:project2d({0,-n[3]/n[2],1})

			-- horizon 'normal'
			n[3]=0
			v_normz(n)
			-- spread clouds
			v_scale(n,16)
			local u,v=n[1],n[2]
			
			fillp(0xf0f0)
			circfill(x0-3*u+2*v,y0+3*v+2*u,8,0xc7)
			fillp()
			circfill(x0-3*u+2*v,y0+3*v+2*u,5,7)

			-- horizon intersections
			local xl,yl,xr,yr=0,y0-u*x0/v,128,y0+u*(128-x0)/v			
			-- yl: min
			-- yr: max
			if(yl>yr) xl,yl,xr,yr=xr,yr,xl,yl
			
			fillp(0xa5a5.f)
			for _,c in pairs(clouds2) do
				circfill(x0+c.i*v,y0+c.i*u,c.r,6)
				-- alternative?
				--circ(x0+c.i*v,y0+c.i*u,c.r,7)
			end
			fillp()
			for _,c in pairs(clouds) do
				circfill(x0+c.i*v,y0+c.i*u,c.r,ground_color)
			end

			rectfill(0,128,128,yr,ground_color)
			trifill(xl,yl,xr,yr,xl,yr,ground_color)
		end
	}
end

-- "physic body" for simple car

function make_car(p)
	-- last contact face
	local up,oldf={0,1,0}

	local velocity,angularv,forces,torque={0,0,0},0,{0,0,0},0

	local angle,steering_angle,on_air_ttl=0,0,0

	local g={0,-4,0}
	return {
		pos=v_clone(p),
		on_ground=false,
		height=0,
		get_pos=function(self)
	 		return self.pos,angle,steering_angle/0.625,velocity
		end,
		get_up=function()
			-- compensate slope when not facing slope
			local scale=abs(cos(angle))
			local u=v_lerp(v_up,up,scale)
			local m=make_m_from_v_angle(u,angle)
			
			local right=m_right(m)
			v_add(u,right,sin(steering_angle)*scale/2)
			v_normz(u)
			return u
		end,
		apply_force_and_torque=function(self,f,t)
			-- add(debug_vectors,{f=f,p=p,c=11,scale=t})

			v_add(forces,f)
			torque+=t
		end,
		prepare=function(self)
			-- gravity and ground
			self:apply_force_and_torque(g,0)
			-- on ground?
			if self.on_ground==true then
				local n=v_clone(up)
				v_scale(n,-v_dot(n,g))
				-- slope pushing up
				self:apply_force_and_torque(n,0)
			end

			-- update velocities
			v_add(velocity,forces,0.5/30)
			angularv+=torque*0.5/30

			-- apply some damping
			angularv*=0.86
			local f=self.on_ground==true and 0.08 or 0.01
			-- some friction
			--v_scale(velocity,1-f)
			v_add(velocity,velocity,-f*v_dot(velocity,velocity))
		end,
		integrate=function(self)
		 	-- update pos & orientation
			v_add(self.pos,velocity)
			-- limit rotating velocity
			angularv=mid(angularv,-1,1)
			angle+=angularv

			-- reset
			forces,torque={0,0,0},0
		end,
		steer=function(self,steering_dt)
			steering_angle+=mid(steering_dt,-0.15,0.15)
			-- on ground?
			if self.on_ground==true and v_len(velocity)>0.001 then

				-- desired ski direction
				local m=make_m_from_v_angle(up,angle-steering_angle/16)
				local right,fwd=m_right(m),m_fwd(m)
				
				-- slip angle
				local sa=-v_dot(velocity,right)
				if abs(sa)>0.001 then
					-- max grip
					local vn=v_clone(velocity)
					v_normz(vn)
					local grip=1-abs(v_dot(fwd,vn))
					-- more turn: more grip
					sa*=60*grip

					--grip*=grip

					-- todo: review
					sa=mid(sa,-3,3)
				
					-- ski length for torque
					local ski_len=0.8
					--[[
					local fwd=m_fwd(m)
					v_scale(fwd,ski_len)
					local torque=v_cross(fwd,right)
					local l=torque[1]+torque[2]+torque[3]
					]]

					v_scale(right,sa)

					self:apply_force_and_torque(right,-steering_angle*ski_len/4)
				end
			elseif self.on_ground==false then
				self:apply_force_and_torque({0,0,0},-steering_angle/8)
			end			
		end,
		update=function(self)
			steering_angle*=0.8
			on_air_ttl-=1

			-- find ground
			local pos=self.pos

			local newf,newpos,gps=ground:find_face(pos)
			if newf then
				oldf=newf
			end
			self.gps=gps-angle
			-- stop at ground
			self.on_ground=false
			local tgt_height=1
			if newpos and pos[2]<=newpos[2] then
				up=newf.n
				tgt_height=pos[2]-newpos[2]
				pos[2]=newpos[2]				
				self.on_ground=true
				-- big enough jump?
				if(on_air_ttl>5) sfx(12) on_air_ttl=0 sfx(11) 
			end
			if(self.on_ground==false) sfx(11,-2) on_air_ttl=10

			self.height=lerp(self.height,tgt_height,0.4)

			-- alter sound
			-- mix: volume for base pitch
			-- alter with "height" to include slope "details"
			-- sfx 11: 0x3200+68*11
			local pitch=max(flr(20*v_len(velocity)+10*self.height)+rnd(4))
			for src=0x34ec,0x34fb,2 do
				local s=peek2(src)
				poke2(src,bor(band(0xffe0,s),band(0x1f,flr(pitch))))
			end
		end
	}	
end

function make_plyr(p,params)
	local body,hp=make_car(p),3

	local body_update=body.update

	local hit_ttl,jump_ttl,jump_pressed=0,0
	
	-- timers + avoid free airtime on drop!
	local t,bonus,total_t,freeride_t,reverse_t,air_t=params.total_t,{},0,0,0,-60
	local whoa_sfx={5,6,7}

	-- time bonus cannot be negative!
	local function add_time_bonus(tb,msg)
		t+=tb*30
		local ttl=8+rnd(10)
		add(bonus,{t="+"..tb.."s",msg=msg,x=rnd(5),ttl=ttl,duration=ttl})
	end

	body.control=function(self)	
		local da=0
		if(btn(0)) da=1
		if(btn(1)) da=-1
		local do_jump
		if self.on_ground==true then
			-- was flying?
			if(air_t>23) add_time_bonus(1,"air!") sfx(pick(whoa_sfx))
			air_t=0

			if btn(4) then
				if(not jump_pressed) jump_pressed,jump_ttl=true,0
				jump_ttl=min(jump_ttl+1,9)
			elseif jump_pressed then
				-- button released?
				do_jump=true
			end
		else
			-- kill jump
			jump_ttl,jump_pressed,do_jump=0
			-- record flying time
			air_t+=1
		end

		if do_jump then
			self:apply_force_and_torque({0,jump_ttl*7,0},0)
			jump_ttl,jump_pressed,do_jump=0
		end

		self:steer(da/8)
	end

	body.update=function(self)
		t-=1
		hit_ttl-=1
		total_t+=1

		-- warning sound
		if t<3*30 and t%30==0 then
			sfx(2)
		end
		-- collision detection
		local pos,angle,_,velocity=self:get_pos()
		local hit_type,hit_actor=ground:collide(pos,0.2)
		if hit_type==2 then
			-- walls: insta-death
			cam:shake()
			self.dead=true
		elseif hit_type==3 then
			-- coins: bonus time
			add_time_bonus(1)
			sfx(8)
		elseif hit_ttl<0 and hit_type==1 then
			-- props: 
			sfx(pick(hit_actor.sfx))
			cam:shake()
			-- temporary invincibility
			hit_ttl=20
			hp-=1
			-- failed!
			reverse_t=0
		end
		
		local slice,slice_extent=ground:get_track(pos)
		self.on_track=true
		if pos[1]>=slice_extent[1] and pos[1]<=slice_extent[2] then			
			if slice.is_checkpoint then
				if pos[3]>slice_extent[3] then
					add_time_bonus(params.bonus_t)
					sfx(1)
				end
				slice.is_checkpoint=nil
			end
		else
			-- free riding!
			freeride_t+=1
			self.on_track=nil
		end

		if velocity[3]>0 and abs((angle%1+1)%1-0.5)<0.12 then
			reverse_t+=1
		end

		if reverse_t>30 then
			add_time_bonus(2,"reverse!")
			sfx(pick(whoa_sfx))
			reverse_t=0
		end

		for _,b in pairs(bonus) do
			b.ttl-=1
			if(b.ttl<0) del(bonus,b)
		end

		if hp<=0 then
			self.dead=true			
		elseif t<0 then
			sfx(3)
			self.time_over,self.dead=true,true
		end

		-- call parent
		body_update(self)
	end

	body.score=function()
		return t,bonus,total_t,freeride_t
	end

	-- wrapper
	return body
end

function make_snowball(pos)
	local body=make_car(pos)
	
	local body_update=body.update

	body.sx,body.sy=112,0
	body.update=function(self)		

		-- physic update
		self:prepare()
		self:integrate()
		body_update(self)

		return true
	end
	return body
end

-- game states
-- transition to next state
-- game states
-- transition to next state
local states={}

function pop_state()
	assert(#states>0,"missing base state")	
	states[#states]=nil
end

function push_state(state,...)
	add(states,state(...))
end

function menu_state()
	local cols={
		[0]=5,
		[1]=12,
		[6]=7,
		[8]=14}
	-- draw direction box
	-- mode:
	-- 0: normal
	-- 1: focus
	-- 2: selected
	function draw_box(s,x,y,c,blink,freeride)

		palt(0,false)
		palt(14,true)
		sspr(40,0,8,8,x-4,y-12,8,64)

		pal(12,c)
		if blink then
			if (30*time())%8<4 then
				pal(12,cols[c])
				pal(c,cols[c])
				pal(6,cols[6])
			end
		end
		spr(32,x-24,y-6,7,2)
		print(s,x-20,y-2,6)

		if freeride==true then
			spr(234,x-8,y-29,2,2)
			rectfill(x-18,y-13,x+24,y-8,10)
			print("FREERIDING",x-16,y-13,0)
		end
		pal()
	end
   
	-- default records (number of frames)
	local records={900,600,450}
	for i=0,2 do
		local t=dget(i)
		-- avoid bogus data
		records[i+1]=t>0 and t or records[i+1]
	end

	local tree_prop,bush_prop,cow_prop={sx=112,sy=16,r=1.4,sfx={9,10}},{sx=96,sy=32,r=1,sfx={9,10}},{sx=112,sy=48,r=1,sfx={4}}
	local panels={
		{text="marmottes",c=1,params={dslot=0,slope=1.5,tracks=1,bonus_t=2,total_t=30*30,record_t=records[1],props={tree_prop},props_rate=0.85}},
		{text="biquettes★",c=8,params={dslot=1,slope=2,tracks=2,bonus_t=1.5,total_t=20*30,record_t=records[2],props={tree_prop,bush_prop},props_rate=0.87,}},
		{text="chamois★★",c=0,params={dslot=2,slope=3,tracks=3,bonus_t=1.5,total_t=15*30,record_t=records[3],props={tree_prop,tree_prop,tree_prop,cow_prop},props_rate=0.92,}}
	}
	local sel,sel_tgt,blink=0,0,false

	ground=make_ground({slope=0,tracks=0,props_rate=0.8,props={tree_prop}})

	-- reset rotated sprites
	reload()

	-- reset cam	
	cam=make_cam()

	music(0)
	sfx(-1)
	return {
		-- draw
		draw=function()
			cam:draw_horizon(1,12)

			local out={}
			-- get visible voxels
			ground:collect_drawables(cam.pos,cam.angle,out,dist)

			sort(out)
			draw_drawables(out)

			local a,da=1/3,-1/3
			for i=1,#panels do
				local v={8*cos(a),0.8,-8*sin(a)}
				v_add(v,cam.pos)
				v=m_x_v(cam.m,v)
				if v[3]>0 then
					local x0,y0=cam:project2d(v)
					local p=panels[i]
					draw_box(p.text,x0+32,y0,p.c,blink,p.params.tracks>1)
				end
				a+=da
			end

			-- mask
			rectfill(0,0,127,27,0)
			rectfill(0,92,127,127,0)
			palt(14,true)
			palt(0,false)
			spr(128,64,28,8,9)
			spr(128,0,28,8,9,true)
			palt()

			if sel==sel_tgt then
				local s="best⧗: "..time_tostr(panels[sel+1].params.record_t)
				print(s,64-2*#s,96,sget(37,16*(time()%1)))
			end

			printb("⬅️➡️ select track",31,110,7,5,1)
			if((time()%1)<0.5) printb("❎/🅾️ go!",50,120,10,5,1)
			print("▤@freds72 - ♪@gruber",20,2,1)

			-- title
			spr(64,28,8,10,3)	

			srand(12)
			local t=time()
			for i=0,256 do
				local a,s=rnd(),1+rnd(0.5)
				local u,v=s*cos(a),s*abs(2*sin(a))
				local x0=rnd(128)
				local x,y=flr(x0+t*u)%128,8+flr(t*v)%20
				if pget(x,y)==12 then
					pset(x,y,6+x0%2)
				end
			end		
		end,
		-- update
		update=function()
			if(start_game_async) start_game_async=corun(start_game_async)

			if(btnp(0)) sel-=1
			if(btnp(1)) sel+=1
			sel=mid(sel,0,#panels-1)

  			sel_tgt=lerp(sel_tgt,sel,0.18)
   			-- snap when close to target
			if abs(sel_tgt-sel)<0.01 then
				sel_tgt=sel
			end

			if btnp(4) or btnp(5) then
				-- snap track
				sel_tgt=sel
				sfx(8)
				-- sub-state
				start_game_async=cocreate(function()
					for i=1,15 do
						blink=i%2==0 and true
						yield()
					end
					pop_state()
					-- restore random seed
					srand(time())
					push_state(zoomin_state,play_state,panels[sel+1].params)
				end)
			end

			--
			cam:track({64,0,64},sel_tgt/3,v_up)
		end
	}
end

function zoomin_state(next,params)
	local ttl,dttl=30,0.01

	local fade_async=cocreate(whiteout_async)

	-- copy backbuffer
	memcpy(0x0,0x6000,128*64)

	return {
		-- draw
		draw=function()
			-- zoom effect
			local s=(3*(30-ttl)/30+1)
			palt(0,false)
			local dx=-abs(64*s-64)		
			sspr(0,0,128,128,dx,dx,128*s,128*s)
			-- 
			if(fade_async) fade_async=corun(fade_async,15)
		end,
		update=function(self)
			ttl-=dttl
			dttl+=0.08

			-- done with white out?
			if not fade_async then
				pop_state()
				-- restore spritesheet
				reload()
				push_state(next,params)
			end
		end
	}
end

function play_state(params)

	local fade_async=cocreate(whiteout_async)

	-- stop music
	music(-1,250)

	-- start over
	actors,ground={},make_ground(params)

	-- create player in correct direction
	plyr=make_plyr(ground.plyr_pos,params)

	-- reset cam	
	cam=make_cam()

	-- sprites
	local rot_sprites={
		-- flags
		make_rspr(112,32,128),
		make_rspr(48,0,128)}
	-- per track props
	local prev_prop
	for _,prop in pairs(params.props) do
		if prev_prop!=prop then
			add(rot_sprites,make_rspr(prop.sx,prop.sy,128))
			-- avoid double registration!
			prev_prop=prop
		end
	end

	local gps_sprite=make_rspr(96,48,256)

	return {
		-- draw
		draw=function()
			
			cam:draw_horizon(1,12)
	  
			local out={}
			-- get visible voxels
			ground:collect_drawables(cam.pos,cam.angle,out,dist)
			-- misc. actors
			for _,a in pairs(actors) do
				local p=m_x_v(cam.m,a.pos)
				local ax,ay,az=p[1],p[2],p[3]
				if az>z_near and az<64 then
					local x,y,w=cam:project2d(p)
					out[#out+1]={key=1/(ay*ay+az*az),a=a,x=x,y=y,w=4*w,dist=1}
				end
			end

			-- sprite cache
			local angle=atan2(cam.m[5],cam.m[6])+0.25
			for _,f in pairs(rot_sprites) do
				f(-angle)
			end

			sort(out)
			draw_drawables(out)
			 
			--local cpu=flr(10000*stat(1))/100
			--print(cpu.."%\n"..stat(0),96,2,2)

			if plyr then
				local pos,a,steering=plyr:get_pos()
				local dy=plyr.height*24
				-- ski
				spr(9,34+3*cos(time()/4),128+dy-steering*14,4,4)
				spr(9,74-2*cos(time()/5),128+dy+steering*14,4,4,true)
			
				-- hands
				palt(0,false)
				palt(7,true)
				spr(140,abs(steering)*16-24,90-dy/3,4,4)
				spr(140,96-abs(steering)*16+24,90-dy/3,4,4,true)
				palt()

				-- 
				local t,bonus,total_t=plyr:score()
				printb(time_tostr(t),nil,4,10,9,1)
				
				--[[
				tt="total time:\n"..time_tostr(tt)
				print(tt,2,3,1)
				print(tt,2,2,7)
				]]

				for i=1,#bonus do
					local b=bonus[i]					
					
					if b.ttl/b.duration>0.5 or t%2==0 then
						if(b.msg) printb(b.msg,64+b.x-#b.msg/1.5,31+b.ttl,8,2,1)
						printb(b.t,64+b.x-#b.t/1.5,40+b.ttl,10,9,1)
					end
				end

				if(plyr.gps) gps_sprite(-plyr.gps)		
				if plyr.on_track and (32*time())%8<4 then
					local dx=plyr.gps-0.75
					if dx<-0.1 then
						sspr(64,112,16,16,2,32,32,32)
					elseif dx>0.1 then
						sspr(64,112,16,16,96,32,32,32,true)
					end
				end
				spr(108,56,12,2,2)
					
				-- help msg?
				if total_t<90 then
					printb("🅾️: charge jump",nil,112,6,5,1)
				end
			end

			if(fade_async) fade_async=corun(fade_async,0,true)
		end,
		-- update
		update=function()
			cam:update()
			if plyr then
				plyr:control()	
				plyr:prepare()
				plyr:integrate()
				plyr:update()

				-- adjust ground
				ground:update(plyr.pos)

				local pos,a=plyr:get_pos()
				cam:track(pos,a,plyr:get_up())

				if plyr.dead then
					cam:shake()

					-- latest score
					local _,_,total_t,freeride_t=plyr:score()
					
					push_state(plyr_death_state,plyr:get_pos(),total_t,freeride_t,params,plyr.time_over)
					-- not active
					plyr=nil
				end
			end

			for a in all(actors) do
				if not a:update() then
					del(actors,a)
				end
			end
		end
	}
end

function plyr_death_state(pos,total_t,freeride_t,params,time_over)
	-- convert to string
	local active_msg,msgs=0,{
		"total time: "..time_tostr(total_t),
		"freeride time: "..time_tostr(freeride_t)}	
	local msg_colors={
		{10,9,1},{8,2,1}
	}
	local msg_y,msg_tgt_y,msg_tgt_i=-20,{16,-20},0

	local snowball_sprite=make_rspr(112,0,32,0)
	local turn_side=rnd()>0.5 and -1 or 1
	local text_ttl,text_id,text=10,0,{
		"ouch!","aie!","pok!","weee!"
	}

	-- snowballing!!!
	local snowball=make_snowball(pos)
	add(actors,snowball)

	-- save records (if any)
	if total_t>params.record_t then
		dset(params.dslot,total_t)
	end

	return {
		draw=function()
			local c=msg_colors[active_msg+1]
			printb(msgs[active_msg+1],nil,msg_y,c[1],c[2],c[3])
			if(active_msg==0 and total_t>params.record_t) print("★new record★",50+rnd(2)-1,msg_y+8+rnd(2)-1,c[2])

			if text_ttl>0 and not time_over then
				print(text[text_id+1],60,50+text_ttl,8)
			end
			if((time()%1)<0.5) printb("❎/🅾️ retry",42,120,10,5,1) printb("game over!",nil,38,8,2,7)
		end,
		update=function()
			msg_y=lerp(msg_y,msg_tgt_y[msg_tgt_i+1],0.08)
			if(abs(msg_y-msg_tgt_y[msg_tgt_i+1])<1) msg_tgt_i+=1 
			if(msg_tgt_i>#msg_tgt_y-1) msg_tgt_i=0 active_msg=(active_msg+1)%2

			text_ttl-=1
			snowball_sprite(turn_side*time()*2)
			-- adjust ground
			local p=snowball.pos
			ground:update(p)

			if text_ttl<0 and ground:collide(p,0.2) then
				text_id,text_ttl=flr(rnd(#text)),10
				turn_side*=-1
				cam:shake()
			end
			-- keep camera off side walls
			if time_over then
				cam:track(p,0,v_up)
			else
				cam:track({mid(p[1],8,29*4),p[2],p[3]+16},0.5,v_up,0.2)
			end

			if btnp(4) or btnp(5) then
				pop_state()
				pop_state()
				push_state(menu_state)
			end
		end
	}
end

function _init()
	cartdata("freds2_st8p")

	-- todo: remove (only for benchmarks)
	-- srand(12)

	for i=0,15 do
		local r={}
		fadetable[i]=r
		for j=0,4 do
			-- lookup color for c0
			r[j]=sget(24+j,i)
		end
	end

	-- init state machine
	push_state(menu_state)
end

function _update()
	-- state mgt
	for state in all(states) do
		state:update()
	end
end

function _draw()
	for state in all(states) do
		state:draw()
	end
end

local dither_pat={0xffff,0x7fff,0x7fdf,0x5fdf,0x5f5f,0x5b5f,0x5b5e,0x5a5e,0x5a5a,0x1a5a,0x1a4a,0x0a4a,0x0a0a,0x020a,0x0208,0x0000}
-- init palette dithering
function create_hexpal(mem,cx)
	for i=0,15 do
		poke(mem+i,sget(cx,i))
	end
	poke(mem,0x10)
end
create_hexpal(0x4300,33)
create_hexpal(0x4310,34)

function draw_sprite(actor,x,y,w,dist)
	if dist>8 then 
		memcpy(0x5f00,0x4310,16)
	elseif dist>7 then
		memcpy(0x5f00,0x4300,16)
	end
	local sx,sy=actor.sx,actor.sy
	if actor.strip then
		local strip=actor.strip[flr((actor.speed*time())%#actor.strip)+1]
		sx,sy=strip.sx,strip.sy
	end
	sspr(sx,sy,16,16,x-w/2,actor.y or y-w,w,w)
	pal()
end

function draw_drawables(objects)	
	for i=1,#objects do
		local d=objects[i]
		if d.a then
			draw_sprite(d.a,d.x,d.y,d.w,d.dist)
		else
			-- triangle
			local c0=0x76
			if d.f.m==0 then
				-- dirt
				c0=0x54
				if(d.dist>7) c0=0x4d
				if(d.dist>8) c0=0xd5
			elseif d.f.m==1 then
				if(d.dist>7) c0=0x6d
				if(d.dist>8) c0=0xd5
			else
				c0=0x65
				if(d.dist>7) c0=0x5d
				if(d.dist>8) c0=0xd1
			end
			fillp(d.f.cf)
			cam:project_poly(d.v,c0)
			-- face 'details' sprites
			if d.fa then
				local m,v0,u=cam.m,d.v0,d.fa.u
				local x,y,z=v0[1]+u[1],v0[2]+u[2],v0[3]+u[3]
				local az=m[3]*x+m[7]*y+m[11]*z+m[15]
				if az>z_near then	
					local ax,ay=m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14]
					-- sprite
					local w=63.5/az
					local x,y=63.5+flr(w*ax),63.5-flr(w*ay)
					draw_sprite(d.fa,x,y,4*w,d.dist)
				end
			end
		end
	end
	fillp()
end

-->8
-- map tools
-- generate ski tracks
function make_tracks(xmin,xmax,max_tracks)
	local seeds={}
	local function add_seed(x,u,branch)
		-- trick types:
		-- 0: slope
		-- 1: hole
		local ttl,trick_ttl,trick_type

		local function reset_seed_timers()
			ttl,trick_ttl,trick_type=12+rnd(20),4+rnd(4),flr(rnd(2))		
		end

		reset_seed_timers()

		local angle=0.05+rnd(0.45)
	 	return add(seeds,{
			age=0,
			h=0,
	 		x=x or xmin+rnd(xmax-xmin),
			u=u or cos(angle),
			angle=angle,
		 	update=function(self)
			 	if(self.dead) del(seeds,self) return
				self.age+=1
				trick_ttl-=1
				ttl-=1
				if branch and trick_ttl<0 then
					if trick_type==0 then
						self.h+=1.5
					elseif trick_type==1 then
						self.h=-4
					end
					-- not too high + ensure straight line
					if(trick_ttl<-5) self.h=0 ttl=4+rnd(2)
				end
				if ttl<0 then
					-- reset
					reset_seed_timers()
					self.u=cos(0.05+rnd(0.45))
					-- offshoot?
					if rnd()<0.5 and #seeds<max_tracks then
						add_seed(self.x,-self.u,true)
					end
				end
				self.x+=self.u
				if self.x<xmin then
					self.u=-self.u
					self.x=xmin
				elseif self.x>xmax then
					self.u=-self.u
					self.x=xmax
				end
		 	end
	 	})
	end
 	-- init
 	add_seed().main=true

 	-- update function
 	return function()
		for s in all(seeds) do
			s:update()
		end
		-- kill intersections
		for i=1,#seeds do
			local s0=seeds[i]
			for j=i+1,#seeds do
				local s1=seeds[j]
				-- don't kill new seeds
				-- don't kill main track
				if s1.age>0 and flr(s0.x-s1.x)==0then
					-- don'kill main track
					s1=s1.main and s0 or s1
					s1.dead=true
				end
			end
		end
	
		-- active seeds
		return seeds
	end
end

function make_ground(params)
	-- ground params
	local delta_slope=params.slope

	-- number of x/z slices + cell size
	local nx,nz,dx,dy,dz=32,32,4,0,4

	-- ground slices (from 0 to nz-1)
	local slices={}

	-- track generator
	local next_tracks=make_tracks(8,nx-8,params.tracks or 3)

	-- uses m (matrix) and v (vertices) from self
	-- saves the 'if not ...' in inner loop
	local v_ground_cache_cls={
		__index=function(t,k)
			-- inline: local a=m_x_v(t.m,t.v[k]) 
			-- slice index
			local m,i,j=t.m,k%nx,flr(k/nx)
			local s0=slices[j]
			-- generate vertex
			local x,y,z=i*dx,s0.h[i]+s0.y-dy,j*dz
			local ax,az=m[1]*x+m[5]*y+m[9]*z+m[13],m[3]*x+m[7]*y+m[11]*z+m[15]
		
			local outcode=az>z_near and k_far or k_near
			if ax>az then outcode+=k_right
			elseif -ax>az then outcode+=k_left
			end	

			local ay=m[2]*x+m[6]*y+m[10]*z+m[14]
			t[k]={ax,ay,az,outcode=outcode,x=63.5+flr(63.5*ax/az),y=63.5-flr(63.5*ay/az)}
			return t[k]
		end
	}

	local slice_id,slice_y=0,0
	local left_pole,right_pole,coin={sx=48,sy=0},{sx=112,sy=32},{speed=3,r=1,score=1,strip={{sx=64,sy=96},{sx=80,sy=96},{sx=96,sy=96},{sx=112,sy=96}}}
	local function make_slice(y)
		-- smooth altitude changes
		slice_y=lerp(slice_y,y,0.2)
		y=slice_y
		-- generate tracks 
		local tracks=next_tracks()
		
		-- height array + props array
		local h,actors={},{}
		for i=0,nx-1 do
			h[i]=rnd(2*delta_slope)
			-- props?
			if rnd()>params.props_rate then
				actors[i]=clone(pick(params.props))-- {sx=prop.sx,sy=prop.sy,sfx=prop.sfx,r=1.2}
			end
		end
			
		-- smoothing
		for k=1,2 do
 			for i=0,nx-1 do
				h[i]=(h[i]+h[(i+1)%nx])/2
			end
		end

		-- side walls
		h[0],h[nx-1]=15+rnd(5),15+rnd(5)

		-- flatten track
		local main_track_x,xmin,xmax,is_checkpoint
		for _,t in pairs(tracks) do
			local ii=flr(t.x)
			local i0,i1=ii-2,ii+1
			if t.main then
				main_track_x,xmin,xmax=t.x,i0*dx,i1*dx
				for i=i0,i1 do				
					-- smooth track
					h[i]=t.h+h[i]/4
					-- remove props from track
					actors[i]=nil
				end
				-- race markers
				if slice_id%8==0 then
					is_checkpoint={}
					actors[i0]=clone(left_pole)-- {sx=left_pole.sx,sy=left_pole.sy}
					actors[i1]=clone(right_pole)-- {sx=right_pole.sx,sy=right_pole.sy}					
				end
			else
				-- side tracks
				for i=i0,i1 do				
					h[i]=t.h+h[i]/2
				end
				-- coins
				if(slice_id%2==0) actors[ii]=clone(coin)-- {strip=coins_strip,speed=3,r=1,score=1}
			end
		end

		slice_id+=1
		return {
			y=y,
			h=h,
			-- track extent
			x=main_track_x*dx,
			xmin=xmin,
			xmax=xmax,
			actors=actors,
			is_checkpoint=is_checkpoint
		}
	end

	local function mesh(j)
		local s0,s1=slices[j],slices[j+1]
		-- base slope normal
		local sn={0,dz,s0.y-s1.y} 
		v_normz(sn)
		local function with_material(f,i)
			local c=5*f.n[2]
			local cf=(#dither_pat-1)*(1-c%1)
			f.cf=dither_pat[flr(cf)+1]

			if(i==0 or i==nx-2) f.m=1 return f
			f.m=v_dot(sn,f.n)<0.8 and 0 or 1
			return f
		end
 	
		local f,fi,actor={},0
		for i=0,nx-2 do
			local v0={i*dx,s0.h[i]+s0.y,j*dz}
			-- v1-v0
			local u1={dx,s0.h[i+1]-s0.h[i],0}
			-- v2-v0
			local u2={dx,s1.h[i+1]+s1.y-v0[2],dz}
			-- v3-v0
			local u3={0,s1.h[i]+s1.y-v0[2],dz}

			-- normals
			local n0,n1=v_cross(u3,u2),v_cross(u2,u1)
			v_normz(n0)
			v_normz(n1)
			if v_dot(n0,n1)>0.995 then
				-- quad
				-- material:
				-- 0: snow
				-- 1: dirt
				f[fi]=with_material({n=n0},i)
			else
				-- 2 tri
				f[fi]=with_material({n=n0},i)
				f[fi+1]=with_material({n=n1},i)
			end

			-- actor position (if any)
			local u,t0,t1=v_clone(u2),rnd(),rnd()
			v_scale(u,t0)
			v_add(u,u3,t1)
			v_scale(u,1/(rnd()+t0+t1))
			if(s0.actors[i]) s0.actors[i].u=u

			fi+=2
		end
		-- attach faces to slice
		s0.f=f
	end

	local ybase=0
	for j=0,nz-1 do
		slices[j]=make_slice(ybase)
		ybase-=delta_slope+rnd(0.1*delta_slope)
	end
	-- create faces
	for j=0,nz-2 do
		mesh(j)
	end

	local function collect_face(v0,face,actor,vertices,v_cache,cam_pos,out,dist)
		if v_dot(face.n,cam_pos)>v_dot(face.n,v0) then
			local z,y,outcode,verts,is_clipped=0,0,0xffff,{},0
			-- project vertices (indices)
			for ki,vi in pairs(vertices) do
				local a=v_cache[vi]
				y+=a[2]
				z+=a[3]
				outcode=band(outcode,a.outcode)
				-- behind near plane?
				is_clipped+=band(a.outcode,2)
				verts[ki]=a
			end
			-- mix of near/far verts?
			if outcode==0 then
				-- average before clipping verts
				y/=#verts
				z/=#verts

				-- mix of near+far vertices?
				if(is_clipped>0) verts=z_poly_clip(z_near,verts)
				-- still visible?
				if #verts>2 then
					out[#out+1]={key=1/(y*y+z*z),f=face,fa=actor,v=verts,dist=dist,v0=v0}
				end
			end
		elseif actor then
			-- face is not visible but props might be
			local m,u=v_cache.m,actor.u
			local x,y,z=v0[1]+u[1],v0[2]+u[2],v0[3]+u[3]
			local az=m[3]*x+m[7]*y+m[11]*z+m[15]
			if az>z_near and az<64 then	
				local ax,ay=m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14]
				local w=63.5/az
				out[#out+1]={key=1/(ay*ay+az*az),a=actor,x=63.5+flr(w*ax),y=63.5-flr(w*ay),w=4*w,dist=dist}
			end
		end
	end

	-- raycasting constants
	local angles={}
	for i=0,23 do
		add(angles,atan2(7.5,i-12.5))
	end

	local function visible_tiles(pos,angle)
		local x,y=pos[1]/dx,pos[3]/dz
		local x0,y0=flr(x),flr(y)
		local tiles={[x0+y0*nx]=0}

		for i,a in pairs(angles) do
			local v,u=cos(a+angle),-sin(a+angle)
			
			local mapx,mapy=x0,y0
	
			local ddx,ddy,mapdx,distx,mapdy,disty=1/u,1/v
			if u<0 then
				mapdx,ddx=-1,-ddx
				distx=(x-mapx)*ddx
			else
				mapdx,distx=1,(mapx+1-x)*ddx
			end
			if v<0 then
				mapdy,ddy=-1,-ddy
				disty=(y-mapy)*ddy
			else
				mapdy,disty=1,(mapy+1-y)*ddy
			end	
			for dist=0,12 do
				if distx<disty then
					distx+=ddx
					mapx+=mapdx
				else
					disty+=ddy
					mapy+=mapdy
				end
				-- non solid visible tiles
				if band(bor(mapx,mapy),0xffe0)==0 then
					tiles[mapx+mapy*nx]=dist
				end
			end				
		end
		return tiles	
	end

	local plyr_z_index,max_pz=nz/2-1,-32000
	return {
		plyr_pos={slices[plyr_z_index].x,0,plyr_z_index*dz},
		to_tile_coords=function(self,v)
			return flr(v[1]/dx),flr(v[3]/dz)
		end,
		collect_drawables=function(self,cam_pos,cam_angle,out)
			local tiles=visible_tiles(cam_pos,cam_angle)
			-- vertex cache
			-- yoffset
			local v_cache={m=cam.m}
			setmetatable(v_cache,v_ground_cache_cls)

			for k,dist in pairs(tiles) do
				-- get slice(s)
				local i,j=k%nx,flr(k/nx)
				local s0=slices[j]
				if s0 and s0.f then
					-- generate vertex
					local v0={i*dx,s0.h[i]+s0.y-dy,j*dz}
					-- face(s)
					-- force quad rendering for far away tiles
					local f0,f1=s0.f[2*i],s0.f[2*i+1]
					if f1 then
						-- 2 triangles
						if(f0) collect_face(v0,f0,s0.actors[i],{k,k+1+nx,k+nx},v_cache,cam_pos,out,dist)
						if(f1) collect_face(v0,f1,nil,{k,k+1,k+1+nx},v_cache,cam_pos,out,dist)
					else
						-- 1 quad
						if(f0) collect_face(v0,f0,s0.actors[i],{k,k+1,k+1+nx,k+nx},v_cache,cam_pos,out,dist)
					end
				end
			end
		end,
		update=function(self,p)
			-- prevent out of bounds
			p[3]=max(p[3],8*dz)
			local pz=p[3]/dz
			if pz>plyr_z_index then
				-- shift back
				p[3]-=dz
				max_pz-=dz
				local old_y=slices[0].y
				-- drop slice 0
				for i=1,nz-1 do
					slices[i-1]=slices[i]
					slices[i-1].y-=old_y
				end
				-- use previous baseline
				slices[nz-1]=make_slice(slices[nz-2].y-delta_slope*(rnd()+0.5))
				-- create mesh
				mesh(nz-2)
			end
			-- update y offset
			if p[3]>max_pz then
				dy,max_pz=lerp(slices[0].y,slices[1].y,pz%1),p[3]
			end
		end,
		find_face=function(self,p)
			-- z slice
			local i,j=self:to_tile_coords(p)

			local s0=slices[j]
			-- should not happen
			-- assert(s0,"outside of map: "..i.."/"..j)

			local f0,f1=s0.f[2*i],s0.f[2*i+1]
			local f=f0
			-- select face
			if(f1 and (p[3]-dz*j<p[1]-dx*i)) f=f1

			-- intersection point
			local t=-v_dot(make_v({i*dx,s0.h[i]+s0.y-dy,j*dz},p),f.n)/f.n[2]
			-- todo: return y value only
			p=v_clone(p)
			p[2]+=t

			return f,p,atan2(slices[j+2].x-p[1],2*dz)
		end,
		get_track=function(self,p)
			-- z slice
			local i,j=self:to_tile_coords(p)
			local s0=slices[j]
			-- no need for y
			return s0,{s0.xmin,s0.xmax,j*dz}
		end,
		-- find all actors within a given radius from given position
		collide=function(self,p,r)
			local i0,j0=self:to_tile_coords(p)
			-- out of track
			if (i0<=0 or i0>=nx-2) return 2
			
			-- square radius
			r*=r
			
			-- check all 9 cells (overkill but faster)
			for j=j0-1,j0+1 do
				local s0=slices[j]
				if s0 then
					for i=i0-1,i0+1 do
						local actor=s0.actors[i]
						-- collidable actor?
						if actor and actor.r then
							-- generate vertex
							local v0={i*dx,s0.h[i]+s0.y-dy,j*dz}
							v_add(v0,actor.u)
							local d=make_v(p,v0)
							d[2]=0
							if v_dot(d,d)<r+actor.r*actor.r then
								if actor.score then
									s0.actors[i]=nil
									return 3,actor
								end
								return 1,actor
							end
						end
					end
				end	
			end
		end
	}
end

-->8 
-- rotation cache builder
-- returns a function that copies rotated version in place
-- n: number of cache entries
-- tc: transparent color
function make_rspr(sx,sy,n,tc)
	-- target=sprite initial location!
	-- must be on 4 byte boundary
	assert(sx%16==0,"sprite x must be a multiple of 16:"..sx)

	-- if not provided, transparent color is top/left sprite pixel
	tc=tc or sget(sx,sy)

	-- build up cache per angle
	local angles={}
	for i=0,n-1 do
		local a=i/n-0.25
		local ca,sa=cos(a),sin(a)
		local dx0,dy0=(sa-ca)*7.5+8,-(ca+sa)*7.5+8
		
		rectfill(0,0,15,15,tc)
		local cache={}
		-- sprite sheet memory location
		local src,dst=0x6000,512*flr(sy/8)+flr(sx%128)/2
		for iy=0,15 do
			local srcx,srcy=dx0,dy0
			for ix=0,15 do
				if band(bor(srcx,srcy),0xfff0)==0 then
					pset(ix,iy,sget(sx+srcx,sy+srcy))
				end
				srcx-=sa
				srcy+=ca
			end
			dx0+=ca
			dy0+=sa

			cache[dst],cache[dst+4]=peek4(src),peek4(src+4)
			-- one line down
			src+=64  
			dst+=64  
		end
		angles[i]=cache
	end

	-- update spritesheet with rotated version
	return function(angle)
		angle=(angle%1+1)%1
		for k,v in pairs(angles[flr(n*angle)]) do
			poke4(k,v)
		end
	end
end

-->8
-- print helpers
function padding(n)
	n=tostr(flr(min(n,99)))
	return sub("00",1,2-#n)..n
end

function time_tostr(t)
	-- frames per sec
	local s=padding(flr(t/30)%60).."''"..padding(flr(10*t/3)%100)
	-- more than a minute?
	if(t>1800) s=padding(flr(t/1800)).."'"..s
	return s
end

function printb(s,x,y,cf,cs,cb)
	x=x or 64-#s*2
	for i=-1,1 do
		for j=-2,1 do
			print(s,x+i,y+j,cb)
		end
	end
	print(s,x,y,cs)
	print(s,x,y-1,cf)
end

-->8
-- trifill & clipping
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

--[[
function trifill(x0,y0,x1,y1,x2,y2,col)
	line(x0,y0,x1,y1,col)
	line(x2,y2)
	line(x0,y0)
end
]]

function z_poly_clip(znear,v)
	local res={}
	local v0,v1,d1,t,r=v[#v]
	local d0=-znear+v0[3]
 	-- use local closure
 	local clip_line=function()
 		local r,t=make_v(v0,v1),d0/(d0-d1)
 		v_scale(r,t)
 		v_add(r,v0)
 		res[#res+1]=r
 	end
	for i=1,#v do
		v1=v[i]
		d1=-znear+v1[3]
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

__gfx__
000000000000000000eeeeee5566700000000000eed676ee00000000000000000000000000000000000005555555550000000000000000000000000000000000
0000000000000000770eeeeedd66700015500100eed676ee00000000000000000000000000000000000056666666775000000000000000000000000000000000
0000000000000000cc70eeee266770002d500100eed676ee00000000000000000000000000000000000562222222257500000000000000000000000000000000
0000000000000000ccc70eee3666700036d00500eed676ee00000000000000000000000000000000005622222222222600000000009290000000000000000000
0000000000000000cccc70ee4ff7700049600500eed676ee000000000000000000000000000000000052222222222222200000000022200000000e0880e00000
0000000000000000ccccc70e566770005d600d00eed676ee0000000080000000000000000000000000222222222222222000000000929000000000cccc000000
0000000000000000ccccc70e667770006dd00d00eed676ee00000082800000000000000000000000022222822222222220000000000000000000000cc0000000
0000000000000000ccccc70e7777700076d00600eed676ee00000882800000000000000000000000022222282828282220000000000000000000200110020000
0000000000000000cccc70ee8ee770008ed006000000000000000082800000000000000000000000022828222222222220000000000000000000801001080000
0000000000000000ccc70eee9aff700094200700000000000000000080000000000000000000000002828282828282822000000000a8a0000000080000800000
0000000000000000cc70eeeeafff7000a6d00700000000000000000050000000000000000000000002888828282828282000000000a8a0000000008008000000
0000000000000000770eeeeebb677000b3d007000000000000000000600000000000000000000000028888888888888820000000009290000000000000000000
000000000000000000eeeeeecc667000cd1006000000000000000000600000000000000000000000028888888888888820000000002220000000000000000000
0000000000000000eeeeeeeed6677000dd100d000000000000000000600000000000000000000000288888888888888820000000009290000000000000000000
0000000000000000eeeeeeeeee777000e25005000000000000000000600000000000000000000000288888888888888820000000000000000000000000000000
0000000000000000eeeeeeeeff777000f6d001000000000000000001510000000000000000000000288888899988888200000000000000000000000000000000
ee000000000000000000000000000000000000000000000000eeeeee0000000000000000000000002888889aaa98888200000000000000000000000000000000
e00777777777777777777777777777777777777777777777770eeeee000000000000000000000000288889aaaaa98882000000000000000000000007b0000000
e07ccccccccccccccccccccccccccccccccccccccccccccccc70eeee000000000000000000000000288889a888a988820000000000000000000000537b000000
e07cccccccccccccccccccccccccccccccccccccccccccccccc70eee00000000000000000000000028889a88888a98820000000000000000000000b53d000000
e07ccccccccccccccccccccccccccccccccccccccccccccccccc70ee00000000000000000000000288888a88888a8882000000000000000000000357b7b00000
e07cccccccccccccccccccccccccccccccccccccccccccccccccc70e000000000000000000000002888888888888888200000000000000000000053b7b700000
e07cccccccccccccccccccccccccccccccccccccccccccccccccc70e0000000000000000000000028888888898888882000000000000000000000b533bb00000
e07cccccccccccccccccccccccccccccccccccccccccccccccccc70e00000000000000000000000288888889998888820000000000000000000035b5b33d0000
e07ccccccccccccccccccccccccccccccccccccccccccccccccc70ee0000000000000000000000028888889aaa98888200000000000000000000535b7b7b0000
e07cccccccccccccccccccccccccccccccccccccccccccccccc70eee000000000000000000000002888889aa8aa9888200000000000000000000b533b7b70000
e07ccccccccccccccccccccccccccccccccccccccccccccccc70eeee000000000000000000000002888888a888a88820000000000000000000035b5b333b7000
e00777777777777777777777777777777777777777777777770eeeee00000000000000000000000288888888888888200000000000000000000535b5bbb3d000
ee000000000000000000000000000000000000000000000000eeeeee000000000000000000000002888888888888882000000000000000000005535bbbbbb000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000002888888888888882000000000000000000000000110000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000002888888888888882000000000000000000000000440000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000002888888888888882000000000000000000000000440000000
000000000000000000000000000000000000000000000000000000000000000000000000000ccc00000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000ccccc00000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000ccccc00000000000000000000000000000000000000000000000000
000000ccccc0000000ccc000000000000000ccccc0000000cccc00000ccc00000ccc0000cccccc00000000000000000000000000000000000000000000000000
0000cccccccc00000cccc00000000000000ccc0000000000cccc0000ccccc000cccc0000ccccc000000000000000000000000000000000000000000000000000
000cccc00cccc0000cccc0000ccc00000cccc000ccc0000ccccc0000ccccc00ccccc0000ccccc000000000000000000000000000000000000000000b00000000
00cccc000cccc000ccccc000cccc0000cccc00cccccc000ccccc000cccccc00ccccc000ccccc0000000000000000000000000000000000000000000b3b000000
0ccccc000cccc000ccccc00ccccc0000cccc000cccccc00ccccc000cccccc00ccccc000ccccc0000000000000000000000000056660000000000000b3bb00000
0cccc0000ccc000cccccc0cccccc000cccc0000cccccc00ccccc000cccccc00ccccc000ccccc0000000000000000000000000567776000000000000b3b000000
ccccc0000cc0000ccccc0ccccccc000cccc00000ccccc00ccccc00ccccccc00cccc0000cccc00000000000000000000000005636777700000000000b00000000
cccccc000000000ccccccccccccc00ccccc00000ccccc00ccccc00ccccccc00cccc000ccccc0000000000000000000000000633b7b3b00000000000500000000
ccccccccc000000ccccccccccccc00ccccc00000ccccc00ccccc0ccc0ccccc0cccc000ccccc00000000000000000000000000343b3b000000000000600000000
0ccccccccccc00cccccccc0ccccc00ccccc00000ccccc000ccccccc00ccccc0cccc000cccc0000000000000000000000000000333b0000000000000600000000
00ccccccccccc0ccccccc00ccccc00ccccc0000ccccc0000ccccccc00ccccc0ccc0000cccc000000000000000000000000000001100000000000000600000000
00000cccccccc0cccccc000ccccc00cccccc000ccccc0000cccccc000ccccc0ccc0000cc00000000000000000000000000000004000000000000000600000000
00ccc000ccccc0ccccc0000cccccc00cccccccccccc00000cccccc0000cccccccc00000000000000000000000000000000000004000000000000001510000000
0ccc0000cccc00cccc000000ccccc000ccccccccc00000000cccc00000ccccccc000000ccc000000000000000000000000000000000000000000000000000000
cccc0000cccc000cc00000000ccc000000ccccc00000000000cc000000ccccccc00000cccc000000000000000000000000000000000000000000000000000000
cccc00cccc0000000000000000000000000000000000000000000000000ccccc00000ccccc000000000000000000000000000000001100000000000000000000
ccccccccc000000000000000000000000000000000000000000000000000ccc000000cccc000000000000000000000000000000011aa10000000000000000000
0cccccc00000000000000000000000000000000000000000000000000000000000000ccc00000000000000000000000000000011aaaa10000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011aaaaa100000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011aaaaaa1000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001aaaaaaa10000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001999999910000000060000600000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000119999991000000056996500000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001199999100000001441299924000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011999910000004444249940400
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000119910000000ee2444440000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110000000002422e4e0000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000400000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005000500000
eeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000077777777777777777777777777777777
eeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000077777777777777777777777777777777
eeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000077777777777777777770000777777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000077777777777777000000000077777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000077777777777000000000000007777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000077777777770000000000000000777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000077777777700000000000000000777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000077777777000000000000000000777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000077777770000000000000000000777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000077777700000000000000000000777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000077777000000000000000000007777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000077700000000000000005555077777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000077000000000000000055555507770007
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000070000000000000000055555550000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000055555550000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000050000000000000000055555555000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000085500000000000000055555550000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000088550000000000000000555500000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000028885000000000000000000000000007
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000022885500000000000000000000000077
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000082888550000000000000000000000777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000088888850000000000000000000007777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088888855000000000000000000077777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088888885500000000000000007777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088888888500000000000000077777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088888888850000000000077777777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088228888855000000007777777777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088822888885000007777777777777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088882222885000077777777777777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088888222285007777777777777777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088888222222277777777777777777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000088888222222777777777777777777777
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
00eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
00000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000f9990000000000000f900000000000000ff000000000000009f0000000
000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000f9449900000000000f4490000000000000ff0000000000000944f000000
0000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000009499a90000000000094a900000000000009900000000000009a49000000
0000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000009499a90000000000094a400000000000004400000000000004a49000000
00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000099aa99000000000009aa400000000000004400000000000004aa9000000
00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000009999000000000000094000000000000004400000000000000490000000
000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000111111000000eeeeeeeaaeeeeeee00000000000000000000000000000000
000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000001aa9944100000eeeeeea00aeeeeee00000000000000000000000000000000
0000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000001aa9944100000eeeeea0aa0aeeeee00000000000000000000000000000000
00000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000001aa99441000000eeeeea0aa0aeeeee00000000000000000000000000000000
000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000001aa99441000000eeeea0aaaa0aeeee00000000000000000000000000000000
00000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000001aa994410000000eeeea0a00a0aeeee00000000000000000000000000000000
00000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000001aa994410000000eeea0aa00aa0aeee00000000000000000000000000000000
0000000000000000eeeeeeeeeeeeeeeeeeeeeeee00000000000e0000000000001aa9944100000000eeea0aa00aa0aeee00000000000000000000000000000000
00000000000000000000eeeeeeeeeee0000000000000000eeeeeee00000000001aa9944100000000eea0aaa00aaa0aee00000000000000000000000000000000
00000000000000000000000000000000000000000000eeeeeeee00000000000001aa994410000000eea0aaaaaaaa0aee00000000000000000000000000000000
000000000000000000000000000000000000000000eeeeeee00000000000000001aa994410000000ea0aaaa00aaaa0ae00000000000000000000000000000000
0000000000000000000000000000000eeeeeeeee000e00000000000000000000001aa99441000000ea0aaaa00aaaa0ae00000000000000000000000000000000
0000000000000000000eeeeeee00eeeeeeeeeeee000000000000000000000000001aa99441000000a0aaaaaaaaaaaa0a00000000000000000000000000000000
00000000000000000000eeeeeee00eeeee0000000000000000000000000000000001aa9944100000a00000000000000a00000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000001aa9944100000eaaaaaaaaaaaaaae00000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000111111000000eeeeeed676eeeeee00000000000000000000000000000000
__label__
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc777777777777
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc777777777777
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77777777777
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77777777777
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc7777777777
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc777777777
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77777777
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc777777
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
77777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
77777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777777777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
77777777777777777777777777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
77777777777777777777777777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
777777777777777777777777777777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777777777777777777777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
77777777777777777777777777777777777777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
777777777777777777777777777777777777777777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
77777777777777777777777777777777777777777777777777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777777777777777777777777777777777777777777777777777cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
77777777777777777777777777777777777777777777777777777777777776d677777ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777777777777777777777777777777777777777777777776d7777777777777ccccccccccccccccccccccccccccccccccccccccccccccccccccc
77777777777777777777777777777777777777777777777777777777777dd777777777777777777777cccccccccccccccccccccccccccccccccccccccccccccc
7777777777777777777777777777777777777777777777777777777777dd777777777777777777777777777777cccccccccccccccccccccccccccccccccccccc
777777777777777777777777777777777777777777777777777777777dd77777777777777777777777777777777777777777cccccccccccccccccccccccccccc
77777777777777777777777777777777777777777777777777777777dd777777777777777777777777777777777777777777777777777777777777777ccccccc
7777777777777777777777777777777777777777777777777777777dd77777777777777777777777777777777777777777777777777777777777777777777777
7777777777777777777777777777777777777777777777777777776d777777777777777777777777777777777777777777777777777777777777777777777777
777777777777777777777777777777777777777777777777777777d7777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777d67777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777776d77777777777777777777777777777777777777777777777777777777777777777777777777
7777777777777777777777777777777777777777777777777777d677777777777777777777777777777777777777777777777777777777777777777777777777
7777777777777777777777777777777777777777777777777776d777777777777777777777777777777777777777777777777777777777777777777777777777
777777777777777777777777777777777777777777777777777d7777777777777777777777777777777777777777777777777777777777777777777777777777
777777777777777777777777777777777777777777777777776d7777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777d67777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777d77777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777d77777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777776d77777777777777777777777777777777777777777777777777777777777777777777777777777
7777777777777777777777777777777777777777777777777d677777777777777777777777777777777777777777777777777777777777777777777771111777
7777777777777777777777777777777777777777777777776d777777777777777777777777777777777777777777777777777777777777777777777111111777
777777777777777777777777777777777777777777777777dd777777777777777777777777777777777777777777777777777777777777777777771111111777
777777777777777777777777777777777777777777777777dd777777777777777777777777777777777777777777777777777777777777777777771111111777
777777777777777777777777777777777777777777777777dd777777777777777777777777777777777777777777777777777777777777777777711111111777
77777777777777777777777777777777777777777777777d6d777777777777777777777777777777777777777777777777777777777777777777711111117777
77777777777777777777777777777777777777777777777d6d777777777777777777777777777777777777777777777777777777777777777777711111117777
77777777777777777777777777777777777777777777777dd7777777777777777777777777777777777777777777777777777777777777777777111111177777
7777777777777777711111177777777777777777777777d6d7777777777777771111111777777777777777777777711117777777111777777777111111177777
7777777777777711111111117777777777711177777777d6d7777777777771111111111777777777711117777777111111777771111177777777111111177777
7777777777777111111111111777777777111117777777dd77777777777711111117777777777771111111777777111111777711111177777771111111777777
777777777777111111711111117777777111111777777d6d77777777771111111777111117777771111111777771111111777111111177777771111111777777
777777777711111117771111117777771111111777777d1117777777711111117711111111777711111111777771111111777111111177777771111111777777
77777777771111117777111111777777111111177777111111777777111111177111111111177711111111777711111111777111111177777711111117777777
77777777711111117777111111777771111111177771111111777771111111777711111111177711111111777711111111777111111177777711111117777777
77777777111111177777111117777771111111177711111111777771111111777771111111117711111111777711111111777111111177777711111177777777
77777777111111177777111117777711111111177111111111777711111117777777111111117711111111777111111111777111111777777711111177777777
77777771111111177771111177777711111111771111111111777111111117777777111111117711111111777111111111177111111777777111111177777777
77777771111111177771117777777711111111711111111111777111111177777777711111117711111111771111111111177111111777777111111777777777
77777771111111111777777777777111111111111111111111777111111177777777711111117711111111771111111111177111111777777111111777777777
77777771111111111111777777777111111111111111111111771111111177777777711111117711111111771111111111177111111777777111177777777777
77777771111111111111111777777111111111111111111111771111111177777777711111117771111111711117111111177111117777777111777777777777
77777777111111111111111117777111111111111711111111771111111177777777711111177771111111711117111111177111117777777777777777777777
77777777771111111111111111771111111111117d11111111771111111177777777711111177771111111111177111111117111117777777711117777777777
77777777777711111111111111771111111111177d11111111771111111117777777111111777771111111111177111111117111177777771111117777777777
777777777777777711111111117711111111117dd711111111771111111117777771111111777777111111111777111111117111177777711111117777777777
77777777771117777111111111771111111117dd7711111111777111111111777711111117777777111111111777111111117111177777711111117777777777
77777777111117777711111117771111111177d77771111111177111111111111111111177777777111111117777711111111111777777111111177777777777
7777777111117777771111111777111111177d777771111111177711111111111111117777777777711111117777711111111111777777111111177777777777
777777111111777771111111777771111177d7777777111111177777111111111111177777777777711111177777711111111117777777111111777777777777
7777771111117777711111177777dd11177d77777777711117777777777111111177777777777777771111777777771111111117777777711177777777777777
77777711111117711111117777dd777777d777777777777777777777777777777777777777777777777777777777777111111177777777777777777777777777
7777771111111111111177777d6777777d7777777777777777777777777777777777777777777777777777777777777111111177777777777777777777777777
77777771111111111117777dd7777777d77777777777777777777777777777777777777777777777777777777777777771111777777777777777777777777777
777777777111111177777dd7777777dd777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
7777777777777777777dd7777777dd77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777dd7777776dd7777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
7777777777777777d6777777dd777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777dd777777dd67777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
777777777777dd677777dd6777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777d7777777d677777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
777777777dd7777776d7777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
7777777dd77777776d77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777dd77777777dd777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
777dd67777777dd67777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
7dd777777777dd777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
d6777777777d77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
777777777dd777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
7777777dd67777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
777777d6777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777d67777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
777dd677777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77dd6777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
6d677777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
d6777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
67777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777

__map__
0505050505050505050505050505050500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0505050505050505050505050505050500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0505050505050505050505050505050500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0505050505050505050505050505050500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0505050505050505050505050505050500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0505050505050505050505050505050500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0505050505050505050505050505050500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1515150615150615060615061515060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0404040404040404040404040404040400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0404040404040404040404040404040400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0404040404040404040404040404040400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0404040404040404040404040404040400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0404040404040404040404040404040400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0404040404040404040404040404040400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0505050505050505050505050505050500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000500000a16012160000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000600001e35024350313402a3301f100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00080000154601d4501245008450000000000000000235001d5000e30019400000000000029300000000000000000245001e50000000000001a40000000253000000000000000000000000000000000000000000
000600000f450194501a45015450114500e4500b45008450074500545002450014500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000600000f1700f1701017010170101600e1600b150081300114019700017000070007100091000d1000f1000b100081000b100091000b100091000b1000b100091000a1000a1000a1000b1000b1000b1000c100
00020000175501c550235502655028550295502a5502b5502b5502a55026550235501c55019550145500f5500855002550005400052002510181001a1001c1001f10021100241002610027100000000000000000
0002000020560215602256023560235602356022550205501c5401753013530115200f5100d5200a5300853007520085100e51014520195301d5401f550245502656028560295602956026550235501e5401a530
000200001a5601e570225702557026570275702757026560205501a550145601156011560145501c550205502455027550295502b5502a55028550225501e5501c5601c5601a55017550135400d5300952007520
000700001f050260602a0400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000300001b65024650116400564008620056200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0003000017650246500c6501664009620026200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0009000f2f6102c6102f610326102f610306103261030610316103161030610316103161030610306103661035600356003860000000000000000000000000000000000000000000000000000000000000000000
000100000c6501b63022610276102a6202c6102e61030610326103560033600336003260033600336003360034600346003460034600346003460034600346003360033600346003460036600366003760035600
010e0000070402f7142461526713020402b715246152671107040327152461526715020402b713246152f7150b0402a7152461532715060402f715246152a715040403771524615347150b0402f715246152b715
010e00001f5301f5301f5301f5301f5221f51221530215102353023510265302651028530285102b530285402a5412a5302a5302a5202a5222a51226540235402354023530235202352223525235021f5301f510
010e0000237152671523026260162371526715230262601623715267152302626016237152671523026260163171032011327103201032712320122f7102b0102b7102b7102b7122b712237151f715230261f016
010e0000070402b714246152f7130e04032715246152f7111304037515246153b7150704032715246152f71302040367142461539511090403e71524615267110e04039715246152a71509040327142461539713
010e00002853028510265302651023530235101f5301f5102853028510265302651023530235101f5301e5301e5301e5301e5301e5201e5201e5121e512000000000000000000000000000000215302151022535
010e0000237152671523026260162371526715230262601623715267152302626016237152671523026260161e715267151e026260161e715267151e026260161e715267151e026260161e715267151e02626016
010e000002040367142461539715090403e71324615327150e0403b514246153971509040367152461532715020402a01324615347140604032715246152a511070403971524615377150204032715246152f715
010e000023530235102153021510205302051023530215302153021520215122853028530285202a5302a51028535285352653026510255302551028530265302653026530265322652226525265002650020530
010e00001e715267152a026260161e715267152a026260161e715267152a026260161e715267152a026260161e715267151e026260161e715267151e026260162371526715230262601623715267152302626016
010e0000090403171424615347110d0402d71524615317140404028715246152d7150904025513246152871502040267152461532711090402671524615327130e04026715246150205004050000000605000000
010e00002153021510235302351025530255102653026510285302851027530275102853028510295302a5302a5202a5122853028510265302651023530215302153021522215122151200000000002461523530
010e000025715287152502628016257152871525026280162571528715250262801625715287152502628016267152a715260262a016267152a715260262a016247152a715240262a016247152a715240262a016
0003000022030280302c0202e0202d0202c0202b0202a0202802024030210301d0301803014030100300c040090400604003040010400000013000130001200012000110001100011000100000f0000e0000d000
__music__
01 0d0e0f44
00 10111244
00 13141544
02 16171844

