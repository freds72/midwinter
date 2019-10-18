pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
-- st8p
-- by @freds72

-- vector & tools
function lerp(a,b,t)
	return a*(1-t)+b*t
end

function smoothstep(t)
	t=mid(t,0,1)
	return t*t*(3-2*t)
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
function v_cross(a,b)
	local ax,ay,az=a[1],a[2],a[3]
	local bx,by,bz=b[1],b[2],b[3]
	return {ay*bz-az*by,az*bx-ax*bz,ax*by-ay*bx}
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
-- x/z orthogonal vector
function v2_ortho(a,scale)
	return {-scale*a[1],0,scale*a[3]}
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
function m_clone(m)
	local c={}
	for k,v in pairs(m) do
		c[k]=v
	end
	return c
end
function make_m_from_euler(x,y,z)
		local a,b = cos(x),-sin(x)
		local c,d = cos(y),-sin(y)
		local e,f = cos(z),-sin(z)
  
  -- yxz order
  local ce,cf,de,df=c*e,c*f,d*e,d*f
	 return {
	  ce+df*b,a*f,cf*b-de,0,
	  de*b-cf,a*e,df+ce*b,0,
	  a*d,-b,a*c,0,
	  0,0,0,1}
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
-- inline matrix vector multiply invert
-- inc. position
function m_inv_x_v(m,v)
	local x,y,z=v[1]-m[13],v[2]-m[14],v[3]-m[15]
	return {m[1]*x+m[2]*y+m[3]*z,m[5]*x+m[6]*y+m[7]*z,m[9]*x+m[10]*y+m[11]*z}
end

function m_set_pos(m,v)
	m[13],m[14],m[15]=v[1],v[2],v[3]
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
function corun(f)
	local cs=costatus(f)
	if cs=="suspended" then
		assert(coresume(f))
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

-->8
-- main engine
-- global vars
local ground,plyr,cam
local actors={}

local k_far,k_near=0,2
local k_right,k_left=4,8
local z_near=0.05

-- camera
function make_cam()
	-- views
	local switching_async
	-- 0: far
	-- 1: close
	-- 2: cockpit
	local view_mode=0
	-- view offset/angle/lag
	local view_pov={
		{-0,1.8,0.08},
		{-0.7,0.3,0.1},
		{-0.01,0.1,0.6}
	}
	local current_pov=v_clone(view_pov[view_mode+1])

	--
	local up={0,1,0}

	-- screen shake
	local shkx,shky=0,0
	camera()
	
	-- raycasting constants
	local angles={}
	local dists={}
	for i=0,15 do
	 local z=i-7.5
		add(angles,atan2(7.5,z))
	 add(dists,sqrt(7.5*7.5+z*z))
	end

	return {
		pos={0,0,0},
		angle=0,
		shake=function(self,u,v,pow)
			shkx=min(4,shkx+pow*u)
			shky=min(4,shky+pow*v)
		end,
		update=function(self)
			if switching_async then
				switching_async=corun(switching_async)
			elseif btnp(4) then
				local next_mode=(view_mode+1)%#view_pov
				local next_pov=v_clone(view_pov[next_mode+1])
				switching_async=cocreate(function()
					for i=0,29 do
						local t=smoothstep(i/30)
						current_pov=v_lerp(view_pov[view_mode+1],next_pov,t)
						yield()
					end
					-- avoid drift
					current_pov,view_mode=next_pov,next_mode
				end)
			end

			shkx*=-0.7-rnd(0.2)
			shky*=-0.7-rnd(0.2)
			if abs(shkx)<0.5 and abs(shky)<0.5 then
				shkx,shky=0,0
			end
			camera(shkx,shky)
		end,
		track=function(self,pos,a,u)
   			pos=v_clone(pos)
   			-- lerp angle
			self.angle=lerp(self.angle,a,current_pov[3])
			-- lerp orientation
			up=v_lerp(up,u,current_pov[3])
			v_normz(up)

			-- shift cam position			
			local m=make_m_from_v_angle(up,self.angle)
			v_add(pos,m_fwd(m),current_pov[1])
			v_add(pos,m_up(m),current_pov[2])
			
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
			return 63.5+flr(shl(v[1]/v[3],6)),63.5-flr(shl(v[2]/v[3],6))
		end,
		project_poly=function(self,p,c0)

			local p0,p1=p[1],p[2]
			-- magic constants = 89.4% vs. 90.9%
			-- shl = 79.7% vs. 80.6%
			local x0,y0=p0.x or 63.5+flr(shl(p0[1]/p0[3],6)),p0.y or 63.5-flr(shl(p0[2]/p0[3],6))
			local x1,y1=p1.x or 63.5+flr(shl(p1[1]/p1[3],6)),p1.y or 63.5-flr(shl(p1[2]/p1[3],6))
			for i=3,#p do
				local p2=p[i]
				local x2,y2=p2.x or 63.5+flr(shl(p2[1]/p2[3],6)),p2.y or 63.5-flr(shl(p2[2]/p2[3],6))
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
		end
	}
end

-- "physic body" for simple car
function make_car(p,angle)
	-- last contact face
	local up,oldf={0,1,0}

	local velocity,angularv={0,0,0},0
	local forces,torque={0,0,0},0

	local steering_angle=0

	return {
		pos=v_clone(p),
		m=make_m_from_euler(0,a,0),
		get_pos=function(self)
	 		return self.pos,angle
		end,
		get_orient=function()
			return make_m_from_v_angle(oldf and oldf.n or v_up,angle)
		end,
		get_up=function()
			return up
		end,
		-- contact face
		get_ground=function()
			return oldf
		end,
		apply_force_and_torque=function(self,f,t)
			-- add(debug_vectors,{f=f,p=p,c=11,scale=t})

			v_add(forces,f)
			torque+=t
		end,
		prepare=function(self)
			-- update velocities
			v_add(velocity,forces,0.5/30)
			angularv+=torque*0.5/30

			-- apply some damping
			angularv*=0.86
			v_scale(velocity,0.97)
			-- some friction
			-- v_add(velocity,velocity,-0.02*v_dot(velocity,velocity))
		end,
		integrate=function(self)
		 	-- update pos & orientation
			v_add(self.pos,velocity)
			-- fix
			angularv=mid(angularv,-1,1)
			angle+=angularv			
			self.m=make_m_from_euler(0,angle,0)

			-- reset
			forces,torque={0,0,0},0
		end,
		steer=function(self,steering_dt,rpm)
			steering_angle+=mid(steering_dt,-0.15,0.15)

			local fwd=m_fwd(self.m)

			v_scale(fwd,rpm*10)			

			self:apply_force_and_torque(fwd,-steering_angle)

			return min(rpm,max_rpm)
		end,
		update=function(self)
			steering_angle*=0.5

			-- find ground
			local pos=self.pos			
			local newf,newpos=find_face(pos,oldf)
			if newf then
				oldf=newf
			end
			-- above 0
			if newpos and pos[2]<=newpos[2] then
				up=newf.n			
				pos[2]=newpos[2]
			end
			pos[2]=max(pos[2]-0.2,24)
		end
	}	
end

function make_plyr(p,angle)
	local rpm=0
	local body=make_car(p,angle)
	
	-- backup parent methods
	local body_update=body.update
	
	body.control=function(self)	
		local da=0
		if(btn(0)) da=1
		if(btn(1)) da=-1

		-- accelerate
		if btn(2) then
			rpm=rpm+0.1
		end

		rpm=self:steer(da/8,rpm)
	end
	
	body.update=function(self)
		body_update(self)
		rpm*=0.97

	end
	-- wrapper
	return body
end

function find_face(p,oldf)	
	-- not found
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
	function draw_box(s,x,y,c,mode)
		rectfill(x-2,y,x+2,127,5)
		rectfill(x+1,y,x+1,127,6)

		palt(0,false)
		palt(14,true)
		pal(12,c)
		if mode==2 then
			if (30*time())%8<4 then
				pal(12,cols[c])
				pal(c,cols[c])
				pal(6,cols[6])
			end
		end
		local n=#s
		local h,w=6,(n*3+16)/2
		rectfill(x-w,y-h,x+w,y+h,0)
		w-=1
		h-=1
		rectfill(x-w,y-h,x+w,y+h,7)
		w-=1
		h-=1
		rectfill(x-w,y-h,x+w,y+h,c)
		spr(2,x+w+1,y-h-2,1,2)
		print(s,x-#s*1.5-5,y-2,6)
		if mode==1 then
			local k=0
			for j=0,2*h do
				for i=0,3 do
					local x0=(90*time()+i+j)%(2*w+7)
					if x0<2*w+k then
						x0+=x-w
						pset(x0,y-h+j,cols[pget(x0,y-h+j)])
				end
			end
			k+=(j>h and -1 or 1)
			end
		end
		pal()

		-- mask
		rectfill(0,0,127,20,0)
		rectfill(0,92,127,127,0)
		palt(14,true)
		palt(0,false)
		spr(112,64,20,8,9)
		spr(112,0,20,8,9,true)
		palt()
	end
   
	local panels={
		"chamois",0,
		"marmottes",1,
		"biquettes",8
	}
	local sel,sel_tgt,sel_max=0,0,#panels/2
	local blink=false
	local ttl=20

	-- reset cam	
	cam=make_cam()

	return {
		-- draw
		draw=function()
			cls(12)

			local out={}
			local sprites={}
			-- get visible voxels
			ground:collect_drawables(cam.pos,cam.angle,out,dist)

			sort(out)
			draw_drawables(out)

			local y=20
			local a,da=0,1/3
			for i=0,#panels-1,2 do
				local mode=0
				if sel==sel_tgt then
					mode=1
					-- blinking?
					if(blink) mode=2
				end
				local v={5*cos(a),0.8,-5*sin(a)}
				v_add(v,cam.pos)
				v=m_x_v(cam.m,v)
				if v[3]>0 then
					local x0,y0=cam:project2d(v)
					draw_box(panels[i+1],x0+32,y0,panels[i+2],mode)
				end
				a+=da
			end
		end,
		-- update
		update=function()
			if(start_game_async) start_game_async=corun(start_game_async)

			if(btnp(0)) sel-=1
			if(btnp(1)) sel+=1
			sel=mid(sel,0,sel_max-1)

			local k=sel
  			sel_tgt=lerp(sel_tgt,k,0.18)
   			-- snap when close to target
			if abs(sel_tgt-k)<0.01 then
				sel_tgt=k
			end

			if btnp(4) or btnp(5) then
				-- snap track
				sel_tgt=k
				-- sub-state
				start_game_async=cocreate(function()
					for i=1,15 do
						blink=i%2==0 and true
						yield()
					end
					pop_state()
					push_state(zoomin_state,play_state,sel)
				end)
			end

			--
			cam:track({0,1,0},sel/3,v_up)
		end
	}
end

function zoomin_state(next,params)
	local ttl,dttl=20,0.01

	-- 
	local ramps={}
	for i=0,15 do
		local c0,r=i,{}
		ramps[i]=r
		for j=0,4 do
			-- lookup color for c0
			local c=sget(25,c0)
			r[j]=sget(25,c)
			-- move to next color
			c0=c
		end
	end

	-- copy backbuffer
	memcpy(0x0,0x6000,128*64)

	return {
		-- draw
		draw=function()
			-- fade to white
			local c=min(4,flr(5*((20-ttl)/20)))
			for i=0,15 do
				pal(i,ramps[i][c])
			end

			-- zoom effect
			local s=(3*(20-ttl)/20+1)
			palt(0,false)
			local dx=-abs(64*s-64)		
			sspr(0,0,128,128,dx,dx,128*s,128*s)
			pal()
		end,
		update=function(self)
			ttl-=dttl
			dttl+=0.08
			if ttl<0 then
				pop_state()
				-- restore spritesheet
				reload()
				pal()
				push_state(next,params)
			end
		end
	}
end

function play_state()

	-- start over
	actors={}
	-- actors
	for i=1,128 do
		local pos={rnd(256)-128,0,rnd(256)-128}
		local f,p=find_face(pos)
		if f then
			pos[2]=p[2]
			add(actors,{pos=pos,sx=104,sy=16})
		end
	end

	-- create player in correct direction
	plyr=make_plyr({0,0,0},0)

	-- reset cam	
	cam=make_cam()

	return {
		-- draw
		draw=function()
			cls(12)

			local out={}
			local sprites={}
			-- get visible voxels
			ground:collect_drawables(cam.pos,cam.angle,out,dist)
			
			-- sprites
			local m=cam.m
			for _,actor in pairs(actors) do
				local x,y,z=actor.pos[1],actor.pos[2],actor.pos[3]
				local az=m[3]*x+m[7]*y+m[11]*z+m[15]
				if az>z_near and az<32 then	
					local ax,ay=m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14]
					add(out,{key=1/(ay*ay+az*az),a=actor,x=63.5+flr(shl(ax/az,6)),y=63.5-flr(shl(ay/az,6)),w=63.5/az})
				end
			end
			-- sprite cache
			local angle=atan2(cam.m[5],cam.m[6])+0.25
			rspr(104,16,0,64,angle,2)
		
			-- todo: sort by voxels
			sort(out)
			draw_drawables(out)
			 
			 local cpu=flr(10000*stat(1))/100
			 print(cpu.."%",2,2,2)
			print(#actors,2,8,2)
		
			spr(9,24+6*cos(time()/4),128-28+4*sin(time()/5),4,4)
			spr(9,84-5*cos(time()/5),128-28+4*sin(time()/4),4,4,true)
		
			ground:debug_draw()
		end,
		-- update
		update=function()
			cam:update()
			plyr:control()	

			plyr:prepare()
			plyr:integrate()	
			plyr:update()
		
			local pos,a=plyr:get_pos()
			cam:track(pos,a,plyr:get_up())
		end
	}
end

function _init()
	--
	ground=make_ground()

	-- init state machine
	push_state(menu_state)
end

function _update()
	-- state mgt
	for state in all(states) do
		state:update()
	end
end


local dither_pat={0xffff,0x7fff,0x7fdf,0x5fdf,0x5f5f,0x5b5f,0x5b5e,0x5a5e,0x5a5a,0x1a5a,0x1a4a,0x0a4a,0x0a0a,0x020a,0x0208,0x0000}
function draw_drawables(objects)	
	for i=1,#objects do
		local d=objects[i]
		if d.a then
			-- sprite
			local w=4*d.w
			sspr(0,64,16,16,d.x-w/2,d.y-w,w,w)
		else
			-- triangle
			local c0=0x76
			if d.f.n[2]<0.6 then
				c0=0x54
				if(d.dist>4) c0=0x4d
				if(d.dist>5) c0=0xd5
				local c=5*d.f.n[2]
				local cf=(#dither_pat-1)*(1-c%1)
				fillp(dither_pat[flr(cf)+1])
				
			else
				if(d.dist>4) c0=0x6d
				if(d.dist>5) c0=0xd5
				local c=5*d.f.n[2]
				local cf=(#dither_pat-1)*(1-c%1)
				fillp(dither_pat[flr(cf)+1])
			end
			cam:project_poly(d.v,c0)
			fillp()
		end
	end
end

function _draw()
	for state in all(states) do
		state:draw()
	end
end

-->8
-- generate map

function make_ground(model)

	-- number of x/z slices
	local nx,nz=32,32
	-- cell size
	local dx,dz=32,32

	-- ground slices (from 0 to nz-1)
	local slices={}

	-- uses m (matrix) and v (vertices) from self
	-- saves the 'if not ...' in inner loop
	local v_ground_cache_cls={
		__index=function(t,k)
			-- inline: local a=m_x_v(t.m,t.v[k]) 
			local m=t.m
			-- slice index
			local i,j=k%nx,flr(k/nx)
			local s0=slices[j]
			-- generate vertex
			local x,y,z=i*dx,s0.h[i]+s0.y,j*dz
			local ax,az=m[1]*x+m[5]*y+m[9]*z+m[13],m[3]*x+m[7]*y+m[11]*z+m[15]
		
			local outcode=az>z_near and k_far or k_near
			if ax>az then outcode+=k_right
			elseif -ax>az then outcode+=k_left
			end	

			local ay=m[2]*x+m[6]*y+m[10]*z+m[14]
			t[k]={ax,ay,az,outcode=outcode,x=63.5+flr(shl(ax/az,6)),y=63.5-flr(shl(ay/az,6))}
			return t[k]
		end
	}

	local function make_slice(y)
		-- height array
		local h={}
		for i=0,nx-1 do
	 		h[i]=rnd(32)
		end
		-- smoothing
		for k=1,2 do
 			for i=0,nx-1 do
				h[i]=(h[i]+h[(i+1)%nx])/2
			end
		end
		return {
			y=y,
			h=h
		}
	end

	local function mesh(j)
		local s0,s1=slices[j],slices[j+1]
		local f={}
		local fi=0
		for i=0,nx-2 do
			local v0={i*dx,s0.h[i]+s0.y,j*dz}
			local v1={(i+1)*dx,s0.h[i+1]+s0.y,j*dz}
			local v2={(i+1)*dx,s1.h[i+1]+s1.y,(j+1)*dz}
			local v3={i*dx,s1.h[i]+s1.y,(j+1)*dz}

			-- normal 
			local n0=v_cross(make_v(v0,v3),make_v(v0,v2))
			v_normz(n0)
			local n1=v_cross(make_v(v0,v2),make_v(v0,v1))
			v_normz(n1)
			local v=n1
			if v_dot(n0,n1)>0.995 then
				-- quad
				f[fi]={n=n0,cp=v_dot(n0,v0)}
			else
				-- 2 tri
				f[fi]={n=n0,cp=v_dot(n0,v0)}
				f[fi+1]={n=n1,cp=v_dot(n1,v0)}
			end
			fi+=2
		end
		-- attach faces to slice
		s0.f=f
	end

	for j=0,nz-1 do
		slices[j]=make_slice(0)
	end
	-- create faces
	for j=0,nz-2 do
		mesh(j)
	end

	local function collect_face(face,vertices,v_cache,cam_pos,out,dist)		
		if v_dot(face.n,cam_pos)>face.cp then
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
				if #verts>2 then
					out[#out+1]={key=1/(y*y+z*z),f=face,v=verts,dist=dist}
				end
			end
		end
	end

	-- raycasting constants
	local angles={}
	local dists={}
	for i=0,15 do
		local z=i-7.5
		add(angles,atan2(7.5,z))
		add(dists,sqrt(7.5*7.5+z*z))
	end

	local function visible_tiles(pos,angle)
		local x,y=pos[1]/dx,pos[3]/dz
		local x0,y0=flr(x),flr(y)
		local tiles={[x0+y0*nx]=0}

		for i,a in pairs(angles) do
			a+=angle
			local v,u=cos(a),-sin(a)
			
			local mapx,mapy=x0,y0
		
			local ddx,ddy=1/u,1/v
			local mapdx,distx
			if u<0 then
				mapdx,ddx=-1,-ddx
				distx=(x-mapx)*ddx
			else
				mapdx=1
				distx=(mapx+1-x)*ddx
			end
			local mapdy,disty
			if v<0 then
				mapdy,ddy=-1,-ddy
				disty=(y-mapy)*ddy
			else
				mapdy=1
				disty=(mapy+1-y)*ddy
			end	
			for dist=0,dists[i] do
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

	return {
		to_tile_coords=function(self,v)
			return flr(v[1]/dx),flr(v[3]/dz)
		end,
		collect_drawables=function(self,cam_pos,cam_angle,out)
			local tiles=visible_tiles(cam_pos,cam_angle)
			-- vertex cache
			local v_cache={m=cam.m}
			setmetatable(v_cache,v_ground_cache_cls)

			for k,dist in pairs(tiles) do
				-- get slice(s)
				local i,j=k%nx,flr(k/nx)
				local s0=slices[j]
				-- face(s)
				local f0,f1=s0.f[2*i],s0.f[2*i+1]
				if f1 then
					-- 2 triangles
					if(f0) collect_face(f0,{k,k+1+nx,k+nx},v_cache,cam_pos,out,dist)
					if(f1) collect_face(f1,{k,k+1,k+1+nx},v_cache,cam_pos,out,dist)
				else
					-- 1 quad
					if(f0) collect_face(f0,{k,k+1,k+1+nx,k+nx},v_cache,cam_pos,out,dist)
				end
			end

			return tiles
		end,
		debug_draw=function(self)
			local y=8
			for j=0,nz-1 do
				local s0=slices[j]
				if s0.f then
					print(#s0.f,2,y,7)
					y+=6					
					--[[
					for i,f in pairs(s0.f) do
						pset(64+i,nz-j,i%2==0 and 1 or 8)
					end
					]]
				end
			end
		end
	}
end

-->8 
-- rotation
function rspr(sx,sy,x,y,a,w)
	local ca,sa=cos(a),sin(a)
	local ddx0,ddy0=ca,sa
	local mask=shl(0xfff8,(w-1))
	w=shl(w,2)
	local dx0,dy0=(sa-ca)*(w-0.5)+w,(ca+sa)*(0.5-w)+w
	w=shl(w,1)-1
	for ix=x,x+w do
		local srcx,srcy=dx0,dy0
		for iy=y,w+y do
			if band(bor(srcx,srcy),mask)==0 then
				sset(ix,iy,sget(sx+srcx,sy+srcy))
			end
			srcx-=ddy0
			srcy+=ddx0
		end
	dx0+=ddx0
	dy0+=ddy0
	end
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
		-- flag new point
		r[4]=true
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

__gfx__
000000000000000000eeeeee01000000eeeeeeee0000000111222332100000000000000000000000006666660000000000000000000000000000000000000000
0000000000000000770eeeee15000000eeeeeeee0000000000012343210000000000000000000000555555557000000000000000000000000000000000000000
0000000000000000cc70eeee28000000eeeeeeee0000000000124555321000000000000000000002222222222700000000000000000000000000000000000000
0000000001222100ccc70eee3b000000eeeeeeee0000000001347787532110000000000000000022222222222220000000000000009290000000000500000000
0000000013444310cccc70ee49000000eeeeeeee1000111113579876543210000000000000000022222222222220000000000000002220000000005bb0000000
0000000024676421ccccc70e5d000000eeeeeeee312223223578987666432100000000000000022222222222222200000000000000929000000003bb3b000000
0000000135787642ccccc70e67000000eeeeeeee43445555677876656764200000000000000002222222222222220000000000000000000000005b33bb300000
0000001245777754ccccc70e77000000eeeeeeee5556778777665555787420000000000000000222222222222822000000000000000000000005b5bb33b30000
0000012445677654cccc70ee8e000000eeeeeeee5568aa987765445687631000000000000000022222282828222220000000000000000000000b5b53bb3b0000
0000133455556655ccc70eee9a000000eeeeeeee5568abaa8654445676420000000000000000022828228222282820000000000000a8a0000005b5b533bb0000
0011345665445555cc70eeeea7000000eeeeeeee5468aa987544456654210000000000000000022222222282828280000000000000a8a00000005b3bbbb00000
0012457765444444770eeeeeb6000000eeeeeeee44468a986433444554210000000000000000022828282828288888000000000000929000000005b533000000
001246787643333300eeeeeec6000000eeeeeeee4334678875434455553200000000000000000022828288888888880000000000002220000000005bb0000000
0012357876432112eeeeeeeed6000000eeeeeeee4323467999754467775310000000000000000028888888888888880000000000009290000000000000000000
0001245666532101eeeeeeeeef000000eeeeeeee2001359cefca6467896410000000000000000028888888888888880000000000000000000000000000000000
0000123466542100eeeeeeeef7000000eeeeeeee0000148efffea667876420000000000000000028888888998888880000000000000000000000000000000000
00000124666532100000000000000000000000000000037effffc7577764200000000000000000288888899a98888880000000000000000bb000000000000000
00000124787532210000000100012334200000000000015dfffff856776421000000000000000028888889aaa98888800000000000000053bb00000000000000
000001368986422210000011124799877630000000000018fffffc5577542100000000000000002888889a888a98888000000000000000b53d00000000000000
00000147bb96433321001233357beffda973000000000004efffff757654210000000000000000028888988888a98880000000000000035bbbb0000000000000
00000248ab96433321112465558cffffda97555441000002cfffff945543211000000000000000028889a888888a8888000000000000053bbbb0000000000000
00000258a985333321113798667bfffebabbaabdc8300002bfffff93333221100000000000000002888a8888888888880000000000000b533bb0000000000000
0000136aa8632232100159ba8669dedb99adefffeca40001afffffb3222222110000000000000002888888889888888800000000000035b5b33d000000000000
0000258aa7421222100269cd9768aba98abefffffec830016fffffb53222222100000000000000028888888999888888000000000000535bbbbb000000000000
000026adb8422221000159dda7679a988abefffffed940004bffffd753322110000000000000000288888889aa988888000000000000b533bbbb000000000000
000037dfe9522321000148bb97689a9989adfeedffd9400028ffffe96432211000000000000000002888889a88a988888000000000035b5b333bb00000000000
011238dfeb643331000025888668aa98679cddbbefda500015cfffb8643211000000000000000000288888988888888880000000000535b5bbb3d00000000000
122248dffc8654321000268876679a86447acbaacfeb6100038cdb964432100100000000000000002888888888888888800000000005535bbbbbb00000000000
234358cffca765543221369ba87677632369bbbcdfec720002799774322110010000000000000000288888888888888880000000000000011000000000000000
345457addca8776664446adffc9653322347acddffed830001687543221001120000000000000000288888888888888888000000000000044000000000000000
3455679bba88788896669effffb632221237bdedfffea40002687444321011120000000000000000028888888888888888000000000000444400000000000000
355677999877789887779dddeee953221118acdffffeb50001566445321111220000000000000000028888888888888888000000000000000000000000000000
355677988767778876567bbbccdb743221069abcffffc60000365454332222220000000000000000000000000000000000000000000000000000000000000000
244678987766788875334689abcc975321158aaadfffd60000365544433321120000000000000000000000000000000000000000000000000000000000000000
2345678887667898742112479abccc842135799abeffd50001355554433321110000000000000000000000000000000000000000000000000000000000000000
2344456787789aaa852001479bccdeda644579999ceeb30002466554433210000000000000000000000000000000000000000000000000000000000000000000
344444677889abcb96310269acddeeef96547aaa9aba710012576555443210010000000000000000000000000000000000000000000000000000000000000000
456543466889abbca853347acdeeefffc96568999998500123566655543210020000000000000000000000000000000000000000000000000000000000000000
577532345678babcba76568abdeffffffd9767877898300023565545554310020000000000000000000000000000000000000000000000000000000000000000
6886433434579abcbb976677789befffffd976767897200012454434555321130000000000000000000000000000000000000000000000000000000000000000
6887644433579bdccca8765554568adffffb97777898300011344323455431130000000000000000000000000000000000000000000000000000000000000000
578766654347adedcca875432222359efffba9988787400001443222233332130000000000000000000000000000000000000000000000000000000000000000
467777764346bdeecb98765310001269bcb999aa8654200001344321122332230000000000000000000000000000000000000000000000000000000000000000
3455666543369cddba97776410000124666679a97421000002344322122222220000000000000000000000000000000000000000000000000000000000000000
22334444322479bb9888877531000001223358984200001122344432222233210000000000000000000000000000000000000000000000000000000000000000
11122333211246888878888642100000012345652000002333333333222333210000000000000000000000000000000000000000000000000000000000000000
00011122221122456667778753210000112233210000123444322232222222100000000000000000000000000000000000000000000000000000000000000000
00001122321101134457666654322221110111000012234443221122111111000000000000000000000000000000000000000000000000000000000000000000
00000122332100012355654444444443211000000023566643200111100000000000000000000000000000000000000000000000000000000000000000000000
00000012333200001244564433566665421111000124787753110111000000000000000000000000000000000000000000000000000000000000000000000000
0000001233321000112457655689a975432223320125789863211111000000000000000000000000000000000000000000000000000000000000000000000000
00000001344322111113566667abcb86433345442224689975311211000000000000000000000000000000000000000000000000000000000000000000000000
00000001234444332112457778abba87545555443213479985322232100000000000000000000000000000000000000000000000000000000000000000000000
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
eeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000
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
000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
0000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
0000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000
00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000
00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000
000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000
000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000
0000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000
00000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000eeeeeeeeeeeeeeeeeeeeeeee00000000000e0000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000eeeeeeeeeee0000000000000000eeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000eeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000eeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000eeeeeeeee000e000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000eeeeeee00eeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000eeeeeee00eeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
