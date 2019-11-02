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
local fade_ramps={}
function whiteout_async(delay,t0,t1)
	for i=1,delay do
		yield()
	end
	for i=t0,t1,t1>t0 and 1 or -1 do
		-- fade to white
		local c=min(5,flr(5*((t1-i)/(t1-t0))))
		for i=0,15 do
			pal(i,fade_ramps[i][c],1)
		end
		yield()
	end
	-- 
	pal()
end

-->8
-- main engine
-- global vars
local ground,plyr,cam
local actors={}

local k_far,k_near=0,2
local k_right,k_left=4,8
local z_near=0.2

-- camera
function make_cam()
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

	local update_bkg_sprite=make_rspr(32,16,32,0)

	return {
		pos={0,0,0},
		angle=0,
		shake=function(self,u,v,pow)
			shkx=min(4,shkx+pow*u)
			shky=min(4,shky+pow*v)
		end,
		update=function(self)
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
			self.angle=lerp(self.angle,a,0.08)
			-- lerp orientation
			up=v_lerp(up,u,0.08)
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
			return 63.5+flr(63.5*v[1]/v[3]),63.5-flr(63.5*v[2]/v[3])
		end,
		project_poly=function(self,p,c0)
			local p0,p1=p[1],p[2]
			local x0,y0=p0.x or 63.5+flr(63.5*p0[1]/p0[3]),p0.y or 63.5-flr(63.5*p0[2]/p0[3])
			local x1,y1=p1.x or 63.5+flr(63.5*p1[1]/p1[3]),p1.y or 63.5-flr(63.5*p1[2]/p1[3])
			for i=3,#p do
				local p2=p[i]
				local x2,y2=p2.x or 63.5+flr(63.5*p2[1]/p2[3]),p2.y or 63.5-flr(63.5*p2[2]/p2[3])
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
			cls(1)
			-- draw horizon
			local zfar=-128
			local farplane={
					{-zfar,zfar,zfar},
					{-zfar,-zfar,zfar},
					{zfar,-zfar,zfar},
					{zfar,zfar,zfar}}
			-- cam up in world space
			local n=m_up(self.m)
			-- orientation
			local p=m_x_v(self.m,self.pos)
			n[4]=v_dot(p,n)
			local verts=plane_poly_clip(n,farplane)
			if #verts>2 then
				self:project_poly(verts,7)
			end

			-- mountains
			-- intersection between camera up plane (world space)
			local h={0,-n[3]/n[2],1}
			local x0,y0=self:project2d(h)

			-- horizon 'normal'
			local u,v=self.m[5],self.m[6]
			local d=sqrt(u*u+v*v)
			u/=d
			v/=d
			-- get rotated sprite
			local angle=atan2(u,v)+0.25
			update_bkg_sprite(-angle)
			
			u,v=16*v,16*u
			angle=(angle%1+1)%1

			-- round sky
			circfill(x0+16*v,y0-16*u,250,6)
			circfill(x0+16*v,y0-16*u,245,12)

			x0-=v/1.2
			y0-=u/1.2
			for i=-7,7 do
				spr(36,x0+i*u,y0+i*v,2,2)
			end
		end
	}
end

-- "physic body" for simple car
function make_car(p)
	-- last contact face
	local up,on_ground,oldf={0,1,0},false

	local velocity,angularv={0,0,0},0
	local forces,torque={0,0,0},0

	local angle,steering_angle=0,0

	return {
		pos=v_clone(p),
		get_pos=function(self)
	 		return self.pos,angle,steering_angle/0.625
		end,
		get_orient=function()
			-- todo: lean left/right according to steering
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
			-- gravity and ground
			local g={0,-2,0}
			self:apply_force_and_torque(g,0)
			-- on ground?
			if on_ground==true then
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
			-- v_scale(velocity,0.97)
			-- some friction
			v_add(velocity,velocity,-0.05*v_dot(velocity,velocity))
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
			-- on ground?
			if on_ground==true and v_len(velocity)>0.001 then
				steering_angle+=mid(steering_dt,-0.15,0.15)

				-- desired ski direction
				local m=make_m_from_v_angle(up,angle-steering_angle/16)
				local right=m_right(m)
				
				-- slip angle
				local sa=-v_dot(velocity,right)
				if abs(sa)>0.001 then
					-- max grip
					-- todo: review
					sa=mid(sa,-0.1,0.1)
				
					-- ski length for torque
					local ski_len=0.8
					--[[
					local fwd=m_fwd(m)
					v_scale(fwd,ski_len)
					local torque=v_cross(fwd,right)
					local l=torque[1]+torque[2]+torque[3]
					]]

					v_scale(right,30*sa)

					self:apply_force_and_torque(right,-steering_angle*ski_len/2)
				end
			end
			self.friction=on_ground==true and "ground" or "air"
		end,
		update=function(self)
			steering_angle*=0.8

			-- find ground
			local pos=self.pos

			local newf,newpos=ground:find_face(pos)
			if newf then
				oldf=newf
			end
			-- stop at ground
			on_ground=false
			if newpos and pos[2]<=newpos[2] then
				up=newf.n			
				pos[2]=newpos[2]
				on_ground=true
			end
		end
	}	
end

function make_plyr(p)
	local body=make_car(p)

	local body_update=body.update

	local jump_ttl=0 
	body.control=function(self)	
		local da=0
		if(btn(0)) da=1
		if(btn(1)) da=-1

		self:steer(da/8)
	end
	
	body.update=function(self)
		jump_ttl-=1
		-- collision detection
		if not self.dead and ground:collide(self.pos,0.2) then
			self.dead=true
		end

		-- call parent
		body_update(self)
	end

	-- wrapper
	return body
end

function make_snowball(pos)
	local body=make_car(pos)
	
	local body_update=body.update

	local smoke_ttl=0
	body.sx=112
	body.sy=0
	body.update=function(self)		
		smoke_ttl-=1
		if smoke_ttl<0 then
			local pos={rnd(2)-1,rnd(2)-1,rnd(2)-1}
			v_add(pos,self.pos)
			-- add(actors,make_smoke(pos))
			smoke_ttl=10
		end
		-- physic update
		self:prepare()
		self:integrate()
		body_update(self)

		return true
	end
	return body
end

function make_smoke(pos)
	local ttl=20+rnd(10)
	return {
		sx=0,sy=16,
		pos=v_clone(pos),
		update=function(self)
			ttl-=1
			if(ttl<0) return
			self.pos[2]+=0.2
			return true
		end
	}
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
		rectfill(x-2,y-12,x+2,127,5)
		rectfill(x+1,y-12,x+1,127,6)

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

	ground=make_ground(0)

	-- reset cam	
	cam=make_cam()

	-- reset rotated sprites
	reload()

	return {
		-- draw
		draw=function()
			cam:draw_horizon(12,1)

			local out={}
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
				local v={8*cos(a),0.8,-8*sin(a)}
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
			cam:track({8*8,34,8*8},sel/3,v_up)
		end
	}
end

function zoomin_state(next,params)
	local ttl,dttl=30,0.01

	local fade=cocreate(whiteout_async)

	-- copy backbuffer
	memcpy(0x0,0x6000,128*64)

	return {
		-- draw
		draw=function()
			pal()
			-- zoom effect
			local s=(3*(30-ttl)/30+1)
			palt(0,false)
			local dx=-abs(64*s-64)		
			sspr(0,0,128,128,dx,dx,128*s,128*s)
			if(fade) fade=corun(fade,15,0,15)
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

	local fade_async--=cocreate(whiteout_async)

	-- start over
	actors={}

	--
	ground=make_ground(1.8)

	-- create player in correct direction
	plyr=make_plyr({32,0,32},0)

	-- reset cam	
	cam=make_cam()

	-- sprites
	local rot_sprites={
		make_rspr(112,16,32,0),
		make_rspr(48,16,32,0),
		make_rspr(48,0,32,0)}

	return {
		-- draw
		draw=function()
			
			cam:draw_horizon(12,1)
	  
			local out={}
			-- get visible voxels
			ground:collect_drawables(cam.pos,cam.angle,out,dist)
			-- misc. actors
			for _,a in pairs(actors) do
				local p=m_x_v(cam.m,a.pos)
				local ax,ay,az=p[1],p[2],p[3]
				if az>z_near and az<64 then	
					out[#out+1]={key=1/(ay*ay+az*az),a=a,x=63.5+flr(shl(ax/az,6)),y=63.5-flr(shl(ay/az,6)),w=shl(4/az,6),dist=1}
				end
			end

			-- sprite cache
			local angle=atan2(cam.m[5],cam.m[6])+0.25
			for _,f in pairs(rot_sprites) do
				f(-angle)
			end

			sort(out)
			draw_drawables(out)
			 
			local cpu=flr(10000*stat(1))/100
			print(cpu.."%\n"..stat(0),2,2,2)

			if plyr then
				local pos,a,steering=plyr:get_pos()

				spr(9,34+3*cos(time()/4),128-14-14*steering+4*sin(time()/5),4,4)
				spr(9,74-2*cos(time()/5),128-14+14*steering+4*sin(time()/4),4,4,true)

				print(plyr.friction,2,16,2)
			end

			if(fade_async) fade_async=corun(fade_async,0,15,0)
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
					cam:shake(rnd(8),rnd(8),1)

					-- snowballing!!!
					local snowball=make_snowball(plyr.pos)
					add(actors,snowball)

					push_state(plyr_death_state,snowball,v_clone(plyr:get_pos()),v_clone(plyr:get_up()))	
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

function plyr_death_state(snowball,pos)
	local snowball_sprite=make_rspr(112,0,32,0)
	local turn_side=rnd()>0.5 and -1 or 1
	local text_id,text=0,{
		"ouch!","aie!","pok!","weee!"
	}
	local text_ttl=10
	return {
		draw=function()
			if text_ttl>0 then
				print(text[text_id+1],60,112,8)
			end
		end,
		update=function()
			text_ttl-=1
			snowball_sprite(turn_side*time()*2)
			
			-- adjust ground
			local p=snowball.pos
			ground:update(p)

			if text_ttl<0 and ground:collide(p,0.2) then
				text_id=flr(rnd(#text))
				cam:shake(rnd(8),rnd(8),1)
				text_ttl=10
				turn_side*=-1
			end
			cam:track({p[1],p[2],p[3]+16},0.5,v_up)
		end
	}
end

function _init()
	-- todo: remove (only for benchmarks)
	srand(12)

	-- white out ramp
	for i=0,15 do
		local c0,r=i,{[0]=i}
		fade_ramps[i]=r
		for j=1,5 do
			-- lookup color for c0
			local c=sget(25,c0)
			r[j]=sget(25,c)
			-- move to next color
			c0=c
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
	sspr(actor.sx,actor.sy,16,16,x-w/2,y-w,w,w)
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
			if false then --d.f.n[2]<0.6 then
				c0=0x54
				if(d.dist>7) c0=0x4d
				if(d.dist>8) c0=0xd5
				local c=5*d.f.n[2]
				local cf=(#dither_pat-1)*(1-c%1)
				fillp(dither_pat[flr(cf)+1])
			else
				if(d.dist>7) c0=0x6d
				if(d.dist>8) c0=0xd5
				local c=5*d.f.n[2]
				local cf=(#dither_pat-1)*(1-c%1)
				fillp(dither_pat[flr(cf)+1])
			end
			cam:project_poly(d.v,c0)
			fillp()
			-- face 'details' sprites
			if d.fa then
				local m,v0,u=cam.m,d.v0,d.fa.u
				local x,y,z=v0[1]+u[1],v0[2]+u[2],v0[3]+u[3]
				local az=m[3]*x+m[7]*y+m[11]*z+m[15]
				if az>z_near and az<64 then	
					local ax,ay=m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14]
					-- sprite
					local x,y,w=63.5+flr(shl(ax/az,6)),63.5-flr(shl(ay/az,6)),shl(4/az,6)
					draw_sprite(d.fa,x,y,w,d.dist)
				end
			end
		end
	end
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
			ttl=12+rnd(20)
			trick_ttl=4+rnd(4)
			trick_type=flr(rnd(2))		
		end

		reset_seed_timers()

	 	add(seeds,{
			age=0,
			h=0,
	 		x=x or xmin+rnd(xmax-xmin),
	 		u=u or cos(0.05+rnd(0.45)),
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
 	add_seed()

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
				if s1.age>0 and flr(s0.x-s1.x)==0 then
					s1.dead=true
				end
			end
		end
	
		-- active seeds
		return seeds
	end
end

function make_ground(delta_slope)
	-- number of x/z slices
	local nx,nz=32,32
	-- cell size
	local dx,dz=4,4

	local dy=0
	-- ground slices (from 0 to nz-1)
	local slices={}

	-- track generator
	local next_tracks=make_tracks(8,nx-8,3)

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
			local x,y,z=i*dx,s0.h[i]+s0.y-dy,j*dz
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

	local slice_id=0
	local tree_prop,border_pole,warning_pole={sx=112,sy=16},{sx=48,sy=0},{sx=48,sy=16}

	local function make_slice(y)
		slice_id+=1
		-- generate tracks 
		local tracks=next_tracks()

		-- height array + props array
		local h,actors={},{}
		for i=0,nx-1 do
			h[i]=rnd(2*delta_slope)
			-- tree
			actors[i]=rnd()>0.6 and tree_prop
		end

		-- smoothing
		for k=1,2 do
 			for i=0,nx-1 do
				h[i]=(h[i]+h[(i+1)%nx])/2
			end
		end

		-- flatten track
		-- todo: pick up a free slot (eg inside a track)

		for _,t in pairs(tracks) do
			local i0,i1=flr(t.x-2),flr(t.x+2)
			for i=i0,i1 do				
				h[i]=t.h+h[i]/4
				-- remove props from track
				actors[i]=nil
			end
			-- track borders
			if slice_id%2==0 then
				local pole=t.h!=0 and warning_pole or border_pole
				actors[i0-1]=pole
				actors[i1+1]=pole
			end
		end
		
		return {
			y=y,
			h=h,
			actors=actors
		}
	end

	local function mesh(j)
		local s0,s1=slices[j],slices[j+1]
		local f,actor={}
		local fi=0
		for i=0,nx-2 do
			local v0={i*dx,s0.h[i]+s0.y,j*dz}
			-- v1-v0
			local u1={dx,s0.h[i+1]-s0.h[i],0}
			-- v2-v0
			local u2={dx,s1.h[i+1]+s1.y-s0.h[i]-s0.y,dz}
			-- v3-v0
			local u3={0,s1.h[i]+s1.y-s0.h[i]-s0.y,dz}

			-- normal 
			local n0=v_cross(u3,u2)
			v_normz(n0)
			local n1=v_cross(u2,u1)
			v_normz(n1)
			local v=n1
			if v_dot(n0,n1)>0.995 then
				-- quad
				f[fi]={n=n0}
			else
				-- 2 tri
				f[fi]={n=n0}
				f[fi+1]={n=n1}
			end

			-- actor position (if any)
			local u,t=v_clone(u2),rnd(0.5)
			v_scale(u,t)
			v_add(u,u3,0.5-t)
			if(s0.actors[i]) s0.actors[i].u=u

			fi+=2
		end
		-- attach faces to slice
		s0.f=f
	end

	local ybase=32
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
				out[#out+1]={key=1/(ay*ay+az*az),a=actor,x=63.5+flr(shl(ax/az,6)),y=63.5-flr(shl(ay/az,6)),w=shl(4/az,6),dist=dist}
			end
		end
	end

	-- raycasting constants
	local angles,dists={},{}
	for i=0,23 do
		local z=i-12.5
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

	return {
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
					local f0,f1=s0.f[2*i],dist<8 and s0.f[2*i+1]
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
			if p[3]/dz>8 then
				-- shift back
				p[3]-=dz
				local old_y=slices[0].y
				-- drop slice 0
				for i=1,nz-1 do
					slices[i-1]=slices[i]
					slices[i-1].y-=old_y
				end
				-- use previous baseline
				slices[nz-1]=make_slice(slices[nz-2].y-delta_slope-rnd(0.1*delta_slope))
				-- create mesh
				mesh(nz-2)
			end
			-- update y offset
			local t=(p[3]/dz)%1
			dy=slices[0].y*(1-t)+t*slices[1].y
		end,
		find_face=function(self,p)
			-- z slice
			local i,j=self:to_tile_coords(p)

			-- todo: improve
			if(i<0 or j<0) return
			local s0=slices[j]

			-- should not happen
			-- assert(s0,"outside of map: "..i.."/"..j)

			local f0,f1=s0.f[2*i],s0.f[2*i+1]
			local f=f0
			-- select face
			if(f1 and (p[3]-dz*j<p[1]-dx*i)) f=f1

			-- intersection point
			local t=-v_dot(make_v({i*dx,s0.h[i]+s0.y-dy,j*dz},p),f.n)/f.n[2]
			p=v_clone(p)
			p[2]+=t
			return f,p
		end,
		-- find all actors within a given radius from given position
		collide=function(self,p,r)
			local i0,j0=self:to_tile_coords(p)
			
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
							if v_dot(d,d)<r+actor.r*actor.r then
								return true
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

	-- build up cache per angle
	local angles={}
	for i=0,n-1 do
		local a=i/n
		local ca,sa=cos(a),sin(a)
		local ddx0,ddy0=ca,sa
		local dx0,dy0=(sa-ca)*7.5+8,-(ca+sa)*7.5+8
		
		local srcx,srcy

		-- if not provided, transparent color is top/left sprite pixel
		tc=tc or sget(sx,sy)
		local function ssget(dx,dy)
			return band(bor(dx,dy),0xfff0)==0 and sget(sx+dx,sy+dy) or tc
		end

		local cache={}
		-- target=sprite initial location!
		-- must be on 4 byte boundary
		assert(sx%16==0,"sprite x must be a multiple of 16:"..sx)
		-- sprite sheet memory location
		local mem=64*8*flr(sy/8)+flr(sx%128)/2
		for iy=0,15 do
			srcx,srcy=dy0+iy*ddy0,dx0+iy*ddx0
			
			cache[mem]=
				bor(
					bor(
						bor(shr(ssget(srcx,srcy),16),shr(ssget(srcx+ddx0,srcy-ddy0),12)),
						bor(shr(ssget(srcx+2*ddx0,srcy-2*ddy0),8),shr(ssget(srcx+3*ddx0,srcy-3*ddy0),4))
					),
					bor(
						bor(ssget(srcx+4*ddx0,srcy-4*ddy0),shl(ssget(srcx+5*ddx0,srcy-5*ddy0),4)),
						bor(shl(ssget(srcx+6*ddx0,srcy-6*ddy0),8),shl(ssget(srcx+7*ddx0,srcy-7*ddy0),12))
					)
				)
			
			cache[mem+4]=
				bor(
					bor(
						bor(shr(ssget(srcx+8*ddx0,srcy-8*ddy0),16),shr(ssget(srcx+9*ddx0,srcy-9*ddy0),12)),
						bor(shr(ssget(srcx+10*ddx0,srcy-10*ddy0),8),shr(ssget(srcx+11*ddx0,srcy-11*ddy0),4))
					),
					bor(
						bor(ssget(srcx+12*ddx0,srcy-12*ddy0),shl(ssget(srcx+13*ddx0,srcy-13*ddy0),4)),
						bor(shl(ssget(srcx+14*ddx0,srcy-14*ddy0),8),shl(ssget(srcx+15*ddx0,srcy-15*ddy0),12))
					)
				)
		
			-- one line down
			mem+=64  
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

__gfx__
000000000000000000eeeeee01000000000000000000000000000000000000000000000000000000000002255566650000000000000000000000000000000000
0000000000000000770eeeee15000000155000000000000000000000000000000000000000000000000022222255560000000000000000000000000000000000
0000000000000000cc70eeee280000002d5000000000000000000000000000000000000000000000000022222222257000000000000000000000000000000000
0000000000000000ccc70eee3b00000036d000000000000000000000000000000000000000000000002222222222222700000000009290000000000000000000
0000000000000000cccc70ee490000004960000000000000000000000000000000000000000000000022222222222222200000000022200000000e0880e00000
0000000000000000ccccc70e5d0000005d600000000000000000000000000000000000000000000000222222222222222000000000929000000000cccc000000
0000000000000000ccccc70e670000006dd00000000000000000000a000000000000000000000000022222222222222220000000000000000000000cc0000000
0000000000000000ccccc70e7700000076d00000000000000000000a000000000000000000000000022828282222222220000000000000000000200110020000
0000000000000000cccc70ee8e0000008ed000000000000000000008000000000000000000000000022228228828282220000000000000000000801001080000
0000000000000000ccc70eee9a00000094200000000000000000000800000000000000000000000000282222222222222000000000a8a0000000080000800000
0000000000000000cc70eeeea7000000a6d00000000000000000000a00000000000000000000000002282288228228282000000000a8a0000000008008000000
0000000000000000770eeeeeb6000000b3d00000000000000000000a000000000000000000000000028888288282288820000000009290000000000000000000
000000000000000000eeeeeec6000000cd1000000000000000000008000000000000000000000000028888888888888820000000002220000000000000000000
0000000000000000eeeeeeeed6000000dd1000000000000000000008000000000000000000000000288888888888888820000000009290000000000000000000
0000000000000000eeeeeeeeef000000e2500000000000000000000a000000000000000000000000288888888888888820000000000000000000000000000000
0000000000000000eeeeeeeef7000000f6d000000000000000000015100000000000000000000000288888899988888200000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000288888899a98888200000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000288889a8aaa8888200000000000000000000000bb0000000
000000000000000000000000000000000000000000000000000000000000000000000000000000002888898888a88882000000000000000000000053bb000000
0000007777000000000000000000000000000000000000000000000000000000000000000000000028889a88888988820000000000000000000000b53d000000
0000777777700000000000000000000000000000000000000000000000000000000000000000000288888888888a988200000000000000000000035bbbb00000
000677777777000000000000000000000000000d600000000000000800000000000000000000000288888888888aa88200000000000000000000053bbbb00000
00766777777700000000000000000000000000dd16000000000000888000000000000000000000028888888898888882000000000000000000000b533bb00000
0076776777770000000000000000000060000ddd1160000d0000008a80000000000000000000000288888889998888820000000000000000000035b5b33d0000
00767777777700000000000000000000160dddd1111660d10000088a880000000000000000000002888889aaaa88888200000000000000000000535bbbbb0000
0067677667770000000000000000000011dddd111111161100000888880000000000000000000002888889a88a88888200000000000000000000b533bbbb0000
006777767775000000000000000000001d1ddd11111111610000888a8880000000000000000000028888888888988820000000000000000000035b5b333bb000
006667667677000000000000000000001111d111111111110000888888800000000000000000000288888888888888200000000000000000000535b5bbb3d000
00076766777000000000000000000000111111111111111100000005000000000000000000000002888888888888882000000000000000000005535bbbbbb000
00006666660000000000000000000000111111111111111100000006000000000000000000000002888888888888882000000000000000000000000110000000
00000000000000000000000000000000111111111111111100000006000000000000000000000002888888888888882000000000000000000000000440000000
00000000000000000000000000000000111111111111111100000016100000000000000000000002888888888888882000000000000000000000000440000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000888882000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000882000000000000000000000000000000000
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
