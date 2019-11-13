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

-- print helper
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

local k_far,k_near,k_right,k_left,z_near=0,2,4,8,0.2

-- camera
function make_cam()
	--
	local up={0,1,0}

	-- screen shake
	local shkx,shky=0,0
	camera()

	local update_bkg_sprite=make_rspr(32,16,32,0)

	return {
		pos={0,0,0},
		angle=0,
		m=make_m_from_v_angle(v_up,0),
		shake=function()
			shkx=min(4,shkx+rnd(8))
			shky=min(4,shky+rnd(8))
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

			x0-=v
			y0-=u
			for i=-7,7 do
				spr(36,x0+i*u,y0+i*v,2,2)
			end
		end
	}
end

-- "physic body" for simple car

function make_car(p)
	-- last contact face
	local up,oldf={0,1,0}

	local velocity,angularv={0,0,0},0
	local forces,torque={0,0,0},0

	local angle,steering_angle=0,0
	local on_air_ttl=0

	local g={0,-4,0}
	return {
		pos=v_clone(p),
		on_ground=false,
		height=0,
		get_pos=function(self)
	 		return self.pos,angle,steering_angle/0.625
		end,
		get_up=function()
			return v_lerp(v_up,up,abs(cos(angle)))
		end,
		get_velocity=function()
			return velocity
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
			-- v_scale(velocity,0.97)
			-- some friction
			local f=self.on_ground==true and 0.08 or 0.005
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
					sa*=60--*grip

					--grip*=grip

					-- todo: review
					sa=mid(sa,-5*grip,5*grip)
				
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
			self.gps=gps
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
			local src=0x3200+68*11
			local vol=flr(20*v_len(velocity)+10*self.height)			
			for i=0,15 do
				local s=peek2(src)
				poke2(src,bor(band(0xffe0,s),flr(vol+rnd(4))))
				src+=2
			end
		end
	}	
end

function make_plyr(p,hp)
	local body=make_car(p)

	local body_update=body.update

	local hit_ttl,jump_ttl,jump_pressed=0,0
	-- todo: ttl for multiplier
	local multiplier=0

	body.control=function(self)	
		local da=0
		if(btn(0)) da=1
		if(btn(1)) da=-1
		local do_jump
		if self.on_ground==true then
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
		end

		if do_jump then
			self:apply_force_and_torque({0,jump_ttl*7,0},0)
			jump_ttl,jump_pressed,do_jump=0
		end

		self:steer(da/8)
	end
	
	body.update=function(self)
		hit_ttl-=1
		multiplier=max(multiplier-0.01)

		-- collision detection
		local hit_type=ground:collide(self.pos,0.2)
		if hit_type==2 then
			-- insta-death
			cam:shake()
			self.dead=true
		elseif hit_type==3 then
			-- score multiplier
			multiplier+=1
			sfx(8)
		elseif hit_ttl<0 and hit_type==1 then
			sfx(rnd()>0.5 and 9 or 10)
			hp-=1
			cam:shake()
			-- temporary invincibility
			hit_ttl=20
			if hp==0 then
				self.dead=true
			end
		end

		-- call parent
		body_update(self)
	end

	body.score=function()
		local lost_hp=0
		if(hit_ttl>0 and hit_ttl%2==0) lost_hp=1
		return hp+lost_hp,ceil(multiplier)
	end

	-- wrapper
	return body
end

function make_snowball(pos)
	local body=make_car(pos)
	
	local body_update=body.update

	body.sx=112
	body.sy=0
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
		pal()

		-- mask
		rectfill(0,0,127,27,0)
		rectfill(0,92,127,127,0)
		palt(14,true)
		palt(0,false)
		spr(128,64,28,8,9)
		spr(128,0,28,8,9,true)
		palt()
	end
   
	local panels={
		{text="marmottes",c=1,params={slope=1.8,hp=3}},
		{text="biquettes",c=8,params={slope=2.5,hp=2}},
		{text="chamois",c=0,params={slope=3.2,hp=1}}
	}
	local sel,sel_tgt,sel_max=0,0,#panels
	local blink,ttl=false,20

	ground=make_ground({slope=0})

	-- reset rotated sprites
	reload()

	-- reset cam	
	cam=make_cam()

	music(0)
	sfx(-1)
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
			for i=1,#panels do
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
					draw_box(panels[i].text,x0+32,y0,panels[i].c,mode)
				end
				a+=da
			end

			printb("⬅️➡️ select track",31,110,7,5,1)
			if((time()%1)<0.5) printb("❎/🅾️ go!",50,120,10,5,1)
			print("▤@freds72 - ♪@gruber",20,2,1)
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
				sfx(8)
				-- sub-state
				start_game_async=cocreate(function()
					for i=1,15 do
						blink=i%2==0 and true
						yield()
					end
					pop_state()
					push_state(zoomin_state,play_state,panels[sel+1].params)
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

function play_state(params)

	local fade_async--=cocreate(whiteout_async)

	-- stop music
	music(-1,250)

	-- start over
	actors={}

	--
	ground=make_ground(params)

	-- create player in correct direction
	plyr=make_plyr(ground.plyr_pos,params.hp)

	-- reset cam	
	cam=make_cam()

	local score,score_acc=make_big_number(),0

	-- sprites
	local rot_sprites={
		make_rspr(112,16,32,0),
		make_rspr(48,16,32,0),
		make_rspr(48,0,32,0)}

	local gps_sprite=make_rspr(64,32,64,14)

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
			 
			--local cpu=flr(10000*stat(1))/100
			--print(cpu.."%\n"..stat(0),96,2,2)

			if plyr then
				local pos,a,steering=plyr:get_pos()
				local dy=plyr.height*16
				spr(9,34+3*cos(time()/4),128+dy-steering*14,4,4)
				spr(9,74-2*cos(time()/5),128+dy+steering*14,4,4,true)
			
				palt(0,false)
				palt(7,true)
				spr(140,steering*16-24,96,4,4)
				spr(140,96-steering*16+24,96,4,4,true)
				palt()

				-- difficulty levels = less hearts!
				-- ♥
				local s=""
				local hp,multiplier=plyr:score()
				for i=1,hp do
					s=s.."♥"
				end
				printb(s,2,4,7,5,1)

				local dz=plyr:get_velocity()[3]
				multiplier=max(multiplier,1)
				score_acc+=max(multiplier*dz)
				score:add(score_acc)
				score_acc-=flr(score_acc)

				-- rectfill(64-multiplier/2,20,64+multiplier/2,22,8)
				--print(multiplier,64,30,8)

				if(plyr.gps) gps_sprite(plyr.gps)
				palt(0,false)
				palt(14,true)
				pal(8,7)
				for i=-1,1 do
					for j=-2,1 do
						spr(72,108+i,2+j,2,2)
					end
				end
				pal(8,1)
				spr(72,108,2,2,2)
				pal(8,12)
				spr(72,108,1,2,2)

				palt()
				-- print(plyr.gps,64,38,8)
			end

			-- score
			local s=score:tostr()
			printb(s,nil,4,12,1,7)

			-- draw_debug_samples(-1,1)

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
					cam:shake()

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
	local text_ttl,text_id,text=10,0,{
		"ouch!","aie!","pok!","weee!"
	}
	return {
		draw=function()
			if text_ttl>0 then
				print(text[text_id+1],60,50+text_ttl,8)
			end
			if((time()%1)<0.5) printb("❎/🅾️ retry",42,120,10,5,1)
		end,
		update=function()
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
			cam:track({mid(p[1],8,29*4),p[2],p[3]+16},0.5,v_up)

			if btnp(4) or btnp(5) then
				pop_state()
				pop_state()
				push_state(menu_state)
			end
		end
	}
end

function _init()
	-- todo: remove (only for benchmarks)
	-- srand(12)

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
	w=actor.w or w
	local sx,sy=actor.sx,actor.sy
	if actor.strip then
		local strip=actor.strip[flr((actor.speed*time())%#actor.strip)+1]
		sx,sy=strip.sx,strip.sy
	end
	sspr(sx,sy,actor.w or 16,actor.w or 16,x-w/2,actor.y or y-w,w,w)
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
			if d.f.m==1 then
				-- dirt
				c0=0x54
				if(d.dist>7) c0=0x4d
				if(d.dist>8) c0=0xd5
				fillp(d.f.cf)
			else
				if(d.dist>7) c0=0x6d
				if(d.dist>8) c0=0xd5
				fillp(d.f.cf)
			end
			cam:project_poly(d.v,c0)
			fillp()
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
				if s1.age>0 and flr(s0.x-s1.x)==0 and not s1.main then
					-- todo: kill s0
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
			t[k]={ax,ay,az,outcode=outcode,x=63.5+flr(63.5*ax/az),y=63.5-flr(63.5*ay/az)}
			return t[k]
		end
	}

	local slice_id=0
	local tree_prop,border_pole,warning_pole={sx=112,sy=16},{sx=48,sy=0},{sx=48,sy=16}
	local coins_strip={{sx=0,sy=32},{sx=16,sy=32},{sx=32,sy=32},{sx=48,sy=32}}
	local function make_slice(y)
		-- generate tracks 
		local tracks=next_tracks()
		
		-- height array + props array
		local h,actors={},{}
		for i=0,nx-1 do
			h[i]=rnd(2*delta_slope)
			-- tree
			actors[i]=rnd()>0.85 and {sx=112,sy=16,r=1.2}
		end
			
		-- smoothing
		for k=1,2 do
 			for i=0,nx-1 do
				h[i]=(h[i]+h[(i+1)%nx])/2
			end
		end

		-- side walls
		h[0]=15+rnd(5)
		h[nx-1]=15+rnd(5)

		-- flatten track
		local main_track_x
		for _,t in pairs(tracks) do
			local i0,i1=flr(t.x-2),flr(t.x+2)
			if t.main then
				main_track_x=t.x
				for i=i0,i1 do				
					-- smooth track
					h[i]=t.h+h[i]/4
					-- remove props from track
					actors[i]=nil
				end
				-- track borders
				if slice_id%2==0 then
					local pole=t.h!=0 and warning_pole or border_pole
					actors[i0-1]={sx=pole.sx,sy=pole.sy}
					actors[i1+1]={sx=pole.sx,sy=pole.sy}
				end
			else
				for i=i0,i1 do				
					h[i]+=t.h
				end
				-- coins
				actors[flr(t.x)]={strip=coins_strip,speed=3,r=1,score=1}
			end
		end

		slice_id+=1
		return {
			y=y,
			h=h,
			x=main_track_x*dx,
			actors=actors
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

			if(i==0 or i==nx-2) f.m=0 return f
			f.m=v_dot(sn,f.n)<0.8 and 1 or 0
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

	local plyr_z_index=flr(nz/2)-1
	return {
		plyr_pos={slices[plyr_z_index].x,10,plyr_z_index*dz},
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
			local pz=p[3]/dz
			if pz>8 then
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
			dy=lerp(slices[0].y,slices[1].y,pz%1)
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
			if(not f0) return
			local f=f0
			-- select face
			if(f1 and (p[3]-dz*j<p[1]-dx*i)) f=f1

			-- intersection point
			local t=-v_dot(make_v({i*dx,s0.h[i]+s0.y-dy,j*dz},p),f.n)/f.n[2]
			-- todo: return y value only
			p=v_clone(p)
			p[2]+=t
			return f,p,atan2(slices[i+4].x-p[1],4*dz)
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
									return 3
								end
								return 1
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
		local src,dst=0x6000,64*8*flr(sy/8)+flr(sx%128)/2
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
-- infinite number
function make_big_number()
	-- each segment: 0-99
	local segments={}
	return {
		add=function(self,v)
			local i,carry=1,0
			v=flr(v)
			while bor(v,carry)!=0 do
				local inc=v%100
				assert(i<10,"number too big: "..i)
				local n=segments[i] or 0
				n+=inc+carry
				carry=0
				if n>99 then
				 carry=flr(n/100)
				 n-=100
				end
				segments[i],v=n,flr(v/100)
				i+=1
			end
		end,
		tostr=function(self,sep)
			local s=""
			for i=#segments,1,-1 do
				local n=tostr(segments[i])
				s=s..sub("00",1,2-#n)..n
			end
			-- padding
			-- s=sub("0000000000000000",1,20-#s)..s
			-- separator?
			if sep then
				local tmp=""
				for i=1,#s,3 do
					tmp=sub(s,i,i+2)..sep
				end
				s=tmp
			end
			return s
		end,
		save=function(self,slot)
			dset(slot,#segments)
			for i,v in pairs(segments) do
				dset(slot+i,v)
			end
		end,
		load=function(self,slot)
			local len=dget(slot)
			slot+=1
			for i=1,len do
				add(segments,dget(slot))
				slot+=1
			end
			return slot
		end,
		segments=segments,
		-- true if self>b
		gt=function(a,b)
			local alen,blen=#a.segments,#b.segments
			if alen==blen then
				for i=alen,1,-1 do
					if (a.segments[i]>b.segments[i]) return true
				end
				return false
			elseif alen>blen then
				return true
			end
			return false
		end
	}
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
	local dist={}
	for i,a in pairs(v) do
		dist[i]=n[4]-(a[1]*n[1]+a[2]*n[2]+a[3]*n[3])
	end

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
000000000000000000eeeeee01000000000000000000000000000000000000000000000000000000000005555555550000000000000000000000000000000000
0000000000000000770eeeee15000000155000000000000000000000000000000000000000000000000056666666775000000000000000000000000000000000
0000000000000000cc70eeee280000002d5000000000000000000000000000000000000000000000000562222222257500000000000000000000000000000000
0000000000000000ccc70eee3b00000036d000000000000000000000000000000000000000000000005622222222222600000000009290000000000000000000
0000000000000000cccc70ee490000004960000000000000000000000000000000000000000000000052222222222222200000000022200000000e0880e00000
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
00000000000000000011110000000000000000000000000000000000000000000000000000000000288888899a98888200000000000000000000000000000000
00000000000000000199971000000000000000000000000000000000000000000000000000000000288889a8aaa88882000000000000000000000007b0000000
000000000000000019aaaa71000000000000000000000000000000000000000000000000000000002888898888a888820000000000000000000000537b000000
000000777700000019aaaa910000000000000000000000000000000000000000000000000000000028889a88888988820000000000000000000000b53d000000
000077777770000019aaaa910000000000000000000000000000000000000000000000000000000288888888888a9882000000000000000000000357b7b00000
000677777777000014aaaa91000000000000000d600000000000000800000000000000000000000288888888888aa88200000000000000000000053b7b700000
00766777777700000149991000000000000000dd16000000000000888000000000000000000000028888888898888882000000000000000000000b533bb00000
0076776777770000001111000000000060000ddd1160000d0000008a80000000000000000000000288888889998888820000000000000000000035b5b33d0000
00767777777700000000000000000000160dddd1111660d10000088a880000000000000000000002888889aaaa88888200000000000000000000535b7b7b0000
0067677667770000000000000000000011dddd111111161100000888880000000000000000000002888889a88a88888200000000000000000000b533b7b70000
006777767775000000000000000000001d1ddd11111111610000888a8880000000000000000000028888888888988820000000000000000000035b5b333b7000
006667667677000000000000000000001111d111111111110000888888800000000000000000000288888888888888200000000000000000000535b5bbb3d000
00076766777000000000000000000000111111111111111100000005000000000000000000000002888888888888882000000000000000000005535bbbbbb000
00006666660000000000000000000000111111111111111100000006000000000000000000000002888888888888882000000000000000000000000110000000
00000000000000000000000000000000111111111111111100000006000000000000000000000002888888888888882000000000000000000000000440000000
00000000000000000000000000000000111111111111111100000016100000000000000000000002888888888888882000000000000000000000000440000000
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeee000000000888882000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeee000000000000882000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eee88eeeeee88eee000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eee888eeee888eee000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeee888ee888eeee000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeee88888888eeee000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeeee888888eeeee000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeeee888888eeeee000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeeeee8888eeeeee000000000000000000000000000000000000000000000000
000000f9990000000000000f900000000000000ff000000000000009f0000000eeeeee8888eeeeee000000000000000000000000000000000000000000000000
00000f9449900000000000f4490000000000000ff0000000000000944f000000eeeeeee88eeeeeee000000000000000000000000000000000000000000000000
000009499a90000000000094a900000000000009900000000000009a49000000eeeeeee88eeeeeee000000000000000000000000000000000000000000000000
000009499a90000000000094a400000000000004400000000000004a49000000eeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
0000099aa99000000000009aa400000000000004400000000000004aa9000000eeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
0000009999000000000000094000000000000004400000000000000490000000eeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00011111111110000001111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00177177771771000017717700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01771771177177100177177100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
17717711117717711771771100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01771771177177100177177100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00177177771771000017717700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00011111111110000001111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
__label__
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
00000000000000000000000000000000000000000000000000cccccccccccccccccccccccccccc00000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000cccccccccccccccccccccccccccccccccccccc000000000000000000000000000000000000000000000
000000000000000000000000000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccc000000000000000000000000000000000000000
00000000000000000000000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000000000000000000000000000000
00000000000000000000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000000000000000000000000000
000000000000000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc000000000000000000000000000
000000000000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc000000000000000000000000
000000000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc000000000000000000000
000000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc000000000000000000
00000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000000000000
000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc000000000000000
0000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc0000000000000
000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc000000000000
00000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000000
0000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc0000000000
000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc000000000
000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc000000000
000000000ccccccccccccccccccccccccccccccccccccccccccccccc55565cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc000000000
00000000cccccccccccccccccccccccccccccccccccccccccccccccc55565ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000
00000000cccccccccccccccccccccccccccccccccccccccccccccccc55565ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000
000000006666cccccccccccccccccccccccccccccccccccccccccccc55565cccccccccccccccccccccccccccccccccccccccccccccccccccccc6666600000000
00000000666666666ccccccccccccccccccccccccccccccccccccccc55565ccccccccccccccccccccccccccccccccccccccccccccccccc666666666600000000
0000000666666666666666cccccccccccccccccccccccccccccccccc55565cccccccccccccccccccccccccccccccccccccccccccc66666666666666660000000
00000006666666666666666666666cccccccc00000000000000000000000000000000000000000000ccccccccccccccccc666666666666666666666660000000
000000066666666666666666666666666666c077777777777777777777777777777777777777777770ccccccccc6666666666666666666666666666670000000
00000007777666666666666666666666666660711111111111111111111111111111111111111111170666666666666666666666666666666666777770000000
00000007777777776666666666666666666660711111111111111111111111111111111111111111117066666666666666666666666666677777777770000000
00000007777777777777776666666666666660716661666166616661166166616661666116711111111706666666666666666666677777777777777770000000
0000000677777777777777d677776666666660716661616161616661616116111611611161111111111170d666666666666777d677777777777777d670000000
000000016777777777777dd167777777777760716161666166116161616116111611661166611111111170d16667777777777dd16777777777777dd160000000
00000001167777d67777ddd1167777d67777d0716161616161616161616116111611611111611111111170d1167777d67777ddd1167777d67777ddd110000000
0000000111667d1167dddd1111667d1167ddd071616161616161616166111611161166616611111111170d1111667d1167dddd1111667d1167dddd1110000000
00000001111161111dddd111111161111dddd07111111111111111111111111111111111111111111170d111111161111dddd111111161111dddd11110000000
0000000111111611d1ddd11111111611d1ddd0711111111111111111111111111111111111111111170dd11111111611d1ddd11111111611d1ddd11110000000
0000000111111111111d111111111111111d10777777777777777777777777777777777777777777701d111111111111111d111111111111111d111110000000
000000011111111111111111111d11111d11100000000000000000000000000000000000000000000bbb111111d1111111111111111111111111111110000000
0000000bb111111d631111111116153b1d6311611111111dd53b153b5556516115d11d15d1d111bbbbbb3b1153b11111111111111111111111bbb11110000000
00000003bb111113dd111111116ddb5d6d33d6dd111d11d33b5d15bb5556513d5bb1d35bb1d155333bbbbb11b5dd111111111111111111115533bbb110000000
000000053d1111d633311111116653bbbd63666d1116d1dd53bbb53b55565100003ddd553d6155333bbbbbd53bbb16d111111111111111115533bbb110000000
000000053d11113d633111111dddb53bb3d6d6ddd11dddd6b53bbb535556000000003353bb6d55333bbbbbdb53bbd6d11111111111111111bb553dd110000000
00000000bbb111d3d661111116dd5b533663366dd16ddddd5b53333b5556000000006555b361bb555333ddb5b533ded111111111111a111355bbbbbb00000000
00000000bbb111d6633111111116533bb3d3615111111115533bbb5b55500000000009111555bb555333dd3533bb16d111111111111a111355bbbbbb00000000
000000003bb1163d366311111113b5b33b51156555555553b5b33b1455000000000000545555bb555333dd3b5b33be55111111111118111533bbbbbb00000000
00000000b33ddd6d33331111111535bbbb9555555555555535bbbb4d550000000000006dd33355bbbbbbbbbbbbbbbd55555555555518111b55333bbb00000000
00000000bbbbb1155555555555555515555555555555555ddd1ddddd5000000000000006633355bbbbbbbbbbb155555555555555555a555b55333bbb00000000
000000000bbbb55595555555555555455555ddddddddd666664666665000000000000006655533bbbbbbbbbbb46ddddddddd5555555a5335bb55b33000000000
00000000033bbb5555555ddddddddddd6666666666666666666666660000000000000000655533bbbbbbbbbbb66666666666666666d8d335bb55b33000000000
000000000bb33d555dddddddddddd6666666666666666666666666660000000000000000655533bbbbbbbbbbb666666666666666666a655355bbbbb000000000
000000000bb33d66666666666666666666666666666666666666666000000000000000000bbb55333333bbbbb66666666666666666151bb53333bbb000000000
0000000000bbbb66666666666666666666666666666666666666666000000000000000000bbb55333333bbbbb66666666666666666666bb53333bb0000000000
0000000000666666666666666666666666666666666666666666660000000000000000000055bb555bbb33333ddd6666666666666666355b55bb330000000000
0000000000066666666666666666666666666666666666666666600000000000000000000005bb555bbb33333ddd66666666666666665335bb55b00000000000
0000000000006666666666666666666666666666666666666666000000000000000000000000bb555bbb33333ddd66666666666666665335bb55000000000000
000000000000000666666666666666666666666666666666660000000000000000000000000000bbbbbbbbbbbbbb666666666666666655535000000000000000
000000000000000000666666666666666666666666666666660000000000000000000000000000bbbbbbbbbbbbbb666666666666666666000000000000000000
00000000000060000000000066666666666666666666666600000000000000000000000000000000bbbbbbbbbbbb666666666666000000000001000000000000
000000000066666660000000000000000666666666660000000000000000000000000000000000000000bbbbbbbb666000000000000000066644460000000000
00000000000066666666000000000000000000000000000000000000000000000000000000000000000000000000000000000000000066666644000000000000
00000000000000066666660000000000000000000000000000000000000000000000000000000000000000000000000000000000006666666000000000000000
00000000000000000000600066666666600000000000000000000000000000000000000000000000000000000000000666666666000600000000000000000000
00000000000000000000000066666666666600666666600000000000000000000000000000000000000333333b00bb6666666666000000000000000000000000
000000000000000000000000000000666660066666660000000000000000000000000000000000000000bbbbb3300d6666000000000000000000000000000000
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
000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
__music__
01 0d0e0f44
00 10111244
00 13141544
02 16171844

