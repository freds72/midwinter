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
	if(d<0.001) return 0
	x/=d
	y/=d
	z/=d
	return d*(x*x+y*y+z*z)^0.5
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
local track,plyr,cam
local actors={}

local k_far,k_near=0,2
local k_right,k_left=4,8
local z_near=0.05

-- voxel helpers
function to_tile_coords(v)
	local x,y=shr(v[1],3)+16,shr(v[3],3)+16
	return flr(x),flr(y),x,y
end

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
		{-1.65,4.125,0.04},
		{-0.7,0.3,0.1},
		{-0.01,0.1,0.6}
	}
	local current_pov=v_clone(view_pov[view_mode+1])

	--
	local up={0,1,0}

	-- raycasting constants
	local angles={}
	local dists={}
	for i=0,15 do
		local z=i-7.5
		add(angles,atan2(7.5,z))
		add(dists,sqrt(7.5*7.5+z*z))
	end

	-- screen shake
	local shkx,shky=0,0
	camera()
	
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
		project_poly=function(self,p,c0)
			local cache={}
			local p0,p1=p[1],p[2]
			-- magic constants = 89.4% vs. 90.9%
			-- shl = 79.7% vs. 80.6%
			local x0,y0=63.5+flr(shl(p0[1]/p0[3],6)),63.5-flr(shl(p0[2]/p0[3],6))
			cache[1]={x0,y0}
			local x1,y1=63.5+flr(shl(p1[1]/p1[3],6)),63.5-flr(shl(p1[2]/p1[3],6))
			cache[2]={x1,y1}
			for i=3,#p do
				local p2=p[i]
				local x2,y2=63.5+flr(shl(p2[1]/p2[3],6)),63.5-flr(shl(p2[2]/p2[3],6))
				trifill(x0,y0,x1,y1,x2,y2,c0)
				cache[i]={x2,y2}
				x1,y1=x2,y2
			end
		end,
		visible_tiles=function(self)
			local x0,y0,x,y=to_tile_coords(self.pos)
			local tiles={[x0+shl(y0,5)]=0} 

   for i,a in pairs(angles) do
				a+=self.angle
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
						tiles[mapx+shl(mapy,5)]=dist
					end
				end				
			end	
			return tiles
	 end
	}
end

-- "physic body" for simple car
function make_car(p,angle)
	-- last contact face
	local oldf

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
			return oldf and oldf.n or v_up
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
			pos[2]=max(pos[2]-0.1,newpos and newpos[2] or 0)
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


function is_inside(p,f)
	local v=track.v
	local p0=v[f[f.ni]]
	for i=1,f.ni do
		local p1=v[f[i]]
		if((p0[3]-p1[3])*(p[1]-p0[1])+(p1[1]-p0[1])*(p[3]-p0[3])<0) return
		p0=p1
	end
	-- intersection point
	local t=-v_dot(make_v(v[f[1]],p),f.n)/f.n[2]
	p=v_clone(p)
	p[2]+=t
	return f,p
end

function find_face(p,oldf)	
	-- same face as previous hit
	if oldf then
		local newf,newp=is_inside(p,oldf)
		if(newf) return newf,newp
	end
	-- voxel?
	local x,z=flr(p[1]/8+16),flr(p[3]/8+16)
	local faces=track.ground[x+32*z]
	if faces then
		for _,f in pairs(faces) do
			if f!=oldf then
				local newf,newp=is_inside(p,f)
				if(newf) return newf,newp
			end
		end
	end
	-- not found
end
-- reports hit result + correcting force + border normal
function face_collide(f,p,r)
	local force,hit,n={0,0,0}
	for _,b in pairs(f.borders) do
		local pv=make_v(b.v,p)
		local dist=v_dot(pv,b.n)
		if dist<r then
			hit,n=true,b.n
			v_add(force,b.n,r-dist)
		end
	end
	return hit,force,n
end

-- game states
-- transition to next state
function next_state(state,...)
	draw_state,update_state=state(...)
end

function play_state()

	-- start over
	actors={}

	-- create player in correct direction
	plyr=add(actors,make_plyr(track.start_pos,0))

	-- reset cam	
	cam=make_cam()

	return
		-- draw
		function()
		end,
		-- update
		function()
			cam:update()
			plyr:control()	
		end
end

function _init()
	track=unpack_track()

	-- init state machine
	next_state(play_state)
end

function _update()
	-- basic state mgt
	update_state()

	plyr:prepare()
	plyr:integrate()	
	plyr:update()

	if plyr then
		local pos,a=plyr:get_pos()
		cam:track(pos,a,plyr:get_up())
	end
end

-- vertex cache class
-- uses m (matrix) and v (vertices) from self
-- saves the 'if not ...' in inner loop
local v_cache_cls={
	__index=function(t,k)
		-- inline: local a=m_x_v(t.m,t.v[k]) 
		local v,m=t.v[k],t.m
		local x,y,z=v[1],v[2],v[3]
		local ax,az=m[1]*x+m[5]*y+m[9]*z+m[13],m[3]*x+m[7]*y+m[11]*z+m[15]
	
		local outcode=az>z_near and k_far or k_near
		if ax>az then outcode+=k_right
		elseif -ax>az then outcode+=k_left
		end	

		t[k]={ax,m[2]*x+m[6]*y+m[10]*z+m[14],az,outcode=outcode}
		return t[k]
	end
}

function collect_faces(faces,cam_pos,v_cache,out,dist)
	local n=#out+1
	for _,face in pairs(faces) do
		if v_dot(face.n,cam_pos)>face.cp then
			local z,y,outcode,verts,is_clipped=0,0,0xffff,{},0
			-- project vertices
			for ki=1,face.ni do
				local a=v_cache[face[ki]]
				y+=a[2]
				z+=a[3]
				outcode=band(outcode,a.outcode)
				-- behind near plane?
				is_clipped+=band(a.outcode,2)
				verts[ki]=a
			end
			-- mix of near/far verts?
			if outcode==0 then
	   			-- average before changing verts
				y/=#verts
				z/=#verts

				-- mix of near+far vertices?
				if(is_clipped>0) verts=z_poly_clip(z_near,verts)
				if #verts>2 then
					out[n]={key=1/(y*y+z*z),f=face,v=verts,dist=dist}
				 	-- 0.1% faster vs [#out+1]
				 	n+=1
				end
			end
		end
	end
end

local dither_pat={0xffff,0x7fff,0x7fdf,0x5fdf,0x5f5f,0x5b5f,0x5b5e,0x5a5e,0x5a5a,0x1a5a,0x1a4a,0x0a4a,0x0a0a,0x020a,0x0208,0x0000}
local ramp={7,7,6,5,1,1}
function draw_faces(faces,v_cache)
	for i=1,#faces do
		local d=faces[i]
		local c0=0x76
		if d.f.n[2]<0.6 then
			c0=0x54
			if(d.dist>4) c0=0x4d
			if(d.dist>5) c0=0xdc
			local c=5*d.f.n[2]
			local cf=(#dither_pat-1)*(1-c%1)
			fillp(dither_pat[flr(cf)+1])
			
		else
			if(d.dist>4) c0=0x6d
			if(d.dist>5) c0=0xdc
			local c=5*d.f.n[2]
			local cf=(#dither_pat-1)*(1-c%1)
			fillp(dither_pat[flr(cf)+1])
		end
		cam:project_poly(d.v,c0)
	end
	fillp()
end

function _draw()

	cls(12)

	-- map
	local v_cache={m=cam.m,v=track.v}
	setmetatable(v_cache,v_cache_cls)

	local tiles=cam:visible_tiles()
	local out={}
	-- get visible voxels
	for k,dist in pairs(tiles) do
	--for k,_ in pairs(track.voxels) do
		local faces=track.voxels[k]
		if faces then
			collect_faces(faces,cam.pos,v_cache,out,dist)
		end
	end

	-- todo: sort by voxels
	sort(out)
	draw_faces(out,v_cache)
 	
	print("64x64 mesh w.hidden surf removal",1,1,6)
	print("+ 6dof + surf tracking",41,8,6)
	local cpu=flr(10000*stat(1))/100
	print(stat(7).." fps - cpu "..cpu,57,15,5)
	
	spr(9,24+6*cos(time()/4),128-28+4*sin(time()/5),4,4)
	spr(9,84-5*cos(time()/5),128-28+4*sin(time()/4),4,4,true)
end

-->8
-- generate map
function make_map(model)
	-- vertices
	local v=model.v
	for j=0,63 do
		for i=0,63 do
			local y=sget(i,j)
			add(v,{4*i-128,y,4*j-128})
		end
	end

	local voxels=model.voxels

	for i=0,62 do
		for j=0,62 do
			local f={flags=0x16,c=1,ni=4}
			f[1]=i+shl(j,6)+1
			f[2]=i+1+shl(j,6)+1
			f[3]=i+1+shl(j+1,6)+1
			f[4]=i+shl(j+1,6)+1

			-- normal 
			f.n=v_cross(make_v(v[f[1]],v[f[f.ni]]),make_v(v[f[1]],v[f[2]]))
			v_normz(f.n)

			-- cp
			f.cp=v_dot(f.n,model.v[f[1]])

			model.f[i+shl(j,5)+1]=f

			-- voxel location
			local vidx=flr(i/2)+shl(flr(j/2),5)
			voxels[vidx]=voxels[vidx] or {}
			add(voxels[vidx],f)
		end
	end
		
	-- todo: unify
	model.ground=voxels
end

function unpack_track()
	local model={
		v={},
		f={},
		voxels={},
		ground={},
		start_pos={-127,0,-127}}	

	-- fill map
	make_map(model)

	return model	
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
00000000000000000000010000000000000000000000000111222332100000000000000000000000006666660000000000000000c00000000000000000000000
00000000000000000000000000000000000000000000000000012343210000000000000000000000555555557000000000000000c1dd67000000000000000000
00000000000000000000000000000000000000000000000000124555321000000000000000000002222222222700000000000000c00000000000000000000000
00000000012221000000011100011100000000100000000001347787532110000000000000000022222222222220000000000000c12450000000000000000000
00000000134443100001122212234320000122211000111113579876543210000000000000000022222222222220000000000000c00000000000000000000000
00000000246764211123334334566532122345543122232235789876664321000000000000000222222222222222000000000000c00000000000000000000000
00000001357876422345554568abb743234568764344555567787665676420000000000000000222222222222222000000000000000000000000000000000000
000000124577775445666657adeec965445688875556778777665555787420000000000000000222222222222822000000000000000000000000000000000000
000001244567765456877669cfffd975666678875568aa9877654456876310000000000000000222222828282222200000000000000000000000000000000000
000013345555665556776569cfffc966777667765568abaa86544456764200000000000000000228282282222828200000000000000000000000000000000000
001134566544555556766458bdec9766876666665468aa9875444566542100000000000000000222222222828282800000000000000000000000000000000000
001245776544444456654335788765556565566544468a9864334445542100000000000000000228282828282888880000000000000000000000000000000000
00124678764333334543211111112334333346764334678875434455553200000000000000000022828288888888880000000000000000000000000000000000
00123578764321122221000000000000000025664323467999754467775310000000000000000028888888888888880000000000000000000000000000000000
00012456665321011000000000000000000003442001359cefca6467896410000000000000000028888888888888880000000000000000000000000000000000
00001234665421000000000000000000000000000000148efffea667876420000000000000000028888888998888880000000000000000000000000000000000
00000124666532100000000000000000000000000000037effffc7577764200000000000000000288888899a9888888000000000000000000000000000000000
00000124787532210000000100012334200000000000015dfffff856776421000000000000000028888889aaa988888000000000000000000000000000000000
000001368986422210000011124799877630000000000018fffffc5577542100000000000000002888889a888a98888000000000000000000000000000000000
00000147bb96433321001233357beffda973000000000004efffff757654210000000000000000028888988888a9888000000000000000000000000000000000
00000248ab96433321112465558cffffda97555441000002cfffff945543211000000000000000028889a888888a888800000000000000000000000000000000
00000258a985333321113798667bfffebabbaabdc8300002bfffff93333221100000000000000002888a88888888888800000000000000000000000000000000
0000136aa8632232100159ba8669dedb99adefffeca40001afffffb3222222110000000000000002888888889888888800000000000000000000000000000000
0000258aa7421222100269cd9768aba98abefffffec830016fffffb5322222210000000000000002888888899988888800000000000000000000000000000000
000026adb8422221000159dda7679a988abefffffed940004bffffd753322110000000000000000288888889aa98888800000000000000000000000000000000
000037dfe9522321000148bb97689a9989adfeedffd9400028ffffe96432211000000000000000002888889a88a9888880000000000000000000000000000000
011238dfeb643331000025888668aa98679cddbbefda500015cfffb8643211000000000000000000288888988888888880000000000000000000000000000000
122248dffc8654321000268876679a86447acbaacfeb6100038cdb96443210010000000000000000288888888888888880000000000000000000000000000000
234358cffca765543221369ba87677632369bbbcdfec720002799774322110010000000000000000288888888888888880000000000000000000000000000000
345457addca8776664446adffc9653322347acddffed830001687543221001120000000000000000288888888888888888000000000000000000000000000000
3455679bba88788896669effffb632221237bdedfffea40002687444321011120000000000000000028888888888888888000000000000000000000000000000
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
000000001234555432112356799a9876555443322101358886433443210000000000000000000000000000000000000000000000000000000000000000000000
00000000012355543210123456777654333322221101246665445665310000000000000000000000000000000000000000000000000000000000000000000000
00000000001244432210011234443322221111122222234443456777521000000000000000000000000000000000000000000000000000000000000000000000
00000000000122222222000112221111111111233333222222356776532100000000000000000000000000000000000000000000000000000000000000000000
00000000000001113343200000000111111122223344321111245666542100000000000000000000000000000000000000000000000000000000000000000000
00000000000000013555320000001233221111112334321111233344443200000000000000000000000000000000000000000000000000000000000000000000
00000000000000013576532000012455432111101223222232223232232100000000000000000000000000000000000000000000000000000000000000000000
00000000000000012466643210013576432210000122122334333222121100000000000000000000000000000000000000000000000000000000000000000000
00000000000000001345643211123455321100000111112345544321100000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000123433211112333210000001111112334555431000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000011222111001111000000000111111223344321000000000000000000000000000000000000000000000000000000000000000000000000
