pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
local terrain={}

local z,y=0,0
local tick=0

function make_slope(y)
 tick+=1
	local slope={
		tick=tick,
		y=y,
		v={}}
	local v=slope.v
	for i=0,15 do
	 v[i]=rnd(8)-4
	end
	-- smoothing
	for k=1,2 do
 	for i=0,15 do
			v[i]=(v[i]+v[(i+1)%16])/2
		end
	end
	return slope
end

function _init()
 for j=1,16 do
		add(terrain,make_slope(0))
 end
end

function _update60()
	z+=0.1
	if z>1 then
		z-=1
		local old_y=terrain[1] and terrain[1].y or 0
		for i=2,#terrain do
			terrain[i-1].y=terrain[i].y-old_y
			terrain[i-1].v=terrain[i].v
		end
		-- use previous baseline
		terrain[#terrain]=make_slope(terrain[#terrain-1].y-rnd(3))
	end
end

function _draw()
 cls()
 
 --[[
 local y=0
 for k=1,#terrain do
 	local v=terrain[k].v 
 	for i=1,15 do
 		pset(64+i-7.5,k,v[i]+8)
 	end
 end
 ]]
 
 if #terrain>2 then
	 -- slope offset
		local z0=16-z*8	 
		local dy=terrain[1].y*(1-z)+z*terrain[2].y
	 local prev={}
	 for k=1,#terrain do
	  local v=terrain[k].v   
  	local x0=-7.5*8
  	local y0=-dy+v[0]+terrain[k].y-10
  	-- 3d project
			local w0=63.5/z0
			x0,y0=63.5+x0*w0,63.5-y0*w0
			prev[0]={x0,y0}
   for i=1,15 do
   	local x1=(i-7.5)*8
	  	local y1=-dy+v[i]+terrain[k].y-10
				x1,y1=63.5+x1*w0,63.5-y1*w0
								
				line(x0,y0,x1,y1,1)
				if prev[i] then
					line(prev[i][1],prev[i][2])
				end
 			prev[i]={x1,y1}
				x0,y0=x1,y1
			end
			z0+=8
	 end
 end
 print(#terrain,2,2,7)
end
__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
