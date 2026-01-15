// adaptation from the "Orange Book" (http://www.3dshaders.com)

// requires "hatching.vs"

const float frequency = 2.0;

varying float V;
varying float LightIntensity;

void main()
{
	float dp = length(vec2(dFdx(V), dFdy(V)));
	float logdp = -log2(dp * 8.0);
	float ilogdp = floor(logdp);
	float stripes = exp2(ilogdp);
	
	// float noise = texture3D(Noise, ObjPos).x;
	float noise = 0.0;
	
	float sawtooth = fract((V + noise * 0.1) * frequency * stripes);
	float triangle = abs(2.0 * sawtooth - 1.0);
	
	float transition = logdp - ilogdp;
	
	// taper ends
	triangle = abs((1.0 + transition) * triangle - transition);
	
	const float edgew = 0.3; // width of smooth step
	
	float edge0 = clamp(LightIntensity - edgew, 0.0, 1.0);
	float edge1 = clamp(LightIntensity, 0.0, 1.0);
	float square = 1.0 - smoothstep(edge0, edge1, triangle);
	
	gl_FragColor = vec4(vec3(square), 1.0) * LightIntensity;
	
}
