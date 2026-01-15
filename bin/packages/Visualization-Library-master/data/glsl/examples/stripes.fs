// adaptation from the "Orange Book" (http://www.3dshaders.com)

// requires "stripes.vs"

varying float V;
varying float LightIntensity;

void main()
{
	const float Frequency = 3.0;
	
	float sawtooth = fract(V * Frequency);
	float triangle = abs(2.0 * sawtooth - 1.0);
	float dp = length(vec2(dFdx(V), dFdy(V)));
	float edge = dp * Frequency * 2.0;
	float square = smoothstep(0.5 - edge, 0.5 + edge, triangle);
	
	vec4 Color1 = vec4(0.8, 0.8, 0.8, 1.0);
	vec4 Color2 = vec4(0.1, 0.1, 0.15, 1.0);
	gl_FragColor = mix(Color1, Color2, square) * LightIntensity;
}
