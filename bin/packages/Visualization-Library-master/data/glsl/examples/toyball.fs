// adaptation from the "Orange Book" (http://www.3dshaders.com)

varying vec3 N; // normal in object space
varying vec3 L; // light direction in camera space from the vertex

void main()
{
	const vec4 SpecularColor = vec4(0.4, 0.4, 0.4, 60.0);
	const vec4 Red    = vec4(0.6, 0.0, 0.0, 1.0);
	const vec4 Blue   = vec4(0.0, 0.3, 0.6, 1.0);
	const vec4 Yellow = vec4(0.6, 0.5, 0.0, 1.0);
	
	// half spaces used to define the star
	const vec4 HalfSpace0 = vec4(1.0, 0.0, 0.0, 0.2);
	const vec4 HalfSpace1 = vec4(0.309016994, 0.951056516, 0.0, 0.2);
	const vec4 HalfSpace2 = vec4(-0.809016994, 0.587785252, 0.0, 0.2);
	const vec4 HalfSpace3 = vec4(-0.809016994, -0.587785252, 0.0, 0.2);
	const vec4 HalfSpace4 = vec4(0.309016994, -0.951056516, 0.0, 0.2);
	
	const float InOrOutInit = -3.0;
	const float StripeWidth = 0.2;
	const float FWidth = 0.005;
	
	vec4 normal;
	vec4 p;
	vec4 surfColor;
	float intensity;
	vec4 distance;
	float inorout;
	
	p.xyz = normalize(N);
	p.w = 1.0;
	
	inorout = InOrOutInit;
	
	distance[0] = dot(p, HalfSpace0);
	distance[1] = dot(p, HalfSpace1);
	distance[2] = dot(p, HalfSpace2);
	distance[3] = dot(p, HalfSpace3);
	
	distance = smoothstep(-FWidth, FWidth, distance);
	
	inorout += dot(distance, vec4(1.0));
	
	distance.x = dot(p, HalfSpace4);
	distance.y = StripeWidth - abs(p.z);
	distance = smoothstep(-FWidth, FWidth, distance);
	inorout += distance.x;
	
	inorout = clamp(inorout, 0.0, 1.0);
	
	surfColor = mix(Yellow, Red, inorout);
	surfColor = mix(surfColor, Blue, distance.y);
	
	// normal = point on surface for sphere at (0,0,0)
	normal.xyz = gl_NormalMatrix * p.xyz;
	normal.w = 1.0;
	
	// Per-fragment diffuse lighting
	intensity = 0.2; // ambient
	intensity += 0.8 * clamp(dot(L, normal.xyz), 0.0, 1.0);
	surfColor *= intensity;

	// Per-framgment specular lighting
	vec4 HVector = vec4( normalize(L+vec3(0.0, 0.0, 1.0)), 0.0);
	intensity = clamp(dot(HVector, normal), 0.0, 1.0);
	intensity = pow(intensity, SpecularColor.a);
	
	surfColor += SpecularColor * intensity;

	gl_FragColor = vec4(surfColor.rgb, 1.0);
	
	
}
