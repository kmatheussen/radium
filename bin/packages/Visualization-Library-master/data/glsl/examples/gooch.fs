// adaptation from the "Orange Book" (http://www.3dshaders.com)

const vec3 SurfaceColor = vec3(0.75, 0.75, 0.75);
const vec3 WarmColor = vec3(0.6, 0.6, 0.0);
const vec3 CoolColor = vec3(0.0, 0.0, 0.6);
const float DiffuseWarm = 0.45;
const float DiffuseCool = 0.45;

varying float NdotL;
varying vec3 ReflectVec;
varying vec3 ViewVec;

void main()
{
	vec3 kcool = min(CoolColor + DiffuseCool * SurfaceColor, 1.0);
	vec3 kwarm = min(WarmColor + DiffuseWarm * SurfaceColor, 1.0);
	vec3 kfinal = mix(kcool, kwarm, NdotL);
	
	vec3 nreflect = normalize(ReflectVec);
	vec3 nview    = normalize(ViewVec);
	
	float spec = max(dot(nreflect, nview), 0.0);
	spec = pow(spec, 32.0);
	gl_FragColor = vec4(min(kfinal + spec, 1.0), 1.0);
}

