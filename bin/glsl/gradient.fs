

//#version 110

uniform vec4 color1;
uniform vec4 color2;
uniform float y;
uniform float height;

void main()
{
  gl_FragColor = mix(color1,
                     color2,
                     (gl_FragCoord.y-y) / height
                     )
    ;
}
