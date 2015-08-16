

//#version 110

uniform vec4 color1;
uniform vec4 color2;

uniform float x;
uniform float width;



void main()
{    

  gl_FragColor = mix(color1,
                     color2,
                     (gl_FragCoord.x - x) / width
                     )
    ;
}
