

//#version 110

uniform vec4 color1;
uniform vec4 color2;
uniform float y;
uniform float height;

#if 0
static GE_Context *get_note_background(int notenum, bool highlight){
  notenum = R_BOUNDARIES(0,notenum,127);
  const int split1 = 50;
  const int split2 = 95;

  GE_Rgb rgb;

  if(notenum<split1)
    rgb = GE_mix(GE_get_rgb(5), GE_get_rgb(1), scale(notenum,0,split1,0,1000));
  else if(notenum<split2)
    rgb = GE_mix(GE_get_rgb(6), GE_get_rgb(5), scale(notenum,split1,split2,0,1000));
  else
    rgb = GE_mix(GE_get_rgb(2), GE_get_rgb(6), scale(notenum,split2,160,0,1000));

  if (highlight)
    rgb = GE_mix(rgb, GE_get_rgb(2), 650);

  rgb = GE_alpha(rgb, 0.7);

  return GE(rgb);
  //return GE_gradient(rgb, GE_get_rgb(0));
}
#endif

/*
  GNU nano 2.3.2                                              New Buffer                                                                                          Modified  

  //vec2 texCoord = gl_TexCoord[0].st;
  //float y2 = gl_TexCoord[1].y;
  //float y2 = gl_MultiTexCoord0.y;
  //float y2 = texCoord.y;
  //vTrans = projection * modelview * incomingVertex

  //float y2 = (gl_ProjectionMatrix * gl_ModelViewMatrix * gl_TexCoord[0] ).y;
  //float y2 = (gl_ModelViewProjectionMatrix * gl_TexCoord[0]).y;
  //float y2 = gl_ProjectionMatrix.y;

  //float y2 = (gl_ModelViewMatrix * gl_Vertex)[0].y;
*/

void main()
{    
  //float y2 = (gl_ModelViewMatrix * gl_Vertex)[0].y;
  //float y2 = (1.0*gl_ModelViewProjectionMatrix).y;
  gl_FragColor = mix(color1,
                     color2,
                     //(gl_FragCoord - y) / height // nice effect
                     (gl_FragCoord.y - y) / height
                     )
    ;
}
