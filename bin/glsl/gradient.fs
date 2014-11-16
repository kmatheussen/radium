/**************************************************************************************/
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi.                                            */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  This file is part of Visualization Library                                        */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Released under the OSI approved Simplified BSD License                            */
/*  http://www.opensource.org/licenses/bsd-license.php                                */
/*                                                                                    */
/**************************************************************************************/



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
