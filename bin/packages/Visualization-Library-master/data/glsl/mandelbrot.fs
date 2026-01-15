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

varying vec3 Position;
uniform float ColorOffset;
uniform float Zoom;
uniform float Xcenter;
uniform float Ycenter;
uniform float MaxIterations;
uniform vec3 InnerColor;
uniform sampler1D sampler0;

void main(void)
{
	float real = Position.x * Zoom + Xcenter;
	float imag = Position.y * Zoom + Ycenter;
	float Creal = real; // for a julia set = -0.765
	float Cimag = imag; // for a julia set = +0.110

	float r2 = 0.0;
	float iter;

	for( iter = 0.0; iter < MaxIterations && r2 < 4.0; ++iter )
	{
	  float tempreal = real;
	  real = (tempreal * tempreal) - (imag * imag) + Creal;
	  imag = 2.0 * tempreal * imag + Cimag;
	  r2 = (real*real) + (imag*imag);
	}

	// base the color on the number of iterations

	vec3 color;
	if (r2 < 4.0)
		color = InnerColor;
	else
		color = texture1D(sampler0, fract(iter*0.01+ColorOffset) ).rgb;

	gl_FragColor = vec4( color, 1.0);
}