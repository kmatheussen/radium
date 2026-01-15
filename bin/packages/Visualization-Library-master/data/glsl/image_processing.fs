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

uniform sampler2D sampler0;
uniform float image_width;
uniform float image_height;
uniform int test;
void main(void)
{
  // this must reflect the texture size
	float tx = 1.0/image_width;
	float ty = 1.0/image_height;

	vec2 tc[9];
	tc[0] = vec2(-tx,-ty);
	tc[1] = vec2(0,-ty);
	tc[2] = vec2(+tx,-ty);
	tc[3] = vec2(-tx,0);
	tc[4] = vec2(0,0);
	tc[5] = vec2(+tx,0);
	tc[6] = vec2(-tx,+ty);
	tc[7] = vec2(0,+ty);
	tc[8] = vec2(+tx,+ty);
	
	vec3 compensation = vec3(0.0,0.0,0.0);

	float coeff[9];
	// no operation
	// 0, 0, 0
	// 0, 1, 0
	// 0, 0, 0
	coeff[0] = 0.0;
	coeff[1] = 0.0;
	coeff[2] = 0.0;
	coeff[3] = 0.0;
	coeff[4] = 1.0;
	coeff[5] = 0.0;
	coeff[6] = 0.0;
	coeff[7] = 0.0;
	coeff[8] = 0.0;

	if (test == 1)
	{
		// blur
		coeff[0] = 1.0 / 9.0; coeff[1] = 1.0 / 9.0; coeff[2] = 1.0 / 9.0;
		coeff[3] = 1.0 / 9.0; coeff[4] = 1.0 / 9.0; coeff[5] = 1.0 / 9.0;
		coeff[6] = 1.0 / 9.0; coeff[7] = 1.0 / 9.0; coeff[8] = 1.0 / 9.0;
	}
	else
	if (test == 2)
	{
		// sharpen
		// -1, -1, -1,
		// -1,  9, -1,
		// -1, -1, -1
		coeff[0] =-1.0;
		coeff[1] =-1.0;
		coeff[2] =-1.0;
		coeff[3] =-1.0;
		coeff[4] = 9.0;
		coeff[5] =-1.0;
		coeff[6] =-1.0;
		coeff[7] =-1.0;
		coeff[8] =-1.0;
	}
	else
	if (test == 3)
	{
		// edge
		// -1, -1, -1,
		// -1,  8, -1,
		// -1, -1, -1
		coeff[0] = 0.0; coeff[1] =-1.0; coeff[2] = 0.0;
		coeff[3] =-1.0; coeff[4] = 4.0; coeff[5] =-1.0;
		coeff[6] = 0.0; coeff[7] =-1.0; coeff[8] = 0.0;
	}
	else
	if (test == 4)
	{
		// emboss
		// 2, 0, 0,
		// 0,-1, 0,
		// 0, 0, -1
		coeff[0] = 2.0; coeff[1] = 0.0; coeff[2] = 0.0;
		coeff[3] = 0.0; coeff[4] =-1.0; coeff[5] = 0.0;
		coeff[6] = 0.0; coeff[7] = 0.0; coeff[8] =-1.0;
		
		compensation = vec3(0.5,0.5,0.5);
	}

	vec3 frag = vec3(0,0,0);
	// for(int i=0; i<9; i++)
	//   frag += texture2D(sampler0, gl_TexCoord[0].st + tc[i]).rgb * coeff[i];
	frag += texture2D(sampler0, gl_TexCoord[0].st + tc[0]).rgb * coeff[0];
	frag += texture2D(sampler0, gl_TexCoord[0].st + tc[1]).rgb * coeff[1];
	frag += texture2D(sampler0, gl_TexCoord[0].st + tc[2]).rgb * coeff[2];
	frag += texture2D(sampler0, gl_TexCoord[0].st + tc[3]).rgb * coeff[3];
	frag += texture2D(sampler0, gl_TexCoord[0].st + tc[4]).rgb * coeff[4];
	frag += texture2D(sampler0, gl_TexCoord[0].st + tc[5]).rgb * coeff[5];
	frag += texture2D(sampler0, gl_TexCoord[0].st + tc[6]).rgb * coeff[6];
	frag += texture2D(sampler0, gl_TexCoord[0].st + tc[7]).rgb * coeff[7];
	frag += texture2D(sampler0, gl_TexCoord[0].st + tc[8]).rgb * coeff[8];

	// grayscale
	if (test == 5)
	{
		float gray = dot(frag,vec3(0.299,0.587,0.114));
		gray = ((gray-0.5)*1.5)+0.5;
		gl_FragColor.rgb = vec3(gray,gray,gray);
	}
	else
	// contrast
	if (test == 6)
	{
		frag = frag - vec3(0.5,0.5,0.5);
		frag = frag * 2.0;
		frag = frag + vec3(0.5,0.5,0.5);
		gl_FragColor.rgb = frag;
	}
	else
	// brighten
	if (test == 7)
	{
		frag = frag * 2.0;
		gl_FragColor.rgb = frag;
	}
	else
	// darken
	if (test == 8)
	{
		frag = frag / 2.0;
		gl_FragColor.rgb = frag;
	}
	else
	{
		gl_FragColor.rgb = frag + compensation;
	}
  gl_FragColor.a = 1.0;
}
