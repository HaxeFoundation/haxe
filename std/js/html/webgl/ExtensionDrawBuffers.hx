/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

// This file is generated from mozilla\WebGLRenderingContext.webidl. Do not edit!

package js.html.webgl;

/**
	The `WEBGL_draw_buffers` extension is part of the WebGL API and enables a fragment shader to write to several textures, which is useful for deferred shading, for example.

	Documentation [WEBGL_draw_buffers](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_draw_buffers) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_draw_buffers$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WEBGL_draw_buffers>
**/
@:native("WEBGL_draw_buffers")
extern class ExtensionDrawBuffers
{
	static inline var COLOR_ATTACHMENT0_WEBGL : Int = 36064;
	static inline var COLOR_ATTACHMENT1_WEBGL : Int = 36065;
	static inline var COLOR_ATTACHMENT2_WEBGL : Int = 36066;
	static inline var COLOR_ATTACHMENT3_WEBGL : Int = 36067;
	static inline var COLOR_ATTACHMENT4_WEBGL : Int = 36068;
	static inline var COLOR_ATTACHMENT5_WEBGL : Int = 36069;
	static inline var COLOR_ATTACHMENT6_WEBGL : Int = 36070;
	static inline var COLOR_ATTACHMENT7_WEBGL : Int = 36071;
	static inline var COLOR_ATTACHMENT8_WEBGL : Int = 36072;
	static inline var COLOR_ATTACHMENT9_WEBGL : Int = 36073;
	static inline var COLOR_ATTACHMENT10_WEBGL : Int = 36074;
	static inline var COLOR_ATTACHMENT11_WEBGL : Int = 36075;
	static inline var COLOR_ATTACHMENT12_WEBGL : Int = 36076;
	static inline var COLOR_ATTACHMENT13_WEBGL : Int = 36077;
	static inline var COLOR_ATTACHMENT14_WEBGL : Int = 36078;
	static inline var COLOR_ATTACHMENT15_WEBGL : Int = 36079;
	static inline var DRAW_BUFFER0_WEBGL : Int = 34853;
	static inline var DRAW_BUFFER1_WEBGL : Int = 34854;
	static inline var DRAW_BUFFER2_WEBGL : Int = 34855;
	static inline var DRAW_BUFFER3_WEBGL : Int = 34856;
	static inline var DRAW_BUFFER4_WEBGL : Int = 34857;
	static inline var DRAW_BUFFER5_WEBGL : Int = 34858;
	static inline var DRAW_BUFFER6_WEBGL : Int = 34859;
	static inline var DRAW_BUFFER7_WEBGL : Int = 34860;
	static inline var DRAW_BUFFER8_WEBGL : Int = 34861;
	static inline var DRAW_BUFFER9_WEBGL : Int = 34862;
	static inline var DRAW_BUFFER10_WEBGL : Int = 34863;
	static inline var DRAW_BUFFER11_WEBGL : Int = 34864;
	static inline var DRAW_BUFFER12_WEBGL : Int = 34865;
	static inline var DRAW_BUFFER13_WEBGL : Int = 34866;
	static inline var DRAW_BUFFER14_WEBGL : Int = 34867;
	static inline var DRAW_BUFFER15_WEBGL : Int = 34868;
	static inline var MAX_COLOR_ATTACHMENTS_WEBGL : Int = 36063;
	static inline var MAX_DRAW_BUFFERS_WEBGL : Int = 34852;
	
	
	/**
		
		 Defines the draw buffers to which all fragment colors are written. (When using `WebGL2RenderingContext`, this method is available as `WebGL2RenderingContext.drawBuffers()` by default).
		 
	**/
	function drawBuffersWEBGL( buffers : Array<Int> ) : Void;
}