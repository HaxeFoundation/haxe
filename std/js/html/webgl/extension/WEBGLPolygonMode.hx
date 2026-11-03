/*
 * Copyright (C)2005-2025 Haxe Foundation
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

package js.html.webgl.extension;

/**
	This extension exposes the ANGLE_polygon_mode functionality to WebGL.
	There are no WebGL-specific behavioral changes.
**/
@:native("WEBGL_polygon_mode ")
extern class WEBGLPolygonMode  {
	
	static inline var POLYGON_MODE_WEBGL : Int = 2880;
	static inline var POLYGON_OFFSET_LINE_WEBGL : Int = 10754;
	static inline var LINE_WEBGL : Int = 6913;
	static inline var FILL_WEBGL : Int = 6914;

	/**
		Face must be FRONT_AND_BACK.
		Mode must be LINE_WEBGL or FILL_WEBGL (default).
	**/
	function polygonModeWEBGL( face : Int, mode : Int ) : Void;
}