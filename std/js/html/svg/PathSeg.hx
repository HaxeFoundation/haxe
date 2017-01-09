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

// This file is generated from mozilla\SVGPathSeg.webidl. Do not edit!

package js.html.svg;

@:native("SVGPathSeg")
extern class PathSeg
{
	static inline var PATHSEG_UNKNOWN : Int = 0;
	static inline var PATHSEG_CLOSEPATH : Int = 1;
	static inline var PATHSEG_MOVETO_ABS : Int = 2;
	static inline var PATHSEG_MOVETO_REL : Int = 3;
	static inline var PATHSEG_LINETO_ABS : Int = 4;
	static inline var PATHSEG_LINETO_REL : Int = 5;
	static inline var PATHSEG_CURVETO_CUBIC_ABS : Int = 6;
	static inline var PATHSEG_CURVETO_CUBIC_REL : Int = 7;
	static inline var PATHSEG_CURVETO_QUADRATIC_ABS : Int = 8;
	static inline var PATHSEG_CURVETO_QUADRATIC_REL : Int = 9;
	static inline var PATHSEG_ARC_ABS : Int = 10;
	static inline var PATHSEG_ARC_REL : Int = 11;
	static inline var PATHSEG_LINETO_HORIZONTAL_ABS : Int = 12;
	static inline var PATHSEG_LINETO_HORIZONTAL_REL : Int = 13;
	static inline var PATHSEG_LINETO_VERTICAL_ABS : Int = 14;
	static inline var PATHSEG_LINETO_VERTICAL_REL : Int = 15;
	static inline var PATHSEG_CURVETO_CUBIC_SMOOTH_ABS : Int = 16;
	static inline var PATHSEG_CURVETO_CUBIC_SMOOTH_REL : Int = 17;
	static inline var PATHSEG_CURVETO_QUADRATIC_SMOOTH_ABS : Int = 18;
	static inline var PATHSEG_CURVETO_QUADRATIC_SMOOTH_REL : Int = 19;
	
	var pathSegType(default,null) : Int;
	var pathSegTypeAsLetter(default,null) : String;
	
}