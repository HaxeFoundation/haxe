/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package js;

class Scroll {

	public static function getTop() : Int untyped {
		var sy = window.pageYOffset;
		if( __js__("typeof")(sy) == 'number' )
			return sy;
		if( document.body ) {
			sy = document.body.scrollTop;
			if( sy ) return sy;
		}
		return document.documentElement.scrollTop;
	}

	public static function getLeft() : Int untyped {
		var sx = window.pageXOffset;
		if( __js__("typeof")(sx) == 'number' )
			return sx;
		if( document.body ) {
			sx = document.body.scrollLeft;
			if( sx ) return sx;
		}
		return document.documentElement.scrollLeft;
	}

	public static function set(left:Int,top:Int) untyped {
		window.scroll(left,top);
	}

}