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
package haxe.io;

class Path {

	public var ext : String;
	public var dir : String;
	public var file : String;
	public var backslash : Bool;

	public function new( path : String ) {
		var c1 = path.lastIndexOf("/");
		var c2 = path.lastIndexOf("\\");
		if( c1 < c2 ) {
			dir = path.substr(0,c2);
			path = path.substr(c2+1);
			backslash = true;
		} else if( c2 < c1 ) {
			dir = path.substr(0,c1);
			path = path.substr(c1+1);
		} else
			dir = null;
		var cp = path.lastIndexOf(".");
		if( cp != -1 ) {
			ext = path.substr(cp+1);
			file = path.substr(0,cp);
		} else {
			ext = null;
			file = path;
		}
	}

	public function toString() {
		return (if( dir == null ) "" else dir + if( backslash ) "\\" else "/") + file + (if( ext == null ) "" else "." + ext);
	}

	public static function withoutExtension( path : String ) {
		var s = new Path(path);
		s.ext = null;
		return s.toString();
	}

	public static function withoutDirectory( path ) {
		var s = new Path(path);
		s.dir = null;
		return s.toString();
	}

	public static function directory( path ) {
		var s = new Path(path);
		if( s.dir == null )
			return "";
		return s.dir;
	}

	public static function extension( path ) {
		var s = new Path(path);
		if( s.ext == null )
			return "";
		return s.ext;
	}

	public static function withExtension( path, ext ) {
		var s = new Path(path);
		s.ext = ext;
		return s.toString();
	}

}