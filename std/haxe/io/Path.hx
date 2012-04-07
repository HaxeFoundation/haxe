/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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