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

/**
	This class provides a convenient way of working with paths. It supports the
	common path formats:
		directory1/directory2/filename.extension
		directory1\directory2\filename.excention
**/
class Path {

	/**
		The directory.
		
		This is the leading part of the path that is not part of the file name
		and the extension.
		
		Does not end with a / or \ separator.
		
		If the path has no directory, the value is null.
	**/
	public var dir : String;
	
	/**
		The file name.
		
		This is the part of the part between the directory and the extension.
		
		If there is no file name, e.g. for ".htaccess" or "/dir/", the value
		is the empty String "".
	**/
	public var file : String;
	
	/**
		The file extension.
		
		It is separated from the file name by a dot. This dot is not part of
		the extension.
		
		If the path has no extension, the value is null.
	**/
	public var ext : String;
		
	/**
		True if the last directory separator is a backslash, false otherwise.
	**/
	public var backslash : Bool;

	/**
		Creates a new Path instance by parsing [path].
		
		Path information can be retrieved by accessing the dir, file and ext
		properties.
	**/
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

	/**
		Returns a String representation of [this] path.
		
		If [this].backslash is true, backslash is used as directory separator,
		otherwise slash is used. This only affects the separator between
		[this].dir and [this].file.
		
		If [this].directory or [this].extension is null, their representation
		is the empty String "".
	**/
	public function toString() {
		return (if( dir == null ) "" else dir + if( backslash ) "\\" else "/") + file + (if( ext == null ) "" else "." + ext);
	}

	/**
		Returns the String representation of [path] without the file extension.
		
		If [path] is null, the result is unspecified.
	**/
	public static function withoutExtension( path : String ) {
		var s = new Path(path);
		s.ext = null;
		return s.toString();
	}

	/**
		Returns the String representation of [path] without the directory.
		
		If [path] is null, the result is unspecified.
	**/
	public static function withoutDirectory( path ) {
		var s = new Path(path);
		s.dir = null;
		return s.toString();
	}

	/**
		Returns the directory of [path].
		
		If the directory is null, the empty String "" is returned.
		
		If [path] is null, the result is unspecified.
	**/
	public static function directory( path ) {
		var s = new Path(path);
		if( s.dir == null )
			return "";
		return s.dir;
	}

	/**
		Returns the extension of [path].
		
		If the extension is null, the empty String "" is returned.
		
		If [path] is null, the result is unspecified.
	**/
	public static function extension( path ) {
		var s = new Path(path);
		if( s.ext == null )
			return "";
		return s.ext;
	}

	/**
		Returns a String representation of [path] where the extension is [ext].
		
		If [path] has no extension, [ext] is added as extension.
		
		If [path] or [ext] are null, the result is unspecified.
	**/
	public static function withExtension( path, ext ) {
		var s = new Path(path);
		s.ext = ext;
		return s.toString();
	}

	/**
		Adds a trailing slash to [path], if it does not have one already.
		
		If the last slash in [path] is a backslash, a backslash is appended to
		[path].
		
		If the last slash in [path] is a slash, or if no slash is found, a slash
		is appended to [path]. In particular, this applies to the empty String
		"".
		
		If [path] is null, the result is unspecified.
	**/
	public static function addTrailingSlash( path : String ) : String {
		if (path.length == 0)
			return "/";
		var c1 = path.lastIndexOf("/");
		var c2 = path.lastIndexOf("\\");
		return if ( c1 < c2 ) {
			if (c2 != path.length - 1) path + "\\";
			else path;
		} else {
			if (c1 != path.length - 1) path + "/";
			else path;
		}
	}
}