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
package sys.net;

@:coreApi
class Host {

	public var ip(default,null) : Int;

	public function new( name : String ) : Void {
		ip = host_resolve(name);
	}

	public function toString() : String {
		return new String(host_to_string(ip));
	}

	public function reverse() : String {
		return new String(host_reverse(ip));
	}

	public static function localhost() : String {
		return new String(host_local());
	}

	static function __init__() : Void {
		cpp.Lib.load("std","socket_init",0)();
	}

	private static var host_resolve = cpp.Lib.load("std","host_resolve",1);
	private static var host_reverse = cpp.Lib.load("std","host_reverse",1);
	private static var host_to_string = cpp.Lib.load("std","host_to_string",1);
	private static var host_local = cpp.Lib.load("std","host_local",0);

}
