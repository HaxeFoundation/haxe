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

	private var _ip : String;
	public var ip(default,null) : Int;

	public function new( name : String ) : Void {
		if(~/^(\d{1,3}\.){3}\d{1,3}$/.match(name)) {
		  _ip = name;
		} else {
			_ip = untyped __call__('gethostbyname', name);
			if(_ip == name) {
				ip = 0;
				return;
			}
		}
		var p = _ip.split('.');
		ip = untyped __call__('intval', __call__('sprintf', '%02X%02X%02X%02X', p[3], p[2], p[1], p[0]), 16);
	}

	public function toString() : String {
		return _ip;
	}

	public function reverse() : String {
		return untyped __call__('gethostbyaddress', _ip);
	}

	public static function localhost() : String {
		return untyped __var__('_SERVER', 'HTTP_HOST');
	}
}
