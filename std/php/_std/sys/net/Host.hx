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
 *
 */
package sys.net;

@:core_api
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
