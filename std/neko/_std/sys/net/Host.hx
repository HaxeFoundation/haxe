/*
 * Copyright (c) 2005-2012, The haXe Project Contributors
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

	public var ip(default,null) : Int;

	public function new( name : String ) : Void {
		ip = host_resolve(untyped name.__s);
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
		neko.Lib.load("std","socket_init",0)();
	}

	private static var host_resolve = neko.Lib.load("std","host_resolve",1);
	private static var host_reverse = neko.Lib.load("std","host_reverse",1);
	private static var host_to_string = neko.Lib.load("std","host_to_string",1);
	private static var host_local = neko.Lib.load("std","host_local",0);

}
