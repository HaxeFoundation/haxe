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
package neko.net;

class Poll {

	var d : Void;

	public function new( n : Int ) {
		d = socket_poll_alloc(n);
	}

	public function poll( a : Array<Socket>, ?t : Float ) : Array<Socket> {
		untyped {
			var c = __dollar__hnew(16);
			var r = __dollar__amake(a.length);
			var i = 0;
			var len = a.length;
			while( i < len ){
				r[i] = a[i].__s;
				__dollar__hadd(c,a[i].__s,a[i]);
				i += 1;
			}
			r = socket_poll(r,d,t);
			i = 0;
			len = __dollar__asize(r);
			while( i < len ) {
				r[i] = __dollar__hget(c,r[i],null);
				i += 1;
			}
			return Array.new1(r,len);
		}
	}

	static var socket_poll_alloc = neko.Lib.load("std","socket_poll_alloc",1);
	static var socket_poll = neko.Lib.load("std","socket_poll",3);

}
