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
package neko;

class NekoDate__ //implements Date
{
	private var __t : Void;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) {
		__t = date_now();
		date_set_day(__t,year,month,day);
		date_set_hour(__t,hour,min,sec);
	}

	public function getTime() : Float {
		return int32_to_float(__t) * 100;
	}

	public function toString():String {
		return new String(date_format(__t,null));
	}

	private static function now() {
		return new1(date_now());
	}

	private static function new1(t) {
		var d = new NekoDate__(2005,1,1,0,0,0);
		d.__t = t;
		return d;
	}

	static var date_new = Lib.load("std","date_new",1);
	static var date_now = Lib.load("std","date_now",0);
	static var date_format = Lib.load("std","date_format",2);
	static var date_set_hour = Lib.load("std","date_set_hour",4);
	static var date_set_day = Lib.load("std","date_set_day",4);
	static var int32_to_float = Lib.load("std","int32_to_float",1);

}


