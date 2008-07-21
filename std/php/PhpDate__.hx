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
package php;

class PhpDate__ //implements Date
{
	static var __name__ = ["Date"];
	private var __t : Float;

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) {
		__t = untyped __call__("mktime", hour, min, sec, month+1, day, year);
	}

	public function getTime() : Float {
		return __t*1000;
	}
	
	public function getPhpTime() : Float {
		return __t;
	}

	public function getFullYear() : Int {
		return untyped __call__("intval", __call__("date", "Y", this.__t));
	}

	public function getMonth() : Int {
		return -1 + untyped __call__("intval", __call__("date", "n", this.__t));
	}

	public function getDate() : Int {
		return untyped __call__("intval", __call__("date", "j", this.__t));
	}

	public function getHours() : Int {
		return untyped __call__("intval", __call__("date", "G", this.__t));
	}

	public function getMinutes() : Int {
		return untyped __call__("intval", __call__("date", "i", this.__t));
	}

	public function getSeconds() : Int {
		return untyped __call__("intval", __call__("date", "s", this.__t));
	}

	public function getDay() : Int {
		return untyped __call__("intval", __call__("date", "w", this.__t));
	}

	public function toString():String {
		return untyped __call__("date", "Y-m-d H:i:s", this.__t);
	}

	public static function now() {
		return fromPhpTime(untyped __call__("time"));
	}

	public static function fromPhpTime( t : Float ){
		var d = new PhpDate__(2000,1,1,0,0,0);
		d.__t = t;
		return d;
	}
	
	public static function fromTime( t : Float ){
		var d = new PhpDate__(2000,1,1,0,0,0);
		d.__t = t/1000;
		return d;
	}

	public static function fromString( s : String ) {
		return fromPhpTime(untyped __call__("strtotime", s));
	}
}


