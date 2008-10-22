/*
 * Copyright (c) 2006, Motion-Twin
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
 * THIS SOFTWARE IS PROVIDED BY MOTION-TWIN "AS IS" AND ANY
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

package mtwin.web;
import neko.Web;

class Request {

	var pathInfoParts  : Array<String>;
	var params         : Hash<String>;

	public function new( ?uri ) {
		pathInfoParts = (uri == null ? neko.Web.getURI() : uri).split( "/" );
		pathInfoParts.shift();
		params = Web.getParams();
	}

	public function getPathInfoPart( level:Int ) : String {
		if( pathInfoParts.length > level )
			return pathInfoParts[ level ];
		return "";
	}

	public function setParams( list : Hash<String> ) {
		params = new Hash();
		for( k in list.keys() ){
			params.set( k, list.get(k) );
		}
	}

	public function getParamsObject( ?keys : List<String> ) : Dynamic<String> {
		var ret : Dynamic<String> = cast {};
		if( keys == null )
			for( k in params.keys() )
				Reflect.setField( ret, k, params.get(k) );
		else
			for( k in keys )
				Reflect.setField( ret, k, params.get(k) );
		return ret;
	}

	public function set( key : String , value : String ) {
		params.set(key,value);
	}

	public function get( key : String , ?or : String ) : String {
		if( params.exists( key ) ) return params.get( key );
		return or;
	}

	public function getInt( key : String, ?or : Int ) : Int {
		if( params.exists(key) ){
			var v = params.get(key);
			if( v == "NULL" )
				return null;
			return Std.parseInt(v);
		}
		return or;
	}

	public function getFloat( key : String, ?or : Float ) : Float {
		if( params.exists(key) ){
			var v = params.get(key);
			if( v == "NULL" )
				return null;
			return Std.parseFloat(v);
		}
		return or;
	}

	public function getBool( key:String ) : Bool {
		var val = params.get(key);
		return (val != null) && (val == "1" || val == "true");
	}

	public function getURI() : String {
		return Web.getURI();
	}

	public function getReferer() : String {
		return Web.getClientHeader("Referer");
	}

	static var REG_IP = ~/^\s*([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)/;
	public function getIP() : String{
		var ip = Web.getClientIP();
		var xf = Web.getClientHeader("X-Forwarded-For");
		if( xf != null && REG_IP.match( xf ) ){
			var fip = REG_IP.matched(1);
			if( !~/^(127\.0\.0\.1|192\.168\..*|172\.16\..*|10\..*|224\..*|240\..*)$/.match(fip) )
				ip = fip;
		}
		return ip;
	}

	public function getIPs() : List<String> {
		var ret = new List();
		ret.add(Web.getClientIP());
		var xf = Web.getClientHeader("X-Forwarded-For");
		if( xf != null ){
			var a = xf.split(",");
			for( ip in a ){
				if( REG_IP.match( ip ) ){
					var fip = REG_IP.matched(1);
					if( !~/^(127\.0\.0\.1|192\.168\..*|172\.16\..*|10\..*|224\..*|240\..*)$/.match(fip) )
						ret.add(fip);
				}
			}
		}
		return ret;
	}

	public function exists( key ) {
		return params.exists( key );
	}

	public function toString() : String {
		var lst = new List();
		for (i in params.keys()){
			lst.add("['"+i+"'] => '"+params.get(i)+"'");
		}
		return lst.join(",\n");
	}
}
