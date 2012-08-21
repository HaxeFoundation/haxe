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

typedef ProxySettings = {
	var host : String;
	var port : Int;
	var auth : {
		var user : String;
		var pass : String;
	};
}

class ProxyDetect {

	static function parseSettings( settings : String ) {
		var r = ~/^([^:]+):([^@]*)@([^:]+):([0-9]+)$/;
		if( r.match(settings) )
			return {
				auth : {
					user : r.matched(1),
					pass : r.matched(2),
				},
				host : r.matched(3),
				port : Std.parseInt(r.matched(4)),
			};
		var r = ~/^([^:]+):([0-9]+)$/;
		if( !r.match(settings) ) {
			var r2 = ~/http=([^:]+):([0-9]+)/;
			if( !r2.match(settings) )
				throw "Invalid settings '"+settings+"'";
			r = r2;
		}
		return {
			host : r.matched(1),
			port : Std.parseInt(r.matched(2)),
			auth : null,
		};
	}

	static function detectFF( basedir : String ) {
		var files = try sys.FileSystem.readDirectory(basedir) catch( e : Dynamic ) return null;
		var profile = null;
		for( f in files )
			if( f.substr(-8) == ".default" ) {
				profile = f;
				break;
			}
		if( profile == null )
			return null;
		var prefs = sys.io.File.getContent(basedir+"/"+profile+"/prefs.js");
		// enabled ?
		var r = ~/user_pref\("network\.proxy\.type", 1\);/;
		if( !r.match(prefs) )
			return null;
		// prefs
		var r = ~/user_pref\("network\.proxy\.http", "([^"]+)"\);/;
		if( !r.match(prefs) )
			return null;
		var host = r.matched(1);
		var r = ~/user_pref\("network\.proxy\.http_port", ([0-9]+)\);/;
		if( !r.match(prefs) )
			return null;
		var port = r.matched(1);
		return parseSettings(host+":"+port);
	}

	static function detectIE() {
		var dir = Sys.getEnv("TMP");
		if( dir == null )
			dir = ".";
		var temp = dir + "/proxy.txt";
		if( Sys.command('regedit /E "'+temp+'" "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings"') != 0 ) {
			// might fail without appropriate rights
			return null;
		}
		// it's possible that if registry access was disabled the proxy file is not created
		var content = try sys.io.File.getContent(temp) catch( e : Dynamic ) return null;
		sys.FileSystem.deleteFile(temp);
		// turn 16-bit string into 8-bit one
		var b = new StringBuf();
		var p = 0;
		while( p < content.length ) {
			b.addChar(content.charCodeAt(p));
			p += 2;
		}
		content = b.toString();
		// enabled ?
		var renabled = ~/"ProxyEnable"=dword:0000000(0|1)/;
		if( !renabled.match(content) )
			return null;
		if( renabled.matched(1) == "0" )
			return null;
		// value ?
		var rproxy = ~/"ProxyServer"="([^"]+)"/;
		if( !rproxy.match(content) )
			return null;
		return parseSettings(rproxy.matched(1));
	}

	static function parseOSXConfiguration(xml : Xml) : Dynamic {
		switch( xml.nodeName ) {
		case "dict":
			var o = {};
			var it = xml.elements();
			for( x in it ) {
				if( x.nodeName != "key" ) throw "Missing key";
				var v = x.firstChild().nodeValue;
				var r = parseOSXConfiguration(it.next());
				Reflect.setField(o,v,r);
			}
			return o;
		case "string":
			return xml.firstChild().nodeValue;
		case "integer":
			return Std.parseInt(xml.firstChild().nodeValue);
		case "array":
			var a = new Array();
			for( x in xml.elements() )
				a.push(parseOSXConfiguration(x));
			return a;
		case "true":
			return true;
		case "false":
			return false;
		case "data":
			return xml.firstChild().nodeValue;
		default:
			throw "Invalid value type '"+xml.nodeName+"'";
		}
	}

	static function detectOSX() {
		var prefs = sys.io.File.getContent("/Library/Preferences/SystemConfiguration/preferences.plist");
		var xml = Xml.parse(prefs).firstElement().firstElement(); // plist/dict
		var data : Dynamic = parseOSXConfiguration(xml);
		for( nsname in Reflect.fields(data.NetworkServices) ) {
			var ns : Dynamic = Reflect.field(data.NetworkServices,nsname);
			if( ns.Proxies != null && ns.Proxies.HTTPEnable == 1 )
				return { host : ns.Proxies.HTTPProxy, port : ns.Proxies.HTTPPort, auth : null };
		}
		return null;
	}

	static function detectAll() : ProxySettings {
		switch( Sys.systemName() ) {
		case "Windows":
			try {
				var ffdir = Sys.getEnv("APPDATA")+"/Mozilla/Firefox/Profiles";
				var p = detectFF(ffdir);
				if( p == null )
					throw "No Firefox proxy";
				return p;
			} catch( e : Dynamic ) {
				return detectIE();
			}
		case "Mac":
			var p = detectOSX();
			if( p != null )
				return p;
			var ffdir = Sys.getEnv("HOME")+"/Library/Application Support/Firefox/Profiles";
			return detectFF(ffdir);
		case "Linux":
			var ffdir = Sys.getEnv("HOME")+"/.mozilla/firefox";
			return detectFF(ffdir);
		default:
			throw "This system is not supported";
		}
	}

	static var save : { r : ProxySettings } = null;

	public static function detect() {
		if( save == null )
			save = { r : try detectAll() catch( e : Dynamic ) null };
		return save.r;
	}

	static function main() {
		trace(detect());
	}

}
