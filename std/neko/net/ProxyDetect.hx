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
		if( !r.match(settings) )
			throw "Invalid settings '"+settings+"'";
		return {
			host : r.matched(1),
			port : Std.parseInt(r.matched(2)),
			auth : null,
		};
	}

	static function detectFF( basedir : String ) {
		var profile = neko.FileSystem.readDirectory(basedir).pop();
		var prefs = neko.io.File.getContent(basedir+"/"+profile+"/prefs.js");
		var r = ~/user_pref\("network\.proxy\.http", "([^"]+)"\);/;
		if( !r.match(prefs) )
			return null;
		var host = r.matched(1);
		var r = ~/user_pref\("network\.proxy\.http_port", ([0-9]+)\);/;
		if( !r.match(prefs) )
			throw "No http_port in prefs.js";
		var port = r.matched(1);
		return parseSettings(host+":"+port);
	}

	static function detectIE() {
		var temp = neko.Sys.getEnv("TMP") + "/proxy.txt";
		if( neko.Sys.command('regedit /E "'+temp+'" "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings"') != 0 )
			throw "Failed to call REGEDIT";
		var content = neko.io.File.getContent(temp);
		neko.FileSystem.deleteFile(temp);
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
			throw "Could not find 'ProxyEnable'";
		if( renabled.matched(1) == "0" )
			return null;
		// value ?
		var rproxy = ~/"ProxyServer"="([^"]+)"/;
		if( !rproxy.match(content) )
			throw "Could not find 'ProxyServer'";
		return parseSettings(rproxy.matched(1));
	}

	static function detectAll() : ProxySettings {
		switch( neko.Sys.systemName() ) {
		case "Windows":
			try {
				var ffdir = neko.Sys.getEnv("APPDATA")+"/Mozilla/Firefox/Profiles";
				var p = detectFF(ffdir);
				if( p == null )
					throw "No Firefox proxy";
				return p;
			} catch( e : Dynamic ) {
				return detectIE();
			}
		case "Mac":
			var ffdir = neko.Sys.getEnv("HOME")+"/Library/Application Support/Firefox/Profiles";
			return detectFF(ffdir);
		default:
			throw "This system is not supported";
		}
	}

	static var save : { r : ProxySettings } = null;

	static function detect() {
		if( save == null )
			save = { r : detectAll() };
		return save.r;
	}

	static function main() {
		trace(detect());
	}

}
