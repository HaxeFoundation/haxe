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
import haxe.macro.Context;

class ImportAll {

	public static function run( ?pack ) {
		if( pack == null ) {
			pack = "";
			haxe.macro.Compiler.define("doc_gen");
		}
		switch( pack ) {
		case "php":
			if( !Context.defined("php") ) return;
		case "neko":
			if( !Context.defined("neko") ) return;
		case "js":
			if( !Context.defined("js") ) return;
		case "cpp":
			if( !Context.defined("cpp") ) return;
		case "flash8":
			if( !Context.defined("flash") || Context.defined("flash9") ) return;
		case "flash":
			if( !Context.defined("flash9") ) return;
		case "mt","mtwin":
			return;
		case "sys":
			if( !Context.defined("neko") && !Context.defined("php") && !Context.defined("cpp") ) return;
		case "java":
			if( !Context.defined("java") ) return;
		case "cs":
			if( !Context.defined("cs") ) return;
		case "tools":
			return;
		case "build-tool":
			return;
		}
		for( p in Context.getClassPath() ) {
			if( p == "/" )
				continue;
			// skip if we have a classpath to haxe
			if( pack.length == 0 && sys.FileSystem.exists(p+"std") )
				continue;
			var p = p + pack.split(".").join("/");
			if( StringTools.endsWith(p,"/") )
				p = p.substr(0,-1);
			if( !sys.FileSystem.exists(p) || !sys.FileSystem.isDirectory(p) )
				continue;
			for( file in sys.FileSystem.readDirectory(p) ) {
				if( file == ".svn" || file == "_std" )
					continue;
				var full = (pack == "") ? file : pack + "." + file;
				if( StringTools.endsWith(file, ".hx") ) {
					var cl = full.substr(0, full.length - 3);
					if( StringTools.startsWith(cl,"flash8.") )
						cl = "flash."+cl.substr(7);
					switch( cl ) {
					case "ImportAll", "neko.db.MacroManager": continue;
					case "haxe.TimerQueue": if( Context.defined("neko") || Context.defined("php") || Context.defined("cpp") ) continue;
					case "Sys": if( !(Context.defined("neko") || Context.defined("php") || Context.defined("cpp")) ) continue;
					case "haxe.web.Request": if( !(Context.defined("neko") || Context.defined("php") || Context.defined("js")) ) continue;
					case "haxe.macro.ExampleJSGenerator","haxe.macro.Context", "haxe.macro.Compiler": if( !Context.defined("neko") ) continue;
					case "haxe.remoting.SocketWrapper": if( !Context.defined("flash") ) continue;
					case "haxe.remoting.SyncSocketConnection": if( !(Context.defined("neko") || Context.defined("php") || Context.defined("cpp")) ) continue;
					}
					Context.getModule(cl);
				} else if( sys.FileSystem.isDirectory(p + "/" + file) )
					run(full);
			}
		}
	}

}
