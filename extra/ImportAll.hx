/*
 * Copyright (C)2005-2018 Haxe Foundation
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
import haxe.macro.Context;

class ImportAll {

	static function isSysTarget() {
		return Context.defined("neko") || Context.defined("php") || Context.defined("cpp") ||
		       Context.defined("java") || Context.defined("python") ||
		       Context.defined("lua") || Context.defined("hl") || Context.defined("eval"); // TODO: have to add cs here, SPOD gets in the way at the moment
	}

	public static function run( ?pack ) {
		if( pack == null ) {
			pack = "";
			haxe.macro.Compiler.define("doc_gen");
		}
		if (Context.defined("interp")) {
			haxe.macro.Compiler.define("macro");
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
		case "flash":
			if( !Context.defined("flash9") ) return;
		case "mt","mtwin":
			return;
		case "sys":
			if(!isSysTarget()) return;
		case "sys.thread":
			if ( !Context.defined("target.threaded") ) return;
		case "java":
			if( !Context.defined("java") ) return;
		case "jvm":
			if( !Context.defined("jvm") ) return;
		case "cs":
			if( !Context.defined("cs") ) return;
		case "python":
			if ( !Context.defined("python") ) return;
		case "hl":
			if( !Context.defined("hl") ) return;
		case "lua":
			if( !Context.defined("lua") ) return;
		case "eval":
			if( !Context.defined("eval") ) return;
		case "ssl":
			if (!Context.defined("neko") && !Context.defined("cpp")) return;
		case "tools", "build-tool", "jar-tool": return;
		}
		for( p in Context.getClassPath() ) {
			if( p == "/" || p == "" )
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
				if( StringTools.endsWith(file, ".hx") && file.substr(0, file.length - 3).indexOf(".") < 0 ) {
					var cl = full.substr(0, full.length - 3);
					switch( cl ) {
					case "ImportAll", "neko.db.MacroManager": continue;
					case "haxe.TimerQueue": if( Context.defined("neko") || Context.defined("php") || Context.defined("cpp") ) continue;
					case "Sys": if(!isSysTarget()) continue;
					case "haxe.web.Request": if( !(Context.defined("neko") || Context.defined("php") || Context.defined("js")) ) continue;
					case "haxe.macro.ExampleJSGenerator","haxe.macro.Context", "haxe.macro.Compiler": if( !Context.defined("eval") ) continue;
					case "haxe.remoting.SocketWrapper": if( !Context.defined("flash") ) continue;
					case "haxe.remoting.SyncSocketConnection": if( !(Context.defined("neko") || Context.defined("php") || Context.defined("cpp")) ) continue;
					case "neko.vm.Ui" | "sys.db.Sqlite" | "sys.db.Mysql" if ( Context.defined("interp") ): continue;
					case "sys.db.Sqlite" | "sys.db.Mysql" | "cs.db.AdoNet" if ( Context.defined("cs") ): continue;
					case "haxe.Atomic" if(!Context.defined("target.atomics")): continue;
					}
					Context.getModule(cl);
				} else if( sys.FileSystem.isDirectory(p + "/" + file) )
					run(full);
			}
		}
	}

}
