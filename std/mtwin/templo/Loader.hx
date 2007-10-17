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

package mtwin.templo;

import neko.Sys;
import neko.io.File;
import neko.FileSystem;

class Loader {

	public static var BASE_DIR = "";
	public static var TMP_DIR = "/tmp/";
	public static var MACROS = "macros.mtt";
	public static var OPTIMIZED = false;

	public var execute : Dynamic -> String;

	public function new( file:String ) {
		if( !OPTIMIZED )
			compileTemplate(file);
		execute = loadTemplate(tmpFileId(file));
	}

	static function tmpFileId( path:String ) : String {
		var rpath = path;
		var temp = path;
		if( temp.charAt(0) == "/" ) temp = temp.substr(1, temp.length-1);
		temp = temp.split("/").join("__");
		temp = temp.split("\\").join("__");
		temp = temp.split(":").join("__");
		temp = temp.split("____").join("__");
		return TMP_DIR + temp + ".n";
	}

	static function compileTemplate( path:String ) : Void {
		if( FileSystem.exists(tmpFileId(path)) ) {
			var macroStamp = if( FileSystem.exists(BASE_DIR+MACROS) ) FileSystem.stat(BASE_DIR+MACROS).mtime.getTime() else null;
			var sourceStamp = FileSystem.stat(BASE_DIR+path).mtime.getTime();
			var stamp = FileSystem.stat(tmpFileId(path)).mtime.getTime();
			if( stamp >= sourceStamp && (macroStamp == null || macroStamp < stamp) )
				return;
		}
		var result = 0;

		var macroArg = if (MACROS == null) "" else "-m \""+BASE_DIR+MACROS+"\"";

		if (BASE_DIR == "")
			result = Sys.command("temploc -s "+macroArg+" -o \""+TMP_DIR+"\" \""+path+"\" 2> \""+TMP_DIR+"temploc.out\"");
		else
			result = Sys.command("temploc -s "+macroArg+" -o \""+TMP_DIR+"\" -r \""+BASE_DIR+"\" \""+path+"\" 2> \""+TMP_DIR+"temploc.out\"");
		if( result != 0 )
			throw "temploc compilation or "+path+" failed : "+neko.io.File.getContent(Loader.TMP_DIR+"temploc.out");
	}

	static function loadTemplate( nPath:String ) : Dynamic -> String {
		return untyped {
			var loader = __dollar__loader;
			var oldCache = loader.cache;
			loader.cache = __dollar__new(oldCache);
			loader.String = String;
			loader.Array = Array;
			loader.iter = function(loop : Dynamic, fnc){
				if (loop == null){
					throw "repeat or foreach called on null value";
				}
				if (loop.iterator != null){
					var it : Iterable<Dynamic> = loop;
					for (v in it.iterator() ) fnc(v);
				}
				else if (loop.hasNext != null && loop.next != null){
					var it : Iterator<Dynamic> = loop;
					for (v in it) fnc(v);
				}
				else {
					throw "repeat or foreach called on non iterable object";
				}
			};
			var code = loader.loadmodule(nPath.__s, loader);
			loader.cache = oldCache;
			function(context){
				var wrapCache = loader.cache;
				loader.cache = __dollar__new(wrapCache);
				var macro = function(path){
					if (mtwin.templo.Loader.OPTIMIZED == false){
						mtwin.templo.Loader.compileTemplate(new String(path));
					}
					return loader.loadmodule(mtwin.templo.Loader.tmpFileId(new String(path)).__s, loader);
				}
				var result = new String(code.template(macro, context));
				loader.cache = wrapCache;
				return result;
			}
		}
	}
}
