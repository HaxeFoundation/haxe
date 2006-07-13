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

class Template {

	/** Enable production mode (no file check). */
	public static var OPTIMIZED = false;
	/** Destination of .neko and .n files */
	public static var TMP_DIR = "/tmp/";
	/** Templates repository */
	public static var BASE_DIR = null;
	/** Macros file */
	public static var MACROS = "macros.mtt";

	public var execute : Dynamic -> String;

	public function new( file:String ){
		execute = if (OPTIMIZED) loadTemplate(nekoBin(file)) else fromFile(file);
	}

	static function nekoId( path:String ) : String {
		var rpath = path;
		var temp = path;
		if (temp.charAt(0) == "/") temp = temp.substr(1, temp.length-1);
		temp = StringTools.replace(temp, "/", "__");
		temp = StringTools.replace(temp, "\\", "__");
		temp = StringTools.replace(temp, ":", "__");
		temp = StringTools.replace(temp, "____", "__");
		return temp;
	}

	static function nekoBin( path:String ) : String {
		return TMP_DIR + nekoId(path) + ".n";
	}

	static function nekoSrc( path:String ) : String {
		return TMP_DIR + nekoId(path) + ".neko";
	}

	public static function fromFile( path:String ) : Dynamic -> String {
		if (OPTIMIZED)
			return loadTemplate(nekoBin(path));
		if (MACROS != null && mtwin.templo.Preprocessor.macroFileStamp == null)
			mtwin.templo.Preprocessor.registerMacroFile(BASE_DIR+MACROS);
		var binPath = nekoBin(path);
		if (neko.FileSystem.exists(binPath)){
			var macroStamp  = mtwin.templo.Preprocessor.macroFileStamp;
			var sourceStamp = neko.FileSystem.stat(BASE_DIR+"/"+path).mtime.getTime();
			var stamp       = neko.FileSystem.stat(binPath).mtime.getTime();
			if ((stamp >= sourceStamp) && (macroStamp == null || macroStamp < stamp)){
				return loadTemplate(binPath);
			}
		}
		var content = neko.File.getContent(BASE_DIR+"/"+path);
		return fromString(content, nekoId(path));
	}

	public static function fromString( src:String, ?id:String ) : Dynamic -> String {
		if (id == null){
			id = Md5.encode(src);
		}
		src = mtwin.templo.Preprocessor.process(src);

		var path = nekoSrc(id);
		var x = null;
		try {
			x = Xml.parse(src);
		}
		catch (e:Dynamic){
			throw { message:"Error in "+id+"\n"+Std.string(e), source:src };
		}

		var p = new mtwin.templo.Parser();
		var s = p.parse(x);

		s = "// generated from " + id + "\n//" + src.split("\n").join("//") + "\n" + s;

		var f = neko.File.write(path, false);
		f.write(s);
		f.close();

		var r = neko.Sys.command("nekoc -o "+TMP_DIR+" "+path+" 2> "+TMP_DIR+"/nekoc.out");
		if (r != 0){
			if (neko.FileSystem.exists(TMP_DIR+"/nekoc.out")){
				throw "nekoc compilation of "+path+" failed ("+r+") : "+neko.File.getContent(TMP_DIR+"/nekoc.out");
			}
			else {
				throw "nekoc compilation of "+path+" failed ("+r+") -- no nekoc.out available";
			}
		}

		return loadTemplate(nekoBin(id));
	}

	static function loadTemplate( nPath:String ) : Dynamic -> String {
		return untyped {
			var loader = __dollar__loader;
			var oldCache = loader.cache;
			loader.cache = __dollar__new(oldCache);
			loader.String = String;
			loader.iter = function(loop, fnc){
				if (loop == null){
					throw "repeat or foreach called on null value";
				}
				if (loop.iterator != null){
					for (v in loop.iterator()){ fnc(v); }
				}
				else if (loop.hasNext != null && loop.next != null){
					for (v in loop){ fnc(v); }
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
					mtwin.templo.Template.fromFile(new String(path));
					return loader.loadmodule(mtwin.templo.Template.nekoBin(new String(path)).__s, loader);
				}
				var result = new String(code.template(macro, context));
				loader.cache = wrapCache;
				return result;
			}
		}
	}
}
