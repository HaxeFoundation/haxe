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
import haxe.Md5;

class Template {

	static var VERSION = "0.9.0";

	public static var compiledFiles : Hash<Bool> = new Hash();
	public var execute : Dynamic -> String;

	public function new( file:String ){
		execute = if (Loader.OPTIMIZED) loadTemplate(nekoBin(file)) else fromFile(file);
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
		return Loader.TMP_DIR + nekoId(path) + ".n";
	}

	static function nekoSrc( path:String ) : String {
		return Loader.TMP_DIR + nekoId(path) + ".neko";
	}

	public static function fromFile( path:String ) : Dynamic -> String {
		if (Loader.OPTIMIZED)
			return loadTemplate(nekoBin(path));
		if (Loader.MACROS != null && mtwin.templo.Preprocessor.macroFileStamp == null)
			mtwin.templo.Preprocessor.registerMacroFile(Loader.BASE_DIR+Loader.MACROS);
		var binPath = nekoBin(path);
		if (neko.FileSystem.exists(binPath)){
			var macroStamp  = mtwin.templo.Preprocessor.macroFileStamp;
			var sourceStamp = neko.FileSystem.stat(Loader.BASE_DIR+"/"+path).mtime.getTime();
			var stamp       = neko.FileSystem.stat(binPath).mtime.getTime();
			if ((stamp >= sourceStamp) && (macroStamp == null || macroStamp < stamp)){
				return loadTemplate(binPath);
			}
		}
		compiledFiles.set(path,true);
		var content = neko.io.File.getContent(Loader.BASE_DIR+"/"+path);
		var isXhtml = StringTools.endsWith(path, ".mtt") || StringTools.endsWith(path,".html") || StringTools.endsWith(path,".tpl");
		return fromString(content, nekoId(path), isXhtml);
	}

	public static function fromString( src:String, ?id:String, ?isXhtml:Bool ) : Dynamic -> String {
		if (id == null){
			id = Md5.encode(src);
		}

		// remove BOM
		if (StringTools.startsWith(src, "\xEF\xBB\xBF"))
			src = src.substr(3);

		src = StringTools.replace(src, "\r\n", "\n");
		src = StringTools.replace(src, "\r", "\n");

		src = mtwin.templo.Preprocessor.process(src);

		var path = nekoSrc(id);
		var x = null;
		try {
			x = Xml.parse(src);
		}
		catch (e:Dynamic){
			var source : String = src;
			var str = Std.string(e);
			var reg = ~/Xml parse error : .* at line ([0-9]+)/gs;
			if (reg.match(str)){
				var margin = 10;
				var line = Std.parseInt(reg.matched(1));
				var lines = src.split("\n");
				var start = Std.int(Math.max(0,line-margin));
				var splice = lines.splice(start, line+margin);
				var splice = Lambda.map(splice, function(s:String){ return (start++) + " : " + s; });
				source = splice.join("\n");
			}
			throw { message:"Error in "+id+"\n"+str, source:source };
		}

		var p = new mtwin.templo.Parser(isXhtml == true);
		var s = p.parse(x);

		s = "// generated from " + id + "\n// temploc v"+mtwin.templo.Template.VERSION+"\n" + s;

		var f = neko.io.File.write(path, false);
		f.writeString(s);
		f.close();

		var r = null;
		if ((neko.Sys.systemName()=="Windows")&&(neko.Sys.getEnv("OS")!="Windows_NT")){
		    // Neither Windows 95 or Windows 98 support the 2> redirect
		    r = neko.Sys.command("nekoc -o \""+Loader.TMP_DIR+"\" \""+path+"\" > \""+Loader.TMP_DIR+"nekoc.out\"");
		}
		else {
		    r = neko.Sys.command("nekoc -o \""+Loader.TMP_DIR+"\" \""+path+"\" 2> \""+Loader.TMP_DIR+"nekoc.out\"");
		}
		if (r != 0){
			if (neko.FileSystem.exists(Loader.TMP_DIR+"nekoc.out")){
				throw "nekoc compilation of "+path+" failed ("+r+") : "+neko.io.File.getContent(Loader.TMP_DIR+"nekoc.out");
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
			loader.Array = Array;
			loader.iter = function(loop : Dynamic, fnc){
				if (loop == null){
					throw "repeat or foreach called on null value";
				}
				if (loop.iterator != null){
					var it : Iterable<Dynamic> = loop;
					for (v in it.iterator()) fnc(v);
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
