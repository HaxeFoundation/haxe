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

class Main {

	static var VERSION = "0.2 -- haxe flavoured";
	static var USAGE = "Usage: temploc -o </destination/dir> -m <macrofile.xml> -r </templates/repository> <files...>\n";
	static var args : Array<String>;
	static var files : List<String>;
	static var silence : Bool = false;

	static function parseArgs(){
		files = new List();
		args = neko.Sys.args();
		var i = 0;
		var macros = null;
		while (i < args.length){
			var arg = args[i];
			switch (arg){
				case "-h":
					throw USAGE;

				case "-s":
					silence = true;

				case "-r":
					var value = args[i+1];
					if (value.charAt(value.length-1) == "/"){
						value = value.substr(0, value.length-1);
					}
					Loader.BASE_DIR = value;
					++i;

				case "-o":
					var value = args[i+1];
					if (value.charAt(value.length-1) != "/"){
						value = value + "/";
					}
					Loader.TMP_DIR = value;
					++i;

				case "-m":
					i++;
					macros = args[i];
					mtwin.templo.Preprocessor.registerMacroFile(macros);

				default:
					files.push(arg);
			}
			++i;
		}
		if( macros != null )
			files.remove(macros);
		if (args.length == 0){
			neko.Lib.print("temploc - v"+VERSION+"\n");
			neko.Lib.print(USAGE);
		}
		else if (files.length == 0){
			neko.Lib.print("temploc - v"+VERSION+"\n");
			neko.Lib.print(USAGE);
		}
		else if (Loader.BASE_DIR == null){
			var sampleFile = Lambda.array(files)[0];
			var pslah = sampleFile.lastIndexOf("/",sampleFile.length);
			var aslah = sampleFile.lastIndexOf("/",sampleFile.length);
			var pos = Std.int(Math.max(pslah, aslah));
			if (pos == -1){
				neko.Lib.print("missing template BASE_DIR\n");
				throw USAGE;
			}
			Loader.BASE_DIR = sampleFile.substr(0,pos);
		}
	}

	static function mtime(file:String) : Float {
		return neko.FileSystem.stat(file).mtime.getTime();
	}

	static function main(){
		Loader.MACROS = null;
		parseArgs();
		for (file in files){
			file = StringTools.replace(file, Loader.BASE_DIR+"/", "");
			if (!silence) neko.Lib.print("* "+file+"...");
			mtwin.templo.Template.fromFile(file);
			if (!silence) neko.Lib.print(" done\n");
		}
	}
}
