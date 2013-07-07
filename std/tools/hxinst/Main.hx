/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package tools.hxinst;

class Main {

	static var SYS = Sys.systemName();
	static var NULL = if( SYS == "Windows" ) "NUL" else "/dev/null";
	#if xcross
	static var wnd : xcross.Winlog;
	#end

	var baseDir : String;
	var binDir : String;
	var libDir : String;
	var debug : Bool;

	function new(dbg) {
		debug = dbg;
		if( debug ) {
			libDir = "/usr/local/lib";
			binDir = "/usr/local/bin";
		} else {
			libDir = "/usr/lib";
			binDir = "/usr/bin";
		}
		baseDir = if( SYS == "Windows" ) {
			// use C:/ for Vista, Win7
			var baseDir = (Sys.getEnv("ALLUSERSPROFILE") == "C:\\ProgramData") ? "C:" : Sys.getEnv("ProgramFiles");
			baseDir + "/HaxeFoundation";
		} else
			libDir;
	}

	function newVersion(v1,v2) {
		if( v1 == null )
			return true;
		return (v1.major * 10000 + v1.minor * 100 + v1.build < v2.major * 10000 + v2.minor * 100 + v2.build);
	}

	function ask( txt ) {
		#if xcross
		return xcross.Api.confirm("Question",txt);
		#else
		var answer = null;
		while( true ) {
			Sys.print(txt+" [y/n] ");
			switch( Sys.stdin().readLine() ) {
			case "n": answer = false; break;
			case "y": answer = true; break;
			}
		}
		return answer;
		#end
	}

	function error( txt ) {
		#if xcross
		xcross.Api.error("Error",txt);
		#else
		Sys.stderr().writeString(txt+"\n");
		#end
		throw "Installation aborted";
	}

	function display( txt ) {
		#if xcross
		wnd.log(txt);
		#else
		Sys.println(txt);
		#end
		Sys.sleep(0.03);
	}

	function version(v : { major : Int, minor : Int, build : Int }, twoDigitMinor ) {
		var min = twoDigitMinor && v.minor < 10 ? "0"+v.minor : Std.string(v.minor);
		return v.major+"."+min+if( v.build > 0 ) "."+v.build else "";
	}

	function command( cmd ) {
		display("Execute "+cmd);
		if( Sys.command(cmd) != 0 )
			error("Command '"+cmd+"' failed !");
	}

	function commandOutput( cmd ) {
		var p = try new sys.io.Process(cmd,[]) catch( e : Dynamic ) return "";
		return p.stderr.readAll().toString() + p.stdout.readAll().toString();
	}

	function run() {
		try {
			install();
			display("Installation Completed");
			#if xcross
			xcross.Api.message("Done","Installation Completed");
			#end
		} catch( e : Dynamic ) {
			display("");
			display("");
			display("ERROR = "+Std.string(e));
			display(haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
			#if xcross
			xcross.Api.error("Error","Installation aborted");
			#end
		}
		#if xcross
		wnd.enabled = true;
		#end
	}

	function checkRights() {
		try {
			if( !sys.FileSystem.exists(baseDir) )
				sys.FileSystem.createDirectory(baseDir);
			var tmp = baseDir + "/.tmp.haxe.inst";
			var f = sys.io.File.write(tmp,true);
			f.close();
			sys.FileSystem.deleteFile(tmp);
			return true;
		} catch( e : Dynamic ) {
			#if xcross
			if( xcross.Api.authorize() )
				return false;
			#end
			var msg;
			if( SYS == "Windows" )
				msg = "You don't have administrative access on this computer,\nplease login on an administrator account.\nOnce haxe is installed, execute '"+baseDir+"\\haxesetup' on your own account.";
			else
				msg = "You don't have the rights to write in "+baseDir+", please run the installer using 'sudo'";
			try error(msg) catch( e : Dynamic ) {};
			return false;
		}
	}

	var cacheResult : Null<Bool>;
	function is64() {
		if( cacheResult != null )
			return cacheResult;
		var p = new sys.io.Process("uname", ["-m"]);
		var ret = p.stdout.readAll().toString();
		p.exitCode();
		cacheResult = ret.indexOf("x86_64") != -1;
		return cacheResult;
	}

	function install() {
		// CLEANUP
		var dirs = [
			"/usr/local/lib/neko",
			"/usr/local/lib/haxe",
			"/opt/neko",
			"/opt/haxe",
			"c:/motion-twin",
			"c:/program files/motion-twin",
		];
		for( d in dirs )
			if( !debug && sys.FileSystem.exists(d) )
				error("A previous Haxe/Neko version seems to be installed in '"+d+"', please remove it first");
		if( debug )
			display("DEBUG MODE ON");

		// PROXY
		var p = neko.net.ProxyDetect.detect();
		if( p != null ) {
			display("Testing proxy "+p.host+":"+p.port);
			haxe.Http.PROXY = p;
			try {
				haxe.Http.requestUrl("http://google.com");
			} catch( e : Dynamic ) {
				display("Could not connect on Google, trying with no proxy");
				haxe.Http.PROXY = null;
			}
		}

		// GET haxe Version
		display("Getting Local Haxe Version");
		var content = commandOutput("haxe");
		var r = ~/^Haxe Compiler ([0-9]+)\.([0-9]+)(\.([0-9]+))?/;
		var haxeVersion = null;
		if( r.match(content) ) {
			haxeVersion = {
				major : Std.parseInt(r.matched(1)),
				minor : Std.parseInt(r.matched(2)),
				build : Std.parseInt(r.matched(4))
			};
			if( haxeVersion.build == null ) haxeVersion.build = 0;
		}


		// GET Neko Version
		display("Getting Local Neko Version");
		var content = commandOutput("neko");
		var r = ~/^NekoVM ([0-9]+)\.([0-9]+)(\.([0-9]+))?/;
		var nekoVersion = null;
		if( r.match(content) )
			nekoVersion = {
				major : Std.parseInt(r.matched(1)),
				minor : Std.parseInt(r.matched(2)),
				build : Std.parseInt(r.matched(4))
			};

		// GET Haxe files list
		display("Getting Latest Haxe Version");
		var haxeFile = null;
		var r = ~/^haxe-([0-9]+)\.([0-9]+)(-win|-linux|-osx)(\.zip|\.tar\.gz)$/;
		for( f in haxe.Http.requestUrl("http://haxe.org/wiki/latest").split("\n") )
			if( r.match(f) ) {
				var pf = r.matched(3);
				switch( SYS ) {
				case "Windows": if( pf != "-win" ) continue;
				case "Linux": if( pf != "-linux" ) continue;
				case "Mac": if( pf != "-osx" ) continue;
				default: continue;
				}
				haxeFile = {
					file : f,
					version : {
						major : Std.parseInt(r.matched(1)),
						minor : Std.parseInt(r.matched(2)),
						build : 0,
					},
				};
				break;
			}
		if( haxeFile == null )
			error("No Haxe File found for your plaform");

		// GET Neko files list
		display("Getting Latest Neko Version");
		var nekoFile = null;
		var r = ~/^neko-([0-9]+)\.([0-9]+)(\.([0-9]+))?(-win|-linux|-osx|-linux64)(\.zip|\.tar\.gz)$/;
		for( f in haxe.Http.requestUrl("http://nekovm.org/latest.n").split("\n") )
			if( r.match(f) ) {
				var pf = r.matched(5);
				switch( SYS ) {
				case "Windows": if( pf != "-win" ) continue;
				case "Linux": if( pf != "-linux"+(is64() ? "64":"") ) continue;
				case "Mac": if( pf != "-osx" ) continue;
				default: continue;
				}
				nekoFile = {
					file : f,
					version : {
						major : Std.parseInt(r.matched(1)),
						minor : Std.parseInt(r.matched(2)),
						build : Std.parseInt(r.matched(4)),
					}
				};
				break;
			}
		if( nekoFile == null )
			error("No Haxe File found for your plaform");

		// ASK QUESTIONS IF OK TO INSTALL
		var needHaxe = newVersion(haxeVersion,haxeFile.version);
		var needNeko = newVersion(nekoVersion,nekoFile.version);
		if( !needHaxe && !needNeko ) {
			if( !ask("Both your Haxe and Neko versions are up-to-date, do you want to reinstall everything ?") )
				throw "Installation Aborted";
			needHaxe = true;
			needNeko = true;
		} else {
			var txt = "";
			if( needNeko ) {
				txt += "Neko "+version(nekoFile.version,false);
				if( needHaxe )
					txt += " and ";
			}
			if( needHaxe )
				txt += "Haxe "+version(haxeFile.version,true);
			if( !ask("Do you want to install "+txt+" ?") )
				error("Installation Aborted");
		}

		// DOWNLOAD
		if( needNeko )
			download("http://nekovm.org/_media",nekoFile.file);
		if( needHaxe )
			download("http://haxe.org/file",haxeFile.file);

		// INSTALL
		if( needNeko ) {
			copy(nekoFile.file,true);
			installNeko();
		}
		if( needHaxe ) {
			copy(haxeFile.file,false);
			installHaxe();
		}
	}

	static function logProgress( txt ) {
		#if xcross
		wnd.logProgress(txt);
		#else
		Sys.print(txt+"\r");
		#end
	}

	function download( url, file ) {
		if( sys.FileSystem.exists(file) ) {
			display("Using local version of "+file+", skipping download");
			return;
		}

		var str = new haxe.io.BytesOutput();
		var progress = new Progress(str);
		progress.update = function() {
			var p = progress.cur * 100 / progress.max;
			p = Std.int(p * 10) / 10;
			logProgress("Downloading "+file+" ("+p+"%)");
		};
		var h = new haxe.Http(url+"/"+file);
		var me = this;
		h.onError = function(e) {
			me.error(Std.string(e));
		};
		logProgress("Downloading "+file+"...");
		h.customRequest(false,progress);
		#if xcross
		wnd.log("");
		#else
		Sys.print("\n");
		#end

		var f = sys.io.File.write(file,true);
		f.write(str.getBytes());
		f.close();
	}

	function unzip( file ) {
		var ch = sys.io.File.read(file,true);
		var entries;
		if( haxe.io.Path.extension(file) == "zip" )
			entries = haxe.zip.Reader.readZip(ch);
		else {
			entries = new List();
			for( f in new format.tgz.Reader(ch).read() )
				entries.add({
					fileName : f.fileName,
					fileTime : f.fileTime,
					fileSize : f.fileSize,
					data : f.data,
					dataSize : f.data.length,
					compressed : false,
					crc32 : null,
				});
		}
		ch.close();
		return entries;
	}

	function copy( file, isNeko ) {
		var data = unzip(file);
		var dir = baseDir + "/" + if( isNeko ) "neko" else "haxe";
		if( !sys.FileSystem.exists(dir) )
			sys.FileSystem.createDirectory(dir);
		if( !isNeko ) {
			try {
				removeRec(dir+"/std");
			} catch( e : Dynamic ) {
			}
		}
		for( f in data ) {
			var path = f.fileName.split("/");
			path.shift(); // base directory
			if( path[path.length-1] == "" ) {
				path.pop();
				if( path.length == 0 )
					continue;
				var ddir = dir+"/"+path.join("/");
				display("Installing directory "+path.join("/"));
				if( !sys.FileSystem.exists(ddir) )
					sys.FileSystem.createDirectory(ddir);
				continue;
			}
			var filename = dir + "/" + path.join("/");
			var ch = sys.io.File.write(filename,true);
			ch.write(haxe.zip.Reader.unzip(f));
			ch.close();
			if( SYS != "Windows" ) {
				var exe = haxe.io.Path.extension(filename) == "";
				Sys.command("chmod "+(if( exe ) 755 else 644)+" "+filename);
			}
		}
	}

	function link( dir, file, dest ) {
		command("rm -rf "+dest+"/"+file);
		command("ln -s "+baseDir+"/"+dir+"/"+file+" "+dest+"/"+file);
	}

	function installNeko() {
		if( SYS == "Windows" )
			return;
		var so = if( SYS == "Mac" ) ".dylib" else ".so";
		link("neko","neko",binDir);
		link("neko","nekoc",binDir);
		link("neko","nekotools",binDir);
		link("neko","libneko"+so,libDir);
	}

	function installHaxe() {
		if( SYS == "Windows" ) {
			command('"'+baseDir+'/haxe/haxesetup" -silent');
			return;
		}
		link("haxe","haxe",binDir);
		link("haxe","haxelib",binDir);
		link("haxe","haxedoc",binDir);
		// HAXELIB setup
		var haxelib = baseDir + "/haxe/lib";
		if( !sys.FileSystem.exists(haxelib) ) {
			sys.FileSystem.createDirectory(haxelib);
			Sys.command("chmod 777 "+haxelib);
		}
	}

	function removeRec( file ) {
		if( !sys.FileSystem.isDirectory(file) ) {
			sys.FileSystem.deleteFile(file);
			return;
		}
		for( f in sys.FileSystem.readDirectory(file) )
			removeRec(file+"/"+f);
		sys.FileSystem.deleteDirectory(file);
	}

	static function main() {
		var debug = Sys.getEnv("INST_DEBUG") != null;
		var i = new Main(debug);
		if( !i.checkRights() )
			return;
		#if xcross
		wnd = new xcross.Winlog("Haxe Installer");
		wnd.button = "Exit";
		wnd.enabled = false;
		wnd.onClick = function() {
			neko.vm.Ui.stopLoop();
		};
		neko.vm.Thread.create(i.run);
		neko.vm.Ui.loop();
		#else
		i.run();
		#end
	}

}

// --------- TOOLS --------------

class Progress extends haxe.io.Output {

	var o : haxe.io.Output;
	public var cur : Int;
	public var max : Int;

	public function new(o) {
		this.o = o;
		cur = 0;
	}

	public dynamic function update() {
	}

	public override function writeByte(c) {
		o.writeByte(c);
		cur++;
		update();
	}

	public override function writeBytes(s,p,l) {
		var r = o.writeBytes(s,p,l);
		cur += r;
		update();
		return r;
	}

	public override function close() {
		super.close();
		o.close();
	}

	public override function prepare(m) {
		max = m;
	}

}
