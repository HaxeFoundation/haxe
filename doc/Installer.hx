class Installer {

	static var SYS = neko.Sys.systemName();
	static var TMP = "tmp.txt";
	static var NULL = if( SYS == "Windows" ) "NUL" else "/dev/null";

	function new() {
		haxe.Http.PROXY = neko.net.ProxyDetect.detect();
	}

	function newVersion(v1,v2) {
		if( v1 == null )
			return true;
		return (v1.major * 10000 + v1.minor * 100 + v1.build < v2.major * 10000 + v2.minor * 100 + v2.build);
	}

	function ask( txt ) {
		neko.Lib.print(txt);
		return true;
	}

	function error( txt ) {
		neko.Lib.print(txt);
	}

	function display( txt ) {
		neko.Lib.println(txt);
	}

	function version(v : { major : Int, minor : Int, build : Int } ) {
		return v.major+"."+v.minor+if( v.build > 0 ) "."+v.build else "";
	}

	function run() {


		// GET haxe Version
		display("Getting Local haXe Version");
		neko.Sys.command("haxe 2>"+TMP);
		var content = neko.io.File.getContent(TMP);
		neko.FileSystem.deleteFile(TMP);
		var r = ~/^Haxe Compiler ([0-9]+)\.([0-9]+)/;
		var haxeVersion = null;
		if( r.match(content) )
			haxeVersion = {
				major : Std.parseInt(r.matched(1)),
				minor : Std.parseInt(r.matched(2)),
				build : 0,
			};


		// GET Neko Version
		display("Getting Local Neko Version");
		neko.Sys.command("neko >"+TMP+" 2>"+NULL);
		var content = neko.io.File.getContent(TMP);
		neko.FileSystem.deleteFile(TMP);
		var r = ~/^NekoVM ([0-9]+)\.([0-9]+)(\.([0-9]+))?/;
		var nekoVersion = null;
		if( r.match(content) )
			nekoVersion = {
				major : Std.parseInt(r.matched(1)),
				minor : Std.parseInt(r.matched(2)),
				build : Std.parseInt(r.matched(4))
			};

		// GET haXe files list
		display("Getting Latest haXe Version");
		var haxeFile = null;
		var r = ~/^haxe-([0-9]+)\.([0-9]+)(|-linux|-osx)(\.zip|\.tar\.gz)$/;
		for( f in haxe.Http.request("http://haxe.org/latest.n").split("\n") )
			if( r.match(f) ) {
				var pf = r.matched(3);
				switch( SYS ) {
				case "Windows": if( pf != "" ) continue;
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
		if( haxeFile == null ) {
			error("No haXe File found for your plaform");
			return;
		}

		// GET Neko files list
		display("Getting Latest Neko Version");
		var nekoFile = null;
		var r = ~/^neko-([0-9]+)\.([0-9]+)(\.([0-9]+))?(-win|-linux|-osx)(\.zip|\.tar\.gz)$/;
		for( f in haxe.Http.request("http://nekovm.org/latest.n").split("\n") )
			if( r.match(f) ) {
				var pf = r.matched(5);
				switch( SYS ) {
				case "Windows": if( pf != "-win" ) continue;
				case "Linux": if( pf != "-linux" ) continue;
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
		if( nekoFile == null ) {
			error("No haXe File found for your plaform");
			return;
		}

		var needHaxe = newVersion(haxeVersion,haxeFile.version);
		var needNeko = newVersion(nekoVersion,nekoFile.version);
		if( !needHaxe && !needNeko ) {
			if( !ask("Both your haXe and Neko versions are up-to-date, do you want to reinstall everything ?") )
				return;
			needHaxe = true;
			needNeko = true;
		} else {
			var txt = "";
			if( needNeko ) {
				txt += "Neko "+version(nekoFile.version);
				if( needHaxe )
					txt += " and ";
			}
			if( needHaxe )
				txt += "haXe "+version(haxeFile.version);
			if( !ask("Do you want to install "+txt+" ?") )
				return;
		}
		if( needNeko )
			download("nekovm.org",nekoFile.file);
		if( needHaxe )
			download("haxe.org",haxeFile.file);
		if( needNeko )
			installNeko(nekoFile.file);
		if( needHaxe )
			installHaxe(haxeFile.file);
	}

	function download( domain, file ) {
	}

	function installNeko( file ) {
	}

	function installHaxe( file) {
	}

	static function main() {
		new Installer().run();
	}

}
