package tools.haxelib;

class SiteProxy extends haxe.remoting.Proxy<tools.haxelib.SiteApi> {
}

class Progress extends neko.io.Output {

	var o : neko.io.Output;
	var cur : Int;
	var max : Int;
	var start : Float;

	public function new(o) {
		this.o = o;
		cur = 0;
		start = haxe.Timer.stamp();
	}

	function bytes(n) {
		cur += n;
		if( max == null )
			neko.Lib.print(cur+" bytes\r");
		else
			neko.Lib.print(cur+"/"+max+" ("+Std.int((cur*100.0)/max)+"%)\r");
	}

	public override function writeChar(c) {
		o.writeChar(c);
		bytes(1);
	}

	public override function writeBytes(s,p,l) {
		var r = o.writeBytes(s,p,l);
		bytes(r);
		return r;
	}

	public override function close() {
		super.close();
		o.close();
		var time = haxe.Timer.stamp() - start;
		var speed = (cur / time) / 1024;
		time = Std.int(time * 10) / 10;
		speed = Std.int(speed * 10) / 10;
		neko.Lib.print("Download complete : "+cur+" bytes in "+time+"s ("+speed+"KB/s)\n");
	}

	public override function prepare(m) {
		max = m;
	}

}

class Main {

	static var VERSION = 100;
	static var REPNAME = "lib";
	static var SERVER = {
		host : "localhost",
		port : 2000,
		dir : "",
		url : "index.n"
	};

	var argcur : Int;
	var args : Array<String>;
	var commands : List<{ name : String, doc : String, f : Void -> Void }>;
	var siteUrl : String;
	var site : SiteProxy;

	function new() {
		argcur = 1;
		args = neko.Sys.args();
		commands = new List();
		addCommand("install",install,"install a given project");
//		addCommand("list",list,"list all installed projects");
//		addCommand("update",update,"update all installed projects");
		addCommand("search",search,"list projects matching a word");
		addCommand("infos",infos,"list informations on a given project");
		addCommand("user",user,"list informations on a given user");
		addCommand("submit",submit,"submit or update a project package");
		addCommand("setup",setup,"set the haxelib repository path");
		addCommand("config",config,"print the repository path");
		siteUrl = "http://"+SERVER.host+":"+SERVER.port+"/"+SERVER.dir;
		site = new SiteProxy(haxe.remoting.Connection.urlConnect(siteUrl+SERVER.url).api);
	}

	function param( name ) {
		if( args.length > argcur )
			return args[argcur++];
		neko.Lib.print(name+" : ");
		return neko.io.File.stdin().readLine();
	}

	function addCommand( name, f, doc ) {
		commands.add({ name : name, doc : doc, f : f });
	}

	function usage() {
		var vmin = Std.string(VERSION % 100);
		var ver = Std.int(VERSION/100) + "." + if( vmin.length == 0 ) "0"+vmin else vmin;
		print("Haxe Library Manager "+ver+" - (c)2006 Motion-Twin");
		print(" Usage : haxlib [command] [options]");
		print(" Commands :");
		for( c in commands )
			print("  "+c.name+" : "+c.doc);
		neko.Sys.exit(1);
	}

	function run() {
		var cmd = args[0];
		if( cmd == null )
			usage();
		for( c in commands )
			if( c.name == cmd ) {
				c.f();
				return;
			}
		print("Unknown command "+cmd);
		usage();
	}

	// ---- COMMANDS --------------------

 	function search() {
		var word = param("Search word");
		var l = site.search(word);
		for( s in l )
			print(s.name);
		print(l.length+" projects found");
	}

	function infos() {
		var prj = param("Project name");
		var inf = site.infos(prj);
		print("Name: "+inf.name);
		print("Desc: "+inf.desc);
		print("Website: "+inf.website);
		print("Owner: "+inf.owner);
		print("Version: "+inf.curversion);
		print("Releases: ");
		if( inf.versions.length == 0 )
			print("  (no version released yet)");
		for( v in inf.versions )
			print("   "+v.date+" "+v.name+" : "+v.comments);
	}

	function user() {
		var uname = param("User name");
		var inf = site.user(uname);
		print("Id: "+inf.name);
		print("Name: "+inf.fullname);
		print("Mail: "+inf.email);
		print("Projects: ");
		if( inf.projects.length == 0 )
			print("  (no projects)");
		for( p in inf.projects )
			print("  "+p);
	}

	function register(name) {
		print("This is your first submission as '"+name+"'");
		print("Please enter the following informations for registration");
		var email = param("Email");
		var fullname = param("Fullname");
		var pass = haxe.Md5.encode(param("Password"));
		param("Enter to Confirm");
		site.register(name,pass,email,fullname);
		return pass;
	}

	function submit() {
		var file = param("Package");
		var data = neko.io.File.getContent(file);
		var zip = neko.zip.File.read(new neko.io.StringInput(data));
		var infos = Datas.readInfos(zip);
		var password;
		site.checkOwner(infos.project,infos.owner);
		if( site.isNewUser(infos.owner) )
			password = register(infos.owner);
		else {
			password = haxe.Md5.encode(param("Password"));
			if( !site.checkPassword(infos.owner,password) )
				throw "Invalid password for "+infos.owner;
		}

		// query a submit id that will identify the file
		var id = site.getSubmitId();

		// directly send the file data over Http
		// we can't use haxe.Http because we want *sent* data progress
		var s = new neko.io.Socket();
		s.connect(neko.io.Socket.resolve(SERVER.host),SERVER.port);
		s.write("POST /"+SERVER.url+"?submit="+id);
		s.write(" HTTP/1.1\r\nHost: "+SERVER.host+"\r\n");
		s.write("Content-Type: application/octet-stream\r\n");
		s.write("Content-Length: "+data.length+"\r\n");
		s.write("\r\n");
		var pos = 0;
		var bufsize = 1;
		print("Sending data.... ");
		while( pos < data.length ) {
			s.write(data.substr(pos,bufsize));
			pos += bufsize;
			neko.Lib.print( Std.int((pos * 100.0) / data.length) + "%\r" );
		}
		s.shutdown(false,true);
		s.input.readAll();
		s.close();

		// ask the server to register the sent file
		var msg = site.processSubmit(id,password);
		print(msg);
	}

	function install() {
		var prj = param("Project name");
		var inf = site.infos(prj);
		if( inf.curversion == null )
			throw "This project has not yet released a version";
		var reqversion = if( args.length > argcur ) args[argcur++] else null;
		var version = if( reqversion != null ) reqversion else inf.curversion;
		var found = false;
		for( v in inf.versions )
			if( v.name == version ) {
				found = true;
				break;
			}
		if( !found )
			throw "No such version "+version;

		var rep = getRepository();

		// create/delete directories first
		var project = rep+Datas.safe(inf.name);
		safeDir(project);
		project += "/";
		var target = project+Datas.safe(version);
		if( !safeDir(target) ) {
			if( reqversion == null )
				print(inf.name+" is up-to-date");
			else
				print("You already have "+inf.name+" version "+reqversion+" installed");
			return;
		}
		target += "/";

		// download to temporary file
		var filename = Datas.fileName(inf.name,version);
		var filepath = rep+filename;
		var out = neko.io.File.write(filepath,true);
		var progress = new Progress(out);
		var h = new haxe.Http(siteUrl+Datas.REPOSITORY+"/"+filename);
		h.onError = function(e) {
			progress.close();
			neko.FileSystem.deleteFile(filepath);
			throw e;
		};
		print("Downloading "+filename+"...");
		h.asyncRequest(false,progress);

		// read zip content
		var f = neko.io.File.read(filepath,true);
		var zip = neko.zip.File.read(f);
		f.close();

		// locate haxelib.xml base path
		var basepath = null;
		for( f in zip ) {
			if( StringTools.endsWith(f.fileName,Datas.XML) ) {
				basepath = f.fileName.substr(0,f.fileName.length - Datas.XML.length);
				break;
			}
		}
		if( basepath == null )
			throw "No "+Datas.XML+" found";

		// unzip content
		for( zipfile in zip ) {
			var n = zipfile.fileName;
			if( StringTools.startsWith(n,basepath) ) {
				// remove basepath
				n = n.substr(basepath.length,n.length-basepath.length);
				if( n.charAt(0) == "/" || n.charAt(0) == "\\" || n.split("..").length > 1 )
					throw "Invalid filename : "+n;
				var dirs = ~/[\/\\]/.split(n);
				var path = "";
				var file = dirs.pop();
				for( d in dirs ) {
					path += d;
					safeDir(target+path);
					path += "/";
				}
				if( file == "" ) {
					if( path != "" ) print("  Created "+path);
					continue; // was just a directory
				}
				path += file;
				print("  Install "+path);
				var data = neko.zip.File.unzip(zipfile);
				var f = neko.io.File.write(target+path,true);
				f.write(data);
				f.close();
			}
		}

		// set current version
		if( version == inf.curversion ) {
			var f = neko.io.File.write(project+".current",true);
			f.write(version);
			f.close();
			print("  Current version is now "+version);
		}

		// end
		neko.FileSystem.deleteFile(filepath);
		print("Done");
	}

	function safeDir( dir ) {
		if( neko.FileSystem.exists(dir) ) {
			if( !neko.FileSystem.isDirectory(dir) )
				throw ("A file is preventing "+dir+" to be created");
			return false;
		}
		neko.FileSystem.createDirectory(dir);
		return true;
	}

	function getRepository( ?setup : Bool ) {
		var sys = neko.Sys.systemName();
		if( sys == "Windows" ) {
			var haxepath = neko.Sys.getEnv("HAXEPATH");
			if( haxepath == null )
				throw "HAXEPATH environment variable not defined, please run haxesetup.exe first";
			var rep = haxepath+REPNAME;
			safeDir(rep);
			return rep+"\\";
		}
		var config = neko.Sys.getEnv("HOME")+"/.haxelib";
		var rep = try
			neko.io.File.getContent(config)
		catch( e : Dynamic ) try
			neko.io.File.getContent("/etc/.haxelib")
		catch( e : Dynamic )
			if( setup ) {
				if( sys == "Linux" ) "/usr/lib/haxe/"+REPNAME else "/usr/local/lib/haxe/"+REPNAME;
			} else
				throw "This is the first time you are runing haxelib. Please run haxelib setup first";
		if( setup ) {
			print("Please enter haxelib repository path with write access");
			print("Hit enter for default ("+rep+")");
			var line = param("Path");
			if( line != "" )
				rep = line;
			if( !neko.FileSystem.exists(rep) )
				neko.FileSystem.createDirectory(rep);
			var f = neko.io.File.write(config,true);
			f.write(rep);
			f.close();
		}
		return rep+"/";
	}

	function setup() {
		var path = getRepository(true);
		print("haxelib repository is now "+path);
	}

	function config() {
		print(getRepository());
	}

	// ----------------------------------

	static function print(str) {
		neko.Lib.print(str+"\n");
	}

	static function main() {
		new Main().run();
	}

}
