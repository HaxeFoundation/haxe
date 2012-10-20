package tools.haxelib;
#if haxe3
import haxe.zip.Reader;
#else
import neko.zip.Reader;
#end

enum Answer {
	Yes;
	No;
	Always;
}

class SiteProxy extends haxe.remoting.Proxy<tools.haxelib.SiteApi> {
}

class Progress extends haxe.io.Output {

	var o : haxe.io.Output;
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
			Sys.print(cur+" bytes\r");
		else
			Sys.print(cur+"/"+max+" ("+Std.int((cur*100.0)/max)+"%)\r");
	}

	public override function writeByte(c) {
		o.writeByte(c);
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
		Sys.print("Download complete : "+cur+" bytes in "+time+"s ("+speed+"KB/s)\n");
	}

	public override function prepare(m) {
		max = m;
	}

}

class ProgressIn extends haxe.io.Input {

	var i : haxe.io.Input;
	var pos : Int;
	var tot : Int;

	public function new( i, tot ) {
		this.i = i;
		this.pos = 0;
		this.tot = tot;
	}

	public override function readByte() {
		var c = i.readByte();
		doRead(1);
		return c;
	}

	public override function readBytes(buf,pos,len) {
		var k = i.readBytes(buf,pos,len);
		doRead(k);
		return k;
	}

	function doRead( nbytes : Int ) {
		pos += nbytes;
		Sys.print( Std.int((pos * 100.0) / tot) + "%\r" );
	}

}

class Main {

	static var VERSION = 103;
	static var REPNAME = "lib";
	static var SERVER = {
		host : "lib.haxe.org",
		port : 80,
		dir : "",
		url : "index.n"
	};

	var argcur : Int;
	var args : Array<String>;
	var commands : List<{ name : String, doc : String, f : Void -> Void, net : Bool }>;
	var siteUrl : String;
	var site : SiteProxy;

	function new() {
		args = Sys.args();
		commands = new List();
		addCommand("install",install,"install a given library");
		addCommand("list",list,"list all installed libraries",false);
		addCommand("upgrade",upgrade,"upgrade all installed libraries");
		addCommand("update",update,"update a single library");
		addCommand("remove",remove,"remove a given library/version",false);
		addCommand("set",set,"set the current version for a library",false);
		addCommand("search",search,"list libraries matching a word");
		addCommand("info",info,"list informations on a given library");
		addCommand("user",user,"list informations on a given user");
		addCommand("register",register,"register a new user");
		addCommand("submit",submit,"submit or update a library package");
		addCommand("setup",setup,"set the haxelib repository path",false);
		addCommand("config",config,"print the repository path",false);
		addCommand("path",path,"give paths to libraries",false);
		addCommand("run",run,"run the specified library with parameters",false);
		addCommand("test",test,"install the specified package localy",false);
		addCommand("dev",dev,"set the development directory for a given library",false);
		addCommand("git",git,"uses git repository as library");
		initSite();
	}

	function initSite() {
		siteUrl = "http://"+SERVER.host+":"+SERVER.port+"/"+SERVER.dir;
		site = new SiteProxy(haxe.remoting.HttpConnection.urlConnect(siteUrl+SERVER.url).api);
	}

	function param( name, ?passwd ) {
		if( args.length > argcur )
			return args[argcur++];
		Sys.print(name+" : ");
		if( passwd ) {
			var s = new StringBuf();
			var c;
			while( (c = Sys.getChar(false)) != 13 )
				s.addChar(c);
			print("");
			return s.toString();
		}
		return Sys.stdin().readLine();
	}

	function ask( question ) {
		while( true ) {
			Sys.print(question+" [y/n/a] ? ");
			switch( Sys.stdin().readLine() ) {
			case "n": return No;
			case "y": return Yes;
			case "a": return Always;
			}
		}
		return null;
	}

	function paramOpt() {
		if( args.length > argcur )
			return args[argcur++];
		return null;
	}

	function addCommand( name, f, doc, ?net = true ) {
		commands.add({ name : name, doc : doc, f : f, net : net });
	}

	function usage() {
		var vmin = Std.string(VERSION % 100);
		var ver = Std.int(VERSION/100) + "." + if( vmin.length == 1 ) "0"+vmin else vmin;
		print("Haxe Library Manager "+ver+" - (c)2006 Motion-Twin");
		print(" Usage : haxelib [command] [options]");
		print(" Commands :");
		for( c in commands )
			print("  "+c.name+" : "+c.doc);
		Sys.exit(1);
	}

	function process() {
		var debug = false;
		argcur = 0;
		while( true ) {
			var a = args[argcur++];
			if( a == null )
				break;
			switch( a ) {
			case "-debug":
				debug = true;
			case "-R":
				var path = args[argcur++];
				var r = ~/^(http:\/\/)?([^:\/]+)(:[0-9]+)?\/?(.*)$/;
				if( !r.match(path) )
					throw "Invalid repository format '"+path+"'";
				SERVER.host = r.matched(2);
				if( r.matched(3) != null )
					SERVER.port = Std.parseInt(r.matched(3).substr(1));
				SERVER.dir = r.matched(4);
				initSite();
			default:
				argcur--;
				break;
			}
		}
		var cmd = args[argcur++];
		if( cmd == null )
			usage();
		for( c in commands )
			if( c.name == cmd ) {
				try {
					if( c.net ) {
						var p = neko.net.ProxyDetect.detect();
						if( p != null ) {
							print("Using proxy "+p.host+":"+p.port);
							haxe.Http.PROXY = p;
						}
					}
					c.f();
				} catch( e : Dynamic ) {
					if( e == "std@host_resolve" ) {
						print("Host "+SERVER.host+" was not found");
						print("Please ensure that your internet connection is on");
						print("If you don't have an internet connection or if you are behing a proxy");
						print("please download manually the file from http://lib.haxe.org/files");
						print("and run 'haxelib test <file>' to install the Library.");
						Sys.exit(1);
					}
					if( debug )
						neko.Lib.rethrow(e);
					print(Std.string(e));
					Sys.exit(1);
				}
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
		print(l.length+" libraries found");
	}

	function info() {
		var prj = param("Library name");
		var inf = site.infos(prj);
		print("Name: "+inf.name);
		print("Tags: "+inf.tags.join(", "));
		print("Desc: "+inf.desc);
		print("Website: "+inf.website);
		print("License: "+inf.license);
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
		print("Libraries: ");
		if( inf.projects.length == 0 )
			print("  (no libraries)");
		for( p in inf.projects )
			print("  "+p);
	}

	function register() {
		doRegister(param("User"));
		print("Registration successful");
	}

	function doRegister(name) {
		var email = param("Email");
		var fullname = param("Fullname");
		var pass = param("Password",true);
		var pass2 = param("Confirm",true);
		if( pass != pass2 )
			throw "Password does not match";
		pass = haxe.Md5.encode(pass);
		site.register(name,pass,email,fullname);
		return pass;
	}

	function submit() {
		var file = param("Package");
		var data = sys.io.File.getBytes(file);
		var zip = Reader.readZip(new haxe.io.BytesInput(data));
		var infos = Datas.readInfos(zip,true);
		var user = infos.developers.first();
		var password;
		if( site.isNewUser(user) ) {
			print("This is your first submission as '"+user+"'");
			print("Please enter the following informations for registration");
			password = doRegister(user);
		} else {
			if( infos.developers.length > 1 )
				user = param("User");
			password = haxe.Md5.encode(param("Password",true));
			if( !site.checkPassword(user,password) )
				throw "Invalid password for "+user;
		}
		site.checkDeveloper(infos.project,user);

		// check dependencies validity
		for( d in infos.dependencies ) {
			var infos = site.infos(d.project);
			if( d.version == "" )
				continue;
			var found = false;
			for( v in infos.versions )
				if( v.name == d.version ) {
					found = true;
					break;
				}
			if( !found )
				throw "Library "+d.project+" does not have version "+d.version;
		}

		// check if this version already exists
		var sinfos = try site.infos(infos.project) catch( _ : Dynamic ) null;
		if( sinfos != null )
			for( v in sinfos.versions )
				if( v.name == infos.version && ask("You're about to overwrite existing version '"+v.name+"', please confirm") == No )
					throw "Aborted";

		// query a submit id that will identify the file
		var id = site.getSubmitId();

		// directly send the file data over Http
		var h = new haxe.Http("http://"+SERVER.host+":"+SERVER.port+"/"+SERVER.url);
		h.onError = function(e) { throw e; };
		h.onData = print;
		h.fileTransfert("file",id,new ProgressIn(new haxe.io.BytesInput(data),data.length),data.length);
		print("Sending data.... ");
		h.request(true);

		// processing might take some time, make sure we wait
		print("Processing file.... ");
		haxe.remoting.HttpConnection.TIMEOUT = 1000;
		// ask the server to register the sent file
		var msg = site.processSubmit(id,user,password);
		print(msg);
	}

	function install() {
		var prj = param("Library name");
		if( sys.FileSystem.exists(prj) && !sys.FileSystem.isDirectory(prj) ) {
			if( !StringTools.endsWith(prj,".zip") )
				throw "Local file to install must be a zip";
			doInstallFile(prj,true,true);
			return;
		}
		var inf = site.infos(prj);
		if( inf.curversion == null )
			throw "This library has not yet released a version";
		var reqversion = paramOpt();
		var version = if( reqversion != null ) reqversion else inf.curversion;
		var found = false;
		for( v in inf.versions )
			if( v.name == version ) {
				found = true;
				break;
			}
		if( !found )
			throw "No such version "+version;
		doInstall(inf.name,version,version == inf.curversion);
	}

	function doInstall( project, version, setcurrent ) {
		var rep = getRepository();

		// check if exists already
		if( sys.FileSystem.exists(rep+Datas.safe(project)+"/"+Datas.safe(version)) ) {
			print("You already have "+project+" version "+version+" installed");
			setCurrent(project,version,true);
			return;
		}

		// download to temporary file
		var filename = Datas.fileName(project,version);
		var filepath = rep+filename;
		var out = sys.io.File.write(filepath,true);
		var progress = new Progress(out);
		var h = new haxe.Http(siteUrl+Datas.REPOSITORY+"/"+filename);
		h.onError = function(e) {
			progress.close();
			sys.FileSystem.deleteFile(filepath);
			throw e;
		};
		print("Downloading "+filename+"...");
		h.customRequest(false,progress);

		doInstallFile(filepath,setcurrent);
		site.postInstall(project,version);
	}

	function doInstallFile(filepath,setcurrent,?nodelete) {

		// read zip content
		var f = sys.io.File.read(filepath,true);
		var zip = Reader.readZip(f);
		f.close();
		var infos = Datas.readInfos(zip,false);

		// create directories
		var pdir = getRepository() + Datas.safe(infos.project);
		safeDir(pdir);
		pdir += "/";
		var target = pdir + Datas.safe(infos.version);
		safeDir(target);
		target += "/";

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
				var dirs = ~/[\/\\]/g.split(n);
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
				var data = Reader.unzip(zipfile);
				sys.io.File.saveBytes(target+path,data);
			}
		}

		// set current version
		if( setcurrent || !sys.FileSystem.exists(pdir+".current") ) {
			sys.io.File.saveContent(pdir+".current",infos.version);
			print("  Current version is now "+infos.version);
		}

		// end
		if( !nodelete )
			sys.FileSystem.deleteFile(filepath);
		print("Done");

		// process dependencies
		for( d in infos.dependencies ) {
			print("Installing dependency "+d.project+" "+d.version);
			if( d.version == "" )
				d.version = site.infos(d.project).curversion;
			doInstall(d.project,d.version,false);
		}
	}

	function safeDir( dir ) {
		if( sys.FileSystem.exists(dir) ) {
			if( !sys.FileSystem.isDirectory(dir) )
				throw ("A file is preventing "+dir+" to be created");
			return false;
		}
		try {
			sys.FileSystem.createDirectory(dir);
		} catch( e : Dynamic ) {
			throw "You don't have enough user rights to create the directory "+dir;
		}
		return true;
	}

	function safeDelete( file ) {
		try {
			sys.FileSystem.deleteFile(file);
			return true;
		} catch (e:Dynamic) {
			if( Sys.systemName() == "Windows") {
				try {
					Sys.command("attrib -R \"" +file+ "\"");
					sys.FileSystem.deleteFile(file);
					return true;
				} catch (e:Dynamic) {
				}
			}
			return false;
		}
	}

	function getRepository( ?setup : Bool ) {
		var win = Sys.systemName() == "Windows";
		var haxepath = Sys.getEnv("HAXEPATH");
		if( haxepath != null ) {
			var last = haxepath.charAt(haxepath.length - 1);
			if( last != "/" && last != "\\" )
				haxepath += "/";
		}
		var config_file;
		if( win )
			config_file = Sys.getEnv("HOMEDRIVE") + Sys.getEnv("HOMEPATH");
		else
			config_file = Sys.getEnv("HOME");
		config_file += "/.haxelib";
		var rep = try
			sys.io.File.getContent(config_file)
		catch( e : Dynamic ) try
			sys.io.File.getContent("/etc/.haxelib")
		catch( e : Dynamic ) {
			if( setup ) {
				(win ? haxepath : "/usr/lib/haxe/")+REPNAME;
			} else if( win ) {
				// Windows have a default directory (no need for setup)
				if( haxepath == null )
					throw "HAXEPATH environment variable not defined, please run haxesetup.exe first";
				var rep = haxepath+REPNAME;
				try {
					safeDir(rep);
				} catch( e : Dynamic ) {
					throw "The directory defined by HAXEPATH does not exist, please run haxesetup.exe again";
				}
				return rep+"\\";
			} else
				throw "This is the first time you are runing haxelib. Please run haxelib setup first";
		}
		rep = StringTools.trim(rep);
		if( setup ) {
			print("Please enter haxelib repository path with write access");
			print("Hit enter for default (" + rep + ")");
			var line = param("Path");
			if( line != "" )
				rep = line;
			if( !sys.FileSystem.exists(rep) ) {
				try {
					sys.FileSystem.createDirectory(rep);
				} catch( e : Dynamic ) {
					print("Failed to create directory '"+rep+"' ("+Std.string(e)+"), maybe you need appropriate user rights");
					print("Check also that the parent directory exists");
					Sys.exit(1);
				}
			}
			sys.io.File.saveContent(config_file, rep);
		} else if( !sys.FileSystem.exists(rep) )
			throw "haxelib Repository "+rep+" does not exists. Please run haxelib setup again";
		return rep+"/";
	}

	function setup() {
		var path = getRepository(true);
		print("haxelib repository is now "+path);
	}

	function config() {
		print(getRepository());
	}

	function getCurrent( dir ) {
		return StringTools.trim(sys.io.File.getContent(dir + "/.current"));
	}

	function getDev( dir ) {
		return StringTools.trim(sys.io.File.getContent(dir + "/.dev"));
	}

	function list() {
		var rep = getRepository();
		for( p in sys.FileSystem.readDirectory(rep) ) {
			if( p.charAt(0) == "." )
				continue;
			var versions = new Array();
			var current = getCurrent(rep + p);
			var dev = try StringTools.trim(sys.io.File.getContent(rep+p+"/.dev")) catch( e : Dynamic ) null;
			for( v in sys.FileSystem.readDirectory(rep+p) ) {
				if( v.charAt(0) == "." )
					continue;
				v = Datas.unsafe(v);
				if( dev == null && v == current )
					v = "["+v+"]";
				versions.push(v);
			}
			if( dev != null )
				versions.push("[dev:"+dev+"]");
			print(Datas.unsafe(p) + ": "+versions.join(" "));
		}
	}

	function upgrade() {
		var state = { rep : getRepository(), prompt : true, updated : false };
		for( p in sys.FileSystem.readDirectory(state.rep) ) {
			if( p.charAt(0) == "." || !sys.FileSystem.isDirectory(state.rep+"/"+p) )
				continue;
			var p = Datas.unsafe(p);
			print("Checking " + p);
			doUpdate(p,state);
		}
		if( state.updated )
			print("Done");
		else
			print("All libraries are up-to-date");
	}

	function doUpdate( p : String, state ) {
		var rep = state.rep;
		if( sys.FileSystem.exists(rep + "/" + p + "/git") && sys.FileSystem.isDirectory(rep + "/" + p + "/git") ) {
			var oldCwd = Sys.getCwd();
			Sys.setCwd(rep + "/" + p + "/git");
			Sys.command("git pull");
			// TODO: update haxelib.xml version?
			Sys.setCwd(oldCwd);
		} else {
			var inf = try site.infos(p) catch( e : Dynamic ) { Sys.println(e); return; };
			if( !sys.FileSystem.exists(rep+Datas.safe(p)+"/"+Datas.safe(inf.curversion)) ) {
				if( state.prompt )
					switch ask("Upgrade "+p+" to "+inf.curversion) {
					case Yes:
					case Always: state.prompt = false;
					case No:
						return;
					}
				doInstall(p,inf.curversion,true);
				state.updated = true;
			} else
				setCurrent(p, inf.curversion, true);
		}
	}

	function update() {
		var prj = param("Library");
		var state = { rep : getRepository(), prompt : false, updated : false };
		doUpdate(prj,state);
		if( !state.updated )
			print(prj + " is up to date");
	}

	function deleteRec(dir) {
		for( p in sys.FileSystem.readDirectory(dir) ) {
			var path = dir+"/"+p;
			if( sys.FileSystem.isDirectory(path) )
				deleteRec(path);
			else
				safeDelete(path);
		}
		sys.FileSystem.deleteDirectory(dir);
	}

	function remove() {
		var prj = param("Library");
		var version = paramOpt();
		var rep = getRepository();
		var pdir = rep + Datas.safe(prj);
		if( version == null ) {
			if( !sys.FileSystem.exists(pdir) )
				throw "Library "+prj+" is not installed";
			deleteRec(pdir);
			print("Library "+prj+" removed");
			return;
		}

		var vdir = pdir + "/" + Datas.safe(version);
		if( !sys.FileSystem.exists(vdir) )
			throw "Library "+prj+" does not have version "+version+" installed";

		var cur = getCurrent(pdir);
		if( cur == version )
			throw "Can't remove current version of library "+prj;
		deleteRec(vdir);
		print("Library "+prj+" version "+version+" removed");
	}

	function set() {
		var prj = param("Library");
		var version = param("Version");
		setCurrent(prj,version,false);
	}

	function setCurrent( prj : String, version : String, doAsk : Bool ) {
		var pdir = getRepository() + Datas.safe(prj);
		var vdir = pdir + "/" + Datas.safe(version);
		if( !sys.FileSystem.exists(vdir) )
			throw "Library "+prj+" version "+version+" is not installed";
		if( getCurrent(pdir) == version )
			return;
		if( doAsk && ask("Set "+prj+" to version "+version) == No )
			return;
		sys.io.File.saveContent(pdir+"/.current",version);
		print("Library "+prj+" current version is now "+version);
	}

	function checkRec( prj : String, version : String, l : List<{ project : String, version : String }> ) {
		var pdir = getRepository() + Datas.safe(prj);
		if( !sys.FileSystem.exists(pdir) )
			throw "Library "+prj+" is not installed : run 'haxelib install "+prj+"'";
		var version = if( version != null ) version else getCurrent(pdir);
		var vdir = pdir + "/" + Datas.safe(version);
		if( StringTools.endsWith(vdir, "dev") )
			vdir = getDev(pdir);
		if( !sys.FileSystem.exists(vdir) )
			throw "Library "+prj+" version "+version+" is not installed";
		for( p in l )
			if( p.project == prj ) {
				if( p.version == version )
					return;
				throw "Library "+prj+" has two version included "+version+" and "+p.version;
			}
		l.add({ project : prj, version : version });
		var xml = sys.io.File.getContent(vdir+"/haxelib.xml");
		var inf = Datas.readData(xml,false);
		for( d in inf.dependencies )
			checkRec(d.project,if( d.version == "" ) null else d.version,l);
	}

	function path() {
		var list = new List();
		while( argcur < args.length ) {
			var a = args[argcur++].split(":");
			checkRec(a[0],a[1],list);
		}
		var rep = getRepository();
		for( d in list ) {
			var pdir = Datas.safe(d.project)+"/"+Datas.safe(d.version)+"/";
			var dir = rep + pdir;
			try {
				dir = getDev(rep+Datas.safe(d.project));
				if( dir.length == 0 || (dir.charAt(dir.length-1) != '/' && dir.charAt(dir.length-1) != '\\') )
					dir += "/";
				pdir = dir;
			} catch( e : Dynamic ) {
			}
			var ndir = dir + "ndll";
			if( sys.FileSystem.exists(ndir) ) {
				var sysdir = ndir+"/"+Sys.systemName();
				var is64 = neko.Lib.load("std", "sys_is64", 0)();
				if( is64 ) sysdir += "64";
				if( !sys.FileSystem.exists(sysdir) )
					throw "Library "+d.project+" version "+d.version+" does not have a neko dll for your system";
				Sys.println("-L "+pdir+"ndll/");
			}
			try {
				var f = sys.io.File.getContent(dir + "extraParams.hxml");
				Sys.println(StringTools.trim(f));
			} catch( e : Dynamic ) {
			}
			Sys.println(dir);
			Sys.println("-D "+d.project);
		}
	}

	function dev() {
		var rep = getRepository();
		var project = param("Library");
		var dir = paramOpt();
		var proj = rep + Datas.safe(project);
		if( !sys.FileSystem.exists(proj) ) {
			sys.FileSystem.createDirectory(proj);
			sys.io.File.saveContent(proj + "/.current", "dev");
		}
		var devfile = proj+"/.dev";
		if( dir == null ) {
			if( sys.FileSystem.exists(devfile) )
				sys.FileSystem.deleteFile(devfile);
			print("Development directory disabled");
		} else {
			try {
				sys.io.File.saveContent(devfile, dir);
				print("Development directory set to "+dir);
			}
			catch (e:Dynamic) {
				print("Could not write to " +proj + "/.dev");
			}
		}
	}

	function git() {
		var gitExists = function()
			try { command("git", []); return true; } catch (e:Dynamic) return false;
		if (!gitExists())
		{
			var match = ~/(.*)git([\\|\/])cmd$/ ;
			for (path in Sys.getEnv("PATH").split(";"))
			{
				if (match.match(path.toLowerCase()))
				{
					var newPath = match.matched(1) + "git" +match.matched(2) + "bin";
					Sys.putEnv("PATH", Sys.getEnv("PATH") + ";" +newPath);
				}
			}
			if (!gitExists())
			{
				print("Could not execute git, please make sure it is installed and available in your PATH.");
				return;
			}
		}
		var libName = param("Library name");
		var rep = getRepository();
		var libPath = rep + Datas.safe(libName) + "/git";

		if( sys.FileSystem.exists(libPath) ) {
			print("You already have a git version of "+libName+" installed");
			return;
		}

		var gitPath = param("Git path");
		var subDir = paramOpt();

		var match = ~/@([0-9]+)/;
		var rev = if (match.match(gitPath) && match.matchedRight() == "")
		{
			gitPath = match.matchedLeft();
			match.matched(1);
		}
		else
			null;

		print("Installing " +libName + " from " +gitPath);
		if( Sys.command("git clone \"" +gitPath + "\" \"" +libPath + "\"") != 0 ) {
			print("Could not clone git repository");
			return;
		}
		Sys.setCwd(libPath);
		if (rev != null) {
			var ret = command("git", ["checkout", rev]);
			if (ret.code != 0)
			{
				print("Could not checkout revision: " +ret.out);
				// TODO: We might have to get rid of the cloned repository here
				return;
			}
		}
		var revision = command("git", ["rev-parse", "HEAD"]).out;

		var devPath = libPath + (subDir == null ? "" : "/" + subDir);
		if (!sys.FileSystem.exists(devPath +"/haxelib.xml"))
		{
			var haxelib = "<project name='" +libName + "' url='" +gitPath + "' license='BSD'>"
				+"<description></description>"
				+"<version name='" +revision + "'>Updated from git.</version>"
				+"</project>";
			sys.io.File.saveContent(devPath +"/haxelib.xml", haxelib);
		}

		Sys.setCwd(libPath + "/../");
		sys.io.File.saveContent(".current", "dev");
		sys.io.File.saveContent(".dev", devPath);
		print("Done");
	}

	function run() {
		var rep = getRepository();
		var project = param("Library");
		var temp = project.split(":");
		project = temp[0];
		var pdir = rep + Datas.safe(project);
		if( !sys.FileSystem.exists(pdir) )
			throw "Library "+project+" is not installed";
		pdir += "/";
		var version = temp[1] != null ? temp[1] : getCurrent(pdir);
		var dev = try getDev(pdir) catch ( e : Dynamic ) null;
		var vdir = dev!=null ? dev : pdir + Datas.safe(version);
		var rdir = vdir + "/run.n";
		if( !sys.FileSystem.exists(rdir) )
			throw "Library "+project+" version "+version+" does not have a run script";
		args.push(Sys.getCwd());
		Sys.setCwd(vdir);
		var cmd = "neko run.n";
		for( i in argcur...args.length )
			cmd += " "+escapeArg(args[i]);
		Sys.exit(Sys.command(cmd));
	}

	function escapeArg( a : String ) {
		if( a.indexOf(" ") == -1 )
			return a;
		return '"'+a+'"';
	}

	function test() {
		var file = param("Package");
		doInstallFile(file,true,true);
	}

	function command( cmd:String, args:Array<String> ) {
		var p = new sys.io.Process(cmd, args);
		var code = p.exitCode();
		return { code:code, out: code == 0 ? p.stdout.readAll().toString() : p.stderr.readAll().toString() };
	}

	// ----------------------------------

	static function print(str) {
		Sys.print(str+"\n");
	}

	static function main() {
		new Main().process();
	}

}
