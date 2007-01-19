package tools.haxelib;

enum Answer {
	Yes;
	No;
	Always;
}

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

class ProgressIn extends neko.io.Input {

	var i : neko.io.Input;
	var pos : Int;
	var tot : Int;

	public function new( i, tot ) {
		this.i = i;
		this.pos = 0;
		this.tot = tot;
	}

	public override function readChar() {
		var c = i.readChar();
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
		neko.Lib.print( Std.int((pos * 100.0) / tot) + "%\r" );
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
	var commands : List<{ name : String, doc : String, f : Void -> Void }>;
	var siteUrl : String;
	var site : SiteProxy;

	function new() {
		args = neko.Sys.args();
		commands = new List();
		addCommand("install",install,"install a given project");
		addCommand("list",list,"list all installed projects");
		addCommand("upgrade",upgrade,"upgrade all installed projects");
		addCommand("remove",remove,"remove a given project/version");
		addCommand("set",set,"set the current version for a project");
		addCommand("search",search,"list projects matching a word");
		addCommand("info",info,"list informations on a given project");
		addCommand("user",user,"list informations on a given user");
		addCommand("register",register,"register a new user");
		addCommand("submit",submit,"submit or update a project package");
		addCommand("setup",setup,"set the haxelib repository path");
		addCommand("config",config,"print the repository path");
		addCommand("path",path,"give paths to libraries");
		addCommand("run",run,"run the specified project with parameters");
		addCommand("test",test,"install the specified package localy");
		siteUrl = "http://"+SERVER.host+":"+SERVER.port+"/"+SERVER.dir;
		site = new SiteProxy(haxe.remoting.Connection.urlConnect(siteUrl+SERVER.url).api);
	}

	function param( name, ?passwd ) {
		if( args.length > argcur )
			return args[argcur++];
		neko.Lib.print(name+" : ");
		if( passwd ) {
			var s = new StringBuf();
			var c;
			while( (c = neko.io.File.getChar(false)) != 13 )
				s.addChar(c);
			print("");
			return s.toString();
		}
		return neko.io.File.stdin().readLine();
	}

	function ask( question ) {
		while( true ) {
			neko.Lib.print(question+" [y/n/a] ? ");
			switch( neko.io.File.stdin().readLine() ) {
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

	function addCommand( name, f, doc ) {
		commands.add({ name : name, doc : doc, f : f });
	}

	function usage() {
		var vmin = Std.string(VERSION % 100);
		var ver = Std.int(VERSION/100) + "." + if( vmin.length == 1 ) "0"+vmin else vmin;
		print("Haxe Library Manager "+ver+" - (c)2006 Motion-Twin");
		print(" Usage : haxelib [command] [options]");
		print(" Commands :");
		for( c in commands )
			print("  "+c.name+" : "+c.doc);
		neko.Sys.exit(1);
	}

	function process() {
		var debug = false;
		argcur = 0;
		if( args[argcur] == "-debug" ) {
			argcur++;
			debug = true;
		}
		var cmd = args[argcur++];
		if( cmd == null )
			usage();
		for( c in commands )
			if( c.name == cmd ) {
				try {
					c.f();
				} catch( e : Dynamic ) {
					if( e == "std@host_resolve" ) {
						print("Host "+SERVER.host+" was not found");
						print("Please ensure that your internet connection is on");
						print("If you don't have an internet connection or if you are behing a proxy");
						print("please download manually the file from http://lib.haxe.org/files");
						print("and run 'haxelib test <file>' to install the Library.");
						neko.Sys.exit(1);
					}
					if( debug )
						neko.Lib.rethrow(e);
					print(Std.string(e));
					neko.Sys.exit(1);
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
		print(l.length+" projects found");
	}

	function info() {
		var prj = param("Project name");
		var inf = site.infos(prj);
		print("Name: "+inf.name);
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
		print("Projects: ");
		if( inf.projects.length == 0 )
			print("  (no projects)");
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
		var data = neko.io.File.getContent(file);
		var zip = neko.zip.File.read(new neko.io.StringInput(data));
		var infos = Datas.readInfos(zip);
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
				throw "Project "+d.project+" does not have version "+d.version;
		}

		// query a submit id that will identify the file
		var id = site.getSubmitId();

		// directly send the file data over Http
		var h = new haxe.Http("http://"+SERVER.host+":"+SERVER.port+"/"+SERVER.url);
		h.onError = function(e) { throw e; };
		h.onData = print;
		h.fileTransfert("file",id,new ProgressIn(new neko.io.StringInput(data),data.length),data.length);
		print("Sending data.... ");
		h.request(true);

		// ask the server to register the sent file
		var msg = site.processSubmit(id,user,password);
		print(msg);
	}

	function install() {
		var prj = param("Project name");
		var inf = site.infos(prj);
		if( inf.curversion == null )
			throw "This project has not yet released a version";
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
		if( neko.FileSystem.exists(rep+Datas.safe(project)+"/"+Datas.safe(version)) ) {
			print("You already have "+project+" version "+version+" installed");
			setCurrent(project,version,true);
			return;
		}

		// download to temporary file
		var filename = Datas.fileName(project,version);
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

		doInstallFile(filepath,setcurrent);
	}

	function doInstallFile(filepath,setcurrent,?nodelete) {

		// read zip content
		var f = neko.io.File.read(filepath,true);
		var zip = neko.zip.File.read(f);
		f.close();
		var infos = Datas.readInfos(zip);

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
				var data = neko.zip.File.unzip(zipfile);
				var f = neko.io.File.write(target+path,true);
				f.write(data);
				f.close();
			}
		}

		// set current version
		if( setcurrent || !neko.FileSystem.exists(pdir+".current") ) {
			var f = neko.io.File.write(pdir+".current",true);
			f.write(infos.version);
			f.close();
			print("  Current version is now "+infos.version);
		}

		// end
		if( !nodelete )
			neko.FileSystem.deleteFile(filepath);
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
		if( neko.FileSystem.exists(dir) ) {
			if( !neko.FileSystem.isDirectory(dir) )
				throw ("A file is preventing "+dir+" to be created");
			return false;
		}
		try {
			neko.FileSystem.createDirectory(dir);
		} catch( e : Dynamic ) {
			throw "You don't have enough user rights to create the directory "+dir;
		}
		return true;
	}

	function getRepository( ?setup : Bool ) {
		var sys = neko.Sys.systemName();
		if( sys == "Windows" ) {
			var haxepath = neko.Sys.getEnv("HAXEPATH");
			if( haxepath == null )
				throw "HAXEPATH environment variable not defined, please run haxesetup.exe first";
			var rep = haxepath+REPNAME;
			try {
				safeDir(rep);
			} catch( e : Dynamic ) {
				throw "The directory defined by HAXEPATH does not exist, please run haxesetup.exe again";
			}
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
			if( !neko.FileSystem.exists(rep) ) {
				try {
					neko.FileSystem.createDirectory(rep);
				} catch( e : Dynamic ) {
					print("Failed to create directory '"+rep+"' ("+Std.string(e)+"), maybe you need appropriate user rights");
					neko.Sys.exit(1);
				}
			}
			var f = neko.io.File.write(config,true);
			f.write(rep);
			f.close();
		} else if( !neko.FileSystem.exists(rep) )
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

	function list() {
		var rep = getRepository();
		for( p in neko.FileSystem.readDirectory(rep) ) {
			if( p.charAt(0) == "." )
				continue;
			var versions = new Array();
			var current = neko.io.File.getContent(rep+p+"/.current");
			for( v in neko.FileSystem.readDirectory(rep+p) ) {
				if( v.charAt(0) == "." )
					continue;
				v = Datas.unsafe(v);
				if( v == current )
					v = "["+v+"]";
				versions.push(v);
			}
			print(Datas.unsafe(p) + ": "+versions.join(" "));
		}
	}

	function upgrade() {
		var rep = getRepository();
		var prompt = true;
		var update = false;
		for( p in neko.FileSystem.readDirectory(rep) ) {
			if( p.charAt(0) == "." )
				continue;
			var p = Datas.unsafe(p);
			print("Checking "+p);
			var inf = site.infos(p);
			if( !neko.FileSystem.exists(rep+Datas.safe(p)+"/"+Datas.safe(inf.curversion)) ) {
				if( prompt )
					switch ask("Upgrade "+p+" to "+inf.curversion) {
					case Yes:
					case Always: prompt = false;
					case No: continue;
					}
				doInstall(p,inf.curversion,true);
				update = true;
			} else
				setCurrent(p,inf.curversion,true);
		}
		if( update )
			print("Done");
		else
			print("All projects are up-to-date");
	}

	function deleteRec(dir) {
		for( p in neko.FileSystem.readDirectory(dir) ) {
			var path = dir+"/"+p;
			if( neko.FileSystem.isDirectory(path) )
				deleteRec(path);
			else
				neko.FileSystem.deleteFile(path);
		}
		neko.FileSystem.deleteDirectory(dir);
	}

	function remove() {
		var prj = param("Project");
		var version = paramOpt();
		var rep = getRepository();
		var pdir = rep + Datas.safe(prj);

		if( version == null ) {
			if( !neko.FileSystem.exists(pdir) )
				throw "Project "+prj+" is not installed";
			deleteRec(pdir);
			print("Project "+prj+" removed");
			return;
		}

		var vdir = pdir + "/" + Datas.safe(version);
		if( !neko.FileSystem.exists(vdir) )
			throw "Project "+prj+" does not have version "+version+" installed";

		var cur = neko.io.File.getContent(pdir+"/.current");
		if( cur == version )
			throw "Can't remove current version of project "+prj;
		deleteRec(vdir);
		print("Project "+prj+" version "+version+" removed");
	}

	function set() {
		var prj = param("Project");
		var version = param("Version");
		setCurrent(prj,version,false);
	}

	function setCurrent( prj : String, version : String, doAsk : Bool ) {
		var pdir = getRepository() + Datas.safe(prj);
		var vdir = pdir + "/" + Datas.safe(version);
		if( !neko.FileSystem.exists(vdir) )
			throw "Project "+prj+" version "+version+" is not installed";
		var current = pdir+"/.current";
		if( neko.io.File.getContent(current) == version )
			return;
		if( doAsk && ask("Set "+prj+" to version "+version) == No )
			return;
		var f = neko.io.File.write(current,true);
		f.write(version);
		f.close();
		print("Project "+prj+" current version is now "+version);
	}

	function checkRec( prj : String, version : String, l : List<{ project : String, version : String }> ) {
		var pdir = getRepository() + Datas.safe(prj);
		if( !neko.FileSystem.exists(pdir) )
			throw "Project "+prj+" is not installed";
		var version = if( version != null ) version else neko.io.File.getContent(pdir+"/.current");
		var vdir = pdir + "/" + Datas.safe(version);
		if( !neko.FileSystem.exists(vdir) )
			throw "Project "+prj+" version "+version+" is not installed";
		for( p in l )
			if( p.project == prj ) {
				if( p.version == version )
					return;
				throw "Project "+prj+" has two version included "+version+" and "+p.version;
			}
		l.add({ project : prj, version : version });
		var xml = neko.io.File.getContent(vdir+"/haxelib.xml");
		var inf = Datas.readData(xml);
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
			var ndir = dir + "ndll";
			if( neko.FileSystem.exists(ndir) ) {
				var sysdir = ndir+"/"+neko.Sys.systemName();
				if( !neko.FileSystem.exists(sysdir) )
					throw "Project "+d.project+" version "+d.version+" does not have a neko dll for your system";
				neko.Lib.println("-L "+pdir+"ndll/");
			}
			neko.Lib.println(dir);
		}
	}

	function run() {
		var rep = getRepository();
		var project = param("Project");
		var pdir = rep + Datas.safe(project);
		if( !neko.FileSystem.exists(pdir) )
			throw "Project "+project+" is not installed";
		pdir += "/";
		var version = neko.io.File.getContent(pdir+".current");
		var vdir = pdir + Datas.safe(version);
		var rdir = vdir + "/run.n";
		if( !neko.FileSystem.exists(rdir) )
			throw "Project "+project+" version "+version+" does not have a run script";
		args.push(neko.Sys.getCwd());
		neko.Sys.setCwd(vdir);
		var cmd = "neko run.n";
		for( i in argcur...args.length )
			cmd += " "+escapeArg(args[i]);
		neko.Sys.exit(neko.Sys.command(cmd));
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

	// ----------------------------------

	static function print(str) {
		neko.Lib.print(str+"\n");
	}

	static function main() {
		haxe.Http.PROXY = neko.net.ProxyDetect.detect();
		new Main().process();
	}

}
