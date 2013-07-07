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
package tools.haxelib;

import haxe.crypto.Md5;
import haxe.*;
import haxe.io.Path;
import haxe.zip.Reader;
import tools.haxelib.Data;
import sys.FileSystem;
import sys.io.*;
import haxe.ds.Option;

using StringTools;

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
		start = Timer.stamp();
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
		var time = Timer.stamp() - start;
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

	static var VERSION = SemVer.ofString('3.0.0');
	static var REPNAME = "lib";
	static var SERVER = {
		host : "lib.haxe.org",
		port : 80,
		dir : "",
		url : "index.n",
		apiVersion : VERSION.major+"."+VERSION.minor,
	};

	var argcur : Int;
	var args : Array<String>;
	var commands : List<{ name : String, doc : String, f : Void -> Void, net : Bool }>;
	var siteUrl : String;
	var site : SiteProxy;

	function new() {
		args = Sys.args();
		commands = new List();
		addCommand("install", install, "install a given library, or all libraries from a hxml file");
		addCommand("list", list, "list all installed libraries", false);
		addCommand("upgrade", upgrade, "upgrade all installed libraries");
		addCommand("update", update, "update a single library");
		addCommand("selfupdate", updateSelf, "update haxelib itself");
		addCommand("remove", remove, "remove a given library/version", false);
		addCommand("set", set, "set the current version for a library", false);
		addCommand("search", search, "list libraries matching a word");
		addCommand("info", info, "list informations on a given library");
		addCommand("user", user, "list informations on a given user");
		addCommand("register", register, "register a new user");
		addCommand("submit", submit, "submit or update a library package");
		addCommand("setup", setup, "set the haxelib repository path", false);
		addCommand("convertxml", convertXml, "convert haxelib.xml file to haxelib.json");
		addCommand("config", config, "print the repository path", false);
		addCommand("path", path, "give paths to libraries", false);
		addCommand("run", run, "run the specified library with parameters", false);
		addCommand("local", local, "install the specified package locally", false);
		addCommand("dev", dev, "set the development directory for a given library", false);
		addCommand("git", git, "uses git repository as library");
		addCommand("proxy", proxy, "setup the Http proxy");
		initSite();
	}
	
	

	function initSite() {
		siteUrl = "http://" + SERVER.host + ":" + SERVER.port + "/" + SERVER.dir;
		site = new SiteProxy(haxe.remoting.HttpConnection.urlConnect(siteUrl + "api/" + SERVER.apiVersion + "/" + SERVER.url).api);
	}

	function param( name, ?passwd ) {
		if( args.length > argcur )
			return args[argcur++];
		Sys.print(name+" : ");
		if( passwd ) {
			var s = new StringBuf();
			do switch Sys.getChar(false) {
				case 10, 13: break;
				case c: s.addChar(c);
			}
			while (true);
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
		print("Haxe Library Manager " + VERSION + " - (c)2006-2013 Haxe Foundation");
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
			case "-notimeout":
				haxe.remoting.HttpConnection.TIMEOUT = 0;
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
					if( c.net ) loadProxy();
					c.f();
				} catch( e : Dynamic ) {
					if( e == "std@host_resolve" ) {
						print("Host "+SERVER.host+" was not found");
						print("Please ensure that your internet connection is on");
						print("If you don't have an internet connection or if you are behing a proxy");
						print("please download manually the file from http://lib.haxe.org/files/3.0/");
						print("and run 'haxelib local <file>' to install the Library.");
						print("You can also setup the proxy with 'haxelib proxy'.");
						Sys.exit(1);
					}
					if( e == "Blocked" ) {
						print("Http connection timeout. Try running haxelib -notimeout <command> to disable timeout");
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
		pass = Md5.encode(pass);
		site.register(name,pass,email,fullname);
		return pass;
	}

	function submit() {
		var file = param("Package");
		var data = File.getBytes(file);
		var zip = Reader.readZip(new haxe.io.BytesInput(data));
		var infos = Data.readInfos(zip,true);
		var user = infos.developers.first();
		var password;
		if( site.isNewUser(user) ) {
			print("This is your first submission as '"+user+"'");
			print("Please enter the following informations for registration");
			password = doRegister(user);
		} else {
			if( infos.developers.length > 1 )
				user = param("User");
			password = Md5.encode(param("Password",true));
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
				if( v.name == infos.version.toString() && ask("You're about to overwrite existing version '"+v.name+"', please confirm") == No )
					throw "Aborted";

		// query a submit id that will identify the file
		var id = site.getSubmitId();

		// directly send the file data over Http
		var h = new Http("http://"+SERVER.host+":"+SERVER.port+"/"+SERVER.url);
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
		var prj = param("Library name or hxml file:");

		// No library given, install libraries listed in *.hxml in given directory
		if( prj == "all")
		{
			installFromAllHxml();
			return;
		}

		if( sys.FileSystem.exists(prj) && !sys.FileSystem.isDirectory(prj) ) {
			// *.hxml provided, install all libraries/versions in this hxml file
			if( prj.endsWith(".hxml") )
			{
				installFromHxml(prj);
				return;
			}
			// *.zip provided, install zip as haxe library
			if( prj.endsWith(".zip") )
			{
				doInstallFile(prj,true,true);
				return;
			}
		}

		// Name provided that wasn't a local hxml or zip, so try to install it from server
		var inf = site.infos(prj);
		var reqversion = paramOpt();
		var version = getVersion(inf, reqversion);
		doInstall(inf.name,version,version == inf.curversion);
	}

	function getVersion( inf:ProjectInfos, ?reqversion:String )
	{
		if( inf.curversion == null )
			throw "The library "+inf.name+" has not yet released a version";
		var version = if( reqversion != null ) reqversion else inf.curversion;
		var found = false;
		for( v in inf.versions )
			if( v.name == version ) {
				found = true;
				break;
			}
		if( !found )
			throw "No such version "+version+" for library "+inf.name;
		
		return version;
	}

	function installFromHxml( path )
	{
		var hxml = sys.io.File.getContent(path);
		var lines = hxml.split("\n");

		var libsToInstall = new Map<String, {name:String,version:String}>();
		for (l in lines)
		{
			l = l.trim();
			if (l.startsWith("-lib"))
			{
				var key = l.substr(5);
				var parts = key.split(":");
				var libName = parts[0].trim();
				var libVersion = if (parts.length > 1) parts[1].trim() else null;

				if (libsToInstall.exists(key) == false)
				{
					libsToInstall.set(key, { name:libName, version:libVersion });
				}
			}
		}
		installMany(libsToInstall);
	}

	function installFromAllHxml()
	{
		var hxmlFiles = sys.FileSystem.readDirectory(Sys.getCwd()).filter(function (f) return f.endsWith(".hxml"));
		if (hxmlFiles.length > 0)
		{
			for (file in hxmlFiles)
			{
				if (file.endsWith(".hxml"))
				{
					print('Installing all libraries from $file:');
					installFromHxml(Sys.getCwd()+file);
				}
			}
		}
		else 
		{
			print ("No hxml files found in the current directory.");
		}
	}

	function installMany( libs:Iterable<{name:String,version:String}>, ?setCurrent=true )
	{
		if (Lambda.count(libs) == 0) return;

		// Check the version numbers are all good
		// TODO: can we collapse this into a single API call?  It's getting too slow otherwise.
		print("Loading info about the required libraries");
		for (l in libs)
		{
			var inf = site.infos(l.name);
			l.version = getVersion(inf, l.version);
		}

		// Print a list with all the info
		print("Haxelib is going to install these libraries:");
		for (l in libs)
		{
			var vString = (l.version == null) ? "" : " - " + l.version;
			print("  " + l.name + vString);
		}

		// Install if they confirm
		if (ask("Continue?") != No)
		{
			for (l in libs)
			{
				doInstall(l.name, l.version, setCurrent);
			}
		}
	}

	function doInstall( project, version, setcurrent ) {
		var rep = getRepository();

		// check if exists already
		if( FileSystem.exists(rep+Data.safe(project)+"/"+Data.safe(version)) ) {
			print("You already have "+project+" version "+version+" installed");
			setCurrent(project,version,true);
			return;
		}

		// download to temporary file
		var filename = Data.fileName(project,version);
		var filepath = rep+filename;
		var out = File.write(filepath,true);
		var progress = new Progress(out);
		var h = new Http(siteUrl+Data.REPOSITORY+"/"+filename);
		h.onError = function(e) {
			progress.close();
			FileSystem.deleteFile(filepath);
			throw e;
		};
		print("Downloading "+filename+"...");
		h.customRequest(false,progress);

		doInstallFile(filepath, setcurrent);
		site.postInstall(project, version);
	}

	function doInstallFile(filepath,setcurrent,?nodelete) {
		// read zip content
		var f = File.read(filepath,true);
		var zip = Reader.readZip(f);
		f.close();
		var infos = Data.readInfos(zip,false);
		// create directories
		var pdir = getRepository() + Data.safe(infos.project);
		safeDir(pdir);
		pdir += "/";
		var target = pdir + Data.safe(infos.version.toString());
		safeDir(target);
		target += "/";

		// locate haxelib.json base path
		var basepath = Data.locateBasePath(zip);

		// unzip content
		for( zipfile in zip ) {
			var n = zipfile.fileName;
			if( n.startsWith(basepath) ) {
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
				File.saveBytes(target+path,data);
			}
		}

		// set current version
		if( setcurrent || !FileSystem.exists(pdir+".current") ) {
			File.saveContent(pdir + ".current", infos.version.toString());
			print("  Current version is now "+infos.version);
		}

		// end
		if( !nodelete )
			FileSystem.deleteFile(filepath);
		print("Done");

		// process dependencies
		doInstallDependencies(infos.dependencies);
	}

	function doInstallDependencies( dependencies:List<{ project: String, version : String }> )
	{
		for( d in dependencies ) {
			print("Installing dependency "+d.project+" "+d.version);
			if( d.version == "" )
				d.version = site.infos(d.project).curversion;
			doInstall(d.project,d.version,false);
		}
	}

	function safeDir( dir ) {
		if( FileSystem.exists(dir) ) {
			if( !FileSystem.isDirectory(dir) )
				throw ("A file is preventing "+dir+" to be created");
		}
		try {
			FileSystem.createDirectory(dir);
		} catch( e : Dynamic ) {
			throw "You don't have enough user rights to create the directory "+dir;
		}
		return true;
	}

	function safeDelete( file ) {
		try {
			FileSystem.deleteFile(file);
			return true;
		} catch (e:Dynamic) {
			if( Sys.systemName() == "Windows") {
				try {
					Sys.command("attrib -R \"" +file+ "\"");
					FileSystem.deleteFile(file);
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
			File.getContent(config_file)
		catch( e : Dynamic ) try
			File.getContent("/etc/.haxelib")
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
				} catch( e : String ) {
					throw "Error accessing Haxelib repository: $e";
				}
				return rep+"\\";
			} else
				throw "This is the first time you are runing haxelib. Please run `haxelib setup` first";
		}
		rep = rep.trim();
		if( setup ) {
			if( args.length <= argcur ) {
				print("Please enter haxelib repository path with write access");
				print("Hit enter for default (" + rep + ")");
			}
			var line = param("Path");
			if( line != "" )
				rep = line;
			if( !FileSystem.exists(rep) ) {
				try {
					FileSystem.createDirectory(rep);
				} catch( e : Dynamic ) {
					print("Failed to create directory '"+rep+"' ("+Std.string(e)+"), maybe you need appropriate user rights");
					print("Check also that the parent directory exists");
					Sys.exit(1);
				}
			}
			rep = try FileSystem.fullPath(rep) catch( e : Dynamic ) rep;
			File.saveContent(config_file, rep);
		} else if( !FileSystem.exists(rep) ) {
			throw "haxelib Repository "+rep+" does not exists. Please run `haxelib setup` again";
		} else if ( !FileSystem.isDirectory(rep) ) {
			throw "haxelib Repository "+rep+" exists, but was a file, not a directory.  Please remove it and run `haxelib setup` again.";
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

	function getCurrent( dir ) {
		return (FileSystem.exists(dir+"/.dev")) ? "dev" : File.getContent(dir + "/.current").trim();
	}

	function getDev( dir ) {
		return File.getContent(dir + "/.dev").trim();
	}

	function list() {
		var rep = getRepository();
		var folders = FileSystem.readDirectory(rep);
		var filter = paramOpt();
		if ( filter != null )
			folders = folders.filter( function (f) return f.toLowerCase().indexOf(filter.toLowerCase()) > -1 );
		for( p in folders ) {
			if( p.charAt(0) == "." )
				continue;
			var versions = new Array();
			var current = getCurrent(rep + p);
			var dev = try File.getContent(rep+p+"/.dev").trim() catch( e : Dynamic ) null;
			for( v in FileSystem.readDirectory(rep+p) ) {
				if( v.charAt(0) == "." )
					continue;
				v = Data.unsafe(v);
				if( dev == null && v == current )
					v = "["+v+"]";
				versions.push(v);
			}
			if( dev != null )
				versions.push("[dev:"+dev+"]");
			print(Data.unsafe(p) + ": "+versions.join(" "));
		}
	}

	function upgrade() {
		var state = { rep : getRepository(), prompt : true, updated : false };
		for( p in FileSystem.readDirectory(state.rep) ) {
			if( p.charAt(0) == "." || !FileSystem.isDirectory(state.rep+"/"+p) )
				continue;
			var p = Data.unsafe(p);
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
		if( FileSystem.exists(rep + "/" + p + "/git") && FileSystem.isDirectory(rep + "/" + p + "/git") ) {
			checkGit();
			var oldCwd = Sys.getCwd();
			Sys.setCwd(rep + "/" + p + "/git");
			Sys.command("git pull");
			Sys.setCwd(oldCwd);
			state.updated = true;
		} else {
			var inf = try site.infos(p) catch( e : Dynamic ) { Sys.println(e); return; };
			if( !FileSystem.exists(rep+Data.safe(p)+"/"+Data.safe(inf.curversion)) ) {
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
	function updateByName(prj:String) {
		var state = { rep : getRepository(), prompt : false, updated : false };
		doUpdate(prj,state);
		return state.updated;
	}
	function update() {
		var prj = param('Library');
		if (!updateByName(prj))
			print(prj + " is up to date");
	}	
	
	function updateSelf() {
		function tryBuild() {
			var p = new Process('haxe', ['-neko', 'test.n', '-lib', 'haxelib_client', '-main', 'tools.haxelib.Main', '--no-output']);
			return 
				if (p.exitCode() == 0) None;
				else Some(p.stderr.readAll().toString());
		}
		if (!updateByName('haxelib_client'))
			print("haxelib is up to date");
		switch tryBuild() {
			case None:
				var win = Sys.systemName() == "Windows";
				var haxepath = 
					if (win) Sys.getEnv("HAXEPATH");
					else new Path(new Process('which', ['haxelib']).stdout.readAll().toString()).dir + '/';
					
				if (haxepath == null) 
					throw (win ? 'HAXEPATH environment variable not defined' : 'unable to locate haxelib through `which haxelib`');
				else 
					haxepath += 
						switch (haxepath.charAt(haxepath.length - 1)) {
							case '/', '\\': '';
							default: '/';
						}
				
				if (win) {
					File.saveContent('update.hxml', '-lib haxelib_client\n--run tools.haxelib.Rebuild');
					Sys.println('Please run haxe update.hxml');
				}
				else {
					var p = new Process('haxelib', ['path', 'haxelib_client']);
					if (p.exitCode() == 0) {
						var args = [];
						for (arg in p.stdout.readAll().toString().split('\n')) {
							arg = arg.trim();
							if (arg.charAt(0) == '-') 
								args.push(arg);
							else if (arg.length > 0) 
								args.push('-cp "$arg"');
						};
						
						var file = haxepath+'haxelib';
						try File.saveContent(
							file,
							'#!/bin/sh\nhaxe '+args.join(' ')+' --run tools.haxelib.Main $@'
						)
						catch (e:Dynamic) 
							throw 'Error writing file $file. Please ensure you have write permissions. \n  ' + Std.string(e);
					}
					else throw p.stdout.readAll();
				}
			case Some(error):
				throw 'Error compiling haxelib client: $error';
		}
	}

	function deleteRec(dir) {
		for( p in FileSystem.readDirectory(dir) ) {
			var path = dir+"/"+p;
			if( FileSystem.isDirectory(path) )
				deleteRec(path);
			else
				safeDelete(path);
		}
		FileSystem.deleteDirectory(dir);
	}

	function remove() {
		var prj = param("Library");
		var version = paramOpt();
		var rep = getRepository();
		var pdir = rep + Data.safe(prj);
		if( version == null ) {
			if( !FileSystem.exists(pdir) )
				throw "Library "+prj+" is not installed";
			deleteRec(pdir);
			print("Library "+prj+" removed");
			return;
		}

		var vdir = pdir + "/" + Data.safe(version);
		if( !FileSystem.exists(vdir) )
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
		var pdir = getRepository() + Data.safe(prj);
		var vdir = pdir + "/" + Data.safe(version);
		if( !FileSystem.exists(vdir) )
			throw "Library "+prj+" version "+version+" is not installed";
		if( getCurrent(pdir) == version )
			return;
		if( doAsk && ask("Set "+prj+" to version "+version) == No )
			return;
		File.saveContent(pdir+"/.current",version);
		print("Library "+prj+" current version is now "+version);
	}

	function checkRec( prj : String, version : String, l : List<{ project : String, version : String }> ) {
		var pdir = getRepository() + Data.safe(prj);
		if( !FileSystem.exists(pdir) )
			throw "Library "+prj+" is not installed : run 'haxelib install "+prj+"'";
		var version = if( version != null ) version else getCurrent(pdir);
		var vdir = pdir + "/" + Data.safe(version);
		if( vdir.endsWith("dev") )
			vdir = getDev(pdir);
		if( !FileSystem.exists(vdir) )
			throw "Library "+prj+" version "+version+" is not installed";
		for( p in l )
			if( p.project == prj ) {
				if( p.version == version )
					return;
				throw "Library "+prj+" has two version included "+version+" and "+p.version;
			}
		l.add({ project : prj, version : version });
		var json = try File.getContent(vdir+"/"+Data.JSON) catch( e : Dynamic ) null;
		if( json == null )
			return; // ignore missing haxelib.json, assume no dependencies
		var inf = Data.readData(json,false);
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
			var pdir = Data.safe(d.project)+"/"+Data.safe(d.version)+"/";
			var dir = rep + pdir;
			try {
				dir = getDev(rep+Data.safe(d.project));
				if( dir.length == 0 || (dir.charAt(dir.length-1) != '/' && dir.charAt(dir.length-1) != '\\') )
					dir += "/";
				pdir = dir;
			} catch( e : Dynamic ) {
			}
			var ndir = dir + "ndll";
			if( FileSystem.exists(ndir) ) {
				var sysdir = ndir+"/"+Sys.systemName();
				var is64 = neko.Lib.load("std", "sys_is64", 0)();
				if( is64 ) sysdir += "64";
				if( !FileSystem.exists(sysdir) )
					throw "Library "+d.project+" version "+d.version+" does not have a neko dll for your system";
				Sys.println("-L "+pdir+"ndll/");
			}
			try {
				var f = File.getContent(dir + "extraParams.hxml");
				Sys.println(f.trim());
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
		var proj = rep + Data.safe(project);
		if( !FileSystem.exists(proj) ) {
			FileSystem.createDirectory(proj);
			File.saveContent(proj + "/.current", "dev");
		}
		var devfile = proj+"/.dev";
		if( dir == null ) {
			if( FileSystem.exists(devfile) )
				FileSystem.deleteFile(devfile);
			print("Development directory disabled");
		} else {
			dir = try FileSystem.fullPath(dir)+"/" catch( e : Dynamic ) rep;
			try {
				File.saveContent(devfile, dir);
				print("Development directory set to "+dir);
				
				try {
					// Check for haxelib.json, install dependencies
					var haxelibJsonPath = dir + "haxelib.json";
					if (FileSystem.exists(haxelibJsonPath))
					{
						var haxelibJson = File.getContent(haxelibJsonPath);
						var infos = Data.readData(haxelibJson,true);
						doInstallDependencies(infos.dependencies);
					}
				}
				catch (e:Dynamic) {
					print('Error installing dependencies for $project:\n  $e');
				}
			}
			catch (e:Dynamic) {
				print("Could not write to " +proj + "/.dev");
			}

		}
	}

	function checkGit() {
		var gitExists = function()
			try { command("git", []); return true; } catch (e:Dynamic) return false;
		if( gitExists() )
			return;
		// if we have already msys git/cmd in our PATH
		var match = ~/(.*)git([\\|\/])cmd$/ ;
		for (path in Sys.getEnv("PATH").split(";"))	{
			if (match.match(path.toLowerCase()))
			{
				var newPath = match.matched(1) + "git" +match.matched(2) + "bin";
				Sys.putEnv("PATH", Sys.getEnv("PATH") + ";" +newPath);
			}
		}
		if( gitExists() )
			return;
		// look at a few default paths
		for( path in ["C:\\Program Files (x86)\\Git\\bin","C:\\Progra~1\\Git\\bin"] )
			if( FileSystem.exists(path) ) {
				Sys.putEnv("PATH", Sys.getEnv("PATH") + ";" +path);
				if( gitExists() )
					return;
			}
		print("Could not execute git, please make sure it is installed and available in your PATH.");
	}

	function git() {
		var libName = param("Library name");
		var rep = getRepository();
		var libPath = rep + Data.safe(libName) + "/git";

		if( FileSystem.exists(libPath) ) {
			var state = { rep : rep, prompt : false, updated : false };
			doUpdate(libName,state);
			if( !state.updated )
				print("You already have a git version of "+libName+" installed");
			return;
		}

		var gitPath = param("Git path");
		var branch = paramOpt();
		var subDir = paramOpt();

		print("Installing " +libName + " from " +gitPath);
		checkGit();

		if( Sys.command("git clone \"" +gitPath + "\" \"" +libPath + "\"") != 0 ) {
			print("Could not clone git repository");
			return;
		}
		Sys.setCwd(libPath);
		if (branch != null) {
			var ret = command("git", ["checkout", branch]);
			if (ret.code != 0)
			{
				print("Could not checkout branch, tag or path: " +ret.out);
				// TODO: We might have to get rid of the cloned repository here
				return;
			}
		}
		var revision = command("git", ["rev-parse", "HEAD"]).out;

		var devPath = libPath + (subDir == null ? "" : "/" + subDir);
		var haxelibJsonPath = devPath + "/haxelib.json";
		var haxelibJson:String;
		if (FileSystem.exists(haxelibJsonPath))
		{
			haxelibJson = File.getContent(haxelibJsonPath);
		}
		else
		{
			haxelibJson = '{
  "name": "$libName",
  "url" : "$gitPath",
  "license": "",
  "tags": [],
  "description": "",
  "version": "0.0.0",
  "releasenote": "Updated from git.",
  "contributors": [],
  "dependencies": {}
}';
			File.saveContent(haxelibJsonPath, haxelibJson);
		}

		var infos = Data.readData(haxelibJson,true);
		doInstallDependencies(infos.dependencies);

		Sys.setCwd(libPath + "/../");
		File.saveContent(".current", "dev");
		File.saveContent(".dev", devPath);
		print("Done");
	}

	function run() {
		var rep = getRepository();
		var project = param("Library");
		var temp = project.split(":");
		project = temp[0];
		var pdir = rep + Data.safe(project);
		if( !FileSystem.exists(pdir) )
			throw "Library "+project+" is not installed";
		pdir += "/";
		var version = temp[1] != null ? temp[1] : getCurrent(pdir);
		var dev = try getDev(pdir) catch ( e : Dynamic ) null;
		var vdir = dev!=null ? dev : pdir + Data.safe(version);
		var rdir = vdir + "/run.n";
		if( !FileSystem.exists(rdir) )
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

	function local() {
		var file = param("Package");
		doInstallFile(file,true,true);
	}

	function command( cmd:String, args:Array<String> ) {
		var p = new sys.io.Process(cmd, args);
		var code = p.exitCode();
		return { code:code, out: code == 0 ? p.stdout.readAll().toString() : p.stderr.readAll().toString() };
	}

	function proxy() {
		var rep = getRepository();
		var host = param("Proxy host");
		if( host == "" ) {
			if( FileSystem.exists(rep + "/.proxy") ) {
				FileSystem.deleteFile(rep + "/.proxy");
				print("Proxy disabled");
			} else
				print("No proxy specified");
			return;
		}
		var port = Std.parseInt(param("Proxy port"));
		var authName = param("Proxy user login");
		var authPass = authName == "" ? "" : param("Proxy user pass");
		var proxy = {
			host : host,
			port : port,
			auth : authName == "" ? null : { user : authName, pass : authPass },
		};
		Http.PROXY = proxy;
		print("Testing proxy...");
		try Http.requestUrl("http://www.google.com") catch( e : Dynamic ) {
			print("Proxy connection failed");
			return;
		}
		File.saveContent(rep + "/.proxy", haxe.Serializer.run(proxy));
		print("Proxy setup done");
	}

	function loadProxy() {
		var rep = getRepository();
		try Http.PROXY = haxe.Unserializer.run(File.getContent(rep + "/.proxy")) catch( e : Dynamic ) { };
	}

	function convertXml() {
		var cwd = Sys.getCwd();
		var xmlFile = cwd + "haxelib.xml";
		var jsonFile = cwd + "haxelib.json";

		if (!FileSystem.exists(xmlFile))
		{
			print('No `haxelib.xml` file was found in the current directory.');
			Sys.exit(0);
		}

		var xmlString = File.getContent(xmlFile);
		var json = convert(xmlString);
		var jsonString = prettyPrint(json);

		File.saveContent(jsonFile, jsonString);
		print('Saved to $jsonFile');
	}

	function convert(inXml:String) {
		// Set up the default JSON structure
		var json = {
			"name": "",
			"url" : "",
			"license": "",
			"tags": [],
			"description": "",
			"version": "0.0.1",
			"releasenote": "",
			"contributors": [],
			"dependencies": {}
		};

		// Parse the XML and set the JSON
		var xml = Xml.parse(inXml);
		var project = xml.firstChild();
		json.name = project.get("name");
		json.license = project.get("license");
		json.url = project.get("url");
		for (node in project)
		{
			switch (node.nodeType)
			{
				case Xml.Element:
					switch (node.nodeName)
					{
						case "tag": 
							json.tags.push(node.get("v"));
						case "user":
							json.contributors.push(node.get("name"));
						case "version":
							json.version = node.get("name");
							json.releasenote = node.firstChild().toString();
						case "description":
							json.description = node.firstChild().toString();
						case "depends":
							var name = node.get("name");
							var version = node.get("version");
							if (version == null) version = "";
							Reflect.setField(json.dependencies, name, version);
						default: 
					}
				default: 
			}
		}

		return json;
	}

	function prettyPrint(json:Dynamic, indent="")
	{
		var sb = new StringBuf();
		sb.add("{\n");

		var firstRun = true;
		for (f in Reflect.fields(json))
		{
			if (!firstRun) sb.add(",\n");
			firstRun = false;

			var value = switch (f) {
				case "dependencies":
					var d = Reflect.field(json, f);
					prettyPrint(d, indent + "  ");
				default: 
					Json.stringify(Reflect.field(json, f));
			}
			sb.add(indent+'  "$f": $value');
		}

		sb.add('\n$indent}');
		return sb.toString();
	}

	// ----------------------------------

	static function print(str) {
		Sys.print(str+"\n");
	}

	static function main() {
		new Main().process();
	}

}
