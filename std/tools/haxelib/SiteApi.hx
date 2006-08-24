package tools.haxelib;
import tools.haxelib.Datas;
import tools.haxelib.SiteDb;

class SiteApi {

	var db : neko.db.Connection;

	public function new( db ) {
		this.db = db;
	}

	public function search( word : String ) : List<{ name : String }> {
		return Lib.manager.containing(word);
	}

	public function infos( lib : String ) : LibraryInfos {
		var l = Lib.manager.search({ name : lib }).first();
		if( l == null )
			throw "No such library : "+lib;
		var vl = Version.manager.search({ library : l.id });
		var versions = new Array();
		for( v in vl )
			versions.push({ name : v.name, comments : v.comments, date : v.date });
		return {
			name : l.name,
			curversion : if( l.version == null ) null else l.version.name,
			desc : l.description,
			versions : versions,
			owner : l.owner.name,
			url : l.website,
		};
	}

	public function user( name : String ) : UserInfos {
		var u = User.manager.search({ name : name }).first();
		if( u == null )
			throw "No such user : "+name;
		var pl = Lib.manager.search({ owner : u.id });
		var libraries = new Array();
		for( p in pl )
			libraries.push(p.name);
		return {
			name : u.name,
			fullname : u.fullname,
			email : u.email,
			libraries : libraries,
		};
	}

	public function register( name : String, pass : String, mail : String, fullname : String ) : Bool {
		if( !Datas.alphanum.match(name) )
			throw "Invalid user name, please use alphanumeric characters";
		var u = new User();
		u.name = name;
		u.pass = pass;
		u.email = mail;
		u.fullname = fullname;
		u.insert();
		return null;
	}

	public function isNewUser( name : String ) : Bool {
		return User.manager.search({ name : name }).first() == null;
	}

	public function checkLibOwner( lib : String, user : String ) : Void {
		var l = Lib.manager.search({ name : lib }).first();
		if( l == null )
			return;
		if( l.owner.name != user )
			throw "Owner of "+l.name+" is '"+l.owner.name+"'";
	}

	public function checkPassword( user : String, pass : String ) : Bool {
		var u = User.manager.search({ name : user }).first();
		return u != null && u.pass == pass;
	}

	public function getSubmitId() : String {
		return Std.string(Std.random(100000000));
	}

	public function processSubmit( id : String, pass : String ) : String {
		var path = Site.TMP_DIR+"/"+Std.parseInt(id)+".tmp";

		var file = try neko.io.File.read(path,true) catch( e : Dynamic ) throw "Invalid file id #"+id;
		var zip = try neko.zip.File.read(file) catch( e : Dynamic ) { file.close(); neko.Lib.rethrow(e); };
		file.close();

		var infos = Datas.readInfos(zip);
		var u = User.manager.search({ name : infos.user }).first();
		if( u == null || u.pass != pass )
			throw "Invalid username or password";

		var l = Lib.manager.search({ name : infos.lib }).first();
		if( l == null ) {
			l = new Lib();
			l.name = infos.lib;
			l.description = infos.desc;
			l.website = infos.url;
			l.owner = u;
			l.insert();
			neko.FileSystem.deleteFile(path);
			return "Project added : submit one more time to send a first version";
		}

		// check owner
		if( l.owner != u )
			throw "Invalid owner";

		// update public infos
		var update = false;
		if( infos.desc != l.description || l.website != infos.url ) {
			l.description = infos.desc;
			l.website = infos.url;
			l.update();
			update = true;
			neko.FileSystem.deleteFile(path);
			return "Project infos updated : submit one more time to send a new version";
		}

		// check version
		var vl = Version.manager.search({ library : l.id });
		for( v in vl )
			if( v.name == infos.version ) {
				neko.FileSystem.deleteFile(path);
				return "This version is already commited, please change version number";
			}

		neko.FileSystem.rename(path,Site.REP_DIR+"/"+Datas.fileName(l.name,infos.version));

		var v = new Version();
		v.library = l;
		v.name = infos.version;
		v.comments = infos.versionDesc;
		v.downloads = 0;
		v.date = Date.now().toString();
		v.insert();

		l.version = v;
		l.update();
		return "Version "+v.name+" (id#"+v.id+") added";
	}

}

