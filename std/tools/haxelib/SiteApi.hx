package tools.haxlib;
import tools.haxlib.Datas;
import tools.haxlib.SiteDb;

class SiteApi {

	var db : neko.db.Connection;

	static var alphanum = ~/^[A-Za-z0-9_-]+$/;

	public function new( db ) {
		this.db = db;
	}

	public function search( word : String ) : List<{ name : String, fullname : String }> {
		return Lib.manager.containing(word);
	}

	public function infos( prj : String ) : LibraryInfos {
		var p = Lib.manager.search({ name : prj }).first();
		if( p == null )
			throw "No such library : "+prj;
		var vl = Version.manager.search({ library : p.id });
		var versions = new Array();
		for( v in vl )
			versions.push({ name : v.name, comments : v.comments });
		return {
			name : p.name,
			fullname : p.fullname,
			curversion : if( p.version == null ) null else p.version.name,
			desc : p.description,
			versions : versions,
			owner : p.owner.name,
			url : p.website,
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
		if( !alphanum.match(name) )
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

	public function processSubmit( id : String, pass : String ) : Void {
		var path = Site.TMP_DIR+Std.parseInt(id);
		var file = try neko.io.File.read(path,true) catch( e : Dynamic ) throw "Invalid file id #"+id;
		var zip = try neko.zip.File.read(file) catch( e : Dynamic ) { file.close(); neko.Lib.rethrow(e); };
		file.close();

		var xmldata = null;
		for( f in zip )
			if( StringTools.endsWith(f.fileName,Datas.XML) ) {
				xmldata = neko.zip.File.unzip(f);
				break;
			}
		if( xmldata == null )
			throw Datas.XML+" not found in package";

		var infos = Datas.readInfos(xmldata);
	}

}

