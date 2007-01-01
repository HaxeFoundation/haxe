package tools.haxelib;

class User extends neko.db.Object {

	public static var manager = new neko.db.Manager<User>(User);

	public var id : Int;
	public var name : String;
	public var fullname : String;
	public var email : String;
	public var pass : String;

}

class Project extends neko.db.Object {

	static function RELATIONS() {
		return [
			{ key : "owner", prop : "owner", manager : User.manager },
			{ key : "version", prop : "version", manager : Version.manager },
		];
	}

	public static var manager = new ProjectManager(Project);

	public var id : Int;
	public var name : String;
	public var description : String;
	public var website : String;
	public var license : String;
	public var owner(dynamic,dynamic) : User;
	public var version(dynamic,dynamic) : Version;

}

class Version extends neko.db.Object {

	static function RELATIONS() {
		return [{ key : "project", prop : "project", manager : Project.manager }];
	}

	public static var manager = new VersionManager(Version);

	public var id : Int;
	public var project(dynamic,dynamic) : Project;
	public var name : String;
	public var date : String; // sqlite does not have a proper 'date' type
	public var comments : String;
	public var downloads : Int;

}

class Developer extends neko.db.Object {

	static var TABLE_IDS = ["user","project"];
	static function RELATIONS() {
		return [
			{ key : "user", prop : "user", manager : User.manager },
			{ key : "project", prop : "project", manager : Project.manager },
		];
	}

	public static var manager = new neko.db.Manager<Developer>(Developer);

	public var user(dynamic,dynamic) : User;
	public var project(dynamic,dynamic) : Project;

}

class ProjectManager extends neko.db.Manager<Project> {

	public function containing( word ) {
		word = quote("%"+word+"%");
		return results("SELECT name FROM Project WHERE name LIKE "+word+" OR description LIKE "+word);
	}

	public function allByName() {
		return objects("SELECT * FROM Project ORDER BY name COLLATE NOCASE",false);
	}

}

class VersionManager extends neko.db.Manager<Version> {

	public function latest( n : Int ) {
		return objects("SELECT * FROM Version ORDER BY date DESC LIMIT "+n,false);
	}

	public function byProject( p : Project ) {
		return objects("SELECT * FROM Version WHERE project = "+p.id+" ORDER BY date DESC",false);
	}

}

class SiteDb {

	public static function create( db : neko.db.Connection ) {
		db.request("DROP TABLE IF EXISTS User");
		db.request("
			CREATE TABLE User (
				id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
				name VARCHAR(16) NOT NULL UNIQUE,
				fullname VARCHAR(50) NOT NULL,
				pass VARCHAR(32) NOT NULL,
				email VARCHAR(50) NOT NULL
			)
		");
		db.request("DROP TABLE IF EXISTS Project");
		db.request("
			CREATE TABLE Project (
				id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
				owner INTEGER NOT NULL,
				name VARCHAR(32) NOT NULL UNIQUE,
				license VARCHAR(20) NOT NULL,
				description TEXT NOT NULL,
				website VARCHAR(100) NOT NULL,
				version INT
			)
		");
		db.request("DROP TABLE IF EXISTS Version");
		db.request("
			CREATE TABLE Version (
				id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
				project INTEGER NOT NULL,
				downloads INTEGER NOT NULL,
				date VARCHAR(19) NOT NULL,
				name VARCHAR(32) NOT NULL,
				comments TEXT NOT NULL
			)
		");
		db.request("DROP TABLE IF EXISTS Developer");
		db.request("
			CREATE TABLE Developer (
				user INTEGER NOT NULL PRIMARY KEY,
				project INTEGER NOT NULL PRIMARY KEY
			)
		");
	}
}
