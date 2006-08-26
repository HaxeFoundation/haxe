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
	public var owner(dynamic,dynamic) : User;
	public var version(dynamic,dynamic) : Version;

}

class Version extends neko.db.Object {

	static function RELATIONS() {
		return [{ key : "project", prop : "project", manager : Project.manager }];
	}

	public static var manager = new neko.db.Manager<Version>(Version);

	public var id : Int;
	public var project(dynamic,dynamic) : Project;
	public var name : String;
	public var date : String; // sqlite does not have a proper 'date' type
	public var comments : String;
	public var downloads : Int;

}

class ProjectManager extends neko.db.Manager<Project> {

	public function containing( word ) {
		word = quote("%"+word+"%");
		return results("SELECT name FROM Project WHERE name LIKE "+word+" OR description LIKE "+word);
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
	}
}
