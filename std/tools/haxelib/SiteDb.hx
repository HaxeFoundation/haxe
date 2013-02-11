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
	public var downloads : Int;
	public var owner(dynamic,dynamic) : User;
	public var version(dynamic,dynamic) : Version;

}

class Tag extends neko.db.Object {

	static function RELATIONS() {
		return [
			{ key : "project", prop : "project", manager : Project.manager },
		];
	}

	public static var manager = new TagManager(Tag);

	public var id : Int;
	public var tag : String;
	public var project(dynamic,dynamic) : Project;

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
	public var documentation : Null<String>;

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

	public function containing( word ) : List<{ id : Int, name : String }> {
		word = quote("%"+word+"%");
		return results("SELECT id, name FROM Project WHERE name LIKE "+word+" OR description LIKE "+word);
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

class TagManager extends neko.db.Manager<Tag> {

	public function topTags( n : Int ) {
		return results("SELECT tag, COUNT(*) as count FROM Tag GROUP BY tag ORDER BY count DESC LIMIT "+n);
	}

}

class SiteDb {

	public static function create( db : sys.db.Connection ) {
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
				version INT,
				downloads INT NOT NULL
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
				comments TEXT NOT NULL,
				documentation TEXT NULL
			)
		");
		db.request("DROP TABLE IF EXISTS Developer");
		db.request("
			CREATE TABLE Developer (
				user INTEGER NOT NULL,
				project INTEGER NOT NULL
			)
		");
		db.request("DROP TABLE IF EXISTS Tag");
		db.request("
			CREATE TABLE Tag (
				id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
				tag VARCHAR(32) NOT NULL,
				project INTEGER NOT NULL
			)
		");
		db.request("DROP INDEX IF EXISTS TagSearch");
		db.request("CREATE INDEX TagSearch ON Tag(tag)");
	}
}
