package tools.haxelib;

import neko.zip.File;
import haxe.xml.Check;

typedef UserInfos = {
	var name : String;
	var fullname : String;
	var email : String;
	var projects : Array<String>;
}

typedef VersionInfos = {
	var date : String;
	var name : String;
	var comments : String;
}

typedef ProjectInfos = {
	var name : String;
	var desc : String;
	var website : String;
	var owner : String;
	var license : String;
	var curversion : String;
	var versions : Array<VersionInfos>;
}

typedef XmlInfos = {
	var project : String;
	var website : String;
	var desc : String;
	var license : String;
	var version : String;
	var versionComments : String;
	var developers : List<String>;
	var dependencies : List<{ project : String, version : String }>;
}

class Datas {

	public static var XML = "haxelib.xml";
	public static var REPOSITORY = "files";
	public static var alphanum = ~/^[A-Za-z0-9_.-]+$/;
	static var LICENSES = ["GPL","LGPL","BSD","Public"];

	static function requiredAttribute( x : Xml, name ) {
		var v = x.get(name);
		if( v == null )
			throw "Missing required attribute '"+name+"' in node "+x.nodeName;
		return v;
	}

	static function requiredNode( x : Xml, name ) {
		var v = x.elementsNamed(name).next();
		if( v == null )
			throw "Missing required node '"+name+"' in node "+x.nodeName;
		return v;
	}

	static function requiredText( x : Xml ) {
		var v = x.firstChild();
		if( v == null || (v.nodeType != Xml.PCData && v.nodeType != Xml.CData) )
			throw "Missing required text in node "+x.nodeName;
		return v.nodeValue;
	}

	public static function safe( name : String ) {
		if( !alphanum.match(name) )
			throw "Invalid parameter : "+name;
		return name.split(".").join(",");
	}

	public static function unsafe( name : String ) {
		return name.split(",").join(".");
	}

	public static function fileName( lib : String, ver : String ) {
		return safe(lib)+"-"+safe(ver)+".zip";
	}

	public static function readInfos( zip : List<ZipEntry> ) : XmlInfos {
		var xmldata = null;
		for( f in zip )
			if( StringTools.endsWith(f.fileName,XML) ) {
				xmldata = neko.zip.File.unzip(f);
				break;
			}
		if( xmldata == null )
			throw XML+" not found in package";
		return readData(xmldata);

	}

	public static function readData( xmldata : String ) : XmlInfos {
		var sname = Att("name",FReg(alphanum));
		var schema = RNode(
			"project",
			[ sname, Att("url"), Att("license",FEnum(LICENSES)) ],
			RList([
				RMulti( RNode("user",[sname]), true ),
				RNode("description",[],RData()),
				RNode("version",[sname],RData()),
				RMulti(	RNode("depends",[sname,Att("version",FReg(alphanum),"")]) ),
			])
		);
		var doc = Xml.parse(xmldata);
		haxe.xml.Check.checkDocument(doc,schema);

		var p = new haxe.xml.Fast(doc).node.project;
		var project = p.att.name;
		if( project.length < 3 )
			throw "Project name must contain at least 3 characters";
		var devs = new List();
		for( d in p.nodes.user )
			devs.add(d.att.name);
		var deps = new List();
		for( d in p.nodes.depends )
			deps.add({ project : d.att.name, version : d.att.version });
		return {
			project : project,
			website : p.att.url,
			desc : p.node.description.innerData,
			version : p.node.version.att.name,
			versionComments : p.node.version.innerData,
			license : p.att.license,
			developers : devs,
			dependencies : deps,
		}
	}

}
