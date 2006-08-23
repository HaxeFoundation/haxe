package tools.haxlib;

import neko.zip.File;

typedef UserInfos = {
	var name : String;
	var fullname : String;
	var email : String;
	var libraries : Array<String>;
}

typedef VersionInfos = {
	var date : String;
	var name : String;
	var comments : String;
}

typedef LibraryInfos = {
	var name : String;
	var desc : String;
	var url : String;
	var owner : String;
	var curversion : String;
	var versions : Array<VersionInfos>;
}

typedef XmlInfos = {
	var lib : String;
	var url : String;
	var user : String;
	var desc : String;
	var version : String;
	var versionDesc : String;
}

class Datas {


	static var XML = "haxlib.xml";

	public static var REPOSITORY = "files";
	public static var alphanum = ~/^[A-Za-z0-9_.-]+$/;


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

	public static function fileName( lib : String, ver : String ) {
		return lib.split(".").join("-")+"-"+ver.split(".").join("-")+".zip";
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
		var x = Xml.parse(xmldata).firstElement();
		var lib = requiredAttribute(x,"name");
		if( lib.length < 3 || !alphanum.match(lib) )
			throw "Library name must contain at least 3 characters and only AZaz09_.- characters";
		var url = requiredAttribute(x,"url");
		var user = requiredAttribute(requiredNode(x,"user"),"name");
		if( user.length < 3 || !alphanum.match(user) )
			throw "User name must contain at least 3 characters and only AZaz09_.- characters";
		var desc = requiredText(requiredNode(x,"description"));
		var vnode = requiredNode(x,"version");
		var version = requiredAttribute(vnode,"name");
		if( version.length < 1 || !alphanum.match(version) )
			throw "Version name must contain at least 1 character and only AZaz09_.- characters";
		var vdesc = requiredText(vnode);
		return {
			lib : lib,
			url : url,
			user : user,
			desc : desc,
			version : version,
			versionDesc : vdesc,
		}
	}

}
