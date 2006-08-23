package tools.haxlib;

typedef UserInfos = {
	var name : String;
	var fullname : String;
	var email : String;
	var libraries : Array<String>;
}

typedef VersionInfos = {
	var name : String;
	var comments : String;
}

typedef LibraryInfos = {
	var name : String;
	var fullname : String;
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


	public static var XML = "haxlib.xml";

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

	public static function readInfos( xmldata : String ) : XmlInfos {
		var x = Xml.parse(xmldata).firstElement();
		var lib = requiredAttribute(x,"name");
		var url = requiredAttribute(x,"url");
		var user = requiredAttribute(requiredNode(x,"user"),"name");
		var desc = requiredText(requiredNode(x,"description"));
		var vnode = requiredNode(x,"version");
		var version = requiredAttribute(vnode,"name");
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
