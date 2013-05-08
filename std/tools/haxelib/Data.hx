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
import haxe.zip.Reader;
import haxe.zip.Entry;

import haxe.Json;

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
	var tags : List<String>;
}

typedef Infos = {
	var project : String;
	var website : String;
	var desc : String;
	var license : String;
	var version : String;
	var versionComments : String;
	var developers : List<String>;
	var tags : List<String>;
	var dependencies : List<{ project : String, version : String }>;
}

class Data {

	public static var JSON = "haxelib.json";
	public static var DOCXML = "haxedoc.xml";
	public static var REPOSITORY = "files";
	public static var alphanum = ~/^[A-Za-z0-9_.-]+$/;
	static var LICENSES = ["GPL","LGPL","BSD","Public","MIT"];

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

	public static function locateBasePath( zip : List<Entry> ) {
		for( f in zip ) {
			if( StringTools.endsWith(f.fileName,JSON) ) {
				return f.fileName.substr(0,f.fileName.length - JSON.length);
			}
		}
		throw "No "+JSON+" found";
	}

	public static function readDoc( zip : List<Entry> ) : String {
		for( f in zip )
			if( StringTools.endsWith(f.fileName,DOCXML) )
				return Reader.unzip(f).toString();
		return null;
	}

	public static function readInfos( zip : List<Entry>, check : Bool ) : Infos {
		var infodata = null;
		for( f in zip )
			if( StringTools.endsWith(f.fileName,JSON) ) {
				infodata = Reader.unzip(f).toString();
				break;
			}
		if( infodata == null )
			throw JSON + " not found in package";
		
		return readData(infodata,check);
	}

	static function doCheck( doc : Dynamic ) {
		if( Lambda.indexOf(LICENSES, doc.license) == -1 )
			throw "License must be one of the following: " + LICENSES;
		switch Type.typeof(doc.contributors) {
			case TNull: throw "At least one contributor must be included";
			//case TClass(String): doc.contributors = [doc.contributors];
			case TClass(Array):
			default: throw 'invalid type for contributors';
		}
		switch Type.typeof(doc.version) {
			case TClass(String):
				SemVer.ofString(doc.version);
			default: throw 'version must be defined as string';
		}
		switch Type.typeof(doc.tags) {
			case TClass(Array), TNull:
			default: throw 'tags must be defined as array';
		}
		switch Type.typeof(doc.dependencies) {
			case TObject, TNull:
			default: throw 'dependencies must be defined as object';
		}
		switch Type.typeof(doc.releasenote) {
			case TClass(String):
			case TNull: throw 'no releasenote specified';
			default: throw 'releasenote should be string';
		}
	}

	public static function readData( jsondata: String, check : Bool ) : Infos {
		var doc = try Json.parse(jsondata) catch( e : Dynamic ) throw "Error in JSON data : " + e;
		if( check )
			doCheck(doc);
		var project:String = doc.name;
		if( project.length < 3 )
			throw "Project name must contain at least 3 characters";
		var tags = new List();
		if( doc.tags != null) {
			var tagsArray:Array<String> = doc.tags;
			for( t in tagsArray )
				tags.add(t);
		}
		var devs = new List();
		var developers:Array<String> = doc.contributors;
		
		for( d in developers )
			devs.add(d);
		var deps = new List();
		if( doc.dependencies != null ) {
			for( d in Reflect.fields(doc.dependencies) ) {
				deps.add({ project: d, version: Std.string(Reflect.field(doc.dependencies, d)) });
			}
		}
		
		return {
			project : project,
			website : doc.url,
			desc : doc.description,
			version : doc.version,
			versionComments : doc.releasenote,
			license : doc.license,
			tags : tags,
			developers : devs,
			dependencies : deps
		};
	}

}