/*
 * Copyright (c) 2005-2009, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe.rtti;
import haxe.rtti.CType;
import haxe.xml.Fast;

class XmlParser {

	public var root : TypeRoot;
	var curplatform : String;

	public function new() {
		root = new Array();
	}

	public function sort( ?l ) {
		if( l == null ) l = root;
		l.sort(function(e1,e2) {
			var n1 = switch e1 {
				case TPackage(p,_,_) : " "+p;
				default: TypeApi.typeInfos(e1).path;
			};
			var n2 = switch e2 {
				case TPackage(p,_,_) : " "+p;
				default: TypeApi.typeInfos(e2).path;
			};
			if( n1 > n2 )
				return 1;
			return -1;
		});
		for( x in l )
			switch( x ) {
			case TPackage(_,_,l): sort(l);
			case TClassdecl(c):
				c.fields = sortFields(c.fields);
				c.statics = sortFields(c.statics);
			case TEnumdecl(e):
			case TTypedecl(_):
			}
	}

	function sortFields(fl) {
		var a = Lambda.array(fl);
		a.sort(function(f1 : ClassField,f2 : ClassField) {
			var v1 = TypeApi.isVar(f1.type);
			var v2 = TypeApi.isVar(f2.type);
			if( v1 && !v2 )
				return -1;
			if( v2 && !v1 )
				return 1;
			if( f1.name == "new" )
				return -1;
			if( f2.name == "new" )
				return 1;
			if( f1.name > f2.name )
				return 1;
			return -1;
		});
		return Lambda.list(a);
	}

	public function process( x : Xml, platform ) {
		curplatform = platform;
		xroot(new Fast(x));
	}

	// merge inline and not inline
	function mergeRights( f1 : ClassField, f2 : ClassField ) {
		if( f1.get == RInline && f1.set == RNo && f2.get == RNormal && f2.set == RMethod ) {
			f1.get = RNormal;
			f1.set = RMethod;
			return true;
		}
		return false;
	}

	function mergeDoc( f1 : ClassField, f2 : ClassField ) {
		if( f1.doc == null )
			f2.doc = f2.doc;
		else if( f2.doc == null )
			f2.doc = f1.doc;
		return true;
	}

	function mergeFields( f : ClassField, f2 : ClassField ) {
		return TypeApi.fieldEq(f,f2) || (f.name == f2.name && (mergeRights(f,f2) || mergeRights(f2,f)) && mergeDoc(f,f2) && TypeApi.fieldEq(f,f2));
	}

	public dynamic function newField( c : Classdef, f : ClassField ) {
	}

	function mergeClasses( c : Classdef, c2 : Classdef ) {
		// todo : compare supers & interfaces
		if( c.isInterface != c2.isInterface )
			return false;
		if( curplatform != null )
			c.platforms.add(curplatform);
		if( c.isExtern != c2.isExtern )
			c.isExtern = false;

		for( f2 in c2.fields ) {
			var found = null;
			for( f in c.fields )
				if( mergeFields(f,f2) ) {
					found = f;
					break;
				}
			if( found == null ) {
				newField(c,f2);
				c.fields.add(f2);
			} else if( curplatform != null )
				found.platforms.add(curplatform);
		}
		for( f2 in c2.statics ) {
			var found = null;
			for( f in c.statics )
				if( mergeFields(f,f2) ) {
					found = f;
					break;
				}
			if( found == null ) {
				newField(c,f2);
				c.statics.add(f2);
			} else if( curplatform != null )
				found.platforms.add(curplatform);
		}
		return true;
	}

	function mergeEnums( e : Enumdef, e2 : Enumdef ) {
		if( e.isExtern != e2.isExtern )
			return false;
		if( curplatform != null )
			e.platforms.add(curplatform);
		for( c2 in e2.constructors ) {
			var found = null;
			for( c in e.constructors )
				if( TypeApi.constructorEq(c,c2) ) {
					found = c;
					break;
				}
			if( found == null )
				return false; // don't allow by-platform constructor ?
			if( curplatform != null )
				found.platforms.add(curplatform);
		}
		return true;
	}

	function mergeTypedefs( t : Typedef, t2 : Typedef ) {
		if( curplatform == null )
			return false;
		t.platforms.add(curplatform);
		t.types.set(curplatform,t2.type);
		return true;
	}

	function merge( t : TypeTree ) {
		var inf = TypeApi.typeInfos(t);
		var pack = inf.path.split(".");
		var cur = root;
		var curpack = new Array();
		pack.pop();
		for( p in pack ) {
			var found = false;
			for( pk in cur )
				switch( pk ) {
				case TPackage(pname,_,subs):
					if( pname == p ) {
						found = true;
						cur = subs;
						break;
					}
				default:
				}
			curpack.push(p);
			if( !found ) {
				var pk = new Array();
				cur.push(TPackage(p,curpack.join("."),pk));
				cur = pk;
			}
		}
		var prev = null;
		for( ct in cur ) {
			var tinf;
			try
				tinf = TypeApi.typeInfos(ct)
			catch( e : Dynamic )
				continue;
			// compare params ?
			if( tinf.path == inf.path ) {
				var sameType = true;
				if( (tinf.doc == null) != (inf.doc == null) ) {
					if( inf.doc == null )
						inf.doc = tinf.doc;
					else
						tinf.doc = inf.doc;
				}
				if( tinf.module == inf.module && tinf.doc == inf.doc && tinf.isPrivate == inf.isPrivate )
					switch( ct ) {
					case TClassdecl(c):
						switch( t ) {
						case TClassdecl(c2):
							if( mergeClasses(c,c2) )
								return;
						default:
							sameType = false;
						}
					case TEnumdecl(e):
						switch( t ) {
						case TEnumdecl(e2):
							if( mergeEnums(e,e2) )
								return;
						default:
							sameType = false;
						}
					case TTypedecl(td):
						switch( t ) {
						case TTypedecl(td2):
							if( mergeTypedefs(td,td2) )
								return;
						default:
						}
					case TPackage(_,_,_):
						sameType = false;
					}
				// we already have a mapping, but which is incompatible
				var msg = if( tinf.module != inf.module ) "module "+inf.module+" should be "+tinf.module;
					else if( tinf.doc != inf.doc ) "documentation is different";
					else if( tinf.isPrivate != inf.isPrivate ) "private flag is different";
					else if( !sameType ) "type kind is different";
					else "could not merge definition";
				throw "Incompatibilities between "+tinf.path+" in "+tinf.platforms.join(",")+" and "+curplatform+" ("+msg+")";
			}
		}
		cur.push(t);
	}

	function mkPath( p : String ) : Path {
		return p;
	}

	function mkTypeParams( p : String ) : TypeParams {
		var pl = p.split(":");
		if( pl[0] == "" )
			return new Array();
		return pl;
	}

	function mkRights( r : String ) : Rights {
		return switch( r ) {
		case "null": RNo;
		case "method": RMethod;
		case "dynamic": RDynamic;
		case "inline": RInline;
		default: RCall(r);
		}
	}

	function xerror( c : Fast ) : Dynamic {
		return throw "Invalid "+c.name;
	}

	function xroot( x : Fast ) {
		for( c in x.x.elements() )
			merge(processElement(c));

	}

	public function processElement( x : Xml ) {
		var c = new haxe.xml.Fast(x);
		return switch( c.name ) {
		case "class": TClassdecl(xclass(c));
		case "enum": TEnumdecl(xenum(c));
		case "typedef": TTypedecl(xtypedef(c));
		default: xerror(c);
		}
	}

	function xpath( x : Fast ) : PathParams {
		var path = mkPath(x.att.path);
		var params = new List();
		for( c in x.elements )
			params.add(xtype(c));
		return {
			path : path,
			params : params,
		};
	}

	function xclass( x : Fast ) : Classdef {
		var csuper = null;
		var doc = null;
		var tdynamic = null;
		var interfaces = new List();
		var fields = new List();
		var statics = new List();
		for( c in x.elements )
			switch( c.name ) {
			case "haxe_doc": doc = c.innerData;
			case "extends": csuper = xpath(c);
			case "implements": interfaces.add(xpath(c));
			case "haxe_dynamic": tdynamic = xtype(new Fast(c.x.firstElement()));
			case "meta":
			default:
				if( c.x.exists("static") )
					statics.add(xclassfield(c));
				else
					fields.add(xclassfield(c));
			}
		return {
			file : if(x.has.file) x.att.file else null,
			path : mkPath(x.att.path),
			module : if( x.has.module ) mkPath(x.att.module) else null,
			doc : doc,
			isPrivate : x.x.exists("private"),
			isExtern : x.x.exists("extern"),
			isInterface : x.x.exists("interface"),
			params : mkTypeParams(x.att.params),
			superClass : csuper,
			interfaces : interfaces,
			fields : fields,
			statics : statics,
			tdynamic : tdynamic,
			platforms : defplat(),
		};
	}

	function xclassfield( x : Fast ) : ClassField {
		var e = x.elements;
		var t = xtype(e.next());
		var doc = null;
		for( c in e )
			switch( c.name ) {
			case "haxe_doc": doc = c.innerData;
			case "meta":
			default: xerror(c);
			}
		return {
			name : x.name,
			type : t,
			isPublic : x.x.exists("public"),
			isOverride : x.x.exists("override"),
			doc : doc,
			get : if( x.has.get ) mkRights(x.att.get) else RNormal,
			set : if( x.has.set ) mkRights(x.att.set) else RNormal,
			params : if( x.has.params ) mkTypeParams(x.att.params) else null,
			platforms : defplat(),
		};
	}

	function xenum( x : Fast ) : Enumdef {
		var cl = new List();
		var doc = null;
		for( c in x.elements )
			if( c.name == "haxe_doc" )
				doc = c.innerData;
			else if ( c.name == "meta" ) { }
			else
				cl.add(xenumfield(c));
		return {
			file : if(x.has.file) x.att.file else null,
			path : mkPath(x.att.path),
			module : if( x.has.module ) mkPath(x.att.module) else null,
			doc : doc,
			isPrivate : x.x.exists("private"),
			isExtern : x.x.exists("extern"),
			params : mkTypeParams(x.att.params),
			constructors : cl,
			platforms : defplat(),
		};
	}

	function xenumfield( x : Fast ) : EnumField {
		var args = null;
		var xdoc = x.x.elementsNamed("haxe_doc").next();
		if( x.has.a ) {
			var names = x.att.a.split(":");
			var elts = x.elements;
			args = new List();
			for( c in names ) {
				var opt = false;
				if( c.charAt(0) == "?" ) {
					opt = true;
					c = c.substr(1);
				}
				args.add({
					name : c,
					opt : opt,
					t : xtype(elts.next()),
				});
			}
		}
		return {
			name : x.name,
			args : args,
			doc : if( xdoc == null ) null else new Fast(xdoc).innerData,
			platforms : defplat(),
		};
	}

	function xtypedef( x : Fast ) : Typedef {
		var doc = null;
		var t = null;
		for( c in x.elements )
			if( c.name == "haxe_doc" )
				doc = c.innerData;
			else if ( c.name == "meta" ) { }
			else
				t = xtype(c);
		var types = new Hash();
		if( curplatform != null )
			types.set(curplatform,t);
		return {
			file : if(x.has.file) x.att.file else null,
			path : mkPath(x.att.path),
			module : if( x.has.module ) mkPath(x.att.module) else null,
			doc : doc,
			isPrivate : x.x.exists("private"),
			params : mkTypeParams(x.att.params),
			type : t,
			types : types,
			platforms : defplat(),
		};
	}

	function xtype( x : Fast ) : CType {
		return switch( x.name ) {
		case "unknown":
			CUnknown;
		case "e":
			CEnum(mkPath(x.att.path),xtypeparams(x));
		case "c":
			CClass(mkPath(x.att.path),xtypeparams(x));
		case "t":
			CTypedef(mkPath(x.att.path),xtypeparams(x));
		case "f":
			var args = new List();
			var aname = x.att.a.split(":");
			var eargs = aname.iterator();
			for( e in x.elements ) {
				var opt = false;
				var a = eargs.next();
				if( a == null )
					a = "";
				if( a.charAt(0) == "?" ) {
					opt = true;
					a = a.substr(1);
				}
				args.add({
					name : a,
					opt : opt,
					t : xtype(e),
				});
			}
			var ret = args.last();
			args.remove(ret);
			CFunction(args,ret.t);
		case "a":
			var fields = new List();
			for( f in x.elements )
				fields.add({
					name : f.name,
					t : xtype(new Fast(f.x.firstElement())),
				});
			CAnonymous(fields);
		case "d":
			var t = null;
			var tx = x.x.firstElement();
			if( tx != null )
				t = xtype(new Fast(tx));
			CDynamic(t);
		default:
			xerror(x);
		}
	}

	function xtypeparams( x : Fast ) : List<CType> {
		var p = new List();
		for( c in x.elements )
			p.add(xtype(c));
		return p;
	}

	function defplat() {
		var l = new List();
		if( curplatform != null )
			l.add(curplatform);
		return l;
	}

}
