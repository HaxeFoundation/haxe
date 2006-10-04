package tools.haxedoc;
import tools.haxedoc.Type;
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
		var a = Lambda.array(fl.iterator());
		a.sort(function(f1 : ClassField,f2 : ClassField) {
			var v1 = TypeApi.isVar(f1.type);
			var v2 = TypeApi.isVar(f2.type);
			if( v1 && !v2 )
				return -1;
			if( v2 && !v1 )
				return 1;
			if( f1.name > f2.name )
				return 1;
			return -1;
		});
		return Lambda.list(a.iterator());		
	}
	
	public function process( x : Xml, platform ) {
		curplatform = platform;
		xroot(new Fast(x));
	}
	
	function mergeClasses( c : Class, c2 : Class ) {		
		// todo : compare supers & interfaces		
		if( c.isInterface != c2.isInterface || c.isExtern != c2.isExtern )
			return false;
		c.platforms.add(curplatform);
		
		for( f2 in c2.fields ) {
			var found = null;
			for( f in c.fields )
				if( TypeApi.fieldEq(f,f2) ) {
					found = f;
					break;
				}
			if( found == null )
				c.fields.add(f2);
			else
				found.platforms.add(curplatform);
		}
		for( f2 in c2.statics ) {
			var found = null;
			for( f in c.statics )
				if( TypeApi.fieldEq(f,f2) ) {
					found = f;
					break;
				}
			if( found == null )
				c.statics.add(f2);
			else
				found.platforms.add(curplatform);
		}
		return true;
	}

	function mergeEnums( e : Enum, e2 : Enum ) {
		if( e.isExtern != e2.isExtern )
			return false;
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
			found.platforms.add(curplatform);
		}
		return true;
	}

	function mergeTypedefs( t : Typedef, t2 : Typedef ) {
		if( !TypeApi.typeEq(t.type,t2.type) )
			return false;
		t.platforms.add(curplatform);
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
			var tinf = try TypeApi.typeInfos(ct) catch( e : Dynamic ) continue;
			// compare params ?
			if( tinf.path == inf.path ) {
				if( tinf.module == inf.module && tinf.doc == inf.doc && tinf.isPrivate == inf.isPrivate )					
					switch( ct ) {
					case TClassdecl(c):
						switch( t ) {
						case TClassdecl(c2):
							if( mergeClasses(c,c2) )
								return;
						default:
						}
					case TEnumdecl(e):
						switch( t ) {
						case TEnumdecl(e2):
							if( mergeEnums(e,e2) )
								return;
						default:
						}
					case TTypedecl(td):
						switch( t ) {
						case TTypedecl(td2):
							if( mergeTypedefs(td,td2) )
								return;
						default:
						}
					case TPackage(_,_,_):
					}
				// we already have a mapping, but which is incompatible
				throw "Incompatibilities between "+tinf.path+" in "+tinf.platforms.join(",")+" and "+curplatform;
			}
		}
		cur.push(t);
	}
	
	function mkPath( p : String ) : Path {
		return p;
	}
	
	function mkTypeParams( p : String ) : TypeParams {
		return p.split(":");
	}
	
	function mkRights( r : String ) : Rights {
		return switch( r ) {
		case "null": RNo;
		case "dynamic": RDynamic;
		case "f9dynamic": RF9Dynamic;
		default: RMethod(r);
		}
	}
	
	function xerror( c : Fast ) : Dynamic {
		return throw "Invalid "+c.name;
	}
	
	function xroot( x : Fast ) {		
		for( c in x.elements )
			switch( c.name ) {
			case "class":
				var cl = xclass(c);
				merge(TClassdecl(cl));
			case "enum":
				var e = xenum(c);
				merge(TEnumdecl(e));
			case "typedef":
				var td = xtypedef(c);
				merge(TTypedecl(td));
			default:
				xerror(c);
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
	
	function xclass( x : Fast ) : Class {
		var csuper = null;
		var doc = null;
		var dynamic = null;
		var interfaces = new List();		
		var fields = new List();
		var statics = new List();
		for( c in x.elements )
			switch( c.name ) {
			case "haxe_doc": doc = c.innerData;
			case "extends": csuper = xpath(c);
			case "implements": interfaces.add(xpath(c));
			case "haxe_dynamic": dynamic = xtype(new Fast(c.x.firstElement()));
			default:
				if( c.x.exists("static") )
					statics.add(xclassfield(c));
				else
					fields.add(xclassfield(c));
			}
		return {
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
			dynamic : dynamic,
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
			default: xerror(c);			
			}
		return {
			name : x.name,
			type : t,
			isPublic : x.x.exists("public"),
			doc : doc,
			get : if( x.has.get ) mkRights(x.att.get) else RNormal,
			set : if( x.has.set ) mkRights(x.att.set) else RNormal,
			params : if( x.has.params ) mkTypeParams(x.att.params) else null,
			platforms : defplat(),
		};
	}

	function xenum( x : Fast ) : Enum {
		var cl = new List();
		var doc = null;
		for( c in x.elements )
			if( c.name == "haxe_doc" )
				doc = c.innerData;
			else
				cl.add(xenumfield(c));
		return {
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
			else
				t = xtype(c);
		return {
			path : mkPath(x.att.path),
			module : if( x.has.module ) mkPath(x.att.module) else null,
			doc : doc,
			isPrivate : x.x.exists("private"),
			params : mkTypeParams(x.att.params),
			type : t,
			platforms : defplat(),
		};
	}
	
	function xtype( x : Fast ) : Type {
		return switch( x.name ) {
		case "unknown":
			TUnknown;
		case "e":
			TEnum(mkPath(x.att.path),xtypeparams(x));
		case "c":
			TClass(mkPath(x.att.path),xtypeparams(x));
		case "t":
			TTypedef(mkPath(x.att.path),xtypeparams(x));
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
			TFunction(args,ret.t);
		case "a":
			var fields = new List();
			for( f in x.elements )
				fields.add({
					name : f.name,
					t : xtype(new Fast(f.x.firstElement())),
				});
			TAnonymous(fields);
		case "d":
			var t = null;
			var tx = x.x.firstElement();			
			if( tx != null )
				t = xtype(new Fast(tx));
			TDynamic(t);
		default:
			xerror(x);
		}
	}
	
	function xtypeparams( x : Fast ) : List<Type> {
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
