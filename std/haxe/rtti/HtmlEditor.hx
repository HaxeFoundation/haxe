/*
 * Copyright (c) 2006-2009, The haXe Project Contributors
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

class HtmlEditor {

	static var UID = 0;

	var id : String;
	var types : Hash<TypeTree>;
	var buf : StringBuf;
	var nfields : Int;

	public function new() {
		types = new Hash();
	}

	public function add( tl : TypeRoot ) {
		for( t in tl )
			switch(t) {
			case TPackage(_,_,subs):
				add(subs);
			case TClassdecl(c):
				types.set(c.path,t);
			case TEnumdecl(e):
				types.set(e.path,t);
			case TTypedecl(td):
				types.set(td.path,t);
			}
	}

	public function buildHTML( id : String, v : Dynamic, t : CType ) {
		this.id = id;
		nfields = 0;
		buf = new StringBuf();
		buildHTMLRec(v,t,false);
		var str = buf.toString();
		buf = null;
		return str;
	}

	function open(t) {
		buf.add("<"+t);
	}

	function close(?t) {
		buf.add(( t == null ) ? "/>" : "</"+t+">");
	}

	function genUID() {
		return "__u"+id+"_"+(UID++);
	}

	function genFieldName() {
		return "__f"+id+"_"+(nfields++);
	}

	function skipField() {
		nfields++;
	}

	function attrib(name,value) {
		buf.add(" "+name+'="'+value+'"');
	}

	function followTypeDef( name, params : List<CType> ) {
		var td = types.get(name);
		if( td == null ) throw "Missing type "+name;
		if( !params.isEmpty() ) throw "Can't apply parameters";
		return switch( td ) {
			case TTypedecl(t): t.type;
			default: throw "assert";
		};
	}

	function getEnum( name ) {
		var td = types.get(name);
		if( td == null ) throw "Missing type "+name;
		return switch( td ) { case TEnumdecl(e): e; default: throw "assert"; };
	}

	function buildNullField( checked ) {
		open("input");
		attrib("name",genFieldName());
		attrib("class","null");
		attrib("type","checkbox");
		if( checked )
			attrib("checked","checked");
		close();
	}

	function buildHTMLRec( v : Dynamic, t : CType, nullable ) {
		switch( t ) {
		case CUnknown,CDynamic(_),CFunction(_,_):
			buf.add("???");
		case CTypedef(name,params):
			var t = followTypeDef(name,params);
			buildHTMLRec(v,t,nullable || name == "Null");
		case CAnonymous(fl):
			open("table");
			attrib("class","anon");
			buf.add(">");
			for( f in fl ) {
				buf.add("<tr><th>");
				buf.add(f.name);
				buf.add("</th><td>");
				buildHTMLRec(Reflect.field(v,f.name),f.type,false);
				buf.add("</td></tr>");
			}
			close("table");
		case CClass(name,params):
			if( !params.isEmpty() ) throw "Can't use type parameters";
			switch( name ) {
			case "Int":
				open("input");
				attrib("name",genFieldName());
				attrib("class","int");
				if( v != null )
					attrib("value",v);
				close();
			case "String":
				if( nullable )
					buildNullField(v != null);
				open("input");
				attrib("name",genFieldName());
				attrib("class","string");
				if( v != null )
					attrib("value",v);
				close();
			case "Bool":
				if( nullable )
					buildNullField(v != null);
				open("input");
				attrib("name",genFieldName());
				attrib("type","checkbox");
				if( v )
					attrib("checked","checked");
				close();
			default:
				throw "Can't edit instances of "+name;
			}
		case CEnum(name,params):
			if( name == "Bool" ) {
				buildHTMLRec(v,CClass("Bool",params),nullable);
				return;
			}
			if( !params.isEmpty() ) throw "Can't use type parameters";
			var e = getEnum(name);
			var js = genUID();
			open("select");
			attrib("name",genFieldName());
			attrib("class","enum");
			attrib("onchange",js+"(this)");
			buf.add(">");
			var current = if( v == null ) null else Type.enumConstructor(v);
			if( nullable )
				buf.add("<option value=''>---- NULL ----</option>");
			var prefix = if( e.constructors.length <= 1 ) "" else e.constructors.first().name;
			for( c in e.constructors )
				while( prefix.length > 0 )
					if( c.name.substr(0,prefix.length) == prefix )
						break;
					else
						prefix = prefix.substr(0,prefix.length-1);
			for( c in e.constructors ) {
				open("option");
				attrib("value",c.name);
				if( current == c.name )
					attrib("selected","selected");
				buf.add(">");
				buf.add(c.name.substr(prefix.length));
				close("option");
			}
			close("select");
			var ids = new Array();
			for( c in e.constructors ) {
				var id = genUID();
				ids.push({ id : id, c : c });
				open("table");
				attrib("id",id);
				attrib("class","construct");
				if( current != c.name )
					attrib("style","display : none");
				buf.add(">");
				if( c.args != null ) {
					var args = if( current == c.name ) Type.enumParameters(v) else new Array();
					var i = 0;
					for( p in c.args ) {
						buf.add("<tr><th>");
						buf.add(p.name);
						buf.add("</th><td>");
						buildHTMLRec(args[i++],p.t,p.opt);
						buf.add("</td></tr>");
					}
				}
				close("table");
			}
			open("script");
			attrib("type","text/javascript");
			buf.add(">");
			buf.add("function "+js+"(s) {");
			for( c in ids )
				buf.add("document.getElementById('"+c.id+"').style.display = (s.value == '"+c.c.name+"')?'':'none';");
			buf.add("}");
			close("script");
		}
	}

	public function buildObject( id : String, params : Hash<String>, t : CType ) : Dynamic {
		this.id = id;
		nfields = 0;
		return buildObjectRec(params,t,false);
	}

	function buildObjectRec( params : Hash<String>, t : CType, nullable : Bool ) : Dynamic {
		return switch( t ) {
		case CUnknown,CDynamic(_),CFunction(_,_):
			throw Type.enumConstructor(t)+" can't be built";
		case CTypedef(name,pl):
			buildObjectRec(params,followTypeDef(name,pl),nullable || name == "Null");
		case CAnonymous(fl):
			var o = {};
			for( f in fl )
				Reflect.setField(o,f.name,buildObjectRec(params,f.type,false));
			o;
		case CClass(name,_):
			var v = params.get(genFieldName());
			var ret : Dynamic;
			switch( name ) {
			case "Int":
				if( v == null || (v == "" && !nullable) )
					throw "Missing required value";
				if( !~/^[0-9]+$/.match(v) )
					throw "Invalid int format '"+v+"'";
				ret = ( v == "" ) ? null : Std.parseInt(v);
			case "String":
				if( nullable ) {
					var str = params.get(genFieldName());
					ret = if( v == null && str == "" ) null else str;
				} else {
					if( v == null )
						throw "Missing required value";
					ret = v;
				}
			case "Bool":
				if( nullable ) {
					var b = params.exists(genFieldName());
					ret = if( v == null && !b ) null else b;
				} else
					ret = (v != null);
			default:
				throw name+" can't be built";
			}
			ret;
		case CEnum(name,_):
			if( name == "Bool" )
				buildObjectRec(params,CClass("Bool",new List()),nullable);
			else {
				var e = getEnum(name);
				var v = genFieldName();
				var current = params.get(v);
				var value = null;
				for( c in e.constructors ) {
					if( c.name == current ) {
						var args = null;
						if( c.args != null ) {
							args = new Array();
							for( a in c.args )
								args.push(buildObjectRec(params,a.t,a.opt));
						}
						value = Type.createEnum(Type.resolveEnum(name),current,args);
					} else if( c.args != null ) {
						for( a in c.args )
							skipObjectRec(a.t,a.opt);
					}
				}
				if( value == null && !nullable )
					throw name+" can't be null";
				value;
			}
		};
	}

	function skipObjectRec( t : CType, nullable ) {
		switch( t ) {
		case CUnknown,CDynamic(_),CFunction(_,_):
			// nothing
		case CTypedef(name,pl):
			skipObjectRec(followTypeDef(name,pl),nullable || name == "Null");
		case CAnonymous(fl):
			for( f in fl )
				skipObjectRec(f.type,false);
		case CEnum(name,_):
			if( name == "Bool" ) {
				skipObjectRec(CClass("Bool",new List()),nullable);
				return;
			}
			var e = getEnum(name);
			skipField();
			for( c in e.constructors ) {
				if( c.args == null ) continue;
				for( a in c.args )
					skipObjectRec(a.t,a.opt);
			}
		case CClass(name,_):
			switch( name ) {
			case "Int": skipField();
			case "String", "Bool":
				if( nullable ) skipField();
				skipField();
			default:
			}
		}
	}

}
