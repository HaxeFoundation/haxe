/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package haxe.rtti;

import haxe.rtti.CType;
import haxe.xml.Access;

/**
	XmlParser processes the runtime type information (RTTI) which
	is stored as a XML string in a static field `__rtti`.

	@see <https://haxe.org/manual/cr-rtti.html>
**/
class XmlParser {
	public var root:TypeRoot;

	var curplatform:String;

	public function new() {
		root = new Array();
	}

	public function sort(?l:TypeRoot) {
		if (l == null)
			l = root;
		l.sort(function(e1, e2) {
			var n1 = switch e1 {
				case TPackage(p, _, _): " " + p;
				default: TypeApi.typeInfos(e1).path;
			};
			var n2 = switch e2 {
				case TPackage(p, _, _): " " + p;
				default: TypeApi.typeInfos(e2).path;
			};
			if (n1 > n2)
				return 1;
			return -1;
		});
		for (x in l)
			switch (x) {
				case TPackage(_, _, l):
					sort(l);
				case TClassdecl(c):
					sortFields(c.fields);
					sortFields(c.statics);
				case TEnumdecl(_):
				case TAbstractdecl(_):
				case TTypedecl(_):
			}
	}

	function sortFields(a:Array<ClassField>) {
		a.sort(function(f1:ClassField, f2:ClassField) {
			var v1 = TypeApi.isVar(f1.type);
			var v2 = TypeApi.isVar(f2.type);
			if (v1 && !v2)
				return -1;
			if (v2 && !v1)
				return 1;
			if (f1.name == "new")
				return -1;
			if (f2.name == "new")
				return 1;
			if (f1.name > f2.name)
				return 1;
			return -1;
		});
	}

	public function process(x:Xml, platform:String) {
		curplatform = platform;
		xroot(new Access(x));
	}

	// merge inline and not inline
	function mergeRights(f1:ClassField, f2:ClassField) {
		if (f1.get == RInline && f1.set == RNo && f2.get == RNormal && f2.set == RMethod) {
			f1.get = RNormal;
			f1.set = RMethod;
			return true;
		}
		return Type.enumEq(f1.get, f2.get) && Type.enumEq(f1.set, f2.set);
	}

	function mergeDoc(f1:ClassField, f2:ClassField) {
		if (f1.doc == null)
			f1.doc = f2.doc;
		else if (f2.doc == null)
			f2.doc = f1.doc;
		return true;
	}

	function mergeFields(f:ClassField, f2:ClassField) {
		return TypeApi.fieldEq(f, f2)
			|| (f.name == f2.name && (mergeRights(f, f2) || mergeRights(f2, f)) && mergeDoc(f, f2) && TypeApi.fieldEq(f, f2));
	}

	public dynamic function newField(c:Classdef, f:ClassField) {}

	function mergeClasses(c:Classdef, c2:Classdef) {
		// todo : compare supers & interfaces
		if (c.isInterface != c2.isInterface)
			return false;
		if (curplatform != null)
			c.platforms.push(curplatform);
		if (c.isExtern != c2.isExtern)
			c.isExtern = false;

		for (f2 in c2.fields) {
			var found = null;
			for (f in c.fields)
				if (mergeFields(f, f2)) {
					found = f;
					break;
				}
			if (found == null) {
				newField(c, f2);
				c.fields.push(f2);
			} else if (curplatform != null)
				found.platforms.push(curplatform);
		}
		for (f2 in c2.statics) {
			var found = null;
			for (f in c.statics)
				if (mergeFields(f, f2)) {
					found = f;
					break;
				}
			if (found == null) {
				newField(c, f2);
				c.statics.push(f2);
			} else if (curplatform != null)
				found.platforms.push(curplatform);
		}
		return true;
	}

	function mergeEnums(e:Enumdef, e2:Enumdef) {
		if (e.isExtern != e2.isExtern)
			return false;
		if (curplatform != null)
			e.platforms.push(curplatform);
		for (c2 in e2.constructors) {
			var found = null;
			for (c in e.constructors)
				if (TypeApi.constructorEq(c, c2)) {
					found = c;
					break;
				}
			if (found == null)
				e.constructors.push(c2);
			else if (curplatform != null)
				found.platforms.push(curplatform);
		}
		return true;
	}

	function mergeTypedefs(t:Typedef, t2:Typedef) {
		if (curplatform == null)
			return false;
		t.platforms.push(curplatform);
		t.types.set(curplatform, t2.type);
		return true;
	}

	function mergeAbstracts(a:Abstractdef, a2:Abstractdef) {
		if (curplatform == null)
			return false;
		if (a.to.length != a2.to.length || a.from.length != a2.from.length)
			return false;
		for (i in 0...a.to.length)
			if (!TypeApi.typeEq(a.to[i].t, a2.to[i].t))
				return false;
		for (i in 0...a.from.length)
			if (!TypeApi.typeEq(a.from[i].t, a2.from[i].t))
				return false;
		if (a2.impl != null)
			mergeClasses(a.impl, a2.impl);
		a.platforms.push(curplatform);
		return true;
	}

	function merge(t:TypeTree) {
		var inf = TypeApi.typeInfos(t);
		var pack = inf.path.split(".");
		var cur = root;
		var curpack = new Array();
		pack.pop();
		for (p in pack) {
			var found = false;
			for (pk in cur)
				switch (pk) {
					case TPackage(pname, _, subs):
						if (pname == p) {
							found = true;
							cur = subs;
							break;
						}
					default:
				}
			curpack.push(p);
			if (!found) {
				var pk = new Array();
				cur.push(TPackage(p, curpack.join("."), pk));
				cur = pk;
			}
		}
		for (ct in cur) {
			if (ct.match(TPackage(_)))
				continue;
			var tinf = TypeApi.typeInfos(ct);

			// compare params ?
			if (tinf.path == inf.path) {
				var sameType = true;
				if ((tinf.doc == null) != (inf.doc == null)) {
					if (inf.doc == null)
						inf.doc = tinf.doc;
					else
						tinf.doc = inf.doc;
				}
				if (tinf.path == "haxe._Int64.NativeInt64")
					continue;
				if (tinf.module == inf.module && tinf.doc == inf.doc && tinf.isPrivate == inf.isPrivate)
					switch (ct) {
						case TClassdecl(c):
							switch (t) {
								case TClassdecl(c2):
									if (mergeClasses(c, c2))
										return;
								default:
									sameType = false;
							}
						case TEnumdecl(e):
							switch (t) {
								case TEnumdecl(e2):
									if (mergeEnums(e, e2))
										return;
								default:
									sameType = false;
							}
						case TTypedecl(td):
							switch (t) {
								case TTypedecl(td2):
									if (mergeTypedefs(td, td2))
										return;
								default:
							}
						case TAbstractdecl(a):
							switch (t) {
								case TAbstractdecl(a2):
									if (mergeAbstracts(a, a2))
										return;
								default:
									sameType = false;
							}
						case TPackage(_, _, _):
							sameType = false;
					}
				// we already have a mapping, but which is incompatible
				var msg = if (tinf.module != inf.module) "module " + inf.module + " should be " + tinf.module; else if (tinf.doc != inf.doc)
					"documentation is different";
				else if (tinf.isPrivate != inf.isPrivate)
					"private flag is different";
				else if (!sameType)
					"type kind is different";
				else
					"could not merge definition";
				throw "Incompatibilities between " + tinf.path + " in " + tinf.platforms.join(",") + " and " + curplatform + " (" + msg + ")";
			}
		}
		cur.push(t);
	}

	function mkPath(p:String):Path {
		return p;
	}

	function mkTypeParams(p:String):TypeParams {
		var pl = p.split(":");
		if (pl[0] == "")
			return new Array();
		return pl;
	}

	function mkRights(r:String):Rights {
		return switch (r) {
			case "null": RNo;
			case "method": RMethod;
			case "dynamic": RDynamic;
			case "inline": RInline;
			default: RCall(r);
		}
	}

	function xerror(c:Access):Dynamic {
		return throw "Invalid " + c.name;
	}

	function xroot(x:Access) {
		for (c in x.x.elements())
			merge(processElement(c));
	}

	public function processElement(x:Xml) {
		var c = new haxe.xml.Access(x);
		return switch (c.name) {
			case "class": TClassdecl(xclass(c));
			case "enum": TEnumdecl(xenum(c));
			case "typedef": TTypedecl(xtypedef(c));
			case "abstract": TAbstractdecl(xabstract(c));
			default: xerror(c);
		}
	}

	function xmeta(x:Access):MetaData {
		var ml = [];
		for (m in x.nodes.m) {
			var pl = [];
			for (p in m.nodes.e)
				pl.push(p.innerHTML);
			ml.push({name: m.att.n, params: pl});
		}
		return ml;
	}

	function xoverloads(x:Access):Array<ClassField> {
		var l = new Array();
		for (m in x.elements) {
			l.push(xclassfield(m));
		}
		return l;
	}

	function xpath(x:Access):PathParams {
		var path = mkPath(x.att.path);
		var params = new Array();
		for (c in x.elements)
			params.push(xtype(c));
		return {
			path: path,
			params: params,
		};
	}

	function xclass(x:Access):Classdef {
		var csuper = null;
		var doc = null;
		var tdynamic = null;
		var interfaces = new Array();
		var fields = new Array();
		var statics = new Array();
		var meta = [];
		var isInterface = x.x.exists("interface");
		for (c in x.elements)
			switch (c.name) {
				case "haxe_doc":
					doc = c.innerData;
				case "extends":
					if (isInterface) {
						interfaces.push(xpath(c));
					} else {
						csuper = xpath(c);
					}
				case "implements":
					interfaces.push(xpath(c));
				case "haxe_dynamic":
					tdynamic = xtype(new Access(c.x.firstElement()));
				case "meta":
					meta = xmeta(c);
				default:
					if (c.x.exists("static"))
						statics.push(xclassfield(c));
					else
						fields.push(xclassfield(c));
			}
		return {
			file: if (x.has.file) x.att.file else null,
			path: mkPath(x.att.path),
			module: if (x.has.module) mkPath(x.att.module) else null,
			doc: doc,
			isPrivate: x.x.exists("private"),
			isExtern: x.x.exists("extern"),
			isFinal: x.x.exists("final"),
			isInterface: isInterface,
			params: mkTypeParams(x.att.params),
			superClass: csuper,
			interfaces: interfaces,
			fields: fields,
			statics: statics,
			tdynamic: tdynamic,
			platforms: defplat(),
			meta: meta,
		};
	}

	function xclassfield(x:Access, ?defPublic = false):ClassField {
		var e = x.elements;
		var t = xtype(e.next());
		var doc = null;
		var meta = [];
		var overloads = null;
		for (c in e)
			switch (c.name) {
				case "haxe_doc":
					doc = c.innerData;
				case "meta":
					meta = xmeta(c);
				case "overloads":
					overloads = xoverloads(c);
				default:
					xerror(c);
			}
		return {
			name:x.name, type:t, isPublic:x.x.exists("public") || defPublic, isFinal:x.x.exists("final"), isOverride:x.x.exists("override"),
			line:if (x.has.line) Std.parseInt(x.att.line) else null, doc:doc, get:if (x.has.get) mkRights(x.att.get) else RNormal, set:if (x.has.set)
				mkRights(x.att.set)
			else
				RNormal, params:if (x.has.params) mkTypeParams(x.att.params) else [], platforms:defplat(), meta:meta, overloads:overloads, expr:if (x.has.expr)
				x.att.expr
			else
				null
		};
	}

	function xenum(x:Access):Enumdef {
		var cl = new Array();
		var doc = null;
		var meta = [];
		for (c in x.elements)
			if (c.name == "haxe_doc")
				doc = c.innerData;
			else if (c.name == "meta")
				meta = xmeta(c);
			else
				cl.push(xenumfield(c));
		return {
			file: if (x.has.file) x.att.file else null,
			path: mkPath(x.att.path),
			module: if (x.has.module) mkPath(x.att.module) else null,
			doc: doc,
			isPrivate: x.x.exists("private"),
			isExtern: x.x.exists("extern"),
			params: mkTypeParams(x.att.params),
			constructors: cl,
			platforms: defplat(),
			meta: meta,
		};
	}

	function xenumfield(x:Access):EnumField {
		var args = null;
		var docElements = x.x.elementsNamed("haxe_doc");
		var xdoc = if (docElements.hasNext()) docElements.next() else null;
		var meta = if (x.hasNode.meta) xmeta(x.node.meta) else [];
		if (x.has.a) {
			var names = x.att.a.split(":");
			var elts = x.elements;
			args = new Array();
			for (c in names) {
				var opt = false;
				if (c.charAt(0) == "?") {
					opt = true;
					c = c.substr(1);
				}
				args.push({
					name: c,
					opt: opt,
					t: xtype(elts.next()),
				});
			}
		}
		return {
			name: x.name,
			args: args,
			doc: if (xdoc == null) null else new Access(xdoc).innerData,
			meta: meta,
			platforms: defplat(),
		};
	}

	function xabstract(x:Access):Abstractdef {
		var doc = null, impl = null, athis = null;
		var meta = [], to = [], from = [];
		for (c in x.elements)
			switch (c.name) {
				case "haxe_doc":
					doc = c.innerData;
				case "meta":
					meta = xmeta(c);
				case "to":
					for (t in c.elements)
						to.push({t: xtype(new Access(t.x.firstElement())), field: t.has.field ? t.att.field : null});
				case "from":
					for (t in c.elements)
						from.push({t: xtype(new Access(t.x.firstElement())), field: t.has.field ? t.att.field : null});
				case "impl":
					impl = xclass(c.node.resolve("class"));
				case "this":
					athis = xtype(new Access(c.x.firstElement()));
				default:
					xerror(c);
			}
		return {
			file: if (x.has.file) x.att.file else null,
			path: mkPath(x.att.path),
			module: if (x.has.module) mkPath(x.att.module) else null,
			doc: doc,
			isPrivate: x.x.exists("private"),
			params: mkTypeParams(x.att.params),
			platforms: defplat(),
			meta: meta,
			athis: athis,
			to: to,
			from: from,
			impl: impl
		};
	}

	function xtypedef(x:Access):Typedef {
		var doc = null;
		var t = null;
		var meta = [];
		for (c in x.elements)
			if (c.name == "haxe_doc")
				doc = c.innerData;
			else if (c.name == "meta")
				meta = xmeta(c);
			else
				t = xtype(c);
		var types = new haxe.ds.StringMap();
		if (curplatform != null)
			types.set(curplatform, t);
		return {
			file: if (x.has.file) x.att.file else null,
			path: mkPath(x.att.path),
			module: if (x.has.module) mkPath(x.att.module) else null,
			doc: doc,
			isPrivate: x.x.exists("private"),
			params: mkTypeParams(x.att.params),
			type: t,
			types: types,
			platforms: defplat(),
			meta: meta,
		};
	}

	function xtype(x:Access):CType {
		return switch (x.name) {
			case "unknown":
				CUnknown;
			case "e":
				CEnum(mkPath(x.att.path), xtypeparams(x));
			case "c":
				CClass(mkPath(x.att.path), xtypeparams(x));
			case "t":
				CTypedef(mkPath(x.att.path), xtypeparams(x));
			case "x":
				CAbstract(mkPath(x.att.path), xtypeparams(x));
			case "f":
				var args = new Array();
				var aname = x.att.a.split(":");
				var eargs = aname.iterator();
				var evalues = x.has.v ? x.att.v.split(":").iterator() : null;
				for (e in x.elements) {
					var opt = false;
					var a = eargs.hasNext() ? eargs.next() : null;
					if (a == null)
						a = "";
					if (a.charAt(0) == "?") {
						opt = true;
						a = a.substr(1);
					}
					var v = evalues == null || !evalues.hasNext() ? null : evalues.next();
					args.push({
						name: a,
						opt: opt,
						t: xtype(e),
						value: v == "" ? null : v
					});
				}
				var ret = args[args.length - 1];
				args.remove(ret);
				CFunction(args, ret.t);
			case "a":
				var fields = new Array();
				for (f in x.elements) {
					var f = xclassfield(f, true);
					f.platforms = new Array(); // platforms selection are on the type itself, not on fields
					fields.push(f);
				}
				CAnonymous(fields);
			case "d":
				var t = null;
				var tx = x.x.firstElement();
				if (tx != null)
					t = xtype(new Access(tx));
				CDynamic(t);
			default:
				xerror(x);
		}
	}

	function xtypeparams(x:Access):Array<CType> {
		var p = new Array();
		for (c in x.elements)
			p.push(xtype(c));
		return p;
	}

	function defplat() {
		var l = new Array();
		if (curplatform != null)
			l.push(curplatform);
		return l;
	}
}
