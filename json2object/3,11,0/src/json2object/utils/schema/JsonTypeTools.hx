/*
Copyright (c) 2019 Guillaume Desquesnes, Valentin Lemière

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package json2object.utils.schema;

import haxe.macro.Context;
import haxe.macro.Expr;

using StringTools;
using json2object.utils.TypeTools;
using json2object.writer.StringUtils;

typedef AnonDecls = Map<String, {field:String, expr:Expr}>;

class JsonTypeTools {
#if macro
	inline static function str2Expr (str:String) : Expr {
		return macro $v{str};
	}

	inline static function nullable (ct:ComplexType) : ComplexType {
		return TPath({name: "Null", pack:[], params:[ TPType(ct)], sub:null});
	}

	public static function toExpr(jt:JsonType, parsingType:ParsingType) : Expr {
		return _toExpr(jt, '', null, parsingType);
	}

	static function declsToAnonDecl (decls:AnonDecls) : Expr {
		#if haxe4
		return {
			expr:EObjectDecl([ for (k=>v in decls) {field:k, expr:v.expr, quotes:Quoted}]),
			pos:Context.currentPos()
		};
		#else
		return {
			expr:EObjectDecl([ for (value in decls) value]),
			pos:Context.currentPos()
		};
		#end
	}

	static function sort (a:String, b:String) : Int {
		if (a == b) { return 0; }
		return (a > b) ? 1 : -1;
	}

	static function _toExpr(jt:JsonType, descr:String, defaultValue:Null<Expr>, parsingType:ParsingType) : Expr {
		var decls = new AnonDecls();
		inline function declare(name:String, expr:Expr) {
			decls.set(name, {field:name, expr:expr});
		}

		switch (jt) {
			case JTNull:
				declare("type", macro "null");
			case JTSimple(type):
				declare("type", macro $v{type});
			case JTString(s):
				declare("const", macro $v{s});
			case JTBool(b):
				declare("const_bool", (b) ? macro true : macro false);
			case JTFloat(f):
				declare("const_float", macro $v{f});
			case JTInt(i):
				declare("const_int", macro $v{i});
			case JTObject(properties, rq, defaults):
				declare("type", macro 'object');
				var propertiesDecl = [];
				var keys = [ for (k in properties.keys()) k ];
				keys.sort(sort);
				for (key in keys) {
					propertiesDecl.push({
						expr:EBinop(
							OpArrow,
							macro $v{key},
							_toExpr(properties.get(key), '', defaults.get(key), parsingType)
						),
						pos:Context.currentPos()
					});
				}
				var propertiesExpr = {expr: EArrayDecl(propertiesDecl), pos: Context.currentPos()};
				declare("properties", propertiesExpr);
				if (rq.length > 0) {
					rq.sort(sort);
					var requiredExpr = {expr:EArrayDecl(rq.map(str2Expr)), pos: Context.currentPos()};
					declare("required", requiredExpr);
				}
				declare("additionalProperties", macro false);
			case JTArray(type):
				declare("type", macro "array");
				declare("items", toExpr(type, parsingType));
			case JTMap(onlyInt, type):
				declare("type", macro "object");
				if (onlyInt) {
					var patternPropertiesExpr = {
						expr: EArrayDecl([
							{expr:EBinop(OpArrow, macro "/^[-+]?\\d+([Ee][+-]?\\d+)?$/", toExpr(type, parsingType)), pos:Context.currentPos()}
						]),
						pos: Context.currentPos()
					}
					declare("patternProperties", patternPropertiesExpr);
				}
				else {
					declare("additionalProperties_obj", toExpr(type, parsingType));
				}
			case JTRef(name):
				declare("ref", str2Expr('#/definitions/'+name));
			case JTEnumValues(values):
				if (parsingType.useEnumDescriptions) {
					var enumDecls = [];
					var descrDecls = [];
					var descrLength = 0;
					var type = '_string';

					function parseValue (v:JsonType) {
						switch (v) {
							case JTNull:
								enumDecls.push(macro null);
							case JTString(s):
								enumDecls.push(macro $v{s});
							case JTFloat(s):
								enumDecls.push(macro $v{Std.parseFloat(s)});
								type = '_float';
							case JTInt(i):
								enumDecls.push(macro $v{i});
								type = '_int';
							case JTBool(b):
								enumDecls.push(macro $v{b});
								type = '_bool';
							case JTWithDescr(v, descr):
								descrDecls.push(macro $v{clean(descr, parsingType)});
								descrLength += descr.length;
								parseValue(v);
							default:
						}
						if (enumDecls.length > descrDecls.length) {
							descrDecls.push(macro '');
						}
					}

					for (v in values) {
						parseValue(v);
					}

					var enumValues = {expr: EArrayDecl(enumDecls), pos: Context.currentPos()};
					declare("enum"+type, enumValues);

					if (descrLength > 0) {
						var descr = {expr: EArrayDecl(descrDecls), pos: Context.currentPos()};
						var descrLabel = parsingType.useMarkdownLabel ? "markdownEnumDescriptions" : "enumDescriptions";
						declare(descrLabel, descr);
					}
				}
				else {
					return _toExpr(JTAnyOf(values), descr, defaultValue, parsingType);
				}
			case JTAnyOf(types):
				var decls = [ for (t in types) toExpr(t, parsingType)];
				var anyOfExpr = {expr: EArrayDecl(decls), pos:Context.currentPos()};
				declare("anyOf", anyOfExpr);
			case JTWithDescr(type, descr):
				return _toExpr(type, descr, defaultValue, parsingType);
		}

		if (descr != '') {
			var description = parsingType.useMarkdownLabel ? "markdownDescription" : "description";
			declare(description, str2Expr(clean(descr.trim(), parsingType)));
		}

		if (defaultValue != null) {
			declare('defaultValue', defaultValue);
		}

		return declsToAnonDecl(decls);
	}

	/**
	 * Clean the doc
	 * if all line first non whitespace char is * then remove all first *
	 *
	 * if parsingType.useMarkdown:
	 *     keep all new line as they are md formatting
	 * else:
	 *     a single new line is for readability so they are removed
	 */
	static function clean (doc:String, parsingType:ParsingType) : String {
		var lines = [];
		var hasStar = doc.charAt(0) == '*';
		var start = 0;
		var cursor = 0;
		while (cursor < doc.length) {
			switch (doc.charAt(cursor)) {
				case "\r":
					if (doc.charAt(cursor+1) == "\n") {
						cursor++;
					}
					var line = doc.substring(start, cursor).rtrim();
					lines.push(line);
					start = cursor + 1;
					if (line.length > 0) {
						hasStar = hasStar && (line.charAt(0) == '*');
					}
				case "\n":
					var line = doc.substring(start, cursor).rtrim();
					lines.push(line);
					start = cursor + 1;
					if (line.length > 0) {
						hasStar = hasStar && (line.charAt(0) == '*');
					}
				default:
			}
			cursor++;
		}

		lines.push(doc.substring(start).rtrim());

		if (parsingType.useMarkdown) {
			if (!hasStar) {
				lines = keepSharedIndentation(lines);
				return lines.join('\n');
			}
			else {
				return [ for (l in lines) l.substr(2) ].join('\n');
			}
		}

		var consecutiveNewLine = 0;
		var result = [""];
		var i = -1;
		for (line in lines) {
			line = (hasStar) ? line.substr(2) : line;
			line = line.ltrim();
			if (line.trim().length == 0) {
				if (i == -1) {
					continue;
				}
				consecutiveNewLine++;
			}
			else {
				if (i == -1) { i = 0; }
				else {
					var curr = result[i];
					if (curr != "" && curr.charAt(curr.length - 1) != " " && line.charAt(0) != " ") {
						line = " " + line;
					}
				}
				result[i] += line;
				consecutiveNewLine = 1;
			}
			if (consecutiveNewLine > 1) {
				result.push('');
				i++;
				consecutiveNewLine = 0;
			}
		}
		return result.join("\n");
	}

	static function keepSharedIndentation (lines:Array<String>) : Array<String> {
		var first = lines.shift().ltrim();
		var newLines = [];
		if (first.length == 0) {
			while (lines.length > 0) {
				if (lines[0].ltrim().length == 0) {
					lines.shift();
				}
				else {
					break;
				}
			}
		}
		else {
			newLines.push(first);
		}
		var i = 0;
		var flag = true;
		while (flag) {
			var char = null;
			for (line in lines) {
				if (line.length > i) {
					if (!line.isSpace(i)) {
						flag = false;
						break;
					}

					var c = line.charAt(i);
					if (char == null) {
						char = c;
					}
					else if (c != char) {
						flag = false;
						break;
					}
					flag = true;
				}
			}
			if (char == null) {
				flag = false;
			}
			else {
				i++;
			}
		}

		for (line in lines) {
			newLines.push(line.substring(i));
		}

		while (newLines.length > 0) {
			if (newLines[newLines.length - 1].length == 0) {
				newLines.pop();
			}
			else {
				break;
			}
		}

		return newLines;
	}
#end
}