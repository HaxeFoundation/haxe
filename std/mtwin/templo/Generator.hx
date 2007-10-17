/*
 * Copyright (c) 2006, Motion-Twin
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
 * THIS SOFTWARE IS PROVIDED BY MOTION-TWIN "AS IS" AND ANY
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

package mtwin.templo;

/**
	Generates template .neko file.
**/
class Generator {

	var out : StringBuf;
	var htmlBuf : StringBuf;

	public function new(){
		out = new StringBuf();
		htmlBuf = null;
	}

	public function toString() : String {
		flushHtml();

		var result = new StringBuf();
		var headercode = '
			String = $loader.String;
			Array = $loader.Array;
			iter = $loader.iter;
			buffer_new = $loader.loadprim("std@buffer_new", 0);
			buffer_add = $loader.loadprim("std@buffer_add", 2);
			buffer_string = $loader.loadprim("std@buffer_string", 1);
			string_split = $loader.loadprim("std@string_split", 2);

			replace = function( h, n, r ){
				var l = string_split(h, n);
				if (l[1] == null)
					return h;
				var res = buffer_new();
				buffer_add(res, l[0]);
				l = l[1];
				while (l != null){
					buffer_add(res, r);
					buffer_add(res, l[0]);
					l = l[1];
				}
				return buffer_string(res);
			}

			html_escape = function( data ){
				var t = $typeof(data);
				if (t == $tint)
					return data;
				if (t != $tstring)
					data = $string(data);
				if (data == "")
					return data;
				data = replace(data, "&", "&amp;");
				data = replace(data, "<", "&lt;");
				data = replace(data, ">", "&gt;");
				data = replace(data, "\\\"", "&quot;");
				return data;
			}

			is_true = function( data ){
				if (data == "") return false;
				return $istrue(data);
			}

			new_repeat = function( data ){
				var result = $new(null);
				result.data = data;
				result.index = 0-1;
				result.number = 0;
				result.first = true;
				result.last = false;
				result.odd = true;
				result.even = false;
				if (data.get_length != null) result.size = data.get_length();
				else if (data.length != null) result.size = data.length;
				else if (data.size != null) result.size = data.size();
				else result.size = null;
				result.next = function(v){
					this.current = v;
					this.index = this.index + 1;
					this.first = this.index == 0;
					this.number = this.number + 1;
					this.last = (this.number == this.size);
					this.even = (this.number % 2) == 0;
					this.odd = (this.even == false);
				}
				return result;
			}

			new_output_buffer = function( parent ){
				var result = $new(null);
				result.parent = parent;
				result.buf = buffer_new();
				result.add = function(str){ return buffer_add(this.buf, str); }
				result.str = function(){ return buffer_string(this.buf); }
				return result;
			}

			new_context = function( parent, vars ){
				var result = $new(null);
				result.parent = parent;
				result.__isTemplateContext = true;
				if (vars == null){
					result.vars = $new(null);
				}
				else {
					result.vars = vars;
				}
				result.get = function( field ){
					if ($objfield(this.vars, field)) return $objget(this.vars, field);
					if (this.parent == null) return null;
					return this.parent.get(field);
				}
				result.set = function( field, v ){
					$objset(this.vars, field, v);
				}
				return result;
			}

			template = function( macro, params ){
				var __ctx = null;
				if (params.__isTemplateContext) {
					__ctx = new_context(params, null);
				}
				else {
					__ctx = new_context(null, params);
				}
				var __glb = __ctx;
				var __out = new_output_buffer(null);

//--- HERE COMES THE TEMPLATE CODE ---
';
		result.add(~/[\r\n]+/g.split(headercode).join("\n"));
		result.add(out.toString());
		result.add('//--- END OF TEMPLATE CODE ---
				return __out.str();
			}

			$exports.template = template;
		'); //'
		return result.toString();
	}

	public function writeHtml( str:String ){
		if (htmlBuf == null){
			htmlBuf = new StringBuf();
		}
		htmlBuf.add(str);
	}

	public function writeCode( str:String ){
		if (htmlBuf != null){
			flushHtml();
		}
		out.add("__out.add("+str+");\n");
	}

	public function writeEscapedCode( str:String ){
		if (htmlBuf != null){
			flushHtml();
		}
		out.add("__out.add(html_escape("+str+"));\n");
	}

	public function add( code:String ){
		if (htmlBuf != null){
			flushHtml();
		}
		out.add(code);
	}

	public static function hash( name:String ) : Int {
		return untyped __dollar__hash(name.__s);
	}

	public function getVar( name:String ) : String {
		return "__ctx.get("+hash(name)+")";
	}

	public function setVar( name:String, exp:String ){
		add("__ctx.set("+hash(name)+", "+exp+");\n");
	}

	public function flushHtml(){
		if (htmlBuf == null) return;
		var html = htmlBuf.toString();
		html = StringTools.replace(html, "\\", "\\\\");
		html = StringTools.replace(html, "\\'", "\\'");
		html = StringTools.replace(html, "\"", "\\\"");
		html = StringTools.replace(html, "\n", "\\n");
		out.add("__out.add(\"" + html + "\");\n");
		htmlBuf = null;
	}
}
