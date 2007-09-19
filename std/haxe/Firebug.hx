/*
 * Copyright (c) 2007, The haXe Project Contributors
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
package haxe;

class Firebug {

	public static function detect() : Bool {
		#if js
		try {
			return untyped console != null && console.error != null;
		} catch( e : Dynamic ) {
			return false;
		}
		#else flash
		if( !flash.external.ExternalInterface.available )
			return false;
		return flash.external.ExternalInterface.call("console.error.toString") != null;
		#else neko
		return true;
		#end
	}

	public static function redirectTraces() {
		haxe.Log.trace = trace;
		#if flash9
		#else flash
		flash.Lib.setErrorHandler(onError);
		#else js
		js.Lib.setErrorHandler(onError);
		#end
	}

	public static function onError( err : String, stack : Array<String> ) {
		var buf = err+"\n";
		for( s in stack )
			buf += "Called from "+s+"\n";
		haxe.Firebug.trace(buf,null);
		#if js
		return true;
		#end
	}

	public static function trace(v : Dynamic, ?inf : haxe.PosInfos ) {
		var type = if( inf != null && inf.customParams != null ) inf.customParams[0] else null;
		if( type != "warn" && type != "info" && type != "debug" && type != "error" )
			type = if( inf == null ) "error" else "log";
		#if flash
			var str = if( inf == null ) "" else inf.fileName + ":" + inf.lineNumber + " : ";
			try	str += Std.string(v) catch( e : Dynamic ) str += "????";
			#if flash9
			// in Flash9, it is needed to use _self with getURL and
			// only the latest call per frame is processed, so it's
			// needed to use ExternalInterface (URLLoader displays
			// security errors for remote files)
			str = str.split("\\").join("\\\\");
			flash.external.ExternalInterface.call("console."+type,str);
			#else true
			str = str.split("\\").join("\\\\").split("'").join("\\'").split("\n").join("\\n").split("\r").join("\\r");
			str = StringTools.urlEncode(str);
			var out = "javascript:console."+ type +"('"+str+"');";
			flash.Lib.getURL(out);
			#end // flash9
		#else js
			untyped console[type]( (if( inf == null ) "" else inf.fileName+":"+inf.lineNumber+" : ") + Std.string(v) );
		#else neko
			var str = inf.fileName + ":" + inf.lineNumber + " : ";
			try str += Std.string(v) catch( e : Dynamic ) str += "???";
			neko.Lib.print('<script type="text/javascript">console.'+type+'(decodeURIComponent("'+StringTools.urlEncode(str)+'"))</script>');
		#else error
		#end
	}

}
