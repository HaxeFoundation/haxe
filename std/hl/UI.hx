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

package hl;

typedef SentinelHandle = hl.Abstract<"ui_sentinel">;

abstract Sentinel(SentinelHandle) {
	public var pause(get, set):Bool;

	public function new(timeout, callback) {
		this = create_sentinel(timeout, callback);
	}

	function get_pause() {
		return is_paused(this);
	}

	function set_pause(p:Bool) {
		_pause(this, p);
		return p;
	}

	public function tick() {
		_tick(this);
	}

	@:hlNative("ui", "ui_start_sentinel") static function create_sentinel(timeout:Float, callb:Void->Void):SentinelHandle {
		return null;
	}

	@:hlNative("ui", "ui_sentinel_tick") static function _tick(h:SentinelHandle):Void {}

	@:hlNative("ui", "ui_sentinel_pause") static function _pause(h:SentinelHandle, b:Bool):Void {}

	@:hlNative("ui", "ui_sentinel_is_paused") static function is_paused(h:SentinelHandle):Bool {
		return false;
	}
}

typedef WinHandle = hl.Abstract<"ui_window">;

class Window {
	var h:WinHandle;

	public function setText(text:String) {
		win_set_text(h, @:privateAccess text.bytes);
	}

	public function setEnable(b:Bool) {
		win_set_enable(h, b);
	}

	public function destroy() {
		win_destroy(h);
	}

	@:hlNative("ui", "ui_win_destroy")
	static function win_destroy(win:WinHandle):Void {}

	@:hlNative("ui", "ui_win_set_text")
	static function win_set_text(win:WinHandle, text:hl.Bytes):Void {}

	@:hlNative("ui", "ui_win_set_enable")
	static function win_set_enable(win:WinHandle, enable:Bool):Void {}
}

class Button extends Window {
	public function new(parent:Window, text:String) {
		h = button_new(parent.h, @:privateAccess text.bytes, function() this.onClick());
	}

	public dynamic function onClick() {}

	@:hlNative("ui", "ui_button_new")
	static function button_new(parent:WinHandle, text:hl.Bytes, onClick:Void->Void):WinHandle {
		return null;
	}
}

class WinLog extends Window {
	public function new(title:String, width, height) {
		h = winlog_new(@:privateAccess title.bytes, width, height);
	}

	public function setTextContent(text:String, autoScroll = false) {
		winlog_set_text(h, @:privateAccess text.bytes, autoScroll);
	}

	@:hlNative("ui", "ui_winlog_new")
	static function winlog_new(text:hl.Bytes, width:Int, height:Int):WinHandle {
		return null;
	}

	@:hlNative("ui", "ui_winlog_set_text")
	static function winlog_set_text(win:WinHandle, text:hl.Bytes, autoScroll:Bool):Void {}
}

enum DialogFlags {
	YesNo;
	IsError;
}

enum abstract LoopResult(Int) {
	var NoMessage = 0;
	var HandledMessage = 1;
	var Quit = 2;
}

typedef FileOptions = {
	var ?window:Window;
	var ?filters:Array<{name:String, exts:Array<String>}>;
	var ?filterIndex:Int;
	var ?fileName:String;
	var ?title:String;
}

/**
	These are the bindings for the HL `ui.hdll` library, which contains some low level system access.
**/
class UI {
	@:hlNative("ui", "ui_init") static function init() {}

	static function __init__() {
		init();
	}

	@:hlNative("ui", "ui_dialog")
	static function _dialog(title:hl.Bytes, text:hl.Bytes, flags:Int):Int {
		return 0;
	}

	public static function dialog(title:String, text:String, flags:haxe.EnumFlags<DialogFlags>) {
		return @:privateAccess _dialog(title.bytes, text.bytes, flags.toInt()) != 0;
	}

	@:hlNative("ui", "ui_loop")
	public static function loop(blocking:Bool):LoopResult {
		return Quit;
	}

	@:hlNative("ui", "ui_stop_loop")
	public static function stopLoop():Void {}

	@:hlNative("ui", "ui_close_console")
	public static function closeConsole():Void {}

	public static function loadFile(opts:FileOptions) {
		return chooseFile(false, opts);
	}

	public static function saveFile(opts:FileOptions) {
		return chooseFile(true, opts);
	}

	static function chooseFile(save:Bool, opts:FileOptions) @:privateAccess {
		var out:Dynamic = {};
		if (opts.fileName != null) {
			var file = sys.FileSystem.absolutePath(opts.fileName);
			if (Sys.systemName() == "Windows")
				file = file.split("/").join("\\");
			var dir = null;
			if (sys.FileSystem.isDirectory(file)) {
				dir = file;
				file = null;
			} else {
				var path = new haxe.io.Path(file);
				dir = path.dir;
				file = path.file + (path.ext == null ? "" : "." + path.ext);
			}
			if (file != null)
				out.fileName = file.bytes;
			if (dir != null)
				out.directory = dir.bytes;
		}
		if (opts.title != null)
			out.title = opts.title.bytes;
		if (opts.filters != null) {
			var filters = new hl.NativeArray<hl.Bytes>(opts.filters.length * 2);
			var i = 0;
			for (f in opts.filters) {
				filters[i++] = f.name.bytes;
				filters[i++] = [for (e in f.exts) "*." + e].join(";").bytes;
			}
			out.filters = filters;
			out.filterIndex = opts.filterIndex;
		}
		if (opts.window != null)
			out.window = opts.window.h;
		var str = _chooseFile(save, out);
		return str == null ? null : String.fromUCS2(str);
	}

	@:hlNative("ui", "ui_choose_file")
	static function _chooseFile(forSave:Bool, obj:Dynamic):hl.Bytes {
		return null;
	}
	
	#if (hl_ver >= version("1.12.0"))
	public static function setClipboardText(text:String):Bool {
		if(text == null)
			return false;
		return @:privateAccess _setClipboardText(text.toUtf8());
	}

	@:hlNative("?ui", "ui_set_clipboard_text")
	static function _setClipboardText(text:hl.Bytes):Bool {
		return false;
	}

	public static function getClipboardText():String {
		var t = _getClipboardText();
		if( t == null )
			return null;
		return @:privateAccess String.fromUTF8(t);
	}

	@:hlNative("?ui", "ui_get_clipboard_text")
	static function _getClipboardText():hl.Bytes {
		return null;
	}
	#else
	public static function setClipboardText(text:String):Bool {
		return false;
	}
	public static function getClipboardText():String {
		return null;
	}
	#end
	
}
