package cases;

import Types;
using Lambda;

class Toplevel extends DisplayTestCase {
	/**
	class Main {
		static var myField;
		static function main() {{-1-}
			{-2-}
	**/
	function testToplevelResuming() {
		eq(true, hasToplevel(toplevel(pos(1)), "static", "myField"));
		eq(true, hasToplevel(toplevel(pos(2)), "static", "myField"));
	}

	/**
	class Main {
		static var myField;
		static function main() {
			{-1-}
			var a = "foo";
			{-2-}
	**/
	function testToplevelScoping() {
		var toplevel1 = toplevel(pos(1));
		var toplevel2 = toplevel(pos(2));
		eq(true, hasToplevel(toplevel1, "static", "myField"));
		eq(true, hasToplevel(toplevel2, "static", "myField"));
		eq(false, hasToplevel(toplevel1, "local", "a"));
		eq(true, hasToplevel(toplevel2, "local", "a"));
	}

	/**
	class Main {
		static var myField;
		static function main() {
			var a:{-1-}
	**/
	function testTypeCompletionLocal() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Main"));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
		eq(false, hasToplevel(typesCompletion, "static", "myField"));
	}

	/**
	class Main {
		var a:{-1-}
	**/
	function testTypeCompletionField() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Main"));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
	class Main {
		static function f(a:{-1-})
	**/
	function testTypeCompletionArgument() {
		// TODO: this currently doesn't work if there's no closing paren for function arguments
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Main"));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
	var a = function(a:{-1-}
	**/
	@:funcCode function testTypeCompletionArgumentLocal() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
	import {-1-}
	**/
	function testTypeCompletionImport() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
	using {-1-}
	**/
	function testTypeCompletionUsing() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
	class C extends {-1-} {
	**/
	function testTypeCompletionExtends() {
		// TODO: this currently doesn't work if there's no token after extends
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
	class C implements {-1-} {
	**/
	function testTypeCompletionImplements() {
		// TODO: this currently doesn't work if there's no token after implements
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
	typedef T = {a:{-1-}}
	**/
	function testTypeCompletionStructureField() {
		// TODO: this currently doesn't work if there's no token after the completion position
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
	var a:{a:{-1-}
	**/
	@:funcCode function testTypeCompletionStructureFieldAsType() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	static function hasToplevel(a:Array<ToplevelElement>, kind:String, name:String):Bool {
		return a.exists(function(t) return t.kind == kind && t.name == name);
	}
}
