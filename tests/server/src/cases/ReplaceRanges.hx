package cases;

import haxe.PosInfos;
import haxe.display.FsPath;
import haxe.display.Display;
import utest.Assert.*;

// TODO: somebody has to clean this up
class ReplaceRanges extends TestCase {
	@:coroutine
	function complete<S, T>(content:String, markerIndex:Int) {
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[markerIndex], wasAutoTriggered: true});
		var result = parseCompletion();
		return {response: result.result, markers: transform.markers};
	}

	function checkReplaceRange<S, T>(markers:Map<Int, Int>, startIndex:Int, endIndex:Int, response:CompletionResponse<S, T>, ?p:PosInfos) {
		equals(markers[startIndex], response.replaceRange.start.character, p);
		equals(markers[endIndex], response.replaceRange.end.character, p);
	}

	function testType() {
		var result = complete("{-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("{-1-}cl{-2-}", 2);
		equals("cl", result.response.filterString);
		checkReplaceRange(result.markers, 1, 2, result.response);
	}

	function testModifier() {
		var result = complete("extern {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("extern {-1-}cl{-2-}", 2);
		equals("cl", result.response.filterString);
		checkReplaceRange(result.markers, 1, 2, result.response);
	}

	function testExtends() {
		var result = complete("class C extends {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("class C extends {-1-}Cl{-2-}", 2);
		equals("Cl", result.response.filterString);
		checkReplaceRange(result.markers, 1, 2, result.response);

		var result = complete("class C {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("class C {-1-}ex{-2-}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("ex", result.response.filterString);

		var result = complete("class C {-1-}ext{-2-} {}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("ext", result.response.filterString);
	}

	function testImplements() {
		var result = complete("class C implements {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("class C implements {-1-}Cl{-2-}", 2);
		equals("Cl", result.response.filterString);
		checkReplaceRange(result.markers, 1, 2, result.response);

		var result = complete("class C {-1-} {}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("class C {-1-}impl{-2-} {}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("impl", result.response.filterString);
	}

	function testImport() {
		var result = complete("import {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("import {-1-}Cl{-2-}", 2);
		// equals("Cl", result.response.filterString);
		checkReplaceRange(result.markers, 1, 2, result.response);
	}

	function testUsing() {
		var result = complete("using {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("using {-1-}Cl{-2-}", 2);
		// equals("Cl", result.response.filterString);
		checkReplaceRange(result.markers, 1, 2, result.response);
	}

	function testTo() {
		var result = complete("abstract A(String) to {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("abstract A(String) to {-1-} { }", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("abstract A(String) to {-1-}Cl{-2-}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Cl", result.response.filterString);

		var result = complete("abstract A(String) to {-1-}Cl{-2-} { }", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Cl", result.response.filterString);
	}

	function testFrom() {
		var result = complete("abstract A(String) from {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("abstract A(String) from {-1-} { }", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("abstract A(String) from {-1-}Cl{-2-}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Cl", result.response.filterString);

		var result = complete("abstract A(String) from {-1-}Cl{-2-} { }", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Cl", result.response.filterString);
	}

	function testStructuralExtension() {
		var result = complete("typedef Main = { } & {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("typedef Main = { } & {-1-}Cl{-2-}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Cl", result.response.filterString);

		var result = complete("typedef Main = { > {-1-}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete("typedef Main = { > {-1-}Cl{-2-}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Cl", result.response.filterString);
	}

	function testFields() {
		var result = complete('class Main { static function main() "".{-1-}', 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete('class Main { static function main() "".{-1-}char', 1);
		checkReplaceRange(result.markers, 1, 1, result.response);

		var result = complete('class Main { static function main() "".{-1-}char{-2-}', 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("char", result.response.filterString);
	}

	function testOverride() {
		var result = complete("import haxe.io.Bytes; class Main extends Bytes { static function main() { } override {-1-}}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);
		equals("", result.response.filterString);

		var result = complete("import haxe.io.Bytes; class Main extends Bytes { static function main() { } override {-1-}get{-2-}}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("get", result.response.filterString);
	}

	function testTypedef() {
		var result = complete("typedef Foo = {-1-}
		", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);
		equals("", result.response.filterString);

		var result = complete("typedef Foo = {-1-}Cl{-2-}
		", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Cl", result.response.filterString);
	}

	function testTypehint() {
		var result = complete("class Main { static function main() { var t:{-1-} }}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);
		equals("", result.response.filterString);

		var result = complete("class Main { static function main() { var t:{-1-}Cl{-2-} }}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Cl", result.response.filterString);

		var result = complete("class Main { static function main() { var t:{-1-}String{-2-} }}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("String", result.response.filterString);

		var result = complete("class Main { static function main() { var t:{-1-}Str{-2-}ing }}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Str", result.response.filterString);
	}

	function testTypeParameter() {
		var result = complete("class Main { static function main() { var t:{-1-} }}", 1);
		checkReplaceRange(result.markers, 1, 1, result.response);
		equals("", result.response.filterString);

		var result = complete("class Main { static function main() { var t:{-1-}Cl{-2-} }}", 2);
		checkReplaceRange(result.markers, 1, 2, result.response);
		equals("Cl", result.response.filterString);
	}
}
