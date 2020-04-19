package cases;

import haxe.PosInfos;
import haxe.display.FsPath;
import haxe.display.Display;
import utest.Assert.*;

@:timeout(5000)
// TODO: somebody has to clean this up
class ReplaceRanges extends TestCase {
	function complete<S, T>(content:String, markerIndex:Int, cb:(response:CompletionResponse<S, T>, markers:Map<Int, Int>) -> Void) {
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[markerIndex], wasAutoTriggered: true}, function() {
			var result = parseCompletion();
			cb(result.result, transform.markers);
		});
	}

	function checkReplaceRange<S, T>(markers:Map<Int, Int>, startIndex:Int, endIndex:Int, response:CompletionResponse<S, T>, ?p:PosInfos) {
		equals(markers[startIndex], response.replaceRange.start.character, p);
		equals(markers[endIndex], response.replaceRange.end.character, p);
	}

	function testType() {
		complete("{-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("{-1-}cl{-2-}", 2);
		equals("cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);
	}

	function testModifier() {
		complete("extern {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("extern {-1-}cl{-2-}", 2);
		equals("cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);
	}

	function testExtends() {
		complete("class C extends {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("class C extends {-1-}Cl{-2-}", 2);
		equals("Cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);

		complete("class C {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("class C {-1-}ex{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("ex", response.filterString);

		complete("class C {-1-}ext{-2-} {}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("ext", response.filterString);
	}

	function testImplements() {
		complete("class C implements {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("class C implements {-1-}Cl{-2-}", 2);
		equals("Cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);

		complete("class C {-1-} {}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("class C {-1-}impl{-2-} {}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("impl", response.filterString);
	}

	function testImport() {
		complete("import {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("import {-1-}Cl{-2-}", 2);
		// equals("Cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);
	}

	function testUsing() {
		complete("using {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("using {-1-}Cl{-2-}", 2);
		// equals("Cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);
	}

	function testTo() {
		complete("abstract A(String) to {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("abstract A(String) to {-1-} { }", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("abstract A(String) to {-1-}Cl{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);

		complete("abstract A(String) to {-1-}Cl{-2-} { }", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}

	function testFrom() {
		complete("abstract A(String) from {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("abstract A(String) from {-1-} { }", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("abstract A(String) from {-1-}Cl{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);

		complete("abstract A(String) from {-1-}Cl{-2-} { }", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}

	function testStructuralExtension() {
		complete("typedef Main = { } & {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("typedef Main = { } & {-1-}Cl{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);

		complete("typedef Main = { > {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("typedef Main = { > {-1-}Cl{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}

	function testFields() {
		complete('class Main { static function main() "".{-1-}', 1);
		checkReplaceRange(markers, 1, 1, response);

		complete('class Main { static function main() "".{-1-}char', 1);
		checkReplaceRange(markers, 1, 1, response);

		complete('class Main { static function main() "".{-1-}char{-2-}', 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("char", response.filterString);
	}

	function testOverride() {
		complete("import haxe.io.Bytes; class Main extends Bytes { static function main() { } override {-1-}}", 1);
		checkReplaceRange(markers, 1, 1, response);
		equals("", response.filterString);

		complete("import haxe.io.Bytes; class Main extends Bytes { static function main() { } override {-1-}get{-2-}}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("get", response.filterString);
	}

	function testTypedef() {
		complete("typedef Foo = {-1-}
		", 1);
		checkReplaceRange(markers, 1, 1, response);
		equals("", response.filterString);

		complete("typedef Foo = {-1-}Cl{-2-}
		", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}

	function testTypehint() {
		complete("class Main { static function main() { var t:{-1-} }}", 1);
		checkReplaceRange(markers, 1, 1, response);
		equals("", response.filterString);

		complete("class Main { static function main() { var t:{-1-}Cl{-2-} }}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);

		complete("class Main { static function main() { var t:{-1-}String{-2-} }}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("String", response.filterString);

		complete("class Main { static function main() { var t:{-1-}Str{-2-}ing }}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Str", response.filterString);
	}

	function testTypeParameter() {
		complete("class Main { static function main() { var t:{-1-} }}", 1);
		checkReplaceRange(markers, 1, 1, response);
		equals("", response.filterString);

		complete("class Main { static function main() { var t:{-1-}Cl{-2-} }}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}
}
