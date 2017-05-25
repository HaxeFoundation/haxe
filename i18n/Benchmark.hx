import haxe.i18n.Utf8;
import haxe.i18n.Utf16;
import haxe.i18n.Ucs2;
import haxe.i18n.Utf32;

using StringTools;

#if !macro
import BenchmarkMacro;
#end

typedef Test = { method : String, id : String, time : Float, loops:Int, res:Dynamic };

class Benchmark {

	static function main () {


		var t = haxe.Timer.stamp();
		var str = 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.

Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.

Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi.

Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat.

Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis.

At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur';

		var substrings = ['justo', 'gubergren'];
		var tests:Array<Test> = [];
		var multiplier:Int = 2;
		benchmarkString(str, str, substrings, tests, multiplier);
		benchmarkUtf32(str, new Utf32(str), substrings.map(Utf32.new), tests, multiplier);
		benchmarkUtf8(str, new Utf8(str), substrings.map(Utf8.new), tests, multiplier);
		benchmarkUtf16(str, new Utf16(str), substrings.map(Utf16.new), tests, multiplier);
		benchmarkUcs2(str, new Ucs2(str), substrings.map(Ucs2.new), tests, multiplier);

		print(str, tests);


		var str = 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr';
		var substrings = ['ipsum', 'amet'];
		var tests = [];
		var multiplier:Int = 20;
		benchmarkString(str, str, substrings, tests, multiplier);
		benchmarkUtf32(str, new Utf32(str), substrings.map(Utf32.new), tests, multiplier);
		benchmarkUtf8(str, new Utf8(str), substrings.map(Utf8.new), tests, multiplier);
		benchmarkUtf16(str, new Utf16(str), substrings.map(Utf16.new), tests, multiplier);
		benchmarkUcs2(str, new Ucs2(str), substrings.map(Ucs2.new), tests, multiplier);

		print(str, tests);
		trace("total: " + (haxe.Timer.stamp() - t));
	}


	static function cutTime (time:Float) {
		return time < 0.0001 ? 0.0001 : time;
	}
	static function print (str:String, tests:Array<Test>) {
		tests.sort(function (a, b) {
			var r = a.method < b.method ? -1 : a.method > b.method ? 1 : 0;
			if (r == 0 && a.method.length != b.method.length) {
				return a.method.length < b.method.length ? -1 : 1;
			}
			if (r == 0) {return switch [a.id, b.id] {
				case [a, b] if (a == b):  0;
				case ['native', _]:  -1;
				case [_, 'native']:   1;
				case ['ucs2', _]:    -1;
				case [_, 'ucs2']:     1;
				case ['utf32', _]:   -1;
				case [_, 'utf32']:    1;
				case ['utf8', _]:    -1;
				case [_, 'utf8']:     1;
				case ['utf16', _]:   -1;
				case [_, 'utf16']:    1;
				case _ : 0;
			}
			}
			return r;
		});

		var lookup = new haxe.ds.StringMap();
		for (t in tests) {
			if (t.id == 'native') lookup.set(t.method, cutTime(t.time));
		}
		var s = tests.map(function (t) {
			return
				StringTools.rpad(t.method, " ", 20) +
				StringTools.rpad(Std.string(t.loops), " ", 6) +
				StringTools.rpad(t.id, " ", 10) +
				StringTools.rpad('' + (Math.floor(t.time*10000)/10000), " ", 12) + ':' +
				StringTools.lpad('' + (Math.floor(cutTime(t.time)/lookup.get(t.method)*100)/100), " ", 8) + "x";
		});
		trace('\nString.length: ' + str.length + '\n' + s.join("\n"));
	}

	static function benchmarkString (raw:String, s:String, substrings:Array<String>, tests:Array<Test>, multiplier:Int) {
		inline function mkNew ():String return raw;
		inline function mkChar (i:Int) return "" + i;
		BenchmarkMacro.getCode('native', multiplier, tests, s, substrings);
	}


	static function benchmarkUtf8 (raw:String, s:Utf8, substrings:Array<Utf8>, tests:Array<Test>, multiplier:Int) {
		inline function mkNew () return new Utf8(raw);
		inline function mkChar (i:Int) return new Utf8("" + i);
		BenchmarkMacro.getCode('utf8', multiplier, tests, s, substrings);
	}
	static function benchmarkUtf16 (raw:String, s:Utf16, substrings:Array<Utf16>, tests:Array<Test>, multiplier:Int) {
		inline function mkNew () return new Utf16(raw);
		inline function mkChar (i:Int) return new Utf16("" + i);
		BenchmarkMacro.getCode('utf16', multiplier, tests, s, substrings);
	}

	static function benchmarkUcs2 (raw:String, s:Ucs2, substrings:Array<Ucs2>, tests:Array<Test>, multiplier:Int) {
		inline function mkNew () return new Ucs2(raw);
		inline function mkChar (i:Int) return new Ucs2("" + i);
		BenchmarkMacro.getCode('ucs2', multiplier, tests, s, substrings);
	}

	static function benchmarkUtf32 (raw:String, s:Utf32, substrings:Array<Utf32>, tests:Array<Test>, multiplier:Int) {
		inline function mkNew () return new Utf32(raw);
		inline function mkChar (i:Int) return new Utf32("" + i);
		BenchmarkMacro.getCode('utf32', multiplier, tests, s, substrings);
	}


}