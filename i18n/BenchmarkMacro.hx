

class BenchmarkMacro {

	macro public static function getCode (id:haxe.macro.Expr, multiplier:haxe.macro.Expr, tests:haxe.macro.Expr, s:haxe.macro.Expr, substrings:haxe.macro.Expr) {

		var ct = haxe.macro.TypeTools.toComplexType(haxe.macro.Context.typeof(s));
		return macro {
			inline function stamp ():Float {
				${haxe.macro.Context.defined("sys") ? macro return Sys.cpuTime() : macro return haxe.Timer.stamp()};
			}
			var multiplier:Int = $multiplier;





			function wrap <T>(method:String, loops:Int, code:$ct->T) {

				var num:Int = loops*$multiplier;
				var x:T = null;




				var t = 0.0;
				for (i in 0...num) {
					var s = s + mkChar(i);
					var t1 = stamp();
					x = code(s);
					t += stamp() - t1;
				}


				$tests.push({ method : method, id : $id, time: t, res:x, loops: loops*$multiplier });

				//trace($id + "-" + method + ":" + t);
			}

			wrap('split', 100, function (s:$ct) {
				var res = [];
				for (i in $substrings) {
					res.push(s.split(i));
				}
				return res;
			});


			wrap('indexOf', 1000, function (s:$ct) {
				var res = 0;
				for (i in $substrings) {
					res += s.indexOf(i);
				}
				return res;
			});

			wrap('lastIndexOf', 100, function (s:$ct) {
				var res = 0;
				for (i in $substrings) {
					res += s.lastIndexOf(i);
				}
				return res;
			});
			var len = $s.length;
			wrap('charCodeAt n/2', 1000, function (s:$ct) {
				var res = s.charCodeAt(Math.floor(len / 2));
				return res;
			});

			wrap('charCodeAt n', 1000, function (s:$ct) {
				var res = s.charCodeAt(len -1);
				return res;
			});

			wrap('charCodeAt 1', 1000, function (s:$ct) {
				var res = s.charCodeAt(0);
				return res;
			});

			wrap('fastCodeAt n/2', 1000, function (s:$ct) {
				var res = s.fastCodeAt(Math.floor(len / 2));
				return res;
			});

			wrap('fastCodeAt n', 1000, function (s:$ct) {
				var res = s.fastCodeAt(len -1);
				return res;
			});

			wrap('fastCodeAt 1', 1000, function (s:$ct) {
				var res = s.fastCodeAt(0);
				return res;
			});

			wrap('charAt n/2', 1000, function (s:$ct) {
				var res = s.charAt(Math.floor(len / 2));
				return res;
			});

			wrap('charAt n', 1000, function (s:$ct) {
				var res = s.charAt(len -1);
				return res;
			});

			wrap('charAt 1', 1000, function (s:$ct) {
				var res = s.charAt(0);
				return res;
			});

			wrap('toUpperCase', 100, function (s:$ct) {
				var res = s.toUpperCase();
				return res;
			});

			wrap('toLowerCase', 100, function (s:$ct) {
				var res = s.toLowerCase();
				return res;
			});

			wrap('substring', 100, function (s:$ct) {
				var res = s.substring(0, Math.floor(len / 2));
				return res;
			});
			wrap('substr', 100, function (s:$ct) {
				var res = s.substr(Math.floor(len / 2));
				return res;
			});
			wrap('length', 1000, function (s:$ct) {
				var res = s.length;
				return res;
			});
			wrap('new', 1000, function (s:$ct) {
				var res = mkNew();
				return res;
			});
			wrap('+', 1000, function (s:$ct) {
				var res = s + s;
				return res;
			});

		}
	}

}