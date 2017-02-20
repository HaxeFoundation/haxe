class BenchmarkMacro {

	macro public static function getCode (id:haxe.macro.Expr) {
		return macro {
			function wrap <T>(method, loops, code:Void->T) {
				var t = haxe.Timer.stamp();	

				var x:T = null;
				for (i in 0...loops*multiplier) {
					x = code();
				}
				var t = haxe.Timer.stamp() - t;
				tests.push({ method : method, id : $id, time: t, res:x });
				//trace($id + "-" + method + ":" + t);
			}

			wrap('split', 100, function () {
				var res = [];
				for (i in substrings) {
					res = res.concat(s.split(i));
				}
				return res;
			});

			wrap('indexOf', 100, function () {
				var res = 0;
				for (i in substrings) {
					res = s.indexOf(i);
				}
				return res;
			});

			wrap('lastIndexOf', 100, function () {
				var res = 0;
				for (i in substrings) {
					res = s.lastIndexOf(i);
				}
				return res;
			});
			
			wrap('charCodeAt n/2', 1000, function () {
				var res = s.charCodeAt(Math.floor(s.length / 2));
				return res;
			});
			
			wrap('charCodeAt n', 1000, function () {
				var res = s.charCodeAt(s.length -1);
			});

			wrap('charCodeAt 1', 1000, function () {
				var res = s.charCodeAt(0);
			});

			wrap('fastCodeAt n/2', 1000, function () {
				var res = s.fastCodeAt(Math.floor(s.length / 2));
				return res;
			});
			
			wrap('fastCodeAt n', 1000, function () {
				var res = s.fastCodeAt(s.length -1);
			});

			wrap('fastCodeAt 1', 1000, function () {
				var res = s.fastCodeAt(0);
			});
				
			wrap('toUpperCase', 100, function () {
				var res = s.toUpperCase();
				return res;
			});

			wrap('toLowerCase', 100, function () {
				var res = s.toLowerCase();
				return res;
			});

			wrap('substring', 100, function () {
				var res = s.substring(0, Math.floor(s.length / 2));
				return res;
			});
			wrap('substr', 100, function () {
				var res = s.substr(Math.floor(s.length / 2));
				return res;
			});
			wrap('length', 1000, function () {
				var res = s.length;
				return res;
			});
		}
	}
}