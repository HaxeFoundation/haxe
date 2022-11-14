package unit.issues;

import unit.HelperMacros.typeString;

class Issue10744 extends Test {
	function test() {
		var v:Null<Int> = 10;
		eq("Int", typeString(v ?? return));
		eq("Int", typeString(v ?? throw true));
		for (i in 0...1) {
			eq("Int", typeString(v ?? break));
			eq("Int", typeString(v ?? continue));
		}
		eq("Int", typeString(v ?? {
			(throw "nope");
		}));
		eq("Null<Int>", typeString(v ?? {
			if (Std.random(0) == 0)
				return;
			else
				v;
		}));
		eq("Null<Int>", typeString(v ?? {
			function foo()
				return;
			v;
		}));
		eq("Int", typeString(v ?? {
			if (Std.random(0) == 0)
				return;
			else
				throw "nope";
		}));
		eq("Int", typeString(v ?? {
			Std.parseInt(return);
		}));
		eq("Int", typeString(v ?? {
			(return)();
		}));
		eq("Int", typeString(v ?? {
			v + return;
			v;
		}));
		eq("Null<Int>", typeString(v ?? {
			false && return ;
			v;
		}));
		eq("Int", typeString(v ?? {
			true && return ;
			v;
		}));
		eq("Int", typeString(v ?? {
			false || return ;
			v;
		}));
		eq("Int", typeString(v ?? {
			final a = return;
			v;
		}));
		eq("Int", typeString(v ?? {
			[0, return, 2];
			v;
		}));
		eq("Null<Int>", typeString(v ?? {
			for (i in 0...Std.random(1)) {
				return;
			}
			v;
		}));
		eq("Int", typeString(v ?? {
			for (i in [0, return, 2]) {
				break;
			}
			v;
		}));
		eq("Int", typeString(v ?? {
			switch (null) {
				case _: return;
			}
			v;
		}));
		eq("Int", typeString(v ?? {
			switch (return) {
				case _: null;
			}
			v;
		}));
		eq("Int", typeString(v ?? {
			final arr = [];
			arr[return];
			v;
		}));
		eq("Int", typeString(v ?? {
			new EReg("", return);
			v;
		}));
		eq("Null<Int>", typeString(v ?? {
			do {
				break;
				return;
			} while (true);
			v;
		}));
		eq("Int", typeString(v ?? {
			do {
				break; // die
				return;
			} while (true); // resurrect
			return; // die again
			v;
		}));
		eq("Null<Int>", typeString(v ?? {
			try {
				throw null;
			} catch (e) {}
			v;
		}));
		eq("Int", typeString(v ?? {
			try {
				throw null;
			} catch (e) {
				return;
			}
			v;
		}));
		eq("Null<Int>", typeString(v ?? {
			try {
				throw null;
			} catch (e:String) {
				// fall through
			} catch (e) {
				return;
			}
			v;
		}));
		eq("Null<Int>", typeString(v ?? {
			try {
				return;
			} catch (e:String) {
				// fall through
			} catch (e) {
				return;
			}
			v;
		}));
		eq("Int", typeString(v ?? {
			try {
				return;
			} catch (e:String) {
				return;
			} catch (e) {
				return;
			}
			v;
		}));
		eq("Null<Int>", typeString(v ?? {
			try {
				// something here COULD throw and end up in the fall through case
			} catch (e:String) {
				// fall through
			} catch (e) {
				return;
			}
			v;
		}));
		eq("Int", typeString(v ?? {
			try {
				return;
			}
			v;
		}));
		eq("Null<Int>", typeString(v ?? {
			try {
				// fall through
			}
			v;
		}));
	}
}
