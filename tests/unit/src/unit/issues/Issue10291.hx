package unit.issues;

import haxe.ds.Option;

class Issue10291 extends Test {
	function test() {
		eq("Case 2: b=null", match());
	}

	function match() {
		var ret = "";
		var o = Some({b: null});
		switch (o) {
			case Some({b: b}) if (b != null):
				ret = "Case 1: b=" + b;
			case Some({b: null}):
				ret = "Case 2: b=null";
			case Some(_):
				ret = "Case 3: Some(_)";
			case None:
				ret = ("Default");
		}
		return ret;
	}
}
