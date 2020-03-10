package unit.issues;

import unit.Test;

class Issue9217 extends Test {
	#if (!java || jvm) // doesn't work on genjava
	function test() {
		eq("default", switch(Ea) {
			case "FB": "FB";
			case _: "default";
		});

		eq("FB", switch(FB) {
			case "FB": "FB";
			case _: "default";
		});

		eq("FB", switch(FB) {
			case "FB": "FB";
			case "Ea": "Ea";
			case _: "default";
		});

		eq("Ea", switch(Ea) {
			case "FB": "FB";
			case "Ea": "Ea";
			case _: "default";
		});

		eq("FB | Ea", switch(Ea) {
			case "FB" | "Ea": "FB | Ea";
			case _: "default";
		});

		var d:Dynamic = this;
		eq("Ea", d.Ea);
		eq("FB", d.FB);
	}

	var Ea = "Ea";
	var FB = "FB";
	#end
}