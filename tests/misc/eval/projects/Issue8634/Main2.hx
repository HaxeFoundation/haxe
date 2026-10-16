class Main2 {
	static function main() {
		var foo = false;
		var foo2 = false;

		var value = (foo = foo2) ? true : false;
		if (foo = foo2) {}
		if (foo = true) {}
		if (foo = null) {} // WConditionAssign
		if ({123; foo = true;}) {}

		while (foo = false) {}
		do {} while (foo = false);

		if ((foo = null) && (foo = true)) {}
		if ((foo = null) && ((foo = true) && (foo = true))) {}

		var dyn:Dynamic = false;
		var dyn2:Dynamic = null;
		if (dyn = 1) {}
		if (dyn = dyn2) {}
		if (dyn = null) {}
		if (dyn = false) {} // WConditionAssignBool

		// no warning
		if ({foo = true; true;}) {}
	}
}
