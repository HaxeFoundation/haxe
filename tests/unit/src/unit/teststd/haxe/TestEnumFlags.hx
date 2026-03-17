package unit.teststd.haxe;

class TestEnumFlags extends unit.Test {
	public function test() {
		// new + has
		var flags = new haxe.EnumFlags();
		f(flags.has(EA));
		flags = new haxe.EnumFlags(1);
		t(flags.has(EA));

		// set
		flags.set(EB);
		t(flags.has(EA));
		t(flags.has(EB));
		f(flags.has(EC));

		// unset
		flags.unset(EC);
		t(flags.has(EA));
		t(flags.has(EB));
		f(flags.has(EC));
		flags.unset(EA);
		f(flags.has(EA));
		t(flags.has(EB));
		f(flags.has(EC));

		// new + has
		var flags = new haxe.EnumFlags();
		f(flags.has(EA));
		flags = new haxe.EnumFlags(1);
		t(flags.has(EA));

		// set
		flags.set(EB);
		t(flags.has(EA));
		t(flags.has(EB));
		f(flags.has(EC));

		// unset
		flags.unset(EC);
		t(flags.has(EA));
		t(flags.has(EB));
		f(flags.has(EC));
		flags.unset(EA);
		f(flags.has(EA));
		t(flags.has(EB));
		f(flags.has(EC));


		// Big Enum (32)
		var bigFlags = new haxe.EnumFlags(1<<31);
		t(bigFlags.has( EF_31 ));
		bigFlags.unset( EF_31 );
		f(bigFlags.has( EF_31 ));
		bigFlags.set( EF_31 );
		t(bigFlags.has( EF_31 ));

		// setTo
		var flags = new haxe.EnumFlags();
		flags.setTo(EB, true);
		t(flags.has(EB));
		flags.setTo(EB, false);
		f(flags.has(EB));
		flags.setTo(EB, true);
		t(flags.has(EB));
	}
}

private enum EnumFlagTest2 {
	EF_00; EF_01; EF_02; EF_03; EF_04; EF_05; EF_06; EF_07;
	EF_08; EF_09; EF_10; EF_11; EF_12; EF_13; EF_14; EF_15;
	EF_16; EF_17; EF_18; EF_19; EF_20; EF_21; EF_22; EF_23;
	EF_24; EF_25; EF_26; EF_27; EF_28; EF_29; EF_30; EF_31;
}
