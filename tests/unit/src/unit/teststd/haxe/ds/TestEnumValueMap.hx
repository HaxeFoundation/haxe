package unit.teststd.haxe.ds;

class TestEnumValueMap extends unit.Test {
	public function test() {
		var em = new haxe.ds.EnumValueMap();

		var test = [
			1 => EContinue,
			2 => EBreak,
			3 => EConst(CString("bar")),
			4 => EConst(CString("foo")),
			5 => EArray(null, null),
		];

		for (k in test.keys()) {
			em.set(test[k], k);
		}
		for (k in test.keys()) {
			eq(k, em.get(test[k]));
		}
		for (k in test.keys()) {
			eq(true, em.exists(test[k]));
		}
		for (k in test.keys()) {
			eq(true, em.remove(test[k]));
		}
		for (k in test.keys()) {
			eq(false, em.exists(test[k]));
		}
		var em = [EConst(CIdent("test")) => "test", EArray(null, null) => "bar", EBreak => "baz"];
		t(em.exists(EConst(CIdent("test"))));
		f(em.exists(EConst(CIdent("test2"))));
		eq(em.get(EConst(CIdent("test"))), "test");
		t(em.remove(EConst(CIdent("test"))));
		f(em.exists(EConst(CIdent("test"))));
		eq(em.get(EConst(CIdent("test"))), null);
		t(em.exists(EArray(null, null)));
		eq(em.get(EArray(null, null)), "bar");
		t(em.remove(EArray(null, null)));
		f(em.exists(EArray(null, null)));
		eq(em.get(EArray(null, null)), null);
		t(em.exists(EBreak));
		eq(em.get(EBreak), "baz");
		t(em.remove(EBreak));
		f(em.exists(EBreak));
		eq(em.get(EBreak), null);
		var evm = new haxe.ds.EnumValueMap();
		evm.set(EVMA, 1);
		evm.set(EVMA, 2);
		t(evm.exists(EVMA));
		eq(evm.get(EVMA), 2);
		t(evm.remove(EVMA));
		f(evm.exists(EVMA));
		evm.set(EVMB(), 8);
		evm.set(EVMB(), 9);
		evm.set(EVMB(null), 10);
		t(evm.exists(EVMB()));
		t(evm.exists(EVMB(null)));
		eq(evm.get(EVMB()), 10);
		eq(evm.get(EVMB(null)), 10);
		t(evm.remove(EVMB()));
		f(evm.remove(EVMB()));
		f(evm.exists(EVMB()));
		f(evm.exists(EVMB(null)));
		evm.set(EVMC("foo"), 4);
		evm.set(EVMC("foo"), 5);
		t(evm.exists(EVMC("foo")));
		f(evm.exists(EVMC(null)));
		evm.set(EVMC(null), 6);
		t(evm.exists(EVMC(null)));
		eq(evm.get(EVMC(null)), 6);
		eq(evm.get(EVMC("foo")), 5);
		t(evm.remove(EVMC("foo")));
		f(evm.exists(EVMC("foo")));
		evm.set(EVMD(null), 91);
		t(evm.exists(EVMD(null)));
		eq(evm.get(EVMD(null)), 91);
		t(evm.remove(EVMD(null)));
		f(evm.exists(EVMD(null)));
		evm.set(EVMD(EVMA), 12);
		t(evm.exists(EVMD(EVMA)));
		eq(evm.get(EVMD(EVMA)), 12);
		t(evm.remove(EVMD(EVMA)));
		f(evm.exists(EVMD(EVMA)));
		evm.set(EVME(null), 99);
		t(evm.exists(EVME(null)));
		t(evm.exists(EVME()));
		eq(evm.get(EVME(null)), 99);
		eq(evm.get(EVME()), 99);
		evm.clear();
		f(evm.exists(EVMF([EVMA, EVMB()])));
		f(evm.exists(EVMF([EVMA, EVMB(null)])));
		eq([for (k => v in evm) k].length, 0);

	}
}
