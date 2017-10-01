var em = new haxe.ds.EnumValueMap();
var test = [
	1 => EContinue,
	2 => EBreak,
	3 => EConst(CString("bar",Double)),
	4 => EConst(CString("foo",Single)),
	5 => EArray(null, null),
];
for (k in test.keys()) {
	em.set(test[k],k);
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

var em = [
	EConst(CIdent("test")) => "test",
	EArray(null,null) => "bar",
	EBreak => "baz"
];
em.exists(EConst(CIdent("test"))) == true;
em.exists(EConst(CIdent("test2"))) == false;
em.get(EConst(CIdent("test"))) == "test";
em.remove(EConst(CIdent("test"))) == true;
em.exists(EConst(CIdent("test"))) == false;
em.get(EConst(CIdent("test"))) == null;

em.exists(EArray(null, null)) == true;
em.get(EArray(null, null)) == "bar";
em.remove(EArray(null, null)) == true;
em.exists(EArray(null, null)) == false;
em.get(EArray(null, null)) == null;

em.exists(EBreak) == true;
em.get(EBreak) == "baz";
em.remove(EBreak) == true;
em.exists(EBreak) == false;
em.get(EBreak) == null;

var evm = new haxe.ds.EnumValueMap();
evm.set(EVMA,1);
evm.set(EVMA,2);
evm.exists(EVMA) == true;
evm.get(EVMA) == 2;
evm.remove(EVMA) == true;
evm.exists(EVMA) == false;

evm.set(EVMB(), 8);
evm.set(EVMB(), 9);
evm.set(EVMB(null), 10);
evm.exists(EVMB()) == true;
evm.exists(EVMB(null)) == true;
evm.get(EVMB()) == 10;
evm.get(EVMB(null)) == 10;
evm.remove(EVMB()) == true;
evm.remove(EVMB()) == false;
evm.exists(EVMB()) == false;
evm.exists(EVMB(null)) == false;

evm.set(EVMC("foo"), 4);
evm.set(EVMC("foo"), 5);
evm.exists(EVMC("foo")) == true;
evm.get(EVMC("foo")) == 5;
evm.remove(EVMC("foo")) == true;
evm.exists(EVMC("foo")) == false;

evm.set(EVMD(null),91);
evm.exists(EVMD(null)) == true;
evm.get(EVMD(null)) == 91;
evm.remove(EVMD(null)) == true;
evm.exists(EVMD(null)) == false;

evm.set(EVMD(EVMA), 12);
evm.exists(EVMD(EVMA)) == true;
evm.get(EVMD(EVMA)) == 12;
evm.remove(EVMD(EVMA)) == true;
evm.exists(EVMD(EVMA)) == false;

evm.set(EVME(null),99);
evm.exists(EVME(null)) == true;
evm.exists(EVME()) == true;
evm.get(EVME(null)) == 99;
evm.get(EVME()) == 99;

evm.set(EVMF([EVMA, EVMB()]), 12);
evm.exists(EVMF([EVMA, EVMB()])) == true;
evm.exists(EVMF([EVMA, EVMB(null)])) == true;
evm.get(EVMF([EVMA, EVMB()])) == 12;
evm.get(EVMF([EVMA, EVMB(null)])) == 12;