package cs.system.reflection.emit;

/** Describes how values are pushed onto a stack or popped off a stack. */
@:native("System.Reflection.Emit.StackBehaviour")
extern enum abstract StackBehaviour(Int) {
	var Pop0 = 0;
	var Pop1 = 1;
	var Pop1_pop1 = 2;
	var Popi = 3;
	var Popi_pop1 = 4;
	var Popi_popi = 5;
	var Popi_popi_popi = 7;
	var Popi_popi8 = 6;
	var Popi_popr4 = 8;
	var Popi_popr8 = 9;
	var Popref = 10;
	var Popref_pop1 = 11;
	var Popref_popi = 12;
	var Popref_popi_pop1 = 28;
	var Popref_popi_popi = 13;
	var Popref_popi_popi8 = 14;
	var Popref_popi_popr4 = 15;
	var Popref_popi_popr8 = 16;
	var Popref_popi_popref = 17;
	var Push0 = 18;
	var Push1 = 19;
	var Push1_push1 = 20;
	var Pushi = 21;
	var Pushi8 = 22;
	var Pushr4 = 23;
	var Pushr8 = 24;
	var Pushref = 25;
	var Varpop = 26;
	var Varpush = 27;
}
