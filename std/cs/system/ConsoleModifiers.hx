package cs.system;

/** Represents the SHIFT, ALT, and CTRL modifier keys on a keyboard. */
@:native("System.ConsoleModifiers")
extern enum abstract ConsoleModifiers(Int) {
	var Alt = 1;
	var Control = 4;
	var Shift = 2;
	@:op(A | B) static function or(lhs:ConsoleModifiers, rhs:ConsoleModifiers):ConsoleModifiers;
	@:op(A & B) static function and(lhs:ConsoleModifiers, rhs:ConsoleModifiers):ConsoleModifiers;
	@:op(A ^ B) static function xor(lhs:ConsoleModifiers, rhs:ConsoleModifiers):ConsoleModifiers;
	@:op(~A) static function complement(value:ConsoleModifiers):ConsoleModifiers;
}
