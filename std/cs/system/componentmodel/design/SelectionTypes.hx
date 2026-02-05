package cs.system.componentmodel.design;

/** Defines identifiers that indicate the type of a selection. */
@:native("System.ComponentModel.Design.SelectionTypes")
extern enum abstract SelectionTypes(Int) {
	var Add = 64;
	var Auto = 1;
	var Click = 16;
	var MouseDown = 4;
	var MouseUp = 8;
	var Normal = 1;
	var Primary = 16;
	var Remove = 128;
	var Replace = 2;
	var Toggle = 32;
	var Valid = 31;
	@:op(A | B) static function or(lhs:SelectionTypes, rhs:SelectionTypes):SelectionTypes;
	@:op(A & B) static function and(lhs:SelectionTypes, rhs:SelectionTypes):SelectionTypes;
	@:op(A ^ B) static function xor(lhs:SelectionTypes, rhs:SelectionTypes):SelectionTypes;
	@:op(~A) static function complement(value:SelectionTypes):SelectionTypes;
}
