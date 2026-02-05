package cs.system.diagnostics;

@:native("System.Diagnostics.DebuggableAttribute.DebuggingModes")
extern enum abstract DebuggableAttribute_DebuggingModes(Int) {
	var Default = 1;
	var DisableOptimizations = 256;
	var EnableEditAndContinue = 4;
	var IgnoreSymbolStoreSequencePoints = 2;
	var None = 0;
	@:op(A | B) static function or(lhs:DebuggableAttribute_DebuggingModes, rhs:DebuggableAttribute_DebuggingModes):DebuggableAttribute_DebuggingModes;
	@:op(A & B) static function and(lhs:DebuggableAttribute_DebuggingModes, rhs:DebuggableAttribute_DebuggingModes):DebuggableAttribute_DebuggingModes;
	@:op(A ^ B) static function xor(lhs:DebuggableAttribute_DebuggingModes, rhs:DebuggableAttribute_DebuggingModes):DebuggableAttribute_DebuggingModes;
	@:op(~A) static function complement(value:DebuggableAttribute_DebuggingModes):DebuggableAttribute_DebuggingModes;
}
