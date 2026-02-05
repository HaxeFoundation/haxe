package cs.system.reflection.emit;

/** Describes the operand type of Microsoft intermediate language (MSIL) instruction. */
@:native("System.Reflection.Emit.OperandType")
extern enum abstract OperandType(Int) {
	var InlineBrTarget = 0;
	var InlineField = 1;
	var InlineI = 2;
	var InlineI8 = 3;
	var InlineMethod = 4;
	var InlineNone = 5;
	var InlinePhi = 6;
	var InlineR = 7;
	var InlineSig = 9;
	var InlineString = 10;
	var InlineSwitch = 11;
	var InlineTok = 12;
	var InlineType = 13;
	var InlineVar = 14;
	var ShortInlineBrTarget = 15;
	var ShortInlineI = 16;
	var ShortInlineR = 17;
	var ShortInlineVar = 18;
}
