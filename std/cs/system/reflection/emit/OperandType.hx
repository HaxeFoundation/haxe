package cs.system.reflection.emit;

/** Describes the operand type of Microsoft intermediate language (MSIL) instruction. */
@:native("System.Reflection.Emit.OperandType")
extern enum OperandType {
	InlineBrTarget;
	InlineField;
	InlineI;
	InlineI8;
	InlineMethod;
	InlineNone;
	InlinePhi;
	InlineR;
	InlineSig;
	InlineString;
	InlineSwitch;
	InlineTok;
	InlineType;
	InlineVar;
	ShortInlineBrTarget;
	ShortInlineI;
	ShortInlineR;
	ShortInlineVar;
}
