package cs.system.xml.serialization;

/** Specifies various options to use when generating .NET Framework types for use with an XML Web Service. */
@:native("System.Xml.Serialization.CodeGenerationOptions")
extern enum abstract CodeGenerationOptions(Int) {
	var EnableDataBinding = 16;
	var GenerateNewAsync = 2;
	var GenerateOldAsync = 4;
	var GenerateOrder = 8;
	var GenerateProperties = 1;
	var None = 0;
	@:op(A | B) static function or(lhs:CodeGenerationOptions, rhs:CodeGenerationOptions):CodeGenerationOptions;
	@:op(A & B) static function and(lhs:CodeGenerationOptions, rhs:CodeGenerationOptions):CodeGenerationOptions;
	@:op(A ^ B) static function xor(lhs:CodeGenerationOptions, rhs:CodeGenerationOptions):CodeGenerationOptions;
	@:op(~A) static function complement(value:CodeGenerationOptions):CodeGenerationOptions;
}
