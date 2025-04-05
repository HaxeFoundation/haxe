class MyMacro {
	#if macro
	static var issue = haxe.macro.ComplexTypeTools.toType(macro :Type1);
	// ^ this causes the issue
	#end

	public static macro function foo() return macro { };
}