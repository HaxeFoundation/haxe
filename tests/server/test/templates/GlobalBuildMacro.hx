class GlobalBuildMacro {
	static function main() {}

	#if macro
	static function use()
		haxe.macro.Compiler.addGlobalMetadata('', '@:build(GlobalBuildMacro.build())', true, true, false);

	static function build():Array<haxe.macro.Expr.Field>
		return null;
	#end
}