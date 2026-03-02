package cases.display.issues;

class Issue7877 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				new issue7877.ProcessedClass(false);
				new issue7877.ProcessedClass(true);
			}
		}
	**/
	function test(_) {
		vfs.putContent("issue7877/ProcessedClass.hx", "package issue7877;\n\n@:build(issue7877.ProcessMacro.build()) class ProcessedClass {\n\tfinal foo:Bool;\n\n\tfunction bar() {\n\t\ttrace(foo);\n\t}\n}");
		vfs.putContent("issue7877/ProcessMacro.hx", "package issue7877;\n\nimport haxe.macro.Expr;\nimport haxe.macro.Context;\n\nclass ProcessMacro {\n\tpublic static macro function build():Array<Field> {\n\t\tvar fields = Context.getBuildFields();\n\t\tvar toInit = [\n\t\t\tfor (field in fields) {\n\t\t\t\tswitch (field) {\n\t\t\t\t\tcase {name: name, kind: FVar(t, e), access: [AFinal]}:\n\t\t\t\t\t\t{name: name, type: t, def: e};\n\t\t\t\t\tcase _:\n\t\t\t\t\t\tcontinue;\n\t\t\t\t}\n\t\t\t}\n\t\t];\n\t\tvar args:Array<FunctionArg> = [];\n\t\tvar exprs = [];\n\t\tfor (init in toInit) {\n\t\t\targs.push({name: init.name, opt: init.def != null, type: init.type, value: init.def});\n\t\t\tvar n = init.name;\n\t\t\texprs.push(macro this.$n = $i{n});\n\t\t}\n\t\tfields.push({pos: Context.currentPos(), name: 'new', access: [APublic], kind: FFun({ret: null, args: args, expr: {pos: Context.currentPos(), expr: EBlock(exprs)}})});\n\t\treturn fields;\n\t}\n}");
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(0, diags.length);
	}
}
