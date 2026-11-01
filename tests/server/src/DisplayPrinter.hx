import haxe.display.Display;
import haxe.display.JsonModuleTypes;

using Lambda;

class DisplayPrinter {
	var indent = "";
	public function new() {}

	public function printPath(path:JsonTypePath) {
		final qualified = !(path.moduleName == "StdTypes" && path.pack.length == 0);
		final isSubType = path.moduleName != path.typeName;
		final isToplevelType = path.pack.length == 0 && !isSubType;

		if (isToplevelType && path.importStatus == Shadowed) {
			path.pack.push("std");
		}

		function printFullPath() {
			var printedPath = if (isSubType) path.typeName else path.moduleName;
			if (path.pack.length > 0) {
				printedPath = path.pack.join(".") + "." + printedPath;
			}
			return printedPath;
		}

		return if (qualified) printFullPath() else path.typeName;
	}

	public function printPathWithParams(path:JsonTypePathWithParams) {
		final s = printPath(path.path);
		if (path.params.length == 0) {
			return s;
		} else {
			var sparams = path.params.map(printType).join(", ");
			return '$s<$sparams>';
		}
	}

	public function printType<T>(t:JsonType<T>) {
		return switch t.kind {
			case TMono: "Unknown<0>";
			case TInst | TEnum | TType | TAbstract: printPathWithParams(t.args);
			case TDynamic:
				if (t.args == null) {
					"Dynamic";
				} else {
					final s = printTypeRec(t.args);
					'Dynamic<$s>';
				}
			case TAnonymous:
				final fields = t.args.fields;
				final s = [
					for (field in fields) {
						var prefix = if (hasMeta(field.meta, ":optional")) "?" else "";
						'$prefix${field.name} : ${printTypeRec(field.type)}';
					}
				].join(", ");
				s == '' ? '{ }' : '{ $s }';
			case TFun:
				var hasNamed = false;
				function printFunctionArgument(arg:JsonFunctionArgument) {
					if (arg.name != "") {
						hasNamed = true;
					}
					return this.printFunctionArgument(arg);
				}
				final args = t.args.args.map(printFunctionArgument);
				var r = printTypeRec(t.args.ret);
				if (t.args.ret.kind == TFun) r = '($r)';
				switch args.length {
					case 0: '() -> $r';
					case 1 if (hasNamed): '(${args[0]}) -> $r';
					case 1: '${args[0]} -> $r';
					case _: '(${args.join(", ")}) -> $r';
				}
		}
	}

	function printTypeRec<T>(t:JsonType<T>) {
		final old = indent;
		indent += "  ";
		final t = printType(t);
		indent = old;
		return t;
	}

	public function printFunctionArgument<T>(arg:JsonFunctionArgument):String {
		final nullRemoval = removeNulls(arg.t);
		final concreteType = if (!arg.opt) arg.t else nullRemoval.type;

		var argument = (if (arg.opt && arg.value == null) "?" else "") + arg.name;
		if (concreteType.kind != TMono || arg.name == "") {
			var hint = printTypeRec(concreteType);
			if (concreteType.kind == TFun) hint = '($hint)';
			argument += (arg.name == "" ? "" : " : ") + hint;
		}
		if (arg.value != null) {
			argument += " = " + arg.value.string;
		}
		return argument;
	}

	public function printSignatureFunctionArgument<T>(arg:JsonFunctionArgument):String {
		final nullRemoval = removeNulls(arg.t);
		final concreteType = if (!arg.opt) arg.t else nullRemoval.type;

		var argument = (if (arg.opt && arg.value == null) "?" else "") + arg.name;
		var hint = printTypeRec(concreteType);
		if (concreteType.kind == TFun) hint = '($hint)';
		argument += ":" + hint;
		if (arg.value != null) {
			argument += " = " + arg.value.string;
		}
		return argument;
	}

	public function printCallArguments<T>(signature:JsonFunctionSignature, printFunctionArgument:JsonFunctionArgument->String) {
		return "(" + signature.args.map(printFunctionArgument).join(", ") + ")";
	}

	function removeNulls<T>(type:JsonType<T>, nullable:Bool = false):{type:JsonType<T>, nullable:Bool} {
		switch type.kind {
			case TAbstract:
				final path:JsonTypePathWithParams = type.args;
				if (getDotPath(type) == "StdTypes.Null") {
					if (path.params != null && path.params[0] != null) {
						return removeNulls(path.params[0], true);
					}
				}
			case _:
		}
		return {type: type, nullable: nullable};
	}

	inline function isVoid<T>(type:JsonType<T>) {
		return getDotPath(type) == "StdTypes.Void";
	}

	function getDotPath<T>(type:JsonType<T>):Null<String> {
		final path = getTypePath(type);
		if (path == null) {
			return null;
		}
		return printPath(path.path);
	}

	function getTypePath<T>(type:JsonType<T>):Null<JsonTypePathWithParams> {
		return switch type.kind {
			case null: null;
			case TInst | TEnum | TType | TAbstract: type.args;
			case _: null;
		}
	}

	function hasMeta(?meta:JsonMetadata, name:String) {
		return meta != null && meta.exists(meta -> meta.name == cast name);
	}
}
