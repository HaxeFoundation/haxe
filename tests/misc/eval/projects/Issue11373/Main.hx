import pack.BuildMacro;
import pack.BuildMacro as BM;

@:build(BuildMacro.build())
class ClassImport {}

@:build(BuildMacro.BuildMacro.build())
class SubClassImport {}

@:build(BM.build())
class ClassImportAlias {}

@:build(BM.BuildMacro.build())
class SubClassImportAlias {}

function main() {
	trace(BuildMacro.report());
}