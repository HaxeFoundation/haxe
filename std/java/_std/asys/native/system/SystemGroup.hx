package asys.native.system;

private typedef NativeGroup = java.nio.file.attribute.GroupPrincipal;

@:coreApi
abstract SystemGroup(NativeGroup) from NativeGroup to NativeGroup {

	public inline function toString():String {
		return #if jvm this.toString() #else (cast this:java.lang.Object).toString() #end;
	}
}