package asys.native.system;

private typedef NativeGroup = java.nio.file.attribute.GroupPrincipal;

@:coreApi
abstract SystemGroup(NativeGroup) from NativeGroup to NativeGroup {

	public inline function toString():String {
		return this.toString();
	}
}