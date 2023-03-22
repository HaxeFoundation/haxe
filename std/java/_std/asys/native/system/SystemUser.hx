package asys.native.system;

private typedef NativeUser = java.nio.file.attribute.UserPrincipal;

@:coreApi
abstract SystemUser(NativeUser) from NativeUser to NativeUser {

	public inline function toString():String {
		return #if jvm this.toString() #else (cast this:java.lang.Object).toString() #end;
	}
}