package cs.system.security;

/** Allows managed code to call into unmanaged code without a stack walk. This class cannot be inherited. */
@:native("System.Security.SuppressUnmanagedCodeSecurityAttribute")
extern class SuppressUnmanagedCodeSecurityAttribute extends cs.system.Attribute {
	function new():Void;
}
