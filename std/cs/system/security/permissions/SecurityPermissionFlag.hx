package cs.system.security.permissions;

/** Specifies access flags for the security permission object. */
@:native("System.Security.Permissions.SecurityPermissionFlag")
extern enum abstract SecurityPermissionFlag(Int) {
	var AllFlags = 16383;
	var Assertion = 1;
	var BindingRedirects = 8192;
	var ControlAppDomain = 1024;
	var ControlDomainPolicy = 256;
	var ControlEvidence = 32;
	var ControlPolicy = 64;
	var ControlPrincipal = 512;
	var ControlThread = 16;
	var Execution = 8;
	var Infrastructure = 4096;
	var NoFlags = 0;
	var RemotingConfiguration = 2048;
	var SerializationFormatter = 128;
	var SkipVerification = 4;
	var UnmanagedCode = 2;
	@:op(A | B) static function or(lhs:SecurityPermissionFlag, rhs:SecurityPermissionFlag):SecurityPermissionFlag;
	@:op(A & B) static function and(lhs:SecurityPermissionFlag, rhs:SecurityPermissionFlag):SecurityPermissionFlag;
	@:op(A ^ B) static function xor(lhs:SecurityPermissionFlag, rhs:SecurityPermissionFlag):SecurityPermissionFlag;
	@:op(~A) static function complement(value:SecurityPermissionFlag):SecurityPermissionFlag;
}
