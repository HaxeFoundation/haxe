package cs.system.security.cryptography.x509certificates;

/** Specifies the way to open the X.509 certificate store. */
@:native("System.Security.Cryptography.X509Certificates.OpenFlags")
extern enum abstract OpenFlags(Int) {
	var IncludeArchived = 8;
	var MaxAllowed = 2;
	var OpenExistingOnly = 4;
	var ReadOnly = 0;
	var ReadWrite = 1;
	@:op(A | B) static function or(lhs:OpenFlags, rhs:OpenFlags):OpenFlags;
	@:op(A & B) static function and(lhs:OpenFlags, rhs:OpenFlags):OpenFlags;
	@:op(A ^ B) static function xor(lhs:OpenFlags, rhs:OpenFlags):OpenFlags;
	@:op(~A) static function complement(value:OpenFlags):OpenFlags;
}
