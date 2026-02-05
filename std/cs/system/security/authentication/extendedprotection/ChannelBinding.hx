package cs.system.security.authentication.extendedprotection;

/** The  class encapsulates a pointer to the opaque data used to bind an authenticated transaction to a secure channel. */
@:native("System.Security.Authentication.ExtendedProtection.ChannelBinding")
extern class ChannelBinding extends cs.microsoft.win32.safehandles.SafeHandleZeroOrMinusOneIsInvalid {
	/**
	 * The  property gets the size, in bytes, of the channel binding token associated
	 * with the  instance.
	 * @return The size, in bytes, of the channel binding token in the  instance.
	 */
	var Size(default, never):Int;
}
