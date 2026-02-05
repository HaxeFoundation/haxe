package cs.system.net;

/** The  class provides additional context about the underlying transport layer. */
@:native("System.Net.TransportContext")
extern class TransportContext {
	/**
	 * Retrieves the requested channel binding.
	 * @param kind The type of channel binding to retrieve.
	 * @return The requested , or  if the channel binding is not supported by the
	 * current transport or by the operating system.
	 */
	function GetChannelBinding(kind:cs.system.security.authentication.extendedprotection.ChannelBindingKind):cs.system.security.authentication.extendedprotection.ChannelBinding;
}
