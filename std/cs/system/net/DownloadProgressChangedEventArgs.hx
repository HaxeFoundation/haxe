package cs.system.net;

/** Provides data for the  event of a . */
@:native("System.Net.DownloadProgressChangedEventArgs")
extern class DownloadProgressChangedEventArgs extends cs.system.componentmodel.ProgressChangedEventArgs {
	/**
	 * Gets the number of bytes received.
	 * @return An  value that indicates the number of bytes received.
	 */
	var BytesReceived(default, never):haxe.Int64;
	/**
	 * Gets the total number of bytes in a  data download operation.
	 * @return An  value that indicates the number of bytes that will be received.
	 */
	var TotalBytesToReceive(default, never):haxe.Int64;
}
