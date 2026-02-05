package cs.system.net.networkinformation;

/** Provides Transmission Control Protocol (TCP) statistical data. */
@:native("System.Net.NetworkInformation.TcpStatistics")
extern class TcpStatistics {
	/**
	 * Gets the number of accepted Transmission Control Protocol (TCP) connection
	 * requests.
	 * @return An  value that specifies the total number of TCP connection requests
	 * accepted.
	 */
	var ConnectionsAccepted(default, never):haxe.Int64;
	/**
	 * Gets the number of Transmission Control Protocol (TCP) connection requests made
	 * by clients.
	 * @return An  value that specifies the total number of TCP connections initiated
	 * by clients.
	 */
	var ConnectionsInitiated(default, never):haxe.Int64;
	/**
	 * Specifies the total number of Transmission Control Protocol (TCP) connections
	 * established.
	 * @return An  value that specifies the total number of connections established.
	 */
	var CumulativeConnections(default, never):haxe.Int64;
	/**
	 * Gets the number of current Transmission Control Protocol (TCP) connections.
	 * @return An  value that specifies the total number of current TCP connections.
	 */
	var CurrentConnections(default, never):haxe.Int64;
	/**
	 * Gets the number of Transmission Control Protocol (TCP) errors received.
	 * @return An  value that specifies the total number of TCP errors received.
	 */
	var ErrorsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of failed Transmission Control Protocol (TCP) connection
	 * attempts.
	 * @return An  value that specifies the total number of failed TCP connection
	 * attempts.
	 */
	var FailedConnectionAttempts(default, never):haxe.Int64;
	/**
	 * Gets the maximum number of supported Transmission Control Protocol (TCP)
	 * connections.
	 * @return An  value that specifies the total number of TCP connections that can be
	 * supported.
	 */
	var MaximumConnections(default, never):haxe.Int64;
	/**
	 * Gets the maximum retransmission time-out value for Transmission Control Protocol
	 * (TCP) segments.
	 * @return An  value that specifies the maximum number of milliseconds permitted by
	 * a TCP implementation for the retransmission time-out value.
	 */
	var MaximumTransmissionTimeout(default, never):haxe.Int64;
	/**
	 * Gets the minimum retransmission time-out value for Transmission Control Protocol
	 * (TCP) segments.
	 * @return An  value that specifies the minimum number of milliseconds permitted by
	 * a TCP implementation for the retransmission time-out value.
	 */
	var MinimumTransmissionTimeout(default, never):haxe.Int64;
	/**
	 * Gets the number of RST packets received by Transmission Control Protocol (TCP)
	 * connections.
	 * @return An  value that specifies the total number of reset TCP connections.
	 */
	var ResetConnections(default, never):haxe.Int64;
	/**
	 * Gets the number of Transmission Control Protocol (TCP) segments sent with the
	 * reset flag set.
	 * @return An  value that specifies the total number of TCP segments sent with the
	 * reset flag set.
	 */
	var ResetsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Transmission Control Protocol (TCP) segments received.
	 * @return An  value that specifies the total number of TCP segments received.
	 */
	var SegmentsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Transmission Control Protocol (TCP) segments re-sent.
	 * @return An  value that specifies the total number of TCP segments retransmitted.
	 */
	var SegmentsResent(default, never):haxe.Int64;
	/**
	 * Gets the number of Transmission Control Protocol (TCP) segments sent.
	 * @return An  value that specifies the total number of TCP segments sent.
	 */
	var SegmentsSent(default, never):haxe.Int64;
}
