package cs.system.threading;

/** Enables multiple tasks to cooperatively work on an algorithm in parallel through multiple phases. */
@:native("System.Threading.Barrier")
extern class Barrier {
	/**
	 * Gets the number of the barrier's current phase.
	 * @return Returns the number of the barrier's current phase.
	 */
	var CurrentPhaseNumber(default, never):haxe.Int64;
	/**
	 * Gets the total number of participants in the barrier.
	 * @return Returns the total number of participants in the barrier.
	 */
	var ParticipantCount(default, never):Int;
	/**
	 * Gets the number of participants in the barrier that haven't yet signaled in the
	 * current phase.
	 * @return Returns the number of participants in the barrier that haven't yet
	 * signaled in the current phase.
	 */
	var ParticipantsRemaining(default, never):Int;
	@:overload(function(participantCount:Int):Void {})
	function new(participantCount:Int, postPhaseAction:cs.system.Action_1<cs.system.threading.Barrier>):Void;
	/**
	 * Notifies the  that there will be an additional participant.
	 * @return The phase number of the barrier in which the new participants will first
	 * participate.
	 */
	function AddParticipant():haxe.Int64;
	/**
	 * Notifies the  that there will be additional participants.
	 * @param participantCount The number of additional participants to add to the
	 * barrier.
	 * @return The phase number of the barrier in which the new participants will first
	 * participate.
	 */
	function AddParticipants(participantCount:Int):haxe.Int64;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/** Notifies the  that there will be one less participant. */
	function RemoveParticipant():Void;
	/**
	 * Notifies the  that there will be fewer participants.
	 * @param participantCount The number of additional participants to remove from the
	 * barrier.
	 */
	function RemoveParticipants(participantCount:Int):Void;
	@:overload(function():Void {})
	@:overload(function(millisecondsTimeout:Int):Bool {})
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(timeout:cs.system.TimeSpan):Bool {})
	@:overload(function(millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Bool {})
	/** Signals that a participant has reached the barrier and waits for all other participants to reach the barrier as well. */
	function SignalAndWait(timeout:cs.system.TimeSpan, cancellationToken:cs.system.threading.CancellationToken):Bool;
}
