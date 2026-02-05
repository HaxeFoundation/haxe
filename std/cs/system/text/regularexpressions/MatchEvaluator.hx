package cs.system.text.regularexpressions;

/**
 * Represents the method that is called each time a regular expression match is
 * found during a  method operation.
 * @param match The  object that represents a single regular expression match
 * during a  method operation.
 * @return A string returned by the method that is represented by the  delegate.
 */
@:native("System.Text.RegularExpressions.MatchEvaluator")
extern class MatchEvaluator extends cs.system.MulticastDelegate {
	function new(func:(match:cs.system.text.regularexpressions.Match)->String):Void;
	function Invoke(match:cs.system.text.regularexpressions.Match):String;
}
