package hxcoro.task.node;

import haxe.Exception;
import haxe.exceptions.CancellationException;
import hxcoro.task.AbstractTask;
import hxcoro.task.CoroTask;

interface INodeStrategy {
	function complete<T>(task:CoroBaseTask<T>):Void;
	function childSucceeds<T>(task:CoroBaseTask<T>, child:AbstractTask):Void;
	function childErrors<T>(task:CoroBaseTask<T>, child:AbstractTask, cause:Exception):Void;
	function childCancels<T>(task:CoroBaseTask<T>, child:AbstractTask, cause:CancellationException):Void;
}
