package hxcoro.task;

import hxcoro.task.CoroScopeTask;

class CoroSupervisorTask<T, C = Any> extends CoroScopeTask<T, C> {
	override function childErrors(_:AbstractTask<C>, _) {}
}

class StartableCoroSupervisorTask<T = Any, C> extends StartableCoroScopeTask<T, C> {
	override function childErrors(_:AbstractTask<C>, _) {}
}
