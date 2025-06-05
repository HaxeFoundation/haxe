package hxcoro.task;

import hxcoro.task.CoroScopeTask;

class CoroSupervisorTask<T> extends CoroScopeTask<T> {
	override function childErrors(_, _) {}
}

class StartableCoroSupervisorTask<T> extends StartableCoroScopeTask<T> {
	override function childErrors(_, _) {}
}
