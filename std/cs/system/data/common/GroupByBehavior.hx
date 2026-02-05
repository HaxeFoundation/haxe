package cs.system.data.common;

/** Specifies the relationship between the columns in a GROUP BY clause and the non-aggregated columns in the select-list of a SELECT statement. */
@:native("System.Data.Common.GroupByBehavior")
extern enum abstract GroupByBehavior(Int) {
	var ExactMatch = 4;
	var MustContainAll = 3;
	var NotSupported = 1;
	var Unknown = 0;
	var Unrelated = 2;
}
