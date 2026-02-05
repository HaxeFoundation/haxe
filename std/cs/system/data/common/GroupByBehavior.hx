package cs.system.data.common;

/** Specifies the relationship between the columns in a GROUP BY clause and the non-aggregated columns in the select-list of a SELECT statement. */
@:native("System.Data.Common.GroupByBehavior")
extern enum GroupByBehavior {
	ExactMatch;
	MustContainAll;
	NotSupported;
	Unknown;
	Unrelated;
}
