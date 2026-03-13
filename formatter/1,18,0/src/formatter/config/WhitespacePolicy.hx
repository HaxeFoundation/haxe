package formatter.config;

enum abstract WhitespacePolicy(String) {
	var None = "none";
	var Before = "before";
	var NoneBefore = "noneBefore";
	var OnlyBefore = "onlyBefore";
	var After = "after";
	var OnlyAfter = "onlyAfter";
	var NoneAfter = "noneAfter";
	var Around = "around";

	public static function has(policy:WhitespacePolicy, wantPolicy:WhitespacePolicy):Bool {
		return (remove(policy, wantPolicy) != policy);
	}

	public static function remove(policy:WhitespacePolicy, removePolicy:WhitespacePolicy):WhitespacePolicy {
		switch (removePolicy) {
			case None:
				return policy;
			case Before:
			case NoneBefore:
				removePolicy = Before;
			case OnlyBefore:
				removePolicy = Before;
			case After:
			case NoneAfter:
				removePolicy = After;
			case OnlyAfter:
				removePolicy = After;
			case Around:
				return None;
		}
		switch (policy) {
			case None:
				return None;
			case Before:
				if (removePolicy == Before) {
					return None;
				}
			case NoneBefore:
				return NoneBefore;
			case OnlyBefore:
				if (removePolicy == Before) {
					return None;
				}
			case After:
				if (removePolicy == After) {
					return None;
				}
			case NoneAfter:
				return NoneAfter;
			case OnlyAfter:
				if (removePolicy == After) {
					return None;
				}
			case Around:
				if (removePolicy == Before) {
					return After;
				}
				if (removePolicy == After) {
					return Before;
				}
		}
		return policy;
	}

	public static function add(policy:WhitespacePolicy, addPolicy:WhitespacePolicy):WhitespacePolicy {
		switch (addPolicy) {
			case None:
				return policy;
			case Before:
			case NoneBefore:
				addPolicy = Before;
			case OnlyBefore:
				addPolicy = Before;
			case After:
			case NoneAfter:
				addPolicy = After;
			case OnlyAfter:
				addPolicy = After;
			case Around:
				return None;
		}
		switch (policy) {
			case None:
				return addPolicy;
			case Before:
				if (addPolicy == After) {
					return Around;
				}
			case NoneBefore:
				if (addPolicy == Before) {
					return OnlyBefore;
				}
				if (addPolicy == After) {
					return OnlyAfter;
				}
			case OnlyBefore:
				if (addPolicy == After) {
					return Around;
				}
			case After:
				if (addPolicy == Before) {
					return Around;
				}
			case NoneAfter:
				if (addPolicy == Before) {
					return OnlyBefore;
				}
				if (addPolicy == After) {
					return OnlyAfter;
				}
			case OnlyAfter:
				if (addPolicy == Before) {
					return Around;
				}
			case Around:
		}
		return policy;
	}
}
