package sys.db.postgreSql.pgsql ;
typedef NoticeArguments = {
	severity  : String,
	sqlstate  : String,
	message   : String,
	?detail   : String,
	?hint     : String,
	?position : String,
	?query    : String,
	?where    : String,
	?file     : String,
	?line     : String,
	?routine  : String
}

class Notice {
	public var severity : String;
	public var sqlstate : String;
	public var message  : String;
	public var detail   : String; // possibly null
	public var hint     : String; // possibly null
	public var position : String; // possibly null
	public var query    : String; // possibly null
	public var where    : String; // possibly null
	public var file     : String; // possibly null
	public var line     : String; // possibly null
	public var routine  : String; // possibly null

	public function new(notice:NoticeArguments){
		this.severity =  notice.severity;
		this.sqlstate =  notice.sqlstate;
		this.message  =  notice.message;
		this.detail   =  notice.detail;
		this.hint     =  notice.hint;
		this.position =  notice.position;
		this.query    =  notice.query;
		this.where    =  notice.where;
		this.file     =  notice.file;
		this.line     =  notice.line;
		this.routine  =  notice.routine;
	}

	public static function translateSqlState(code : String){
		return switch(code){
			//Class 00 — Successful Completion
			case "00000"	: "successful_completion";
							  //Class 01 — Warning
			case "01000"	: "warning";
			case "0100C"	: "dynamic_result_sets_returned";
			case "01008"	: "implicit_zero_bit_padding";
			case "01003"	: "null_value_eliminated_in_set_function";
			case "01007"	: "privilege_not_granted";
			case "01006"	: "privilege_not_revoked";
			case "01004"	: "string_data_right_truncation";
			case "01P01"	: "deprecated_feature";
							  //Class 02 — No Data (this is also a warning class per the SQL standard)
			case "02000"	: "no_data";
			case "02001"	: "no_additional_dynamic_result_sets_returned";
							  //Class 03 — SQL Statement Not Yet Complete
			case "03000"	: "sql_statement_not_yet_complete";
							  //Class 08 — Connection Exception
			case "08000"	: "connection_exception";
			case "08003"	: "connection_does_not_exist";
			case "08006"	: "connection_failure";
			case "08001"	: "sqlclient_unable_to_establish_sqlconnection";
			case "08004"	: "sqlserver_rejected_establishment_of_sqlconnection";
			case "08007"	: "transaction_resolution_unknown";
			case "08P01"	: "protocol_violation";
							  //Class 09 — Triggered Action Exception
			case "09000"	: "triggered_action_exception";
							  //Class 0A — Feature Not Supported
			case "0A000"	: "feature_not_supported";
							  //Class 0B — Invalid Transaction Initiation
			case "0B000"	: "invalid_transaction_initiation";
							  //Class 0F — Locator Exception
			case "0F000"	: "locator_exception";
			case "0F001"	: "invalid_locator_specification";
							  //Class 0L — Invalid Grantor
			case "0L000"	: "invalid_grantor";
			case "0LP01"	: "invalid_grant_operation";
							  //Class 0P — Invalid Role Specification
			case "0P000"	: "invalid_role_specification";
							  //Class 0Z — Diagnostics Exception
			case "0Z000"	: "diagnostics_exception";
			case "0Z002"	: "stacked_diagnostics_accessed_without_active_handler";
							  //Class 20 — Case Not Found
			case "20000"	: "case_not_found";
							  //Class 21 — Cardinality Violation
			case "21000"	: "cardinality_violation";
							  //Class 22 — Data Exception
			case "22000"	: "data_exception";
			case "2202E"	: "array_subscript_error";
			case "22021"	: "character_not_in_repertoire";
			case "22008"	: "datetime_field_overflow";
			case "22012"	: "division_by_zero";
			case "22005"	: "error_in_assignment";
			case "2200B"	: "escape_character_conflict";
			case "22022"	: "indicator_overflow";
			case "22015"	: "interval_field_overflow";
			case "2201E"	: "invalid_argument_for_logarithm";
			case "22014"	: "invalid_argument_for_ntile_function";
			case "22016"	: "invalid_argument_for_nth_value_function";
			case "2201F"	: "invalid_argument_for_power_function";
			case "2201G"	: "invalid_argument_for_width_bucket_function";
			case "22018"	: "invalid_character_value_for_cast";
			case "22007"	: "invalid_datetime_format";
			case "22019"	: "invalid_escape_character";
			case "2200D"	: "invalid_escape_octet";
			case "22025"	: "invalid_escape_sequence";
			case "22P06"	: "nonstandard_use_of_escape_character";
			case "22010"	: "invalid_indicator_parameter_value";
			case "22023"	: "invalid_parameter_value";
			case "2201B"	: "invalid_regular_expression";
			case "2201W"	: "invalid_row_count_in_limit_clause";
			case "2201X"	: "invalid_row_count_in_result_offset_clause";
			case "22009"	: "invalid_time_zone_displacement_value";
			case "2200C"	: "invalid_use_of_escape_character";
			case "2200G"	: "most_specific_type_mismatch";
			case "22004"	: "null_value_not_allowed";
			case "22002"	: "null_value_no_indicator_parameter";
			case "22003"	: "numeric_value_out_of_range";
			case "22026"	: "string_data_length_mismatch";
			case "22001"	: "string_data_right_truncation";
			case "22011"	: "substring_error";
			case "22027"	: "trim_error";
			case "22024"	: "unterminated_c_string";
			case "2200F"	: "zero_length_character_string";
			case "22P01"	: "floating_point_exception";
			case "22P02"	: "invalid_text_representation";
			case "22P03"	: "invalid_binary_representation";
			case "22P04"	: "bad_copy_file_format";
			case "22P05"	: "untranslatable_character";
			case "2200L"	: "not_an_xml_document";
			case "2200M"	: "invalid_xml_document";
			case "2200N"	: "invalid_xml_content";
			case "2200S"	: "invalid_xml_comment";
			case "2200T"	: "invalid_xml_processing_instruction";
							  //Class 23 — Integrity Constraint Violation
			case "23000"	: "integrity_constraint_violation";
			case "23001"	: "restrict_violation";
			case "23502"	: "not_null_violation";
			case "23503"	: "foreign_key_violation";
			case "23505"	: "unique_violation";
			case "23514"	: "check_violation";
			case "23P01"	: "exclusion_violation";
							  //Class 24 — Invalid Cursor State
			case "24000"	: "invalid_cursor_state";
							  //Class 25 — Invalid Transaction State
			case "25000"	: "invalid_transaction_state";
			case "25001"	: "active_sql_transaction";
			case "25002"	: "branch_transaction_already_active";
			case "25008"	: "held_cursor_requires_same_isolation_level";
			case "25003"	: "inappropriate_access_mode_for_branch_transaction";
			case "25004"	: "inappropriate_isolation_level_for_branch_transaction";
			case "25005"	: "no_active_sql_transaction_for_branch_transaction";
			case "25006"	: "read_only_sql_transaction";
			case "25007"	: "schema_and_data_statement_mixing_not_supported";
			case "25P01"	: "no_active_sql_transaction";
			case "25P02"	: "in_failed_sql_transaction";
							  //Class 26 — Invalid SQL Statement Name
			case "26000"	: "invalid_sql_statement_name";
							  //Class 27 — Triggered Data Change Violation
			case "27000"	: "triggered_data_change_violation";
							  //Class 28 — Invalid Authorization Specification
			case "28000"	: "invalid_authorization_specification";
			case "28P01"	: "invalid_password";
							  //Class 2B — Dependent Privilege Descriptors Still Exist
			case "2B000"	: "dependent_privilege_descriptors_still_exist";
			case "2BP01"	: "dependent_objects_still_exist";
							  //Class 2D — Invalid Transaction Termination
			case "2D000"	: "invalid_transaction_termination";
							  //Class 2F — SQL Routine Exception
			case "2F000"	: "sql_routine_exception";
			case "2F005"	: "function_executed_no_return_statement";
			case "2F002"	: "modifying_sql_data_not_permitted";
			case "2F003"	: "prohibited_sql_statement_attempted";
			case "2F004"	: "reading_sql_data_not_permitted";
							  //Class 34 — Invalid Cursor Name
			case "34000"	: "invalid_cursor_name";
							  //Class 38 — External Routine Exception
			case "38000"	: "external_routine_exception";
			case "38001"	: "containing_sql_not_permitted";
			case "38002"	: "modifying_sql_data_not_permitted";
			case "38003"	: "prohibited_sql_statement_attempted";
			case "38004"	: "reading_sql_data_not_permitted";
							  //Class 39 — External Routine Invocation Exception
			case "39000"	: "external_routine_invocation_exception";
			case "39001"	: "invalid_sqlstate_returned";
			case "39004"	: "null_value_not_allowed";
			case "39P01"	: "trigger_protocol_violated";
			case "39P02"	: "srf_protocol_violated";
							  //Class 3B — Savepoint Exception
			case "3B000"	: "savepoint_exception";
			case "3B001"	: "invalid_savepoint_specification";
							  //Class 3D — Invalid Catalog Name
			case "3D000"	: "invalid_catalog_name";
							  //Class 3F — Invalid Schema Name
			case "3F000"	: "invalid_schema_name";
							  //Class 40 — Transaction Rollback
			case "40000"	: "transaction_rollback";
			case "40002"	: "transaction_integrity_constraint_violation";
			case "40001"	: "serialization_failure";
			case "40003"	: "statement_completion_unknown";
			case "40P01"	: "deadlock_detected";
							  //Class 42 — Syntax Error or Access Rule Violation
			case "42000"	: "syntax_error_or_access_rule_violation";
			case "42601"	: "syntax_error";
			case "42501"	: "insufficient_privilege";
			case "42846"	: "cannot_coerce";
			case "42803"	: "grouping_error";
			case "42P20"	: "windowing_error";
			case "42P19"	: "invalid_recursion";
			case "42830"	: "invalid_foreign_key";
			case "42602"	: "invalid_name";
			case "42622"	: "name_too_long";
			case "42939"	: "reserved_name";
			case "42804"	: "datatype_mismatch";
			case "42P18"	: "indeterminate_datatype";
			case "42P21"	: "collation_mismatch";
			case "42P22"	: "indeterminate_collation";
			case "42809"	: "wrong_object_type";
			case "42703"	: "undefined_column";
			case "42883"	: "undefined_function";
			case "42P01"	: "undefined_table";
			case "42P02"	: "undefined_parameter";
			case "42704"	: "undefined_object";
			case "42701"	: "duplicate_column";
			case "42P03"	: "duplicate_cursor";
			case "42P04"	: "duplicate_database";
			case "42723"	: "duplicate_function";
			case "42P05"	: "duplicate_prepared_statement";
			case "42P06"	: "duplicate_schema";
			case "42P07"	: "duplicate_table";
			case "42712"	: "duplicate_alias";
			case "42710"	: "duplicate_object";
			case "42702"	: "ambiguous_column";
			case "42725"	: "ambiguous_function";
			case "42P08"	: "ambiguous_parameter";
			case "42P09"	: "ambiguous_alias";
			case "42P10"	: "invalid_column_reference";
			case "42611"	: "invalid_column_definition";
			case "42P11"	: "invalid_cursor_definition";
			case "42P12"	: "invalid_database_definition";
			case "42P13"	: "invalid_function_definition";
			case "42P14"	: "invalid_prepared_statement_definition";
			case "42P15"	: "invalid_schema_definition";
			case "42P16"	: "invalid_table_definition";
			case "42P17"	: "invalid_object_definition";
							  //Class 44 — WITH CHECK OPTION Violation
			case "44000"	: "with_check_option_violation";
							  //Class 53 — Insufficient Resources
			case "53000"	: "insufficient_resources";
			case "53100"	: "disk_full";
			case "53200"	: "out_of_memory";
			case "53300"	: "too_many_connections";
			case "53400"	: "configuration_limit_exceeded";
							  //Class 54 — Program Limit Exceeded
			case "54000"	: "program_limit_exceeded";
			case "54001"	: "statement_too_complex";
			case "54011"	: "too_many_columns";
			case "54023"	: "too_many_arguments";
							  //Class 55 — Object Not In Prerequisite State
			case "55000"	: "object_not_in_prerequisite_state";
			case "55006"	: "object_in_use";
			case "55P02"	: "cant_change_runtime_param";
			case "55P03"	: "lock_not_available";
							  //Class 57 — Operator Intervention
			case "57000"	: "operator_intervention";
			case "57014"	: "query_canceled";
			case "57P01"	: "admin_shutdown";
			case "57P02"	: "crash_shutdown";
			case "57P03"	: "cannot_connect_now";
			case "57P04"	: "database_dropped";
							  //Class 58 — System Error (errors external to PostgreSQL itself)
			case "58000"	: "system_error";
			case "58030"	: "io_error";
			case "58P01"	: "undefined_file";
			case "58P02"	: "duplicate_file";
							  //Class F0 — Configuration File Error
			case "F0000"	: "config_file_error";
			case "F0001"	: "lock_file_exists";
							  //Class HV — Foreign Data Wrapper Error (SQL/MED)
			case "HV000"	: "fdw_error";
			case "HV005"	: "fdw_column_name_not_found";
			case "HV002"	: "fdw_dynamic_parameter_value_needed";
			case "HV010"	: "fdw_function_sequence_error";
			case "HV021"	: "fdw_inconsistent_descriptor_information";
			case "HV024"	: "fdw_invalid_attribute_value";
			case "HV007"	: "fdw_invalid_column_name";
			case "HV008"	: "fdw_invalid_column_number";
			case "HV004"	: "fdw_invalid_data_type";
			case "HV006"	: "fdw_invalid_data_type_descriptors";
			case "HV091"	: "fdw_invalid_descriptor_field_identifier";
			case "HV00B"	: "fdw_invalid_handle";
			case "HV00C"	: "fdw_invalid_option_index";
			case "HV00D"	: "fdw_invalid_option_name";
			case "HV090"	: "fdw_invalid_string_length_or_buffer_length";
			case "HV00A"	: "fdw_invalid_string_format";
			case "HV009"	: "fdw_invalid_use_of_null_pointer";
			case "HV014"	: "fdw_too_many_handles";
			case "HV001"	: "fdw_out_of_memory";
			case "HV00P"	: "fdw_no_schemas";
			case "HV00J"	: "fdw_option_name_not_found";
			case "HV00K"	: "fdw_reply_handle";
			case "HV00Q"	: "fdw_schema_not_found";
			case "HV00R"	: "fdw_table_not_found";
			case "HV00L"	: "fdw_unable_to_create_execution";
			case "HV00M"	: "fdw_unable_to_create_reply";
			case "HV00N"	: "fdw_unable_to_establish_connection";
							  //Class P0 — PL/pgSQL Error
			case "P0000"	: "plpgsql_error";
			case "P0001"	: "raise_exception";
			case "P0002"	: "no_data_found";
			case "P0003"	: "too_many_rows";
							  //Class XX — Internal Error
			case "XX000"	: "internal_error";
			case "XX001"	: "data_corrupted";
			case "XX002"	: "index_corrupted";
			default : "unknown_code";
		}
	}
}
