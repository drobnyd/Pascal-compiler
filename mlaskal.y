%language "c++"
%require "3.0.4"
%defines
%define parser_class_name{ mlaskal_parser }
%define api.token.constructor
%define api.token.prefix{DUTOK_}
%define api.value.type variant
%define parse.assert
%define parse.error verbose

%locations
%define api.location.type{ unsigned }

%code requires
{
	// this code is emitted to du3g.hpp

		// allow references to semantic types in %type
#include "du5sem.hpp"

	// allow references to semantic types in %type
#include "dutables.hpp"

	// avoid no-case warnings when compiling du3g.hpp
#pragma warning (disable:4065)

// adjust YYLLOC_DEFAULT macro for our api.location.type
#define YYLLOC_DEFAULT(res,rhs,N)	(res = (N)?YYRHSLOC(rhs, 1):YYRHSLOC(rhs, 0))
// supply missing YY_NULL in bfexpg.hpp
#define YY_NULL	0
#define YY_NULLPTR	0
}

%param{ mlc::yyscan_t2 yyscanner }	// formal name "yyscanner" is enforced by flex
%param{ mlc::MlaskalCtx* ctx }

%start mlaskal
// why? %locations

%code
{
	// this code is emitted to du3g.cpp

	// declare yylex here
	#include "bisonflex.hpp"
	YY_DECL;

	// allow access to context
	#include "dutables.hpp"

	// other user-required contents
	#include <assert.h>
	#include <stdlib.h>

    /* local stuff */
    using namespace mlc;

}

%token EOF	0	"end of file"

%token PROGRAM			/* program */
%token LABEL			    /* label */
%token CONST			    /* const */
%token TYPE			    /* type */
%token VAR			    /* var */
%token BEGIN			    /* begin */
%token END			    /* end */
%token PROCEDURE			/* procedure */
%token FUNCTION			/* function */
%token ARRAY			    /* array */
%token OF				    /* of */
%token GOTO			    /* goto */
%token IF				    /* if */
%token THEN			    /* then */
%token ELSE			    /* else */
%token WHILE			    /* while */
%token DO				    /* do */
%token REPEAT			    /* repeat */
%token UNTIL			    /* until */
%token FOR			    /* for */
%token OR				    /* or */
%token NOT			    /* not */
%token RECORD			    /* record */

/* literals */
%token<mlc::ls_id_index> IDENTIFIER			/* identifier */
%token<mlc::ls_int_index> UINT			    /* unsigned integer */
%token<mlc::ls_real_index> REAL			    /* real number */
%token<mlc::ls_str_index> STRING			    /* string */

/* delimiters */
%token SEMICOLON			/* ; */
%token DOT			    /* . */
%token COMMA			    /* , */
%token EQ				    /* = */
%token COLON			    /* : */
%token LPAR			    /* ( */
%token RPAR			    /* ) */
%token DOTDOT			    /* .. */
%token LSBRA			    /* [ */
%token RSBRA			    /* ] */
%token ASSIGN			    /* := */

/* grouped operators and keywords */
%token<mlc::DUTOKGE_OPER_REL> OPER_REL			    /* <, <=, <>, >=, > */
%token<mlc::DUTOKGE_OPER_SIGNADD> OPER_SIGNADD		    /* +, - */
%token<mlc::DUTOKGE_OPER_MUL> OPER_MUL			    /* *, /, div, mod, and */
%token<mlc::DUTOKGE_FOR_DIRECTION> FOR_DIRECTION		    /* to, downto */

%type<mlc::parameter_list_ptr> formal_params
%type<mlc::type_pointer> type_ID
%type<mlc::type_pointer> structured_type_notID
%type<mlc::field_list_ptr> field_list
%type<std::vector<mlc::var_decl>> ident_comma_loop
%type<std::vector<mlc::ls_id_index>> variable
%type<mlc::expr_result> expression_ID
%type<mlc::expr_result> simp_expr_ID
%type<mlc::expr_result> simp_expr_ID_loop
%type<mlc::expr_result> term_ID
%type<mlc::expr_result> factor_ID
%type<std::vector<mlc::expr_result>> real_params_ID
%type<mlc::icblock_pointer> statement_semicolon_loop_e
%type<mlc::icblock_pointer> statement_e
%type<mlc::icblock_pointer> stmt_e
%type<mlc::icblock_pointer> block
%type<mlc::icblock_pointer> blockP

%%

mlaskal:		PROGRAM
		    		IDENTIFIER
		    		SEMICOLON
						blockP
						DOT {ctx->tab->set_main_code($2,$4);}
						;

blockP:		label_block_e
					const_block_e
					type_block_e
					var_block_e
					procedure_function_block_e
					BEGIN
					statement_semicolon_loop_e
					END {$$ = $7;}
					;

label_block_e:	/*empty*/
										|	LABEL
											uint_comma_loop
											SEMICOLON
											;
uint_comma_loop:    UINT {ctx->tab->add_label_entry(@1,$1,ctx->tab->new_label());}
									| UINT
										COMMA
										uint_comma_loop {ctx->tab->add_label_entry(@1,$1,ctx->tab->new_label());}
										;

const_block_e: /*empty*/
						 | CONST
							 const_block_loop
							 ;

const_block_loop: IDENTIFIER
							 		EQ
							 		IDENTIFIER  /*constant_ID ID */
							 		SEMICOLON  	{auto copy = var_decl($1,@1);auto orig = var_decl($3,@3);create_const_copy(copy,orig,ctx->tab);}
								| IDENTIFIER
									EQ
									IDENTIFIER
									SEMICOLON /*constant_ID ID */
									const_block_loop {auto copy = var_decl($1,@1);auto orig = var_decl($3,@3);create_const_copy(copy,orig,ctx->tab);}
								| IDENTIFIER
							 		EQ
							 		REAL
							 		SEMICOLON {ctx->tab->add_const_real(@1, $1, $3);}
								| IDENTIFIER
									EQ
									REAL
									SEMICOLON
									const_block_loop {ctx->tab->add_const_real(@1, $1, $3);}
							 	| IDENTIFIER
									EQ
									UINT
									SEMICOLON {ctx->tab->add_const_int(@1, $1, $3);}
								| IDENTIFIER
									EQ
									UINT
									SEMICOLON
									const_block_loop {ctx->tab->add_const_int(@1, $1, $3);}
								| IDENTIFIER
									EQ
								  STRING
									SEMICOLON {ctx->tab->add_const_str(@1, $1, $3);}
								| IDENTIFIER
									EQ
									STRING
									SEMICOLON
									const_block_loop {ctx->tab->add_const_str(@1, $1, $3);}
								| IDENTIFIER
									EQ
									OPER_SIGNADD
									REAL
									SEMICOLON {if ($3 == mlc::DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS){
																auto nval = ctx->tab->ls_real().add( - *$4);
  															ctx->tab->add_const_real( @1, $1, nval);}
														else {ctx->tab->add_const_real( @1, $1, $4);}
														}
								| IDENTIFIER
									EQ
									OPER_SIGNADD
									REAL
									SEMICOLON
									const_block_loop {if ($3 == mlc::DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS){
																			auto nval = ctx->tab->ls_real().add( - *$4);
			  															ctx->tab->add_const_real( @1, $1, nval);}
																	else {ctx->tab->add_const_real( @1, $1, $4);}
																	}
								| IDENTIFIER
									EQ
									OPER_SIGNADD
									UINT
									SEMICOLON {if ($3 == mlc::DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS){
																auto nval = ctx->tab->ls_int().add( - *$4);
  															ctx->tab->add_const_int( @1, $1, nval);}
														else {ctx->tab->add_const_int( @1, $1, $4);}
														}
								| IDENTIFIER
									EQ
									OPER_SIGNADD
									UINT
									SEMICOLON
									const_block_loop {if ($3 == mlc::DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS){
																			auto nval = ctx->tab->ls_int().add( - *$4);
			  															ctx->tab->add_const_int( @1, $1, nval);}
																		else {ctx->tab->add_const_int( @1, $1, $4);}
																	}
									;

type_block_e: /*empty*/
						| TYPE
							type_block_loop
							;

type_block_loop:  add_type
							 	|	add_type
									type_block_loop
							 		;

add_type: IDENTIFIER
					EQ
					type_ID
					SEMICOLON { ctx->tab->add_type(@1, $1, $3); }

var_block_e: /*empty*/
					 |  VAR
							var_block_loop
							;

var_block_loop: 	  ident_comma_loop
										COLON
										type_ID
										SEMICOLON {add_variables($1,$3,ctx->tab);}
									|	var_block_loop
									  ident_comma_loop
									  COLON
										type_ID
										SEMICOLON {add_variables($2,$4,ctx->tab);}
										;

ident_comma_loop:   IDENTIFIER {auto var = var_decl($1,@1); $$.push_back(var);}
													| IDENTIFIER
													  COMMA
														ident_comma_loop {$$ = std::move($3); auto var = var_decl($1,@1); $$.push_back(var);}
														;

procedure_function_block_e: /*empty*/
													| proc_func_body
														procedure_function_block_e
														;

proc_func_body: procedure_header
								block
								SEMICOLON {ctx->tab->set_subprogram_code(ctx->tab->my_function_name(),$2); ctx->tab->leave(@3);}
							| function_header
								block
								SEMICOLON {ctx->tab->set_subprogram_code(ctx->tab->my_function_name(),$2); ctx->tab->leave(@3);}
								;


block: label_block_e
			 const_block_e
			 type_block_e
			 var_block_e
			 BEGIN
			 statement_semicolon_loop_e
			 END { $$ = $6; }
			 ;



statement_semicolon_loop_e: statement_e { $$ = $1; }
													| statement_e
														SEMICOLON
														statement_semicolon_loop_e { $$ = icblock_merge_and_kill($1,$3);}
														;

procedure_header: PROCEDURE
									IDENTIFIER
									LPAR
									formal_params
									RPAR
									SEMICOLON {  ctx->tab->add_proc(@1,$2,$4); ctx->tab->enter(@1,$2); }
								| PROCEDURE
									IDENTIFIER
									SEMICOLON { ctx->tab->add_proc(@1,$2,create_parameter_list()); ctx->tab->enter(@1,$2);}
									;

function_header:  FUNCTION
									IDENTIFIER
									COLON
									IDENTIFIER /*scalar type ID*/
									SEMICOLON {create_and_add_fnc(var_decl($2,@1),create_parameter_list(),var_decl($4,@4),ctx->tab); ctx->tab->enter(@1,$2); }
								| FUNCTION
									IDENTIFIER
									LPAR
							 		formal_params
							 		RPAR
									COLON
									IDENTIFIER /*scalar type ID*/
									SEMICOLON { create_and_add_fnc(var_decl($2,@1),$4,var_decl($7,@7),ctx->tab); ctx->tab->enter(@1,$2);}
									;

formal_params:  ident_comma_loop
								COLON
								IDENTIFIER /*type ID*/ {$$ = create_value_params_list($1,var_decl($3,@1),ctx->tab);}
							| VAR
							  ident_comma_loop
								COLON
								IDENTIFIER /*type ID*/ {$$ = create_reference_params_list($2,var_decl($4,@1),ctx->tab);}
							| ident_comma_loop
								COLON
								IDENTIFIER /*type ID*/
							  SEMICOLON
								formal_params {
																auto params = create_value_params_list($1,var_decl($3,@1),ctx->tab);
																params->append_and_kill($5);
																$$ = params;
															}
							| VAR
								ident_comma_loop
								COLON
								IDENTIFIER /*type ID*/
							  SEMICOLON
								formal_params {
																auto params = create_reference_params_list($2,var_decl($4,@1),ctx->tab);
																params->append_and_kill($6);
																$$ = params;
															}
							 ;


type_ID: IDENTIFIER /* type ID, ordinal type ID, structured type ID*/ {$$ = get_type_pointer(var_decl($1,@1),ctx->tab);}
			| structured_type_notID {$$ = $1;}
				;


structured_type_notID:  RECORD
											  END {$$ = ctx->tab->create_record_type(create_field_list(),@1);}
											| RECORD
												field_list
												END {$$ = ctx->tab->create_record_type($2,@1);}
											| RECORD
												field_list
												SEMICOLON
												END {$$ = ctx->tab->create_record_type($2,@1);}
											 ;

field_list:	ident_comma_loop
						COLON
						type_ID { $$ = create_and_fill_field_list($1,$3);}
				 | 	field_list
				    SEMICOLON
				    ident_comma_loop
					 	COLON
					 	type_ID {
												mlc::field_list_ptr flptr = create_and_fill_field_list($3,$5);
												$1->append_and_kill(flptr);
												$$ = std::move($1);
											}
						;


statement_e:  stmt_e {$$ = std::move($1);}
						| UINT
							COLON
							stmt_e {$$ = std::move($3);}
						 ;


stmt_e: 			IDENTIFIER /*variable_ID ID, function ID*/ {$$ = std::move(check_and_create_procedure_call(var_decl($1,@1),std::vector<expr_result>(),ctx->tab));}
						| IDENTIFIER /*procedure ID*/
							LPAR
							real_params_ID
							RPAR { $$ = std::move(check_and_create_procedure_call(var_decl($1,@1),$3,ctx->tab));}
						|	variable /* record ID with field */
							ASSIGN
							expression_ID { auto rec = get_record_field($1,ctx->tab); $$ = std::move(assignment(var_decl(rec.id,@1),rec,$3,ctx->tab));}
						|	IDENTIFIER /*variable_ID ID, function ID*/
							ASSIGN
							expression_ID  { $$ = std::move(assignment(var_decl($1,@1),comp_var($1,ctx->tab),$3,ctx->tab));}
			 			| match_stmt_nonID_e { $$ = icblock_create();}
						| unmatch_stmt { $$ = icblock_create();}
							;

match_stmt_nonID_e: /*empty*/
					|	IF
						expression_ID /*boolean expression_ID*/
						THEN
						match_stmt_nonID_e
						ELSE
						match_stmt_nonID_e
					| GOTO
					  UINT
					| BEGIN
						statement_semicolon_loop_e
						END
					| WHILE
						expression_ID /*boolean expression_ID*/
						DO
						match_stmt_nonID_e
					| REPEAT
						statement_semicolon_loop_e
						UNTIL
						match_stmt_nonID_e
					| REPEAT
						statement_semicolon_loop_e
					  UNTIL
						expression_ID /*boolean expression ID*/
					| FOR
						IDENTIFIER /*ordinal type variable ID*/
						ASSIGN
						expression_ID /*ordinal expression ID*/
						FOR_DIRECTION
						expression_ID /*ordinal expression ID*/
						DO
						match_stmt_nonID_e
						;


unmatch_stmt: 	IF
								expression_ID /*boolean expression_ID*/
								THEN
								statement_e
							| IF
								expression_ID /*boolean expression_ID*/
								THEN
								match_stmt_nonID_e
								ELSE
								unmatch_stmt
							| WHILE
								expression_ID /*boolean expression_ID*/
								DO
								unmatch_stmt
							| REPEAT
								statement_semicolon_loop_e
								UNTIL
								unmatch_stmt
							| FOR
								IDENTIFIER /*ordinal type variable ID*/
								ASSIGN
								expression_ID /*ordinal expression ID*/
								FOR_DIRECTION
								expression_ID /*ordinal expression ID*/
								DO
								unmatch_stmt
								;


real_params_ID:	  expression_ID { $$.push_back($1);}
							 | 	real_params_ID
							  	COMMA
									expression_ID { $$ = std::move($1); $$.push_back($3);}
								 ;

expression_ID:  simp_expr_ID {$$ = std::move($1);}
							| simp_expr_ID
								OPER_REL
								simp_expr_ID {$$ = std::move(binary_oper_rel_op($1,$3,$2,@1)); }
							| simp_expr_ID
								EQ
								simp_expr_ID {$$ = std::move(binary_eq_op($1,$3,@1));}
								;

simp_expr_ID: simp_expr_ID_loop {$$ = std::move($1);}
					 |  OPER_SIGNADD
							simp_expr_ID_loop {if ($1 == mlc::DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS) $$ = std::move(unary_minus_op($2,@1));
																 else $$ = std::move($2);}
							;

simp_expr_ID_loop: term_ID {$$ = std::move($1);}
								 | simp_expr_ID_loop
								 	 OR
									 term_ID { $$ = std::move(binary_or_op($1,$3,@1));}
								 | simp_expr_ID_loop
								 	 OPER_SIGNADD
								 	 term_ID { $$ = std::move(binary_oper_signadd_op($1,$3,$2,@1));}
									 ;

term_ID:  factor_ID {$$ = std::move($1);}
				| term_ID
					OPER_MUL
					factor_ID { $$ = std::move(binary_oper_mul_op($1,$3,$2,@1));}
					;

factor_ID: // IDENTIFIER /*constant ID, variable_ID ID, function ID */
						variable  { auto rec = get_record_field($1,ctx->tab); $$ = std::move(get_variable(var_decl(rec.id,@1),rec,ctx->tab));}
					| IDENTIFIER   { $$ = std::move(get_variable(var_decl($1,@1),comp_var($1,ctx->tab),ctx->tab));}
					| UINT { $$ = std::move(get_int($1));}
					| REAL { $$ = std::move(get_real($1));}
					| STRING { $$ = std::move(get_string($1));}
					| IDENTIFIER /*function ID*/
						LPAR
						real_params_ID
						RPAR { $$ = std::move(check_and_create_function_call(var_decl($1,@1),$3,ctx->tab));}
					| LPAR
						expression_ID
						RPAR { $$ = std::move($2);}
					| NOT
					 	factor_ID { $$ = expr_result();}
					 	;

variable:  IDENTIFIER
					 DOT
					 IDENTIFIER { $$.push_back($1); $$.push_back($3);}
				 | variable
				 	 DOT
					 IDENTIFIER { $$ = std::move($1); $$.push_back($3);}
					 ;




%%


namespace yy {

	void mlaskal_parser::error(const location_type& l, const std::string& m)
	{
		message(DUERR_SYNTAX, l, m);
	}

}
