%{
	// this code is emitted into du2l.cpp
	// avoid macro redefinition warnings when compiling du2l.cpp
	#pragma warning (disable:4005)
	// avoid unreferenced parameter warnings when compiling du2l.cpp
	#pragma warning (disable:4100)
	// avoid unreferenced function warnings when compiling du2l.cpp
	#pragma warning (disable:4505)

	// allow access to YY_DECL macro
	#include "bisonflex.hpp"

	// allow access to context
	#include "du5g.hpp"

	const char distance_lu = 'A' - 'a';
	const std::uint_least32_t max_uint = 2147483647;
	const std::uint_least32_t mask_to31uint = 0x7FFFFFFF;


	bool need_trunc_uint(const std::string &from, std::uint_least32_t &result) {
		bool need_trunc = false;
		result = 0;
		std::uint_least32_t lastres = 0;
		for (size_t i = 0; i < from.size(); i++)
		{
			result = result * 10 + (from[i] - '0');
			if (result < lastres) need_trunc = true;
			lastres = result;
		}
		if (result > max_uint)
		{
			result = (result & mask_to31uint);
			return true;
		}
		return need_trunc;
	}


	std::string malformeduint_touint(const char* to_int, size_t to_int_length) {
		std::string res;
		size_t i = 0;
		while (i < to_int_length && isdigit(to_int[i]))
		{
			res += to_int[i];
			i++;
		}
		return res;
	}


	bool is_lowercase_char(char c){
	return (c >= 'a' && c <= 'z') ;
	}

	void to_upper(char* ident) {
		for (size_t i = 0; ident[i] != '\0'; i++){
			if(is_lowercase_char(ident[i])) {
			ident[i] = ident[i] + distance_lu;
			}
		}
	}

%}

/* DO NOT TOUCH THIS OPTIONS! */
%option noyywrap nounput batch noinput stack reentrant
%option never-interactive

WHITESPACE[ \r\t\f]
NEWLINE[\n]
DIGIT[0-9]
LETTER[a-zA-Z]
UINT{DIGIT}+
EXP[Ee][+-]?{UINT}
MALFEXP[Ee][+-]?{MALFUINT}
UREAL{UINT}(((\.{UINT}){EXP}?)|{EXP})
MALFUINT{UINT}{LETTER}({LETTER}|{UINT})*
MALFUREAL{UREAL}{LETTER}({LETTER}|{DIGIT})*

%x STRING
%x COMMENT

%%

%{
	typedef yy::mlaskal_parser parser;
	std::uint_least8_t comment_level = 0;
	std::string curr_string;

%}

<COMMENT,INITIAL>{NEWLINE}	ctx->curline++;

'	{
	curr_string.clear();
	BEGIN(STRING);
	}

\{	{
	comment_level = 1;
	BEGIN(COMMENT);
	}

\}	{
		message(mlc::DUERR_UNEXPENDCMT,ctx->curline);
	}

<STRING>''	curr_string += "\'";

<STRING>'	{
	BEGIN(INITIAL);
	return parser::make_STRING(mlc::ls_str_index(ctx->tab->ls_str().add(curr_string)),ctx->curline);
	}

<STRING><<EOF>>	{
				BEGIN(INITIAL);
				message(mlc::DUERR_EOFINSTRCHR,ctx->curline);
				return parser::make_STRING(ctx->tab->ls_str().add(curr_string),ctx->curline);
				}

<STRING>{NEWLINE}	{
					BEGIN(INITIAL);
					message(mlc::DUERR_EOLINSTRCHR,ctx->curline);
					return parser::make_STRING(ctx->tab->ls_str().add(curr_string),ctx->curline++);
					}
<STRING>.	curr_string += yytext;

<COMMENT>\{	++comment_level;

<COMMENT>\}	{
			if (--comment_level == 0){
				BEGIN(INITIAL);
				}
			}

<COMMENT><<EOF>> {
	message(mlc::DUERR_EOFINCMT,ctx->curline);
	BEGIN(INITIAL);
	}

<COMMENT>.

{WHITESPACE}+		/* go out with whitespaces */

<<EOF>>  return parser::make_EOF(ctx->curline);

  /* program */
[Pp][Rr][Oo][Gg][Rr][Aa][Mm]	return parser::make_PROGRAM(ctx->curline);
  /* label */
[Ll][Aa][Bb][Ee][Ll]			return parser::make_LABEL(ctx->curline);
  /* const */
[Cc][Oo][Nn][Ss][Tt]			return parser::make_CONST(ctx->curline);
 /* type */
[Tt][Yy][Pp][Ee]				return parser::make_TYPE(ctx->curline);
 /* var */
[Vv][Aa][Rr]					return parser::make_VAR(ctx->curline);
  /* begin */
[Bb][Ee][Gg][Ii][Nn]			return parser::make_BEGIN(ctx->curline);
 /* end */
[Ee][Nn][Dd]					return parser::make_END(ctx->curline);
 /* procedure */
[Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee] return parser::make_PROCEDURE(ctx->curline);
 /* function */
[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn] return parser::make_FUNCTION(ctx->curline);
 /* array */
[Aa][Rr][Rr][Aa][Yy]			return parser::make_ARRAY(ctx->curline);
 /* of */
[Oo][Ff]						return parser::make_OF(ctx->curline);
 /* goto */
[Gg][Oo][Tt][Oo]				return parser::make_GOTO(ctx->curline);
 /* if */
[Ii][Ff]						return parser::make_IF(ctx->curline);
 /* then */
[Tt][Hh][Ee][Nn]				return parser::make_THEN(ctx->curline);
 /* else */
[Ee][Ll][Ss][Ee]				return parser::make_ELSE(ctx->curline);
 /* while */
[Ww][Hh][Ii][Ll][Ee]			return parser::make_WHILE(ctx->curline);
 /* do */
[Dd][Oo]						return parser::make_DO(ctx->curline);
 /* repeat */
[Rr][Ee][Pp][Ee][Aa][Tt]		return parser::make_REPEAT(ctx->curline);
 /* until */
[Uu][Nn][Tt][Ii][Ll]			return parser::make_UNTIL(ctx->curline);
 /* for */
[Ff][Oo][Rr]					return parser::make_FOR(ctx->curline);
 /* or */
[Oo][Rr]						return parser::make_OR(ctx->curline);
 /* not */
[Nn][Oo][Tt]					return parser::make_NOT(ctx->curline);
 /* record */
[Rr][Ee][Cc][Oo][Rr][Dd]		return parser::make_RECORD(ctx->curline);
 /* delimiters */
 /* ; */
;				return parser::make_SEMICOLON(ctx->curline);
  /* .. */
\.\.			return parser::make_DOTDOT(ctx->curline);
 /* . */
\.				return parser::make_DOT(ctx->curline);
 /* , */
,				return parser::make_COMMA(ctx->curline);
 /* = */
=				return parser::make_EQ(ctx->curline);
 /* : */
:				return parser::make_COLON(ctx->curline);
  /* ( */
\(				return parser::make_LPAR(ctx->curline);
 /* ) */
\)				return parser::make_RPAR(ctx->curline);
  /* [ */
\[				return parser::make_LSBRA(ctx->curline);
 /* ] */
\]				return parser::make_RSBRA(ctx->curline);
 /* := */
:=				return parser::make_ASSIGN(ctx->curline);
 /* grouped operators and keywords */
  /* <, <=, <>, >=, > */
\<				return parser::make_OPER_REL(mlc::DUTOKGE_OPER_REL::DUTOKGE_LT,ctx->curline);
\>				return parser::make_OPER_REL(mlc::DUTOKGE_OPER_REL::DUTOKGE_GT,ctx->curline);
\<\>			return parser::make_OPER_REL(mlc::DUTOKGE_OPER_REL::DUTOKGE_NE,ctx->curline);
\<=				return parser::make_OPER_REL(mlc::DUTOKGE_OPER_REL::DUTOKGE_LE,ctx->curline);
\>=				return parser::make_OPER_REL(mlc::DUTOKGE_OPER_REL::DUTOKGE_GE,ctx->curline);
  /* +, - */
\+				return parser::make_OPER_SIGNADD(mlc::DUTOKGE_OPER_SIGNADD::DUTOKGE_PLUS,ctx->curline);
-				return parser::make_OPER_SIGNADD(mlc::DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS,ctx->curline);
 /* *, /, div, mod, and */
\*				return parser::make_OPER_MUL(mlc::DUTOKGE_OPER_MUL::DUTOKGE_ASTERISK,ctx->curline);
\/				return parser::make_OPER_MUL(mlc::DUTOKGE_OPER_MUL::DUTOKGE_SOLIDUS,ctx->curline);
[Dd][Ii][Vv]	return parser::make_OPER_MUL(mlc::DUTOKGE_OPER_MUL::DUTOKGE_DIV,ctx->curline);
[Mm][Oo][Dd]	return parser::make_OPER_MUL(mlc::DUTOKGE_OPER_MUL::DUTOKGE_MOD,ctx->curline);
[Aa][Nn][Dd]	return parser::make_OPER_MUL(mlc::DUTOKGE_OPER_MUL::DUTOKGE_AND,ctx->curline);
 /* to, downto */
[Tt][Oo]		return parser::make_FOR_DIRECTION(mlc::DUTOKGE_FOR_DIRECTION::DUTOKGE_TO,ctx->curline);
[Dd][Oo][Ww][Nn][Tt][Oo] return parser::make_FOR_DIRECTION(mlc::DUTOKGE_FOR_DIRECTION::DUTOKGE_DOWNTO,ctx->curline);

 /* literals */


 /*ureal*/
{UREAL}	{
		double res;
		try {
		res = std::stod(yytext,0);
		} catch (const std::out_of_range&) {
			message(mlc::DUERR_REALOUTRANGE,ctx->curline,yytext);
		}
		return parser::make_REAL(ctx->tab->ls_real().add(res), ctx->curline);
	}

{MALFUREAL} {
		message(mlc::DUERR_BADREAL,ctx->curline,yytext);
		double res;
		try {
		 res = std::stod(yytext,0);
		} catch (const std::out_of_range&) {
			message(mlc::DUERR_REALOUTRANGE,ctx->curline,yytext);
		}
		return parser::make_REAL(ctx->tab->ls_real().add(res), ctx->curline);
	}

	/* non-terminating states */
{UINT}\.{DIGIT}+[Ee][+-]? {
		message(mlc::DUERR_BADREAL,ctx->curline,yytext);
		double res;
		try {
		 res = std::stod(yytext,0);
		} catch (const std::out_of_range&) {
			message(mlc::DUERR_REALOUTRANGE,ctx->curline,yytext);
		}
		return parser::make_REAL(ctx->tab->ls_real().add(res), ctx->curline);
	}
{UINT}[Ee][+-]? {
		message(mlc::DUERR_BADREAL,ctx->curline,yytext);
		double res;
		try {
		 res = std::stod(yytext,0);
		} catch (const std::out_of_range&) {
			message(mlc::DUERR_REALOUTRANGE,ctx->curline,yytext);
		}
		return parser::make_REAL(ctx->tab->ls_real().add(res), ctx->curline);
	}

	/* if there are two dots, it should match rule lower, since it searches longest sequence */
{UINT}\. {
		message(mlc::DUERR_BADREAL,ctx->curline,yytext);
		double res;
		try {
		res = std::stod(yytext,0);
		} catch (const std::out_of_range&) {
			message(mlc::DUERR_REALOUTRANGE,ctx->curline,yytext);
		}
		return parser::make_REAL(ctx->tab->ls_real().add(res), ctx->curline);
	}

	/* identifier */
{LETTER}({LETTER}|{DIGIT})* {
	to_upper(yytext);
 	return parser::make_IDENTIFIER(mlc::ls_id_index(ctx->tab->ls_id().add(yytext)), ctx->curline);
	}

{UINT}\.\. {
	std::uint_least32_t truncated = 0;
	/* only digits are taken into account */
	std::string to_int;
	for (size_t i = 0; i < yyleng - 2; i++){
		to_int += yytext[i];
	}
	if (need_trunc_uint(to_int, truncated)){
		message(mlc::DUERR_INTOUTRANGE,ctx->curline,yytext);
		}
	yyless(yyleng - 2);
	return parser::make_UINT(ctx->tab->ls_int().add(truncated), ctx->curline);
	}

{MALFUINT}\.\. {
		size_t malleng = yyleng - 2;
		std::string malf_int;
		for (size_t i = 0; i < yyleng - 2; i++){
			malf_int += yytext[i];
		}
		message(mlc::DUERR_BADINT,ctx->curline,malf_int);
		std::string to_uint = malformeduint_touint(yytext, malleng);
		std::uint_least32_t truncated = 0;
		if (need_trunc_uint(to_uint, truncated)) {
			message(mlc::DUERR_INTOUTRANGE, ctx->curline, to_uint);
		}
		yyless(yyleng - 2);
		return parser::make_UINT(ctx->tab->ls_int().add(truncated), ctx->curline);
	}

  /* unsigned integer */
{UINT} {
	std::uint_least32_t truncated = 0;
	std::string to_int(yytext);
	if (need_trunc_uint(to_int, truncated)){
		message(mlc::DUERR_INTOUTRANGE,ctx->curline,yytext);
		}
	return parser::make_UINT(ctx->tab->ls_int().add(truncated), ctx->curline);
	}

{MALFUINT} {
		message(mlc::DUERR_BADINT,ctx->curline,yytext);
		std::string to_uint = malformeduint_touint(yytext, yyleng);
		std::uint_least32_t truncated = 0;
		if (need_trunc_uint(to_uint, truncated)) {
			message(mlc::DUERR_INTOUTRANGE, ctx->curline, to_uint);
		}
		return parser::make_UINT(ctx->tab->ls_int().add(truncated), ctx->curline);
	}

.			message(mlc::DUERR_UNKCHAR, ctx->curline, *yytext, *yytext);

%%

namespace mlc {

	yyscan_t2 lexer_init(FILE * iff)
	{
		yyscan_t2 scanner;
		yylex_init(&scanner);
		yyset_in(iff, scanner);
		return scanner;
	}

	void lexer_shutdown(yyscan_t2 scanner)
	{
		yyset_in(nullptr, scanner);
		yylex_destroy(scanner);
	}

}
