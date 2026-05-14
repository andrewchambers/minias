/*
  ----------------------------------------------------------------
  | This file is a generated redistributable version of minipeg. |
  |   See https://ach.srht.site/minipeg/ for more information.   |
  ----------------------------------------------------------------
  Copyright (c) 2007-2013, Ian Piumarta
  Copyright (c) 2022, Andrew Chambers
  All rights reserved.
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the 'Software'), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, and/or sell copies of the
  Software, and to permit persons to whom the Software is furnished to do so,
  provided that the above copyright notice(s) and this permission notice appear
  in all copies or substantial portions of the Software.  Inclusion of the
  above copyright notice(s) and this permission notice in supporting
  documentation would be appreciated but is not required.
  
  THE SOFTWARE IS PROVIDED 'AS IS'. USE ENTIRELY AT YOUR OWN RISK.

*/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#line 1 "version.h"
#define MINIPEG_VERSION "73492bd"
#line 1 "tree.h"

enum { Unknown= 0, Rule, Variable, Name, Dot, Character, String, Class, Action, Inline, Predicate, Error, Alternate, Sequence, PeekFor, PeekNot, Query, Star, Plus };

enum {
  RuleUsed	= 1<<0,
  RuleReached	= 1<<1,
};

typedef union Node Node;

struct Rule	 { int type;  Node *next;   char *name;	 Node *variables;  Node *expression;  int id;  int flags;	};
struct Variable	 { int type;  Node *next;   char *name;  Node *value;  int offset;					};
struct Name	 { int type;  Node *next;   Node *rule;  Node *variable;						};
struct Dot	 { int type;  Node *next;										};
struct Character { int type;  Node *next;   char *value;								};
struct String	 { int type;  Node *next;   char *value;								};
struct Class	 { int type;  Node *next;   unsigned char *value;							};
struct Action	 { int type;  Node *next;   char *text;	  Node *list;  char *name;  Node *rule;  int line;		};
struct Inline    { int type;  Node *next;   char *text;									};
struct Predicate { int type;  Node *next;   char *text;									};
struct Error	 { int type;  Node *next;   Node *element;  char *text;							};
struct Alternate { int type;  Node *next;   Node *first;  Node *last;							};
struct Sequence	 { int type;  Node *next;   Node *first;  Node *last;							};
struct PeekFor	 { int type;  Node *next;   Node *element;								};
struct PeekNot	 { int type;  Node *next;   Node *element;								};
struct Query	 { int type;  Node *next;   Node *element;								};
struct Star	 { int type;  Node *next;   Node *element;								};
struct Plus	 { int type;  Node *next;   Node *element;								};
struct Any	 { int type;  Node *next;										};

union Node
{
  int			type;
  struct Rule		rule;
  struct Variable	variable;
  struct Name		name;
  struct Dot		dot;
  struct Character	character;
  struct String		string;
  struct Class		cclass;
  struct Action		action;
  struct Inline		inLine;
  struct Predicate	predicate;
  struct Error		error;
  struct Alternate	alternate;
  struct Sequence	sequence;
  struct PeekFor	peekFor;
  struct PeekNot	peekNot;
  struct Query		query;
  struct Star		star;
  struct Plus		plus;
  struct Any		any;
};

extern Node *actions;
extern Node *rules;
extern Node *start;

extern int   ruleCount;

extern FILE *output;

extern Node *makeRule(char *name);
extern Node *findRule(char *name);
extern Node *beginRule(Node *rule);
extern void  Rule_setExpression(Node *rule, Node *expression);
extern Node *Rule_beToken(Node *rule);
extern Node *makeVariable(char *name);
extern Node *makeName(Node *rule);
extern Node *makeDot(void);
extern Node *makeCharacter(char *text);
extern Node *makeString(char *text);
extern Node *makeClass(char *text);
extern Node *makeAction(int lineNumber, char *text);
extern Node *makeInline(char *text);
extern Node *makePredicate(char *text);
extern Node *makeError(Node *e, char *text);
extern Node *makeAlternate(Node *e);
extern Node *Alternate_append(Node *e, Node *f);
extern Node *makeSequence(Node *e);
extern Node *Sequence_append(Node *e, Node *f);
extern Node *makePeekFor(Node *e);
extern Node *makePeekNot(Node *e);
extern Node *makeQuery(Node *e);
extern Node *makeStar(Node *e);
extern Node *makePlus(Node *e);
extern Node *push(Node *node);
extern Node *top(void);
extern Node *pop(void);

extern void  Rule_compile_c_header(void);
extern void  Rule_compile_c(Node *node, int nolines);

extern void  Node_print(Node *node);
extern void  Rule_print(Node *node);
#line 1 "compile.c"

#ifdef WIN32
# undef inline
# define inline __inline
#endif


static int yyl(void)
{
  static int prev= 0;
  return ++prev;
}

static void charClassSet  (unsigned char bits[], int c)	{ bits[c >> 3] |=  (1 << (c & 7)); }
static void charClassClear(unsigned char bits[], int c)	{ bits[c >> 3] &= ~(1 << (c & 7)); }

typedef void (*setter)(unsigned char bits[], int c);

static inline int oigit(int c)	{ return ('0' <= c && c <= '7'); }
static inline int higit(int c)	{ return ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'); }

static inline int hexval(int c)
{
    if ('0' <= c && c <= '9') return c - '0';
    if ('A' <= c && c <= 'F') return 10 - 'A' + c;
    if ('a' <= c && c <= 'f') return 10 - 'a' + c;
    return 0;
}

static int cnext(unsigned char **ccp)
{
    unsigned char *cclass= *ccp;
    int c= *cclass++;
    if (c)
    {
	if ('\\' == c && *cclass)
	{
	    switch (c= *cclass++)
	    {
		case 'a':  c= '\a';   break;	/* bel */
		case 'b':  c= '\b';   break;	/* bs */
		case 'e':  c= '\033'; break;	/* esc */
		case 'f':  c= '\f';   break;	/* ff */
		case 'n':  c= '\n';   break;	/* nl */
		case 'r':  c= '\r';   break;	/* cr */
		case 't':  c= '\t';   break;	/* ht */
		case 'v':  c= '\v';   break;	/* vt */
		case 'x':
		    c= 0;
		    if (higit(*cclass)) c= (c << 4) + hexval(*cclass++);
		    if (higit(*cclass)) c= (c << 4) + hexval(*cclass++);
		    break;
		default:
		    if (oigit(c))
		    {
			c -= '0';
			if (oigit(*cclass)) c= (c << 3) + *cclass++ - '0';
			if (oigit(*cclass)) c= (c << 3) + *cclass++ - '0';
		    }
		    break;
	    }
	}
	*ccp= cclass;
    }
    return c;
}

static char *makeCharClass(unsigned char *cclass)
{
  unsigned char	 bits[32];
  setter	 set;
  int		 c, prev= -1;
  static char	 string[256];
  char		*ptr;

  if ('^' == *cclass)
    {
      memset(bits, 255, 32);
      set= charClassClear;
      ++cclass;
    }
  else
    {
      memset(bits, 0, 32);
      set= charClassSet;
    }

  while (*cclass)
    {
      if ('-' == *cclass && cclass[1] && prev >= 0)
	{
	  ++cclass;
	  for (c= cnext(&cclass);  prev <= c;  ++prev)
	    set(bits, prev);
	  prev= -1;
	}
      else
	{
	  c= cnext(&cclass);
	  set(bits, prev= c);
	}
    }

  ptr= string;
  for (c= 0;  c < 32;  ++c)
    ptr += sprintf(ptr, "\\%03o", bits[c]);

  return string;
}

static void begin(void)		{ fprintf(output, "\n  {"); }
static void end(void)		{ fprintf(output, "\n  }"); }
static void label(int n)	{ fprintf(output, "\n  l%d:;\t", n); }
static void jump(int n)		{ fprintf(output, "  goto l%d;", n); }
static void save(int n)		{ fprintf(output, "  int yypos%d= yy->__pos, yythunkpos%d= yy->__thunkpos;", n, n); }
static void restore(int n)	{ fprintf(output, "  yy->__pos= yypos%d; yy->__thunkpos= yythunkpos%d;", n, n); }

static void Node_compile_c_ko(Node *node, int ko)
{
  assert(node);
  switch (node->type)
    {
    case Rule:
      fprintf(stderr, "\ninternal error #1 (%s)\n", node->rule.name);
      exit(1);
      break;

    case Dot:
      fprintf(output, "  if (!yymatchDot(yy)) goto l%d;", ko);
      break;

    case Name:
      fprintf(output, "  if (!yy_%s(yy)) goto l%d;", node->name.rule->rule.name, ko);
      if (node->name.variable)
	fprintf(output, "  yyDo(yy, yySet, %d, 0);", node->name.variable->variable.offset);
      break;

    case Character:
    case String:
      {
	int len= strlen(node->string.value);
	if (1 == len)
	  {
	    if ('\'' == node->string.value[0])
	      fprintf(output, "  if (!yymatchChar(yy, '\\'')) goto l%d;", ko);
	    else
	      fprintf(output, "  if (!yymatchChar(yy, '%s')) goto l%d;", node->string.value, ko);
	  }
	else
	  if (2 == len && '\\' == node->string.value[0])
	    fprintf(output, "  if (!yymatchChar(yy, '%s')) goto l%d;", node->string.value, ko);
	  else
	    fprintf(output, "  if (!yymatchString(yy, \"%s\")) goto l%d;", node->string.value, ko);
      }
      break;

    case Class:
      fprintf(output, "  if (!yymatchClass(yy, (unsigned char *)\"%s\")) goto l%d;", makeCharClass(node->cclass.value), ko);
      break;

    case Action:
      fprintf(output, "  yyDo(yy, yy%s, yy->__begin, yy->__end);", node->action.name);
      break;

    case Inline:
      fprintf(output, "  yyText(yy, yy->__begin, yy->__end);\n");
      fprintf(output, "#define yytext yy->__text\n");
      fprintf(output, "#define yyleng yy->__textlen\n");
      fprintf(output, "%s;\n", node->inLine.text);
      fprintf(output, "#undef yytext\n");
      fprintf(output, "#undef yyleng\n");
      break;

    case Predicate:
      fprintf(output, "  yyText(yy, yy->__begin, yy->__end);  {\n");
      fprintf(output, "#define yytext yy->__text\n");
      fprintf(output, "#define yyleng yy->__textlen\n");
      fprintf(output, "if (!(%s)) goto l%d;\n", node->predicate.text, ko);
      fprintf(output, "#undef yytext\n");
      fprintf(output, "#undef yyleng\n");
      fprintf(output, "  }");
      break;

    case Error:
      {
	int eok= yyl(), eko= yyl();
	Node_compile_c_ko(node->error.element, eko);
	jump(eok);
	label(eko);
	fprintf(output, "  yyText(yy, yy->__begin, yy->__end);  {\n");
	fprintf(output, "#define yytext yy->__text\n");
	fprintf(output, "#define yyleng yy->__textlen\n");
	fprintf(output, "  %s;\n", node->error.text);
	fprintf(output, "#undef yytext\n");
	fprintf(output, "#undef yyleng\n");
	fprintf(output, "  }");
	jump(ko);
	label(eok);
      }
      break;

    case Alternate:
      {
	int ok= yyl();
	begin();
	save(ok);
	for (node= node->alternate.first;  node;  node= node->alternate.next)
	  if (node->alternate.next)
	    {
	      int next= yyl();
	      Node_compile_c_ko(node, next);
	      jump(ok);
	      label(next);
	      restore(ok);
	    }
	  else
	    Node_compile_c_ko(node, ko);
	end();
	label(ok);
      }
      break;

    case Sequence:
      for (node= node->sequence.first;  node;  node= node->sequence.next)
	Node_compile_c_ko(node, ko);
      break;

    case PeekFor:
      {
	int ok= yyl();
	begin();
	save(ok);
	Node_compile_c_ko(node->peekFor.element, ko);
	restore(ok);
	end();
      }
      break;

    case PeekNot:
      {
	int ok= yyl();
	begin();
	save(ok);
	Node_compile_c_ko(node->peekFor.element, ok);
	jump(ko);
	label(ok);
	restore(ok);
	end();
      }
      break;

    case Query:
      {
	int qko= yyl(), qok= yyl();
	begin();
	save(qko);
	Node_compile_c_ko(node->query.element, qko);
	jump(qok);
	label(qko);
	restore(qko);
	end();
	label(qok);
      }
      break;

    case Star:
      {
	int again= yyl(), out= yyl();
	label(again);
	begin();
	save(out);
	Node_compile_c_ko(node->star.element, out);
	jump(again);
	label(out);
	restore(out);
	end();
      }
      break;

    case Plus:
      {
	int again= yyl(), out= yyl();
	Node_compile_c_ko(node->plus.element, ko);
	label(again);
	begin();
	save(out);
	Node_compile_c_ko(node->plus.element, out);
	jump(again);
	label(out);
	restore(out);
	end();
      }
      break;

    default:
      fprintf(stderr, "\nNode_compile_c_ko: illegal node type %d\n", node->type);
      exit(1);
    }
}


static int countVariables(Node *node)
{
  int count= 0;
  while (node)
    {
      ++count;
      node= node->variable.next;
    }
  return count;
}

static void defineVariables(Node *node)
{
  int count= 0;
  while (node)
    {
      fprintf(output, "#define %s yy->__val[%d]\n", node->variable.name, --count);
      node->variable.offset= count;
      node= node->variable.next;
    }
  fprintf(output, "#define __ yy->__\n");
  fprintf(output, "#define yypos yy->__pos\n");
  fprintf(output, "#define yythunkpos yy->__thunkpos\n");
}

static void undefineVariables(Node *node)
{
  fprintf(output, "#undef yythunkpos\n");
  fprintf(output, "#undef yypos\n");
  while (node)
    {
      fprintf(output, "#undef %s\n", node->variable.name);
      node= node->variable.next;
    }
}


static void Rule_compile_c2(Node *node)
{
  assert(node);
  assert(Rule == node->type);

  if (!node->rule.expression)
    fprintf(stderr, "rule '%s' used but not defined\n", node->rule.name);
  else
    {
      int ko= yyl(), safe;

      if ((!(RuleUsed & node->rule.flags)) && (node != start))
	fprintf(stderr, "rule '%s' defined but not used\n", node->rule.name);

      safe= ((Query == node->rule.expression->type) || (Star == node->rule.expression->type));

      fprintf(output, "\nYY_RULE(int) yy_%s(yycontext *yy)\n{", node->rule.name);
      if (!safe) save(0);
      if (node->rule.variables)
	fprintf(output, "  yyDo(yy, yyPush, %d, 0);", countVariables(node->rule.variables));
      fprintf(output, "\n  yyprintf((stderr, \"%%s\\n\", \"%s\"));", node->rule.name);
      Node_compile_c_ko(node->rule.expression, ko);
      fprintf(output, "\n  yyprintf((stderr, \"  ok   %%s @ %%s\\n\", \"%s\", yy->__buf+yy->__pos));", node->rule.name);
      if (node->rule.variables)
	fprintf(output, "  yyDo(yy, yyPop, %d, 0);", countVariables(node->rule.variables));
      fprintf(output, "\n  return 1;");
      if (!safe)
	{
	  label(ko);
	  restore(0);
	  fprintf(output, "\n  yyprintf((stderr, \"  fail %%s @ %%s\\n\", \"%s\", yy->__buf+yy->__pos));", node->rule.name);
	  fprintf(output, "\n  return 0;");
	}
      fprintf(output, "\n}");
    }

  if (node->rule.next)
    Rule_compile_c2(node->rule.next);
}

static char *header= 
"#include <stdio.h>\n"
"#include <stdlib.h>\n"
"#include <string.h>\n";

static char *preamble= "\
#ifndef YY_MAYBE_UNUSED\n\
#ifdef __GNUC__\n\
#define YY_MAYBE_UNUSED __attribute__((unused))\n\
#else\n\
#define YY_MAYBE_UNUSED\n\
#endif\n\
#endif\n\
#ifndef YY_MALLOC\n\
#define YY_MALLOC(C, N)		malloc(N)\n\
#endif\n\
#ifndef YY_REALLOC\n\
#define YY_REALLOC(C, P, N)	realloc(P, N)\n\
#endif\n\
#ifndef YY_FREE\n\
#define YY_FREE(C, P)		free(P)\n\
#endif\n\
#ifndef YY_LOCAL\n\
#define YY_LOCAL(T)	static T\n\
#endif\n\
#ifndef YY_ACTION\n\
#define YY_ACTION(T)	static T\n\
#endif\n\
#ifndef YY_RULE\n\
#define YY_RULE(T)	static T\n\
#endif\n\
#ifndef YY_PARSE\n\
#define YY_PARSE(T)	T\n\
#endif\n\
#ifndef YYPARSE\n\
#define YYPARSE		yyparse\n\
#endif\n\
#ifndef YYPARSEFROM\n\
#define YYPARSEFROM	yyparsefrom\n\
#endif\n\
#ifndef YYRELEASE\n\
#define YYRELEASE	yyrelease\n\
#endif\n\
#ifndef YY_BEGIN\n\
#define YY_BEGIN	( yy->__begin= yy->__pos, 1)\n\
#endif\n\
#ifndef YY_END\n\
#define YY_END		( yy->__end= yy->__pos, 1)\n\
#endif\n\
#ifdef YY_DEBUG\n\
# define yyprintf(args)	fprintf args\n\
#else\n\
# define yyprintf(args)\n\
#endif\n\
#ifndef YYSTYPE\n\
#define YYSTYPE	int\n\
#endif\n\
#ifndef YY_STACK_SIZE\n\
#define YY_STACK_SIZE 128\n\
#endif\n\
\n\
#ifndef YY_BUFFER_SIZE\n\
#define YY_BUFFER_SIZE 1024\n\
#endif\n\
\n\
#ifndef YY_PART\n\
\n\
typedef void (*yyaction)(yycontext *yy, char *yytext, int yyleng);\n\
typedef struct _yythunk { int begin, end;  yyaction  action;  struct _yythunk *next; } yythunk;\n\
\n\
struct _yycontext {\n\
  char     *__buf;\n\
  int       __buflen;\n\
  int       __pos;\n\
  int       __limit;\n\
  char     *__text;\n\
  int       __textlen;\n\
  int       __begin;\n\
  int       __end;\n\
  int       __textmax;\n\
  yythunk  *__thunks;\n\
  int       __thunkslen;\n\
  int       __thunkpos;\n\
  YYSTYPE   __;\n\
  YYSTYPE  *__val;\n\
  YYSTYPE  *__vals;\n\
  int       __valslen;\n\
#ifdef YY_CTX_MEMBERS\n\
  YY_CTX_MEMBERS\n\
#endif\n\
};\n\
\n\
#ifdef YY_CTX_LOCAL\n\
#define YY_CTX_PARAM_	yycontext *yyctx,\n\
#define YY_CTX_PARAM	yycontext *yyctx\n\
#define YY_CTX_ARG_	yyctx,\n\
#define YY_CTX_ARG	yyctx\n\
#ifndef YY_INPUT\n\
#define YY_INPUT(yy, buf, result, max_size)		\\\n\
  {							\\\n\
    int yyc= getchar();					\\\n\
    result= (EOF == yyc) ? 0 : (*(buf)= yyc, 1);	\\\n\
    yyprintf((stderr, \"<%c>\", yyc));			\\\n\
  }\n\
#endif\n\
#else\n\
#define YY_CTX_PARAM_\n\
#define YY_CTX_PARAM\n\
#define YY_CTX_ARG_\n\
#define YY_CTX_ARG\n\
yycontext _yyctx= { 0, 0 };\n\
yycontext *yyctx= &_yyctx;\n\
#ifndef YY_INPUT\n\
#define YY_INPUT(buf, result, max_size)			\\\n\
  {							\\\n\
    int yyc= getchar();					\\\n\
    result= (EOF == yyc) ? 0 : (*(buf)= yyc, 1);	\\\n\
    yyprintf((stderr, \"<%c>\", yyc));			\\\n\
  }\n\
#endif\n\
#endif\n\
\n\
YY_LOCAL(int) YY_MAYBE_UNUSED yyrefill(yycontext *yy)\n\
{\n\
  int yyn;\n\
  while (yy->__buflen - yy->__pos < 512)\n\
    {\n\
      yy->__buflen *= 2;\n\
      yy->__buf= (char *)YY_REALLOC(yy, yy->__buf, yy->__buflen);\n\
    }\n\
#ifdef YY_CTX_LOCAL\n\
  YY_INPUT(yy, (yy->__buf + yy->__pos), yyn, (yy->__buflen - yy->__pos));\n\
#else\n\
  YY_INPUT((yy->__buf + yy->__pos), yyn, (yy->__buflen - yy->__pos));\n\
#endif\n\
  if (!yyn) return 0;\n\
  yy->__limit += yyn;\n\
  return 1;\n\
}\n\
\n\
YY_LOCAL(int) YY_MAYBE_UNUSED yymatchDot(yycontext *yy)\n\
{\n\
  if (yy->__pos >= yy->__limit && !yyrefill(yy)) return 0;\n\
  ++yy->__pos;\n\
  return 1;\n\
}\n\
\n\
YY_LOCAL(int) YY_MAYBE_UNUSED yymatchChar(yycontext *yy, int c)\n\
{\n\
  if (yy->__pos >= yy->__limit && !yyrefill(yy)) return 0;\n\
  if ((unsigned char)yy->__buf[yy->__pos] == c)\n\
    {\n\
      ++yy->__pos;\n\
      yyprintf((stderr, \"  ok   yymatchChar(yy, %c) @ %s\\n\", c, yy->__buf+yy->__pos));\n\
      return 1;\n\
    }\n\
  yyprintf((stderr, \"  fail yymatchChar(yy, %c) @ %s\\n\", c, yy->__buf+yy->__pos));\n\
  return 0;\n\
}\n\
\n\
YY_LOCAL(int) YY_MAYBE_UNUSED yymatchString(yycontext *yy, const char *s)\n\
{\n\
  int yysav= yy->__pos;\n\
  while (*s)\n\
    {\n\
      if (yy->__pos >= yy->__limit && !yyrefill(yy)) return 0;\n\
      if (yy->__buf[yy->__pos] != *s)\n\
        {\n\
          yy->__pos= yysav;\n\
          return 0;\n\
        }\n\
      ++s;\n\
      ++yy->__pos;\n\
    }\n\
  return 1;\n\
}\n\
\n\
YY_LOCAL(int) YY_MAYBE_UNUSED yymatchClass(yycontext *yy, unsigned char *bits)\n\
{\n\
  int c;\n\
  if (yy->__pos >= yy->__limit && !yyrefill(yy)) return 0;\n\
  c= (unsigned char)yy->__buf[yy->__pos];\n\
  if (bits[c >> 3] & (1 << (c & 7)))\n\
    {\n\
      ++yy->__pos;\n\
      yyprintf((stderr, \"  ok   yymatchClass @ %s\\n\", yy->__buf+yy->__pos));\n\
      return 1;\n\
    }\n\
  yyprintf((stderr, \"  fail yymatchClass @ %s\\n\", yy->__buf+yy->__pos));\n\
  return 0;\n\
}\n\
\n\
YY_LOCAL(void) YY_MAYBE_UNUSED yyDo(yycontext *yy, yyaction action, int begin, int end)\n\
{\n\
  while (yy->__thunkpos >= yy->__thunkslen)\n\
    {\n\
      yy->__thunkslen *= 2;\n\
      yy->__thunks= (yythunk *)YY_REALLOC(yy, yy->__thunks, sizeof(yythunk) * yy->__thunkslen);\n\
    }\n\
  yy->__thunks[yy->__thunkpos].begin=  begin;\n\
  yy->__thunks[yy->__thunkpos].end=    end;\n\
  yy->__thunks[yy->__thunkpos].action= action;\n\
  ++yy->__thunkpos;\n\
}\n\
\n\
YY_LOCAL(int) YY_MAYBE_UNUSED yyText(yycontext *yy, int begin, int end)\n\
{\n\
  int yyleng= end - begin;\n\
  if (yyleng <= 0)\n\
    yyleng= 0;\n\
  else\n\
    {\n\
      while (yy->__textlen < (yyleng + 1))\n\
	{\n\
	  yy->__textlen *= 2;\n\
	  yy->__text= (char *)YY_REALLOC(yy, yy->__text, yy->__textlen);\n\
	}\n\
      memcpy(yy->__text, yy->__buf + begin, yyleng);\n\
    }\n\
  yy->__text[yyleng]= '\\0';\n\
  return yyleng;\n\
}\n\
\n\
YY_LOCAL(void) yyDone(yycontext *yy)\n\
{\n\
  int pos;\n\
  for (pos= 0;  pos < yy->__thunkpos;  ++pos)\n\
    {\n\
      yythunk *thunk= &yy->__thunks[pos];\n\
      int yyleng= thunk->end ? yyText(yy, thunk->begin, thunk->end) : thunk->begin;\n\
      yyprintf((stderr, \"DO [%d] %p %s\\n\", pos, thunk->action, yy->__text));\n\
      thunk->action(yy, yy->__text, yyleng);\n\
    }\n\
  yy->__thunkpos= 0;\n\
}\n\
\n\
YY_LOCAL(void) yyCommit(yycontext *yy)\n\
{\n\
  if ((yy->__limit -= yy->__pos))\n\
    {\n\
      memmove(yy->__buf, yy->__buf + yy->__pos, yy->__limit);\n\
    }\n\
  yy->__begin -= yy->__pos;\n\
  yy->__end -= yy->__pos;\n\
  yy->__pos= yy->__thunkpos= 0;\n\
}\n\
\n\
YY_LOCAL(int) YY_MAYBE_UNUSED yyAccept(yycontext *yy, int tp0)\n\
{\n\
  if (tp0)\n\
    {\n\
      fprintf(stderr, \"accept denied at %d\\n\", tp0);\n\
      return 0;\n\
    }\n\
  else\n\
    {\n\
      yyDone(yy);\n\
      yyCommit(yy);\n\
    }\n\
  return 1;\n\
}\n\
\n\
YY_LOCAL(void) YY_MAYBE_UNUSED yyPush(yycontext *yy, char *text, int count)\n\
{\n\
  yy->__val += count;\n\
  while (yy->__valslen <= yy->__val - yy->__vals)\n\
    {\n\
      long offset= yy->__val - yy->__vals;\n\
      yy->__valslen *= 2;\n\
      yy->__vals= (YYSTYPE *)YY_REALLOC(yy, yy->__vals, sizeof(YYSTYPE) * yy->__valslen);\n\
      yy->__val= yy->__vals + offset;\n\
    }\n\
}\n\
YY_LOCAL(void) YY_MAYBE_UNUSED yyPop(yycontext *yy, char *text, int count)   { yy->__val -= count; }\n\
YY_LOCAL(void) YY_MAYBE_UNUSED yySet(yycontext *yy, char *text, int count)   { yy->__val[count]= yy->__; }\n\
\n\
#endif /* YY_PART */\n\
\n\
#define YYACCEPT yyAccept(yy, yythunkpos0)\n\
\n\
";

static char *footer= "\n\
\n\
#ifndef YY_PART\n\
\n\
typedef int (*yyrule)(yycontext *yy);\n\
\n\
YY_PARSE(int) YYPARSEFROM(YY_CTX_PARAM_ yyrule yystart)\n\
{\n\
  int yyok;\n\
  if (!yyctx->__buflen)\n\
    {\n\
      yyctx->__buflen= YY_BUFFER_SIZE;\n\
      yyctx->__buf= (char *)YY_MALLOC(yyctx, yyctx->__buflen);\n\
      yyctx->__textlen= YY_BUFFER_SIZE;\n\
      yyctx->__text= (char *)YY_MALLOC(yyctx, yyctx->__textlen);\n\
      yyctx->__thunkslen= YY_STACK_SIZE;\n\
      yyctx->__thunks= (yythunk *)YY_MALLOC(yyctx, sizeof(yythunk) * yyctx->__thunkslen);\n\
      yyctx->__valslen= YY_STACK_SIZE;\n\
      yyctx->__vals= (YYSTYPE *)YY_MALLOC(yyctx, sizeof(YYSTYPE) * yyctx->__valslen);\n\
      yyctx->__begin= yyctx->__end= yyctx->__pos= yyctx->__limit= yyctx->__thunkpos= 0;\n\
    }\n\
  yyctx->__begin= yyctx->__end= yyctx->__pos;\n\
  yyctx->__thunkpos= 0;\n\
  yyctx->__val= yyctx->__vals;\n\
  yyok= yystart(yyctx);\n\
  if (yyok) yyDone(yyctx);\n\
  yyCommit(yyctx);\n\
  return yyok;\n\
}\n\
\n\
YY_PARSE(int) YYPARSE(YY_CTX_PARAM)\n\
{\n\
  return YYPARSEFROM(YY_CTX_ARG_ yy_%s);\n\
}\n\
\n\
YY_PARSE(yycontext *) YYRELEASE(yycontext *yyctx)\n\
{\n\
  if (yyctx->__buflen)\n\
    {\n\
      yyctx->__buflen= 0;\n\
      YY_FREE(yyctx, yyctx->__buf);\n\
      YY_FREE(yyctx, yyctx->__text);\n\
      YY_FREE(yyctx, yyctx->__thunks);\n\
      YY_FREE(yyctx, yyctx->__vals);\n\
    }\n\
  return yyctx;\n\
}\n\
\n\
#endif\n\
";

void Rule_compile_c_header(void)
{
  fprintf(output, "/* Parser generated by minipeg %s */\n", MINIPEG_VERSION);
  fprintf(output, "\n");
  fprintf(output, "%s", header);
  fprintf(output, "#define YYRULECOUNT %d\n", ruleCount);
  fprintf(output, "typedef struct _yycontext yycontext;\n");
}

int consumesInput(Node *node)
{
  if (!node) return 0;

  switch (node->type)
    {
    case Rule:
      {
	int result= 0;
	if (RuleReached & node->rule.flags)
	  fprintf(stderr, "possible infinite left recursion in rule '%s'\n", node->rule.name);
	else
	  {
	    node->rule.flags |= RuleReached;
	    result= consumesInput(node->rule.expression);
	    node->rule.flags &= ~RuleReached;
	  }
	return result;
      }
      break;

    case Dot:		return 1;
    case Name:		return consumesInput(node->name.rule);
    case Character:
    case String:	return strlen(node->string.value) > 0;
    case Class:		return 1;
    case Action:	return 0;
    case Inline:	return 0;
    case Predicate:	return 0;
    case Error:		return consumesInput(node->error.element);

    case Alternate:
      {
	Node *n;
	for (n= node->alternate.first;  n;  n= n->alternate.next)
	  if (!consumesInput(n))
	    return 0;
      }
      return 1;

    case Sequence:
      {
	Node *n;
	for (n= node->alternate.first;  n;  n= n->alternate.next)
	  if (consumesInput(n))
	    return 1;
      }
      return 0;

    case PeekFor:	return 0;
    case PeekNot:	return 0;
    case Query:		return 0;
    case Star:		return 0;
    case Plus:		return consumesInput(node->plus.element);

    default:
      fprintf(stderr, "\nconsumesInput: illegal node type %d\n", node->type);
      exit(1);
    }
  return 0;
}


void Rule_compile_c(Node *node, int nolines)
{
  Node *n;

  for (n= rules;  n;  n= n->rule.next)
    consumesInput(n);

  fprintf(output, "%s", preamble);
  for (n= node;  n;  n= n->rule.next)
    fprintf(output, "YY_RULE(int) yy_%s(yycontext *yy); /* %d */\n", n->rule.name, n->rule.id);
  fprintf(output, "\n");
  for (n= actions;  n;  n= n->action.list)
    {
      fprintf(output, "YY_ACTION(void) yy%s(yycontext *yy, char *yytext, int yyleng)\n{\n", n->action.name);
      defineVariables(n->action.rule->rule.variables);
      fprintf(output, "  yyprintf((stderr, \"do yy%s\\n\"));\n", n->action.name);
      fprintf(output, "  {\n");
      if (!nolines)
	fprintf(output, "#line %i\n", n->action.line);
      fprintf(output, "  %s;\n", n->action.text);
      fprintf(output, "  }\n");
      undefineVariables(n->action.rule->rule.variables);
      fprintf(output, "}\n");
    }
  Rule_compile_c2(node);
  fprintf(output, footer, start->rule.name);
}
#line 1 "tree.c"

#ifdef WIN32
# undef inline
# define inline __inline
#endif


Node *actions= 0;
Node *rules= 0;
Node *thisRule= 0;
Node *start= 0;

FILE *output= 0;

int actionCount= 0;
int ruleCount= 0;
int lastToken= -1;

static inline Node *_newNode(int type, int size)
{
  Node *node= calloc(1, size);
  node->type= type;
  return node;
}

#define newNode(T)	_newNode(T, sizeof(struct T))

Node *makeRule(char *name)
{
  Node *node= newNode(Rule);
  node->rule.name= strdup(name);
  node->rule.id= ++ruleCount;
  node->rule.flags= 0;
  node->rule.next= rules;
  rules= node;
  return node;
}

Node *findRule(char *name)
{
  Node *n;
  char *ptr;
  for (ptr= name;  *ptr;  ptr++) if ('-' == *ptr) *ptr= '_';
  for (n= rules;  n;  n= n->any.next)
    {
      assert(Rule == n->type);
      if (!strcmp(name, n->rule.name))
	return n;
    }
  return makeRule(name);
}

Node *beginRule(Node *rule)
{
  actionCount= 0;
  return thisRule= rule;
}

void Rule_setExpression(Node *node, Node *expression)
{
  assert(node);
#ifdef DEBUG
  Node_print(node);  fprintf(stderr, " [%d]<- ", node->type);  Node_print(expression);  fprintf(stderr, "\n");
#endif
  assert(Rule == node->type);
  node->rule.expression= expression;
  if (!start || !strcmp(node->rule.name, "start"))
    start= node;
}

Node *makeVariable(char *name)
{
  Node *node;
  assert(thisRule);
  for (node= thisRule->rule.variables;  node;  node= node->variable.next)
    if (!strcmp(name, node->variable.name))
      return node;
  node= newNode(Variable);
  node->variable.name= strdup(name);
  node->variable.next= thisRule->rule.variables;
  thisRule->rule.variables= node;
  return node;
}

Node *makeName(Node *rule)
{
  Node *node= newNode(Name);
  node->name.rule= rule;
  node->name.variable= 0;
  rule->rule.flags |= RuleUsed;
  return node;
}

Node *makeDot(void)
{
  return newNode(Dot);
}

Node *makeCharacter(char *text)
{
  Node *node= newNode(Character);
  node->character.value= strdup(text);
  return node;
}

Node *makeString(char *text)
{
  Node *node= newNode(String);
  node->string.value= strdup(text);
  return node;
}

Node *makeClass(char *text)
{
  Node *node= newNode(Class);
  node->cclass.value= (unsigned char *)strdup(text);
  return node;
}

Node *makeAction(int lineNumber, char *text)
{
  Node *node= newNode(Action);
  char name[1024];
  assert(thisRule);
  sprintf(name, "_%d_%s", ++actionCount, thisRule->rule.name);
  node->action.name= strdup(name);
  node->action.text= strdup(text);
  node->action.list= actions;
  node->action.rule= thisRule;
  node->action.line= lineNumber;
  actions= node;
  {
    char *ptr;
    for (ptr= node->action.text;  *ptr;  ++ptr)
      if ('$' == ptr[0] && '$' == ptr[1])
	ptr[1]= ptr[0]= '_';
  }
  return node;
}

Node *makeInline(char *text)
{
  Node *node= newNode(Inline);
  node->inLine.text= strdup(text);
  return node;
}

Node *makePredicate(char *text)
{
  Node *node= newNode(Predicate);
  node->predicate.text= strdup(text);
  return node;
}

Node *makeError(Node *e, char *text)
{
  Node *node= newNode(Error);
  node->error.element= e;
  node->error.text= strdup(text);
  return node;
}

Node *makeAlternate(Node *e)
{
  if (Alternate != e->type)
    {
      Node *node= newNode(Alternate);
      node->alternate.first=
	node->alternate.last= e;
      return node;
    }
  return e;
}

Node *Alternate_append(Node *a, Node *e)
{
  a= makeAlternate(a);
  a->alternate.last->any.next= e;
  a->alternate.last= e;
  return a;
}

Node *makeSequence(Node *e)
{
  if (Sequence != e->type)
    {
      Node *node= newNode(Sequence);
      node->sequence.first=
	node->sequence.last= e;
      return node;
    }
  return e;
}

Node *Sequence_append(Node *a, Node *e)
{
  a= makeSequence(a);
  a->sequence.last->any.next= e;
  a->sequence.last= e;
  return a;
}

Node *makePeekFor(Node *e)
{
  Node *node= newNode(PeekFor);
  node->peekFor.element= e;
  return node;
}

Node *makePeekNot(Node *e)
{
  Node *node= newNode(PeekNot);
  node->peekNot.element= e;
  return node;
}

Node *makeQuery(Node *e)
{
  Node *node= newNode(Query);
  node->query.element= e;
  return node;
}

Node *makeStar(Node *e)
{
  Node *node= newNode(Star);
  node->star.element= e;
  return node;
}

Node *makePlus(Node *e)
{
  Node *node= newNode(Plus);
  node->plus.element= e;
  return node;
}


static Node  *stack[1024];
static Node **stackPointer= stack;


#ifdef DEBUG
static void dumpStack(void)
{
  Node **p;
  for (p= stack + 1;  p <= stackPointer;  ++p)
    {
      fprintf(stderr, "### %d\t", p - stack);
      Node_print(*p);
      fprintf(stderr, "\n");
    }
}
#endif

Node *push(Node *node)
{
  assert(node);
  assert(stackPointer < stack + 1023);
#ifdef DEBUG
  dumpStack();  fprintf(stderr, " PUSH ");  Node_print(node);  fprintf(stderr, "\n");
#endif
  return *++stackPointer= node;
}

Node *top(void)
{
  assert(stackPointer > stack);
  return *stackPointer;
}

Node *pop(void)
{
  assert(stackPointer > stack);
#ifdef DEBUG
  dumpStack();  fprintf(stderr, " POP\n");
#endif
  return *stackPointer--;
}


static void Node_fprint(FILE *stream, Node *node)
{
  assert(node);
  switch (node->type)
    {
    case Rule:		fprintf(stream, " %s", node->rule.name);				break;
    case Variable:	fprintf(stream, " %s:", node->variable.name);				break;
    case Name:		fprintf(stream, " %s", node->name.rule->rule.name);			break;
    case Dot:		fprintf(stream, " .");							break;
    case Character:	fprintf(stream, " '%s'", node->character.value);			break;
    case String:	fprintf(stream, " \"%s\"", node->string.value);				break;
    case Class:		fprintf(stream, " [%s]", node->cclass.value);				break;
    case Action:	fprintf(stream, " { %s }", node->action.text);				break;
    case Predicate:	fprintf(stream, " ?{ %s }", node->action.text);				break;

    case Alternate:	node= node->alternate.first;
			fprintf(stream, " (");
			Node_fprint(stream, node);
			while ((node= node->any.next))
			  {
			    fprintf(stream, " |");
			    Node_fprint(stream, node);
			  }
			fprintf(stream, " )");
			break;

    case Sequence:	node= node->sequence.first;
			fprintf(stream, " (");
			Node_fprint(stream, node);
			while ((node= node->any.next))
			  Node_fprint(stream, node);
			fprintf(stream, " )");
			break;

    case PeekFor:	fprintf(stream, "&");  Node_fprint(stream, node->query.element);	break;
    case PeekNot:	fprintf(stream, "!");  Node_fprint(stream, node->query.element);	break;
    case Query:		Node_fprint(stream, node->query.element);  fprintf(stream, "?");	break;
    case Star:		Node_fprint(stream, node->query.element);  fprintf(stream, "*");	break;
    case Plus:		Node_fprint(stream, node->query.element);  fprintf(stream, "+");	break;
    default:
      fprintf(stream, "\nunknown node type %d\n", node->type);
      exit(1);
    }
}

void Node_print(Node *node)	{ Node_fprint(stderr, node); }

static void Rule_fprint(FILE *stream, Node *node)
{
  assert(Rule == node->type);
  fprintf(stream, "%s.%d =", node->rule.name, node->rule.id);
  if (node->rule.expression)
    Node_fprint(stream, node->rule.expression);
  else
    fprintf(stream, " UNDEFINED");
  fprintf(stream, " ;\n");
}

void Rule_print(Node *node)	{ Rule_fprint(stderr, node); }
#line 1 "peg.c"
/* Parser generated by minipeg 77a5ec7b */

#define YYRULECOUNT 38
typedef struct _yycontext yycontext;
#line 1 "peg.peg"



  typedef struct Header Header;

  struct Header {
    int	    line;
    char   *text;
    Header *next;
  };

  FILE *input= 0;

  int   verboseFlag= 0;
  int   nolinesFlag= 0;

  static int	 lineNumber= 0;
  static int	 headerLine= 0;
  static int	 actionLine= 0;
  static char	*fileName= 0;
  static int	 trailerLine= 0;
  static char	*trailer= 0;
  static Header	*headers= 0;

  void makeHeader(int line, char *text);
  void makeTrailer(int line, char *text);

  void yyerror(char *message);

# define YY_INPUT(buf, result, max)			\
  {							\
    int c= getc(input);					\
    result= (EOF == c) ? 0 : (*(buf)= c, 1);		\
  }

# define YY_LOCAL(T)	static T
# define YY_RULE(T)	static T

#ifndef YY_MAYBE_UNUSED
#ifdef __GNUC__
#define YY_MAYBE_UNUSED __attribute__((unused))
#else
#define YY_MAYBE_UNUSED
#endif
#endif
#ifndef YY_MALLOC
#define YY_MALLOC(C, N)		malloc(N)
#endif
#ifndef YY_REALLOC
#define YY_REALLOC(C, P, N)	realloc(P, N)
#endif
#ifndef YY_FREE
#define YY_FREE(C, P)		free(P)
#endif
#ifndef YY_LOCAL
#define YY_LOCAL(T)	static T
#endif
#ifndef YY_ACTION
#define YY_ACTION(T)	static T
#endif
#ifndef YY_RULE
#define YY_RULE(T)	static T
#endif
#ifndef YY_PARSE
#define YY_PARSE(T)	T
#endif
#ifndef YYPARSE
#define YYPARSE		yyparse
#endif
#ifndef YYPARSEFROM
#define YYPARSEFROM	yyparsefrom
#endif
#ifndef YYRELEASE
#define YYRELEASE	yyrelease
#endif
#ifndef YY_BEGIN
#define YY_BEGIN	( yy->__begin= yy->__pos, 1)
#endif
#ifndef YY_END
#define YY_END		( yy->__end= yy->__pos, 1)
#endif
#ifdef YY_DEBUG
# define yyprintf(args)	fprintf args
#else
# define yyprintf(args)
#endif
#ifndef YYSTYPE
#define YYSTYPE	int
#endif
#ifndef YY_STACK_SIZE
#define YY_STACK_SIZE 128
#endif

#ifndef YY_BUFFER_SIZE
#define YY_BUFFER_SIZE 1024
#endif

#ifndef YY_PART

typedef void (*yyaction)(yycontext *yy, char *yytext, int yyleng);
typedef struct _yythunk { int begin, end;  yyaction  action;  struct _yythunk *next; } yythunk;

struct _yycontext {
  char     *__buf;
  int       __buflen;
  int       __pos;
  int       __limit;
  char     *__text;
  int       __textlen;
  int       __begin;
  int       __end;
  int       __textmax;
  yythunk  *__thunks;
  int       __thunkslen;
  int       __thunkpos;
  YYSTYPE   __;
  YYSTYPE  *__val;
  YYSTYPE  *__vals;
  int       __valslen;
#ifdef YY_CTX_MEMBERS
  YY_CTX_MEMBERS
#endif
};

#ifdef YY_CTX_LOCAL
#define YY_CTX_PARAM_	yycontext *yyctx,
#define YY_CTX_PARAM	yycontext *yyctx
#define YY_CTX_ARG_	yyctx,
#define YY_CTX_ARG	yyctx
#ifndef YY_INPUT
#define YY_INPUT(yy, buf, result, max_size)		\
  {							\
    int yyc= getchar();					\
    result= (EOF == yyc) ? 0 : (*(buf)= yyc, 1);	\
    yyprintf((stderr, "<%c>", yyc));			\
  }
#endif
#else
#define YY_CTX_PARAM_
#define YY_CTX_PARAM
#define YY_CTX_ARG_
#define YY_CTX_ARG
yycontext _yyctx= { 0, 0 };
yycontext *yyctx= &_yyctx;
#ifndef YY_INPUT
#define YY_INPUT(buf, result, max_size)			\
  {							\
    int yyc= getchar();					\
    result= (EOF == yyc) ? 0 : (*(buf)= yyc, 1);	\
    yyprintf((stderr, "<%c>", yyc));			\
  }
#endif
#endif

YY_LOCAL(int) YY_MAYBE_UNUSED yyrefill(yycontext *yy)
{
  int yyn;
  while (yy->__buflen - yy->__pos < 512)
    {
      yy->__buflen *= 2;
      yy->__buf= (char *)YY_REALLOC(yy, yy->__buf, yy->__buflen);
    }
#ifdef YY_CTX_LOCAL
  YY_INPUT(yy, (yy->__buf + yy->__pos), yyn, (yy->__buflen - yy->__pos));
#else
  YY_INPUT((yy->__buf + yy->__pos), yyn, (yy->__buflen - yy->__pos));
#endif
  if (!yyn) return 0;
  yy->__limit += yyn;
  return 1;
}

YY_LOCAL(int) YY_MAYBE_UNUSED yymatchDot(yycontext *yy)
{
  if (yy->__pos >= yy->__limit && !yyrefill(yy)) return 0;
  ++yy->__pos;
  return 1;
}

YY_LOCAL(int) YY_MAYBE_UNUSED yymatchChar(yycontext *yy, int c)
{
  if (yy->__pos >= yy->__limit && !yyrefill(yy)) return 0;
  if ((unsigned char)yy->__buf[yy->__pos] == c)
    {
      ++yy->__pos;
      yyprintf((stderr, "  ok   yymatchChar(yy, %c) @ %s\n", c, yy->__buf+yy->__pos));
      return 1;
    }
  yyprintf((stderr, "  fail yymatchChar(yy, %c) @ %s\n", c, yy->__buf+yy->__pos));
  return 0;
}

YY_LOCAL(int) YY_MAYBE_UNUSED yymatchString(yycontext *yy, const char *s)
{
  int yysav= yy->__pos;
  while (*s)
    {
      if (yy->__pos >= yy->__limit && !yyrefill(yy)) return 0;
      if (yy->__buf[yy->__pos] != *s)
        {
          yy->__pos= yysav;
          return 0;
        }
      ++s;
      ++yy->__pos;
    }
  return 1;
}

YY_LOCAL(int) YY_MAYBE_UNUSED yymatchClass(yycontext *yy, unsigned char *bits)
{
  int c;
  if (yy->__pos >= yy->__limit && !yyrefill(yy)) return 0;
  c= (unsigned char)yy->__buf[yy->__pos];
  if (bits[c >> 3] & (1 << (c & 7)))
    {
      ++yy->__pos;
      yyprintf((stderr, "  ok   yymatchClass @ %s\n", yy->__buf+yy->__pos));
      return 1;
    }
  yyprintf((stderr, "  fail yymatchClass @ %s\n", yy->__buf+yy->__pos));
  return 0;
}

YY_LOCAL(void) YY_MAYBE_UNUSED yyDo(yycontext *yy, yyaction action, int begin, int end)
{
  while (yy->__thunkpos >= yy->__thunkslen)
    {
      yy->__thunkslen *= 2;
      yy->__thunks= (yythunk *)YY_REALLOC(yy, yy->__thunks, sizeof(yythunk) * yy->__thunkslen);
    }
  yy->__thunks[yy->__thunkpos].begin=  begin;
  yy->__thunks[yy->__thunkpos].end=    end;
  yy->__thunks[yy->__thunkpos].action= action;
  ++yy->__thunkpos;
}

YY_LOCAL(int) YY_MAYBE_UNUSED yyText(yycontext *yy, int begin, int end)
{
  int yyleng= end - begin;
  if (yyleng <= 0)
    yyleng= 0;
  else
    {
      while (yy->__textlen < (yyleng + 1))
	{
	  yy->__textlen *= 2;
	  yy->__text= (char *)YY_REALLOC(yy, yy->__text, yy->__textlen);
	}
      memcpy(yy->__text, yy->__buf + begin, yyleng);
    }
  yy->__text[yyleng]= '\0';
  return yyleng;
}

YY_LOCAL(void) yyDone(yycontext *yy)
{
  int pos;
  for (pos= 0;  pos < yy->__thunkpos;  ++pos)
    {
      yythunk *thunk= &yy->__thunks[pos];
      int yyleng= thunk->end ? yyText(yy, thunk->begin, thunk->end) : thunk->begin;
      yyprintf((stderr, "DO [%d] %p %s\n", pos, thunk->action, yy->__text));
      thunk->action(yy, yy->__text, yyleng);
    }
  yy->__thunkpos= 0;
}

YY_LOCAL(void) yyCommit(yycontext *yy)
{
  if ((yy->__limit -= yy->__pos))
    {
      memmove(yy->__buf, yy->__buf + yy->__pos, yy->__limit);
    }
  yy->__begin -= yy->__pos;
  yy->__end -= yy->__pos;
  yy->__pos= yy->__thunkpos= 0;
}

YY_LOCAL(int) YY_MAYBE_UNUSED yyAccept(yycontext *yy, int tp0)
{
  if (tp0)
    {
      fprintf(stderr, "accept denied at %d\n", tp0);
      return 0;
    }
  else
    {
      yyDone(yy);
      yyCommit(yy);
    }
  return 1;
}

YY_LOCAL(void) YY_MAYBE_UNUSED yyPush(yycontext *yy, char *text, int count)
{
  yy->__val += count;
  while (yy->__valslen <= yy->__val - yy->__vals)
    {
      long offset= yy->__val - yy->__vals;
      yy->__valslen *= 2;
      yy->__vals= (YYSTYPE *)YY_REALLOC(yy, yy->__vals, sizeof(YYSTYPE) * yy->__valslen);
      yy->__val= yy->__vals + offset;
    }
}
YY_LOCAL(void) YY_MAYBE_UNUSED yyPop(yycontext *yy, char *text, int count)   { yy->__val -= count; }
YY_LOCAL(void) YY_MAYBE_UNUSED yySet(yycontext *yy, char *text, int count)   { yy->__val[count]= yy->__; }

#endif /* YY_PART */

#define YYACCEPT yyAccept(yy, yythunkpos0)

YY_RULE(int) yy_comment(yycontext *yy); /* 38 */
YY_RULE(int) yy_space(yycontext *yy); /* 37 */
YY_RULE(int) yy_braces(yycontext *yy); /* 36 */
YY_RULE(int) yy_range(yycontext *yy); /* 35 */
YY_RULE(int) yy_char(yycontext *yy); /* 34 */
YY_RULE(int) yy_END(yycontext *yy); /* 33 */
YY_RULE(int) yy_BEGIN(yycontext *yy); /* 32 */
YY_RULE(int) yy_DOT(yycontext *yy); /* 31 */
YY_RULE(int) yy_class(yycontext *yy); /* 30 */
YY_RULE(int) yy_literal(yycontext *yy); /* 29 */
YY_RULE(int) yy_CLOSE(yycontext *yy); /* 28 */
YY_RULE(int) yy_OPEN(yycontext *yy); /* 27 */
YY_RULE(int) yy_COLON(yycontext *yy); /* 26 */
YY_RULE(int) yy_PLUS(yycontext *yy); /* 25 */
YY_RULE(int) yy_STAR(yycontext *yy); /* 24 */
YY_RULE(int) yy_QUESTION(yycontext *yy); /* 23 */
YY_RULE(int) yy_primary(yycontext *yy); /* 22 */
YY_RULE(int) yy_NOT(yycontext *yy); /* 21 */
YY_RULE(int) yy_suffix(yycontext *yy); /* 20 */
YY_RULE(int) yy_AND(yycontext *yy); /* 19 */
YY_RULE(int) yy_AT(yycontext *yy); /* 18 */
YY_RULE(int) yy_action(yycontext *yy); /* 17 */
YY_RULE(int) yy_TILDE(yycontext *yy); /* 16 */
YY_RULE(int) yy_prefix(yycontext *yy); /* 15 */
YY_RULE(int) yy_error(yycontext *yy); /* 14 */
YY_RULE(int) yy_BAR(yycontext *yy); /* 13 */
YY_RULE(int) yy_sequence(yycontext *yy); /* 12 */
YY_RULE(int) yy_expression(yycontext *yy); /* 11 */
YY_RULE(int) yy_EQUAL(yycontext *yy); /* 10 */
YY_RULE(int) yy_identifier(yycontext *yy); /* 9 */
YY_RULE(int) yy_RPERCENT(yycontext *yy); /* 8 */
YY_RULE(int) yy_end_of_line(yycontext *yy); /* 7 */
YY_RULE(int) yy_end_of_file(yycontext *yy); /* 6 */
YY_RULE(int) yy_trailer(yycontext *yy); /* 5 */
YY_RULE(int) yy_definition(yycontext *yy); /* 4 */
YY_RULE(int) yy_declaration(yycontext *yy); /* 3 */
YY_RULE(int) yy__(yycontext *yy); /* 2 */
YY_RULE(int) yy_grammar(yycontext *yy); /* 1 */

YY_ACTION(void) yy_1_end_of_line(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_end_of_line\n"));
  {
#line 137
   ++lineNumber ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_action(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_action\n"));
  {
#line 111
   actionLine= lineNumber ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_9_primary(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_9_primary\n"));
  {
#line 91
   push(makePredicate("YY_END")); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_8_primary(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_8_primary\n"));
  {
#line 90
   push(makePredicate("YY_BEGIN")); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_7_primary(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_7_primary\n"));
  {
#line 89
   push(makeAction(actionLine, yytext)); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_6_primary(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_6_primary\n"));
  {
#line 88
   push(makeDot()); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_5_primary(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_5_primary\n"));
  {
#line 87
   push(makeClass(yytext)); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_4_primary(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_4_primary\n"));
  {
#line 86
   push(makeString(yytext)); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_3_primary(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_3_primary\n"));
  {
#line 84
   push(makeName(findRule(yytext))); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_2_primary(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_2_primary\n"));
  {
#line 83
   Node *name= makeName(findRule(yytext));  name->name.variable= pop();  push(name); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_primary(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_primary\n"));
  {
#line 82
   push(makeVariable(yytext)); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_3_suffix(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_3_suffix\n"));
  {
#line 79
   push(makePlus (pop())); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_2_suffix(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_2_suffix\n"));
  {
#line 78
   push(makeStar (pop())); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_suffix(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_suffix\n"));
  {
#line 77
   push(makeQuery(pop())); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_4_prefix(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_4_prefix\n"));
  {
#line 74
   push(makePeekNot(pop())); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_3_prefix(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_3_prefix\n"));
  {
#line 73
   push(makePeekFor(pop())); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_2_prefix(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_2_prefix\n"));
  {
#line 72
   push(makePredicate(yytext)); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_prefix(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_prefix\n"));
  {
#line 71
   push(makeInline(yytext)); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_error(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_error\n"));
  {
#line 68
   push(makeError(pop(), yytext)); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_sequence(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_sequence\n"));
  {
#line 65
   Node *f= pop();  push(Sequence_append(pop(), f)); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_expression(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_expression\n"));
  {
#line 62
   Node *f= pop();  push(Alternate_append(pop(), f)); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_2_definition(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_2_definition\n"));
  {
#line 60
   Node *e= pop();  Rule_setExpression(pop(), e); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_definition(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_definition\n"));
  {
#line 58
   if (push(beginRule(findRule(yytext)))->rule.expression)
							    fprintf(stderr, "rule '%s' redefined\n", yytext); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_2_trailer(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_2_trailer\n"));
  {
#line 56
   makeTrailer(headerLine, yytext); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_trailer(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_trailer\n"));
  {
#line 55
   headerLine= lineNumber ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_2_declaration(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_2_declaration\n"));
  {
#line 53
   makeHeader(headerLine, yytext); ;
  }
#undef yythunkpos
#undef yypos
}
YY_ACTION(void) yy_1_declaration(yycontext *yy, char *yytext, int yyleng)
{
#define __ yy->__
#define yypos yy->__pos
#define yythunkpos yy->__thunkpos
  yyprintf((stderr, "do yy_1_declaration\n"));
  {
#line 51
   headerLine= lineNumber; ;
  }
#undef yythunkpos
#undef yypos
}

YY_RULE(int) yy_comment(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "comment"));  if (!yymatchChar(yy, '#')) goto l1;
  l2:;	
  {  int yypos3= yy->__pos, yythunkpos3= yy->__thunkpos;
  {  int yypos4= yy->__pos, yythunkpos4= yy->__thunkpos;  if (!yy_end_of_line(yy)) goto l4;  goto l3;
  l4:;	  yy->__pos= yypos4; yy->__thunkpos= yythunkpos4;
  }  if (!yymatchDot(yy)) goto l3;  goto l2;
  l3:;	  yy->__pos= yypos3; yy->__thunkpos= yythunkpos3;
  }  if (!yy_end_of_line(yy)) goto l1;
  yyprintf((stderr, "  ok   %s @ %s\n", "comment", yy->__buf+yy->__pos));
  return 1;
  l1:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "comment", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_space(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "space"));
  {  int yypos6= yy->__pos, yythunkpos6= yy->__thunkpos;  if (!yymatchChar(yy, ' ')) goto l7;  goto l6;
  l7:;	  yy->__pos= yypos6; yy->__thunkpos= yythunkpos6;  if (!yymatchChar(yy, '\t')) goto l8;  goto l6;
  l8:;	  yy->__pos= yypos6; yy->__thunkpos= yythunkpos6;  if (!yy_end_of_line(yy)) goto l5;
  }
  l6:;	
  yyprintf((stderr, "  ok   %s @ %s\n", "space", yy->__buf+yy->__pos));
  return 1;
  l5:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "space", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_braces(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "braces"));
  {  int yypos10= yy->__pos, yythunkpos10= yy->__thunkpos;  if (!yymatchChar(yy, '{')) goto l11;
  l12:;	
  {  int yypos13= yy->__pos, yythunkpos13= yy->__thunkpos;  if (!yy_braces(yy)) goto l13;  goto l12;
  l13:;	  yy->__pos= yypos13; yy->__thunkpos= yythunkpos13;
  }  if (!yymatchChar(yy, '}')) goto l11;  goto l10;
  l11:;	  yy->__pos= yypos10; yy->__thunkpos= yythunkpos10;
  {  int yypos14= yy->__pos, yythunkpos14= yy->__thunkpos;  if (!yymatchChar(yy, '}')) goto l14;  goto l9;
  l14:;	  yy->__pos= yypos14; yy->__thunkpos= yythunkpos14;
  }
  {  int yypos15= yy->__pos, yythunkpos15= yy->__thunkpos;  if (!yy_end_of_line(yy)) goto l16;  goto l15;
  l16:;	  yy->__pos= yypos15; yy->__thunkpos= yythunkpos15;  if (!yymatchDot(yy)) goto l9;
  }
  l15:;	
  }
  l10:;	
  yyprintf((stderr, "  ok   %s @ %s\n", "braces", yy->__buf+yy->__pos));
  return 1;
  l9:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "braces", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_range(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "range"));
  {  int yypos18= yy->__pos, yythunkpos18= yy->__thunkpos;  if (!yy_char(yy)) goto l19;  if (!yymatchChar(yy, '-')) goto l19;  if (!yy_char(yy)) goto l19;  goto l18;
  l19:;	  yy->__pos= yypos18; yy->__thunkpos= yythunkpos18;  if (!yy_char(yy)) goto l17;
  }
  l18:;	
  yyprintf((stderr, "  ok   %s @ %s\n", "range", yy->__buf+yy->__pos));
  return 1;
  l17:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "range", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_char(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "char"));
  {  int yypos21= yy->__pos, yythunkpos21= yy->__thunkpos;  if (!yymatchChar(yy, '\\')) goto l22;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\204\040\000\000\000\000\000\070\146\100\124\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l22;  goto l21;
  l22:;	  yy->__pos= yypos21; yy->__thunkpos= yythunkpos21;  if (!yymatchChar(yy, '\\')) goto l23;  if (!yymatchChar(yy, 'x')) goto l23;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\000\377\003\176\000\000\000\176\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l23;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\000\377\003\176\000\000\000\176\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l23;  goto l21;
  l23:;	  yy->__pos= yypos21; yy->__thunkpos= yythunkpos21;  if (!yymatchChar(yy, '\\')) goto l24;  if (!yymatchChar(yy, 'x')) goto l24;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\000\377\003\176\000\000\000\176\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l24;  goto l21;
  l24:;	  yy->__pos= yypos21; yy->__thunkpos= yythunkpos21;  if (!yymatchChar(yy, '\\')) goto l25;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\000\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l25;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\000\377\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l25;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\000\377\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l25;  goto l21;
  l25:;	  yy->__pos= yypos21; yy->__thunkpos= yythunkpos21;  if (!yymatchChar(yy, '\\')) goto l26;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\000\377\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l26;
  {  int yypos27= yy->__pos, yythunkpos27= yy->__thunkpos;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\000\377\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l27;  goto l28;
  l27:;	  yy->__pos= yypos27; yy->__thunkpos= yythunkpos27;
  }
  l28:;	  goto l21;
  l26:;	  yy->__pos= yypos21; yy->__thunkpos= yythunkpos21;
  {  int yypos29= yy->__pos, yythunkpos29= yy->__thunkpos;  if (!yymatchChar(yy, '\\')) goto l29;  goto l20;
  l29:;	  yy->__pos= yypos29; yy->__thunkpos= yythunkpos29;
  }  if (!yymatchDot(yy)) goto l20;
  }
  l21:;	
  yyprintf((stderr, "  ok   %s @ %s\n", "char", yy->__buf+yy->__pos));
  return 1;
  l20:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "char", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_END(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "END"));  if (!yymatchChar(yy, '>')) goto l30;  if (!yy__(yy)) goto l30;
  yyprintf((stderr, "  ok   %s @ %s\n", "END", yy->__buf+yy->__pos));
  return 1;
  l30:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "END", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_BEGIN(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "BEGIN"));  if (!yymatchChar(yy, '<')) goto l31;  if (!yy__(yy)) goto l31;
  yyprintf((stderr, "  ok   %s @ %s\n", "BEGIN", yy->__buf+yy->__pos));
  return 1;
  l31:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "BEGIN", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_DOT(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "DOT"));  if (!yymatchChar(yy, '.')) goto l32;  if (!yy__(yy)) goto l32;
  yyprintf((stderr, "  ok   %s @ %s\n", "DOT", yy->__buf+yy->__pos));
  return 1;
  l32:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "DOT", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_class(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "class"));  if (!yymatchChar(yy, '[')) goto l33;  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_BEGIN)) goto l33;
#undef yytext
#undef yyleng
  }
  l34:;	
  {  int yypos35= yy->__pos, yythunkpos35= yy->__thunkpos;
  {  int yypos36= yy->__pos, yythunkpos36= yy->__thunkpos;  if (!yymatchChar(yy, ']')) goto l36;  goto l35;
  l36:;	  yy->__pos= yypos36; yy->__thunkpos= yythunkpos36;
  }  if (!yy_range(yy)) goto l35;  goto l34;
  l35:;	  yy->__pos= yypos35; yy->__thunkpos= yythunkpos35;
  }  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_END)) goto l33;
#undef yytext
#undef yyleng
  }  if (!yymatchChar(yy, ']')) goto l33;  if (!yy__(yy)) goto l33;
  yyprintf((stderr, "  ok   %s @ %s\n", "class", yy->__buf+yy->__pos));
  return 1;
  l33:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "class", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_literal(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "literal"));
  {  int yypos38= yy->__pos, yythunkpos38= yy->__thunkpos;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l39;  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_BEGIN)) goto l39;
#undef yytext
#undef yyleng
  }
  l40:;	
  {  int yypos41= yy->__pos, yythunkpos41= yy->__thunkpos;
  {  int yypos42= yy->__pos, yythunkpos42= yy->__thunkpos;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l42;  goto l41;
  l42:;	  yy->__pos= yypos42; yy->__thunkpos= yythunkpos42;
  }  if (!yy_char(yy)) goto l41;  goto l40;
  l41:;	  yy->__pos= yypos41; yy->__thunkpos= yythunkpos41;
  }  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_END)) goto l39;
#undef yytext
#undef yyleng
  }  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l39;  if (!yy__(yy)) goto l39;  goto l38;
  l39:;	  yy->__pos= yypos38; yy->__thunkpos= yythunkpos38;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l37;  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_BEGIN)) goto l37;
#undef yytext
#undef yyleng
  }
  l43:;	
  {  int yypos44= yy->__pos, yythunkpos44= yy->__thunkpos;
  {  int yypos45= yy->__pos, yythunkpos45= yy->__thunkpos;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l45;  goto l44;
  l45:;	  yy->__pos= yypos45; yy->__thunkpos= yythunkpos45;
  }  if (!yy_char(yy)) goto l44;  goto l43;
  l44:;	  yy->__pos= yypos44; yy->__thunkpos= yythunkpos44;
  }  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_END)) goto l37;
#undef yytext
#undef yyleng
  }  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l37;  if (!yy__(yy)) goto l37;
  }
  l38:;	
  yyprintf((stderr, "  ok   %s @ %s\n", "literal", yy->__buf+yy->__pos));
  return 1;
  l37:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "literal", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_CLOSE(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "CLOSE"));  if (!yymatchChar(yy, ')')) goto l46;  if (!yy__(yy)) goto l46;
  yyprintf((stderr, "  ok   %s @ %s\n", "CLOSE", yy->__buf+yy->__pos));
  return 1;
  l46:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "CLOSE", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_OPEN(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "OPEN"));  if (!yymatchChar(yy, '(')) goto l47;  if (!yy__(yy)) goto l47;
  yyprintf((stderr, "  ok   %s @ %s\n", "OPEN", yy->__buf+yy->__pos));
  return 1;
  l47:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "OPEN", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_COLON(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "COLON"));  if (!yymatchChar(yy, ':')) goto l48;  if (!yy__(yy)) goto l48;
  yyprintf((stderr, "  ok   %s @ %s\n", "COLON", yy->__buf+yy->__pos));
  return 1;
  l48:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "COLON", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_PLUS(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "PLUS"));  if (!yymatchChar(yy, '+')) goto l49;  if (!yy__(yy)) goto l49;
  yyprintf((stderr, "  ok   %s @ %s\n", "PLUS", yy->__buf+yy->__pos));
  return 1;
  l49:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "PLUS", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_STAR(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "STAR"));  if (!yymatchChar(yy, '*')) goto l50;  if (!yy__(yy)) goto l50;
  yyprintf((stderr, "  ok   %s @ %s\n", "STAR", yy->__buf+yy->__pos));
  return 1;
  l50:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "STAR", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_QUESTION(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "QUESTION"));  if (!yymatchChar(yy, '?')) goto l51;  if (!yy__(yy)) goto l51;
  yyprintf((stderr, "  ok   %s @ %s\n", "QUESTION", yy->__buf+yy->__pos));
  return 1;
  l51:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "QUESTION", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_primary(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "primary"));
  {  int yypos53= yy->__pos, yythunkpos53= yy->__thunkpos;  if (!yy_identifier(yy)) goto l54;  yyDo(yy, yy_1_primary, yy->__begin, yy->__end);  if (!yy_COLON(yy)) goto l54;  if (!yy_identifier(yy)) goto l54;
  {  int yypos55= yy->__pos, yythunkpos55= yy->__thunkpos;  if (!yy_EQUAL(yy)) goto l55;  goto l54;
  l55:;	  yy->__pos= yypos55; yy->__thunkpos= yythunkpos55;
  }  yyDo(yy, yy_2_primary, yy->__begin, yy->__end);  goto l53;
  l54:;	  yy->__pos= yypos53; yy->__thunkpos= yythunkpos53;  if (!yy_identifier(yy)) goto l56;
  {  int yypos57= yy->__pos, yythunkpos57= yy->__thunkpos;  if (!yy_EQUAL(yy)) goto l57;  goto l56;
  l57:;	  yy->__pos= yypos57; yy->__thunkpos= yythunkpos57;
  }  yyDo(yy, yy_3_primary, yy->__begin, yy->__end);  goto l53;
  l56:;	  yy->__pos= yypos53; yy->__thunkpos= yythunkpos53;  if (!yy_OPEN(yy)) goto l58;  if (!yy_expression(yy)) goto l58;  if (!yy_CLOSE(yy)) goto l58;  goto l53;
  l58:;	  yy->__pos= yypos53; yy->__thunkpos= yythunkpos53;  if (!yy_literal(yy)) goto l59;  yyDo(yy, yy_4_primary, yy->__begin, yy->__end);  goto l53;
  l59:;	  yy->__pos= yypos53; yy->__thunkpos= yythunkpos53;  if (!yy_class(yy)) goto l60;  yyDo(yy, yy_5_primary, yy->__begin, yy->__end);  goto l53;
  l60:;	  yy->__pos= yypos53; yy->__thunkpos= yythunkpos53;  if (!yy_DOT(yy)) goto l61;  yyDo(yy, yy_6_primary, yy->__begin, yy->__end);  goto l53;
  l61:;	  yy->__pos= yypos53; yy->__thunkpos= yythunkpos53;  if (!yy_action(yy)) goto l62;  yyDo(yy, yy_7_primary, yy->__begin, yy->__end);  goto l53;
  l62:;	  yy->__pos= yypos53; yy->__thunkpos= yythunkpos53;  if (!yy_BEGIN(yy)) goto l63;  yyDo(yy, yy_8_primary, yy->__begin, yy->__end);  goto l53;
  l63:;	  yy->__pos= yypos53; yy->__thunkpos= yythunkpos53;  if (!yy_END(yy)) goto l52;  yyDo(yy, yy_9_primary, yy->__begin, yy->__end);
  }
  l53:;	
  yyprintf((stderr, "  ok   %s @ %s\n", "primary", yy->__buf+yy->__pos));
  return 1;
  l52:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "primary", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_NOT(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "NOT"));  if (!yymatchChar(yy, '!')) goto l64;  if (!yy__(yy)) goto l64;
  yyprintf((stderr, "  ok   %s @ %s\n", "NOT", yy->__buf+yy->__pos));
  return 1;
  l64:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "NOT", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_suffix(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "suffix"));  if (!yy_primary(yy)) goto l65;
  {  int yypos66= yy->__pos, yythunkpos66= yy->__thunkpos;
  {  int yypos68= yy->__pos, yythunkpos68= yy->__thunkpos;  if (!yy_QUESTION(yy)) goto l69;  yyDo(yy, yy_1_suffix, yy->__begin, yy->__end);  goto l68;
  l69:;	  yy->__pos= yypos68; yy->__thunkpos= yythunkpos68;  if (!yy_STAR(yy)) goto l70;  yyDo(yy, yy_2_suffix, yy->__begin, yy->__end);  goto l68;
  l70:;	  yy->__pos= yypos68; yy->__thunkpos= yythunkpos68;  if (!yy_PLUS(yy)) goto l66;  yyDo(yy, yy_3_suffix, yy->__begin, yy->__end);
  }
  l68:;	  goto l67;
  l66:;	  yy->__pos= yypos66; yy->__thunkpos= yythunkpos66;
  }
  l67:;	
  yyprintf((stderr, "  ok   %s @ %s\n", "suffix", yy->__buf+yy->__pos));
  return 1;
  l65:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "suffix", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_AND(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "AND"));  if (!yymatchChar(yy, '&')) goto l71;  if (!yy__(yy)) goto l71;
  yyprintf((stderr, "  ok   %s @ %s\n", "AND", yy->__buf+yy->__pos));
  return 1;
  l71:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "AND", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_AT(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "AT"));  if (!yymatchChar(yy, '@')) goto l72;  if (!yy__(yy)) goto l72;
  yyprintf((stderr, "  ok   %s @ %s\n", "AT", yy->__buf+yy->__pos));
  return 1;
  l72:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "AT", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_action(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "action"));  if (!yymatchChar(yy, '{')) goto l73;  yyDo(yy, yy_1_action, yy->__begin, yy->__end);  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_BEGIN)) goto l73;
#undef yytext
#undef yyleng
  }
  l74:;	
  {  int yypos75= yy->__pos, yythunkpos75= yy->__thunkpos;  if (!yy_braces(yy)) goto l75;  goto l74;
  l75:;	  yy->__pos= yypos75; yy->__thunkpos= yythunkpos75;
  }  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_END)) goto l73;
#undef yytext
#undef yyleng
  }  if (!yymatchChar(yy, '}')) goto l73;  if (!yy__(yy)) goto l73;
  yyprintf((stderr, "  ok   %s @ %s\n", "action", yy->__buf+yy->__pos));
  return 1;
  l73:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "action", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_TILDE(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "TILDE"));  if (!yymatchChar(yy, '~')) goto l76;  if (!yy__(yy)) goto l76;
  yyprintf((stderr, "  ok   %s @ %s\n", "TILDE", yy->__buf+yy->__pos));
  return 1;
  l76:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "TILDE", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_prefix(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "prefix"));
  {  int yypos78= yy->__pos, yythunkpos78= yy->__thunkpos;  if (!yy_AT(yy)) goto l79;  if (!yy_action(yy)) goto l79;  yyDo(yy, yy_1_prefix, yy->__begin, yy->__end);  goto l78;
  l79:;	  yy->__pos= yypos78; yy->__thunkpos= yythunkpos78;  if (!yy_AND(yy)) goto l80;  if (!yy_action(yy)) goto l80;  yyDo(yy, yy_2_prefix, yy->__begin, yy->__end);  goto l78;
  l80:;	  yy->__pos= yypos78; yy->__thunkpos= yythunkpos78;  if (!yy_AND(yy)) goto l81;  if (!yy_suffix(yy)) goto l81;  yyDo(yy, yy_3_prefix, yy->__begin, yy->__end);  goto l78;
  l81:;	  yy->__pos= yypos78; yy->__thunkpos= yythunkpos78;  if (!yy_NOT(yy)) goto l82;  if (!yy_suffix(yy)) goto l82;  yyDo(yy, yy_4_prefix, yy->__begin, yy->__end);  goto l78;
  l82:;	  yy->__pos= yypos78; yy->__thunkpos= yythunkpos78;  if (!yy_suffix(yy)) goto l77;
  }
  l78:;	
  yyprintf((stderr, "  ok   %s @ %s\n", "prefix", yy->__buf+yy->__pos));
  return 1;
  l77:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "prefix", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_error(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "error"));  if (!yy_prefix(yy)) goto l83;
  {  int yypos84= yy->__pos, yythunkpos84= yy->__thunkpos;  if (!yy_TILDE(yy)) goto l84;  if (!yy_action(yy)) goto l84;  yyDo(yy, yy_1_error, yy->__begin, yy->__end);  goto l85;
  l84:;	  yy->__pos= yypos84; yy->__thunkpos= yythunkpos84;
  }
  l85:;	
  yyprintf((stderr, "  ok   %s @ %s\n", "error", yy->__buf+yy->__pos));
  return 1;
  l83:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "error", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_BAR(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "BAR"));  if (!yymatchChar(yy, '|')) goto l86;  if (!yy__(yy)) goto l86;
  yyprintf((stderr, "  ok   %s @ %s\n", "BAR", yy->__buf+yy->__pos));
  return 1;
  l86:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "BAR", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_sequence(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "sequence"));  if (!yy_error(yy)) goto l87;
  l88:;	
  {  int yypos89= yy->__pos, yythunkpos89= yy->__thunkpos;  if (!yy_error(yy)) goto l89;  yyDo(yy, yy_1_sequence, yy->__begin, yy->__end);  goto l88;
  l89:;	  yy->__pos= yypos89; yy->__thunkpos= yythunkpos89;
  }
  yyprintf((stderr, "  ok   %s @ %s\n", "sequence", yy->__buf+yy->__pos));
  return 1;
  l87:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "sequence", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_expression(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "expression"));  if (!yy_sequence(yy)) goto l90;
  l91:;	
  {  int yypos92= yy->__pos, yythunkpos92= yy->__thunkpos;  if (!yy_BAR(yy)) goto l92;  if (!yy_sequence(yy)) goto l92;  yyDo(yy, yy_1_expression, yy->__begin, yy->__end);  goto l91;
  l92:;	  yy->__pos= yypos92; yy->__thunkpos= yythunkpos92;
  }
  yyprintf((stderr, "  ok   %s @ %s\n", "expression", yy->__buf+yy->__pos));
  return 1;
  l90:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "expression", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_EQUAL(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "EQUAL"));  if (!yymatchChar(yy, '=')) goto l93;  if (!yy__(yy)) goto l93;
  yyprintf((stderr, "  ok   %s @ %s\n", "EQUAL", yy->__buf+yy->__pos));
  return 1;
  l93:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "EQUAL", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_identifier(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "identifier"));  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_BEGIN)) goto l94;
#undef yytext
#undef yyleng
  }  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\040\000\000\376\377\377\207\376\377\377\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l94;
  l95:;	
  {  int yypos96= yy->__pos, yythunkpos96= yy->__thunkpos;  if (!yymatchClass(yy, (unsigned char *)"\000\000\000\000\000\040\377\003\376\377\377\207\376\377\377\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")) goto l96;  goto l95;
  l96:;	  yy->__pos= yypos96; yy->__thunkpos= yythunkpos96;
  }  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_END)) goto l94;
#undef yytext
#undef yyleng
  }  if (!yy__(yy)) goto l94;
  yyprintf((stderr, "  ok   %s @ %s\n", "identifier", yy->__buf+yy->__pos));
  return 1;
  l94:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "identifier", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_RPERCENT(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "RPERCENT"));  if (!yymatchString(yy, "%}")) goto l97;  if (!yy__(yy)) goto l97;
  yyprintf((stderr, "  ok   %s @ %s\n", "RPERCENT", yy->__buf+yy->__pos));
  return 1;
  l97:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "RPERCENT", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_end_of_line(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "end_of_line"));
  {  int yypos99= yy->__pos, yythunkpos99= yy->__thunkpos;  if (!yymatchString(yy, "\r\n")) goto l100;  goto l99;
  l100:;	  yy->__pos= yypos99; yy->__thunkpos= yythunkpos99;  if (!yymatchChar(yy, '\n')) goto l101;  goto l99;
  l101:;	  yy->__pos= yypos99; yy->__thunkpos= yythunkpos99;  if (!yymatchChar(yy, '\r')) goto l98;
  }
  l99:;	  yyDo(yy, yy_1_end_of_line, yy->__begin, yy->__end);
  yyprintf((stderr, "  ok   %s @ %s\n", "end_of_line", yy->__buf+yy->__pos));
  return 1;
  l98:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "end_of_line", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_end_of_file(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "end_of_file"));
  {  int yypos103= yy->__pos, yythunkpos103= yy->__thunkpos;  if (!yymatchDot(yy)) goto l103;  goto l102;
  l103:;	  yy->__pos= yypos103; yy->__thunkpos= yythunkpos103;
  }
  yyprintf((stderr, "  ok   %s @ %s\n", "end_of_file", yy->__buf+yy->__pos));
  return 1;
  l102:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "end_of_file", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_trailer(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "trailer"));  if (!yymatchString(yy, "%%")) goto l104;  yyDo(yy, yy_1_trailer, yy->__begin, yy->__end);  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_BEGIN)) goto l104;
#undef yytext
#undef yyleng
  }
  l105:;	
  {  int yypos106= yy->__pos, yythunkpos106= yy->__thunkpos;  if (!yymatchDot(yy)) goto l106;  goto l105;
  l106:;	  yy->__pos= yypos106; yy->__thunkpos= yythunkpos106;
  }  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_END)) goto l104;
#undef yytext
#undef yyleng
  }  yyDo(yy, yy_2_trailer, yy->__begin, yy->__end);
  yyprintf((stderr, "  ok   %s @ %s\n", "trailer", yy->__buf+yy->__pos));
  return 1;
  l104:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "trailer", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_definition(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "definition"));  if (!yy_identifier(yy)) goto l107;  yyDo(yy, yy_1_definition, yy->__begin, yy->__end);  if (!yy_EQUAL(yy)) goto l107;  if (!yy_expression(yy)) goto l107;  yyDo(yy, yy_2_definition, yy->__begin, yy->__end);
  yyprintf((stderr, "  ok   %s @ %s\n", "definition", yy->__buf+yy->__pos));
  return 1;
  l107:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "definition", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy_declaration(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "declaration"));  if (!yymatchString(yy, "%{")) goto l108;  yyDo(yy, yy_1_declaration, yy->__begin, yy->__end);  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_BEGIN)) goto l108;
#undef yytext
#undef yyleng
  }
  l109:;	
  {  int yypos110= yy->__pos, yythunkpos110= yy->__thunkpos;
  {  int yypos111= yy->__pos, yythunkpos111= yy->__thunkpos;  if (!yymatchString(yy, "%}")) goto l111;  goto l110;
  l111:;	  yy->__pos= yypos111; yy->__thunkpos= yythunkpos111;
  }
  {  int yypos112= yy->__pos, yythunkpos112= yy->__thunkpos;  if (!yy_end_of_line(yy)) goto l113;  goto l112;
  l113:;	  yy->__pos= yypos112; yy->__thunkpos= yythunkpos112;  if (!yymatchDot(yy)) goto l110;
  }
  l112:;	  goto l109;
  l110:;	  yy->__pos= yypos110; yy->__thunkpos= yythunkpos110;
  }  yyText(yy, yy->__begin, yy->__end);  {
#define yytext yy->__text
#define yyleng yy->__textlen
if (!(YY_END)) goto l108;
#undef yytext
#undef yyleng
  }  if (!yy_RPERCENT(yy)) goto l108;  yyDo(yy, yy_2_declaration, yy->__begin, yy->__end);
  yyprintf((stderr, "  ok   %s @ %s\n", "declaration", yy->__buf+yy->__pos));
  return 1;
  l108:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "declaration", yy->__buf+yy->__pos));
  return 0;
}
YY_RULE(int) yy__(yycontext *yy)
{
  yyprintf((stderr, "%s\n", "_"));
  l115:;	
  {  int yypos116= yy->__pos, yythunkpos116= yy->__thunkpos;
  {  int yypos117= yy->__pos, yythunkpos117= yy->__thunkpos;  if (!yy_space(yy)) goto l118;  goto l117;
  l118:;	  yy->__pos= yypos117; yy->__thunkpos= yythunkpos117;  if (!yy_comment(yy)) goto l116;
  }
  l117:;	  goto l115;
  l116:;	  yy->__pos= yypos116; yy->__thunkpos= yythunkpos116;
  }
  yyprintf((stderr, "  ok   %s @ %s\n", "_", yy->__buf+yy->__pos));
  return 1;
}
YY_RULE(int) yy_grammar(yycontext *yy)
{  int yypos0= yy->__pos, yythunkpos0= yy->__thunkpos;
  yyprintf((stderr, "%s\n", "grammar"));  if (!yy__(yy)) goto l119;
  {  int yypos122= yy->__pos, yythunkpos122= yy->__thunkpos;  if (!yy_declaration(yy)) goto l123;  goto l122;
  l123:;	  yy->__pos= yypos122; yy->__thunkpos= yythunkpos122;  if (!yy_definition(yy)) goto l119;
  }
  l122:;	
  l120:;	
  {  int yypos121= yy->__pos, yythunkpos121= yy->__thunkpos;
  {  int yypos124= yy->__pos, yythunkpos124= yy->__thunkpos;  if (!yy_declaration(yy)) goto l125;  goto l124;
  l125:;	  yy->__pos= yypos124; yy->__thunkpos= yythunkpos124;  if (!yy_definition(yy)) goto l121;
  }
  l124:;	  goto l120;
  l121:;	  yy->__pos= yypos121; yy->__thunkpos= yythunkpos121;
  }
  {  int yypos126= yy->__pos, yythunkpos126= yy->__thunkpos;  if (!yy_trailer(yy)) goto l126;  goto l127;
  l126:;	  yy->__pos= yypos126; yy->__thunkpos= yythunkpos126;
  }
  l127:;	  if (!yy_end_of_file(yy)) goto l119;
  yyprintf((stderr, "  ok   %s @ %s\n", "grammar", yy->__buf+yy->__pos));
  return 1;
  l119:;	  yy->__pos= yypos0; yy->__thunkpos= yythunkpos0;
  yyprintf((stderr, "  fail %s @ %s\n", "grammar", yy->__buf+yy->__pos));
  return 0;
}

#ifndef YY_PART

typedef int (*yyrule)(yycontext *yy);

YY_PARSE(int) YYPARSEFROM(YY_CTX_PARAM_ yyrule yystart)
{
  int yyok;
  if (!yyctx->__buflen)
    {
      yyctx->__buflen= YY_BUFFER_SIZE;
      yyctx->__buf= (char *)YY_MALLOC(yyctx, yyctx->__buflen);
      yyctx->__textlen= YY_BUFFER_SIZE;
      yyctx->__text= (char *)YY_MALLOC(yyctx, yyctx->__textlen);
      yyctx->__thunkslen= YY_STACK_SIZE;
      yyctx->__thunks= (yythunk *)YY_MALLOC(yyctx, sizeof(yythunk) * yyctx->__thunkslen);
      yyctx->__valslen= YY_STACK_SIZE;
      yyctx->__vals= (YYSTYPE *)YY_MALLOC(yyctx, sizeof(YYSTYPE) * yyctx->__valslen);
      yyctx->__begin= yyctx->__end= yyctx->__pos= yyctx->__limit= yyctx->__thunkpos= 0;
    }
  yyctx->__begin= yyctx->__end= yyctx->__pos;
  yyctx->__thunkpos= 0;
  yyctx->__val= yyctx->__vals;
  yyok= yystart(yyctx);
  if (yyok) yyDone(yyctx);
  yyCommit(yyctx);
  return yyok;
}

YY_PARSE(int) YYPARSE(YY_CTX_PARAM)
{
  return YYPARSEFROM(YY_CTX_ARG_ yy_grammar);
}

YY_PARSE(yycontext *) YYRELEASE(yycontext *yyctx)
{
  if (yyctx->__buflen)
    {
      yyctx->__buflen= 0;
      YY_FREE(yyctx, yyctx->__buf);
      YY_FREE(yyctx, yyctx->__text);
      YY_FREE(yyctx, yyctx->__thunks);
      YY_FREE(yyctx, yyctx->__vals);
    }
  return yyctx;
}

#endif
#line 140 "peg.peg"


void yyerror(char *message)
{
  fprintf(stderr, "%s:%d: %s", fileName, lineNumber, message);
  if (yyctx->__text[0]) fprintf(stderr, " near token '%s'", yyctx->__text);
  if (yyctx->__pos < yyctx->__limit || !feof(input))
    {
      yyctx->__buf[yyctx->__limit]= '\0';
      fprintf(stderr, " before text \"");
      while (yyctx->__pos < yyctx->__limit)
	{
	  if ('\n' == yyctx->__buf[yyctx->__pos] || '\r' == yyctx->__buf[yyctx->__pos]) break;
	  fputc(yyctx->__buf[yyctx->__pos++], stderr);
	}
      if (yyctx->__pos == yyctx->__limit)
	{
	  int c;
	  while (EOF != (c= fgetc(input)) && '\n' != c && '\r' != c)
	    fputc(c, stderr);
	}
      fputc('\"', stderr);
    }
  fprintf(stderr, "\n");
  exit(1);
}

void makeHeader(int line, char *text)
{
  Header *header= (Header *)malloc(sizeof(Header));
  header->line= line;
  header->text= strdup(text);
  header->next= headers;
  headers= header;
}

void makeTrailer(int line, char *text)
{
  trailerLine= line;
  trailer= strdup(text);
}

static void version(char *name)
{
  printf("%s version %s\n", name, MINIPEG_VERSION);
}

static void usage(char *name)
{
  version(name);
  fprintf(stderr, "usage: %s [<option>...] [<file>...]\n", name);
  fprintf(stderr, "where <option> can be\n");
  fprintf(stderr, "  -h          print this help information\n");
  fprintf(stderr, "  -o <ofile>  write output to <ofile>\n");
  fprintf(stderr, "  -P          do not generate #line directives\n");
  fprintf(stderr, "  -v          be verbose\n");
  fprintf(stderr, "  -V          print version number and exit\n");
  fprintf(stderr, "if no <file> is given, input is read from stdin\n");
  fprintf(stderr, "if no <ofile> is given, output is written to stdout\n");
  exit(1);
}

int main(int argc, char **argv)
{
  Node *n;
  int   c;

  output= stdout;
  input= stdin;
  lineNumber= 1;
  fileName= "<stdin>";

  while (-1 != (c= getopt(argc, argv, "PVho:v")))
    {
      switch (c)
	{
	case 'V':
	  version(argv[0]);
	  exit(0);

	case 'h':
	  usage(argv[0]);
	  break;

	case 'o':
	  if (!(output= fopen(optarg, "w")))
	    {
	      perror(optarg);
	      exit(1);
	    }
	  break;

	case 'P':
	  nolinesFlag= 1;
	  break;

	case 'v':
	  verboseFlag= 1;
	  break;

	default:
	  fprintf(stderr, "for usage try: %s -h\n", argv[0]);
	  exit(1);
	}
    }
  argc -= optind;
  argv += optind;

  if (argc)
    {
      for (;  argc;  --argc, ++argv)
	{
	  if (!strcmp(*argv, "-"))
	    {
	      input= stdin;
	      fileName= "<stdin>";
	    }
	  else
	    {
	      if (!(input= fopen(*argv, "r")))
		{
		  perror(*argv);
		  exit(1);
		}
	      fileName= *argv;
	    }
	  lineNumber= 1;
	  if (!yyparse())
	    yyerror("syntax error");
	  if (input != stdin)
	    fclose(input);
	}
    }
  else
    if (!yyparse())
      yyerror("syntax error");

  if (verboseFlag)
    for (n= rules;  n;  n= n->any.next)
      Rule_print(n);

  Rule_compile_c_header();

  for (; headers;  headers= headers->next) {
    if (!nolinesFlag)
      fprintf(output, "#line %i \"%s\"\n", headers->line, fileName);
    fprintf(output, "%s\n", headers->text);
  }

  if (rules)
    Rule_compile_c(rules, nolinesFlag);

  if (trailer) {
    if (!nolinesFlag)
      fprintf(output, "#line %i \"%s\"\n", trailerLine, fileName);
    fprintf(output, "%s\n", trailer);
  }

  if (ferror(output)) {
    fprintf(stderr, "io error writing output\n");
    exit(1);
  }

  return 0;
}

