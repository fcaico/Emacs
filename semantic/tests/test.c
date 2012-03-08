/* Test file for C language.
 * Attempt to include as many aspects of the C language as possible.
 *
 * $Id: test.c,v 1.2 2001/08/08 01:01:39 zappo Exp $
 *
 */

/* types of include files */
#include "includeme1.h"
#include <includeme2.h>
#include <subdir/includeme3.h>
#include <includeme.notanhfile>

/* Global types */
struct mystruct1 {
  int slot11;
  char slot12;
  float slot13;
};

struct mystruct2 {
  int slot21;
  char slot22;
  float slot23;
} var_of_type_mystruct2;

struct {
  int slot31;
  char slot32;
  float slot33;
} var_of_anonymous_struct;  

typedef struct mystruct1 typedef_of_mystruct1;
typedef struct mystruct1 *typedef_of_pointer_mystruct1;
typedef struct { int slot_a; } typedef_of_anonymous_struct;
typedef struct A {
} B;

union myunion1 {
  int slot41;
  char slot42;
  float slot43;
};

union myunion2 {
  int slot51;
  char slot52;
  float slot53;
} var_of_type_myunion2;

struct {
  int slot61;
  char slot72;
  float slot83;
} var_of_anonymous_union;  

typedef union myunion1 typedef_of_myunion1;
typedef union myunion1 *typedef_of_pointer_myunion1;
typedef union { int slot_a; } typedef_of_anonymous_union;

enum myenum1 { enum11 = 1, enum12 };
enum myenum2 { enum21, enum22 = 2 } var_of_type_myenum2;
enum { enum31, enum32 } var_of_anonymous_enum;

typedef enum myenum1 typedef_of_myenum1;
typedef enum myenum1 *typedef_of_pointer_myenum1;
typedef enum { int slot_a; } typedef_of_anonymous_enum;

typedef int typedef_of_int;

/* Here are some simpler variable types */
int var1;
char var2;
float var3;
mystruct1 var3;
struct mystruct1 var4;
union myunion1 var5;
enum myenum1 var6;

char *varp1;
char **varp2;
char varv1[1];
char varv2[1][2];

char *varpa1 = "moose";
struct mystruct2 vara2 = { 1, 'a', 0.0 };
enum myenum1 vara3 = enum11;

int mvar1, mvar2, mvar3;
char *mvarp1, *mvarp2, *mvarp3;
char *mvarpa1 = 'a', *mvarpa2 = 'b', *mvarpa3 = 'c';
char mvaras1[10], mvaras2[12][13], *mvaras3 = 'd';

#define MACRO1 1
#define MACRO2(foo) (1+foo)

/* Here are some function prototypes */
fun0();
int funp1();
char funp2(int arg11);
float funp3(char arg21, char arg22);
struct mystrct1 funp4(struct mystruct2 arg31, union myunion2 arg32);
enum myenum1 funp5(char *arg41, union myunion1 *arg42);

char funpp1 __P(char argp1, struct mystruct2 argp2, char *arg4p);

/* Function Definitions */
fun0()
{
  int sv = 0;
}

int fun1 ()
{
  int sv = 1;
}

char fun2(int arg_11)
{
  char sv = 2;
}

float fun3(char arg_21, char arg_22)
{
  char sv = 3;
}

struct mystrct1 fun4(struct mystruct2 arg31, union myunion2 arg32)
{
  sv = 4;
}

enum myenum1 fun5(char *arg41, union myunion1 *arg42)
{
  sv = 5;
}

/* Functions with K&R syntax. */
struct mystrct1 funk1(arg_31, arg_32)
     struct mystruct2 arg_31;
     union myunion2 arg32;
{
  sv = 4;
}

enum myenum1 funk2(arg_41, arg_42)
     char *arg_41;
     union myunion1 *arg_42;
{
  sv = 5;
}

int funk3(arg_51, arg_53)
     int arg_51, *arg_53;
{
  sv = 6;
  funcall(sv, arg51);
  fv = 7;
}

/* End of C tests */
