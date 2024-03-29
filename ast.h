#include <llvm-c/Core.h>
#include "utils.h"

enum expr_type {
  LITERAL,
  LIT_BOOL,
  IDENT,
  LET,
  VAR,
  ASSIGN,
  IF,
  CALL,
  WHILE,
  UN_OP,
  BIN_OP,
  PAIR,
  STRING,
  PROJECTION
};

enum value_type {
  ERROR,
  INTEGER,
  BOOLEAN,
};

struct expr {
  enum expr_type type;
  union {
    int value;

    char *ident;

    char *string;

    struct {
      char *ident;
      struct expr *expr;
    } call;

    struct {
      char *ident;
      struct expr *expr;
      struct expr *body;
    } let;

    struct {
      char *ident;
      struct expr *expr;
      struct expr *body;
    } var;

    struct {
      char *ident;
      struct expr *expr;
    } assign;

    struct {
      struct expr *cond;
      struct expr *e_true;
      struct expr *e_false;
    } if_expr;

    struct {
      struct expr *cond;
      struct expr *body;
    } while_expr;

    struct {
      struct expr *expr;
      int op;
    } unop;

    struct {
      struct expr *lhs;
      struct expr *rhs;
      int op;
    } binop;

    struct {
      struct expr *first;
      struct expr *second;
    } pair;

    struct {
      struct expr *expr;
      int pos;
    } projection;
  };
};


struct expr *make_val(int value);
struct expr *make_bool(int value);
struct expr *make_identifier(char *ident);
struct expr *make_call(char *ident, struct expr *expr);
struct expr *make_let(char *ident, struct expr *expr, struct expr *body);
struct expr *make_var(char *ident, struct expr *expr, struct expr *body);
struct expr *make_assign(char *ident, struct expr *expr);
struct expr *make_if(struct expr *cond, struct expr *e_true, struct expr *e_false);
struct expr *make_while(struct expr *cond, struct expr *body);
struct expr *make_un_op(int op, struct expr *expr);
struct expr *make_bin_op(struct expr *lhs, int op, struct expr *rhs);
struct expr *make_pair(struct expr *first, struct expr *second);
struct expr *make_string(char *string);
struct expr *make_projection(struct expr *expr, int pos);
void llvm_build_free(LLVMValueRef toFree, LLVMBuilderRef builder);
LLVMValueRef llvm_build_copy_out(LLVMValueRef toCopy, LLVMBuilderRef builder, LLVMModuleRef module);

void free_expr(struct expr *e);
LLVMValueRef codegen_expr(
  struct expr *e,
  struct env *env,
  LLVMModuleRef module,
  LLVMBuilderRef builder
);
void jit_eval(struct expr *e);
