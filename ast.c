#include <llvm-c/Analysis.h>
#include <llvm-c/ExecutionEngine.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "y.tab.h"


struct expr *make_val(int value) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = LITERAL;
  e->value = value;

  return e;
}

struct expr *make_bool(int value) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = LIT_BOOL;
  e->value = value;

  return e;
}

struct expr *make_identifier(char *ident) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = IDENT;
  e->ident = ident;

  return e;
}

struct expr *make_call(char *ident, struct expr *expr) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = CALL;
  e->call.ident = ident;
  e->call.expr = expr;

  return e;
}

struct expr *make_let(char *ident, struct expr *expr, struct expr *body) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = LET;
  e->let.ident = ident;
  e->let.expr = expr;
  e->let.body = body;

  return e;
}

struct expr *make_var(char *ident, struct expr *expr, struct expr *body) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = VAR;
  e->var.ident = ident;
  e->var.expr = expr;
  e->var.body = body;

  return e;
}

struct expr *make_assign(char *ident, struct expr *expr) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = ASSIGN;
  e->assign.ident = ident;
  e->assign.expr = expr;

  return e;
}

struct expr *make_if(struct expr *cond, struct expr *e_true, struct expr *e_false) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = IF;
  e->if_expr.cond = cond;
  e->if_expr.e_true = e_true;
  e->if_expr.e_false = e_false;

  return e;
}

struct expr *make_while(struct expr *cond, struct expr *body) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = WHILE;
  e->while_expr.cond = cond;
  e->while_expr.body = body;

  return e;
}

struct expr *make_un_op(int op, struct expr *expr) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = UN_OP;
  e->unop.op = op;
  e->unop.expr = expr;

  return e;
}

struct expr *make_bin_op(struct expr *lhs, int op, struct expr *rhs) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = BIN_OP;
  e->binop.lhs = lhs;
  e->binop.op = op;
  e->binop.rhs = rhs;

  return e;
}

struct expr *make_pair(struct expr *first, struct expr *second){
  struct expr *e = malloc(sizeof(struct expr));

  e->type = PAIR;
  e->pair.first = first;
  e->pair.second = second;

  return e;
}

struct expr *make_string(char *string) {
  struct expr *e = malloc(sizeof(struct expr));

  e->type = STRING;
  e->string = string;

  return e;
}

struct expr *make_projection(struct expr *expr, int pos){
  struct expr *e = malloc(sizeof(struct expr));

  e->type = PROJECTION;
  e->projection.expr = expr;
  e->projection.pos = pos;

  return e;
}

void free_expr(struct expr *e) {
  switch (e->type) {
    case LITERAL:
    case LIT_BOOL:
      break;

    case IDENT:
      free(e->ident);
      break;

    case CALL:
      free(e->let.ident);
      free_expr(e->let.expr);
      break;

    case LET:
      free(e->let.ident);
      free_expr(e->let.expr);
      free_expr(e->let.body);
      break;

    case VAR:
      free(e->var.ident);
      free_expr(e->var.expr);
      free_expr(e->var.body);
      break;

    case ASSIGN:
      free(e->assign.ident);
      free_expr(e->assign.expr);
      break;

    case IF:
      free_expr(e->if_expr.cond);
      free_expr(e->if_expr.e_true);
      free_expr(e->if_expr.e_false);
      break;

    case WHILE:
      free_expr(e->while_expr.cond);
      free_expr(e->while_expr.body);
      break;

    case UN_OP:
      free_expr(e->unop.expr);
      break;

    case BIN_OP:
      free_expr(e->binop.lhs);
      free_expr(e->binop.rhs);
      break;
    case STRING:
      free(e->string);
      break;
    case PAIR:
      free_expr(e->pair.first);
      free_expr(e->pair.second);
      break;
    case PROJECTION:
      free_expr(e->projection.expr);
      break;
  }

  free(e);
}

LLVMValueRef codegen_expr(struct expr *e, struct env *env, LLVMModuleRef module, LLVMBuilderRef builder){
  switch (e->type) {
    case LITERAL: {
      return LLVMConstInt(LLVMInt32Type(), e->value, 0); // 0 ultimo paramtro indica che è senza segno
    }

    case LIT_BOOL: {
      return LLVMConstInt(LLVMInt1Type(), e->value, 0);
    }

    case CALL: {
      LLVMValueRef expr = codegen_expr(e->call.expr, env, module, builder); // Valuto l'argomento della funzione nell'ambiente corrente
      LLVMValueRef args[] = { expr }; // Creo la lista dei parametri, in questo caso solamente uno
      LLVMValueRef fn = LLVMGetNamedFunction(module, e->call.ident); // Prendo la funzione di nome e->call.ident nel modulo module
      if (!fn) {
        fprintf(stderr, "Undefined function: %s\n", e->call.ident);
        return expr;
      }
      LLVMValueRef r = LLVMBuildCall(builder, fn, args, 1, ""); // Costruisce la chiamata alla funzione

      llvm_build_free(expr, builder);
      return r;
    }

    case LET: {
      LLVMValueRef expr = codegen_expr(e->let.expr, env, module, builder);
      struct env *new_env = push(env, e->let.ident, expr);
      LLVMValueRef body = codegen_expr(e->let.body, new_env, module, builder);
      pop(new_env);

      llvm_build_free(expr, builder); // expr è già stato valutato, quindi è un valore

      return body;
    }

    case VAR: {
      LLVMValueRef expr = codegen_expr(e->var.expr, env, module, builder);

      LLVMBasicBlockRef current_bb = LLVMGetInsertBlock(builder);
      LLVMValueRef f = LLVMGetBasicBlockParent(current_bb);
      LLVMBasicBlockRef entry_bb = LLVMGetEntryBasicBlock(f);

      LLVMPositionBuilder(builder, entry_bb, LLVMGetFirstInstruction(entry_bb));
      LLVMValueRef pointer = LLVMBuildAlloca(builder, LLVMTypeOf(expr), e->var.ident);

      LLVMPositionBuilderAtEnd(builder, current_bb);
      LLVMBuildStore(builder, expr, pointer);

      struct env *new_env = push(env, e->var.ident, pointer);
      LLVMValueRef body = codegen_expr(e->var.body, new_env, module, builder);
      pop(new_env);

      // In questo caso pointer non è un valore ma un riferimento.
      // Si esegue quindi una load prima di chiamare la funzione llvm_build_free

      llvm_build_free(expr, builder);
      //llvm_build_free(LLVMBuildLoad(builder, pointer, ""), builder);

      return body;
    }

    case ASSIGN: {
      // Assign può essere chiamata solo un su una variabile dichiarate con var
      // quindi pointer è sempre un riferimento
      LLVMValueRef expr = codegen_expr(e->var.expr, env, module, builder);
      LLVMValueRef pointer = resolve(env, e->assign.ident);

      if (LLVMGetTypeKind(LLVMTypeOf(pointer)) == LLVMPointerTypeKind){
        if (LLVMGetTypeKind(LLVMGetElementType(LLVMTypeOf(pointer))) == LLVMPointerTypeKind) { // caso i8** (stringa dichiarata con var)
          llvm_build_free(LLVMBuildLoad(builder, pointer, ""), builder);
          LLVMBuildStore(builder, expr, pointer); // non cambia l'ambiente perchè il riferimento alla memoria è sempre lo stesso
          return expr;
        } else if(LLVMGetTypeKind(LLVMGetElementType(LLVMTypeOf(pointer))) == LLVMStructTypeKind){ // caso struct* (coppia dichiarata con var)
          llvm_build_free(LLVMBuildLoad(builder, pointer, ""), builder);
          LLVMBuildStore(builder, expr, pointer);
          return expr;
        } else if(LLVMGetIntTypeWidth(LLVMGetElementType(LLVMTypeOf(pointer))) == 8) { // caso i8* (stringa dichiarata con let)
          abort();
        } else {
          LLVMBuildStore(builder, expr, pointer); // caso i32* o i1* (intero o booleano dichiarato con var)
          return expr;
        }
      } else abort(); // caso i32, i1 o struct dichiarati con let
    }

    case IDENT: {
      LLVMValueRef val = resolve(env, e->ident);

      if (LLVMGetTypeKind(LLVMTypeOf(val)) == LLVMPointerTypeKind) {
        if (LLVMGetTypeKind(LLVMGetElementType(LLVMTypeOf(val))) == LLVMPointerTypeKind) { // caso i8** (stringa dichiarata con var)
          LLVMValueRef string = LLVMBuildLoad(builder, val, "");
          return llvm_build_copy_out(string, builder, module);
        } else if (LLVMGetTypeKind(LLVMGetElementType(LLVMTypeOf(val))) == LLVMIntegerTypeKind){ // caso i32* o i8* o i1*
          if (LLVMGetIntTypeWidth(LLVMGetElementType(LLVMTypeOf(val))) != 8){ // caso i32* o i1* (intero o bool dichiarato con var)
            return LLVMBuildLoad(builder, val, "");
          } else { // caso i8* (stringa dichiarata con let)
            return llvm_build_copy_out(val, builder, module);
          }
        } else { // è un puntatore a una struct dichiarata con var
          LLVMValueRef struct_val = LLVMBuildLoad(builder, val, "");
          return llvm_build_copy_out(struct_val, builder, module);
        }
      } else { // i32 e i1 e struct dichiarata con let
        return llvm_build_copy_out(val, builder, module);
      }
    }

    case IF: {
      LLVMValueRef f = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));

      LLVMBasicBlockRef then_bb = LLVMAppendBasicBlock(f, "then");
      LLVMBasicBlockRef else_bb = LLVMAppendBasicBlock(f, "else");
      LLVMBasicBlockRef cont_bb = LLVMAppendBasicBlock(f, "cont");

      LLVMValueRef cond = codegen_expr(e->if_expr.cond, env, module, builder);
      LLVMBuildCondBr(builder, cond, then_bb, else_bb);

      LLVMPositionBuilderAtEnd(builder, then_bb);
      LLVMValueRef then_val = codegen_expr(e->if_expr.e_true, env, module, builder);
      LLVMBuildBr(builder, cont_bb);

      LLVMPositionBuilderAtEnd(builder, else_bb);
      LLVMValueRef else_val = codegen_expr(e->if_expr.e_false, env, module, builder);
      LLVMBuildBr(builder, cont_bb);

      LLVMPositionBuilderAtEnd(builder, cont_bb);
      LLVMValueRef phi = LLVMBuildPhi(builder, LLVMTypeOf(then_val), "");
      LLVMValueRef values[] = { then_val, else_val };
      LLVMBasicBlockRef blocks[] = { then_bb, else_bb };
      LLVMAddIncoming(phi, values, blocks, 2);
      return phi;
    }

    case WHILE: {
      LLVMValueRef f = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
      LLVMBasicBlockRef cond_bb = LLVMAppendBasicBlock(f, "cond");
      LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(f, "body");
      LLVMBasicBlockRef cont_bb = LLVMAppendBasicBlock(f, "cont");

      LLVMValueRef ret = LLVMBuildBr(builder, cond_bb);

      LLVMPositionBuilderAtEnd(builder, cond_bb);
      LLVMValueRef cond = codegen_expr(e->while_expr.cond, env, module, builder);
      LLVMBuildCondBr(builder, cond, body_bb, cont_bb);

      LLVMPositionBuilderAtEnd(builder, body_bb);
      codegen_expr(e->while_expr.body, env, module, builder);
      LLVMBuildBr(builder, cond_bb);

      LLVMPositionBuilderAtEnd(builder, cont_bb);
      return ret;
    }

    case UN_OP: {
      LLVMValueRef expr = codegen_expr(e->unop.expr, env, module, builder);
      switch (e->unop.op) {
        case '#':
          if(LLVMGetTypeKind(LLVMTypeOf(expr)) == LLVMPointerTypeKind){
            LLVMValueRef args[] = { expr };
            LLVMValueRef f_strlen = LLVMGetNamedFunction(module, "strlen");
            LLVMValueRef value = LLVMBuildCall(builder, f_strlen, args, 1, "");

            llvm_build_free(expr, builder);

            return value;
          }else abort();
        case '!':
          return LLVMBuildNot(builder, expr, "");
      }
      break;
    }

    case PROJECTION:{
      LLVMValueRef expr = codegen_expr(e->projection.expr, env, module, builder); // expr è un valore
      LLVMValueRef prj;

      if(LLVMGetTypeKind(LLVMTypeOf(expr)) != LLVMStructTypeKind) abort(); // Se non è una struct, errore

      switch (e->projection.pos) {
        case 1:
          prj = LLVMBuildExtractValue(builder, expr, 0, "");
          break;
        case 2:
          prj = LLVMBuildExtractValue(builder, expr, 1, "");
          break;
        default: abort();
      }

      LLVMValueRef r = llvm_build_copy_out(prj, builder, module);
      llvm_build_free(expr, builder);
      return r;
    }

    case BIN_OP: {
      LLVMValueRef lhs = codegen_expr(e->binop.lhs, env, module, builder);
      LLVMValueRef rhs = codegen_expr(e->binop.rhs, env, module, builder);

      switch (e->binop.op) {
        case '+':
          // Controllo che lhs e rhs abbiano lo stesso tipo: i8*
          if(LLVMGetTypeKind(LLVMTypeOf(lhs)) == LLVMPointerTypeKind && LLVMGetTypeKind(LLVMTypeOf(rhs)) == LLVMPointerTypeKind){
            LLVMValueRef args[] = { lhs, rhs };
            LLVMValueRef f_strcat = LLVMGetNamedFunction(module, "strcat_");
            LLVMValueRef value = LLVMBuildCall(builder, f_strcat, args, 2, "");

            llvm_build_free(rhs, builder);
            return value;
          } else return LLVMBuildAdd(builder, lhs, rhs, "");
        case '-': return LLVMBuildSub(builder, lhs, rhs, "");
        case '*': return LLVMBuildMul(builder, lhs, rhs, "");
        case '/': return LLVMBuildSDiv(builder, lhs, rhs, "");
        case '<': return LLVMBuildICmp(builder, LLVMIntSLT, lhs, rhs, "");
        case '>': return LLVMBuildICmp(builder, LLVMIntSGT, lhs, rhs, "");
        case LE: return LLVMBuildICmp(builder, LLVMIntSLE, lhs, rhs, "");
        case GE: return LLVMBuildICmp(builder, LLVMIntSGE, lhs, rhs, "");
        case '=':
          if(LLVMGetTypeKind(LLVMTypeOf(lhs)) == LLVMPointerTypeKind && LLVMGetTypeKind(LLVMTypeOf(rhs)) == LLVMPointerTypeKind){
            LLVMValueRef args[] = { lhs, rhs };
            LLVMValueRef f_strcmp = LLVMGetNamedFunction(module, "strcmp");
            LLVMValueRef value = LLVMBuildCall(builder, f_strcmp, args, 2, "");

            llvm_build_free(lhs, builder);
            llvm_build_free(rhs, builder);

            return LLVMBuildICmp(builder, LLVMIntEQ, value, LLVMConstInt(LLVMInt32Type(), 0, 0), "");
          }else return LLVMBuildICmp(builder, LLVMIntEQ, lhs, rhs, "");
        case NE:
          if(LLVMGetTypeKind(LLVMTypeOf(lhs)) == LLVMPointerTypeKind && LLVMGetTypeKind(LLVMTypeOf(rhs)) == LLVMPointerTypeKind){
            LLVMValueRef args[] = { lhs, rhs };
            LLVMValueRef f_strcmp = LLVMGetNamedFunction(module, "strcmp");
            LLVMValueRef value = LLVMBuildCall(builder, f_strcmp, args, 2, "");

            llvm_build_free(lhs, builder);
            llvm_build_free(rhs, builder);

            return LLVMBuildICmp(builder, LLVMIntNE, value, LLVMConstInt(LLVMInt32Type(), 0, 0), "");
          }else return LLVMBuildICmp(builder, LLVMIntNE, lhs, rhs, "");
        case '&': return LLVMBuildAnd(builder, lhs, rhs, "");
        case '|': return LLVMBuildOr(builder, lhs, rhs, "");
        default:
        return NULL;
      }
    }

    case PAIR: {
      // Il valore di ritorno è la coppia valutata, dato che LLVMBuildInsertValue torna un valore
      LLVMValueRef first = codegen_expr(e->pair.first, env, module, builder);
      LLVMValueRef second = codegen_expr(e->pair.second, env, module, builder);

      LLVMTypeRef types[] = {LLVMTypeOf(first),LLVMTypeOf(second) };
      LLVMTypeRef type = LLVMStructType(types, 2, 0);

      LLVMValueRef struct_tmp = LLVMBuildInsertValue(builder, LLVMGetUndef(type), first, 0, "");
      return LLVMBuildInsertValue(builder, struct_tmp, second, 1, "");
    }

    case STRING: {
      LLVMValueRef args[] = { LLVMBuildGlobalStringPtr(builder, e->string, "") };
      LLVMValueRef f_strdup = LLVMGetNamedFunction(module, "strdup");
      LLVMValueRef value = LLVMBuildCall(builder, f_strdup, args, 1, "");

      return value;
    }

    default:
    return NULL;
  }
}

LLVMValueRef llvm_build_copy_out(LLVMValueRef toCopy, LLVMBuilderRef builder, LLVMModuleRef module){
  // toCopy deve essere un valore: i8*, struct, i32 o i1.
  // Negli ultimi due casi non avviene una copia

  if (LLVMGetTypeKind(LLVMTypeOf(toCopy)) == LLVMPointerTypeKind) { // Caso i8*
    LLVMValueRef args[] = { toCopy };
    LLVMValueRef f_strdup = LLVMGetNamedFunction(module, "strdup");
    LLVMValueRef value = LLVMBuildCall(builder, f_strdup, args, 1, "");
    return value;
  } else if (LLVMGetTypeKind(LLVMTypeOf(toCopy)) == LLVMStructTypeKind) { // Caso struct
    LLVMValueRef first = LLVMBuildExtractValue(builder, toCopy, 0, "");
    LLVMValueRef second = LLVMBuildExtractValue(builder, toCopy, 1, "");

    LLVMTypeRef types[] = {LLVMTypeOf(first),LLVMTypeOf(second) };
    LLVMTypeRef type = LLVMStructType(types, 2, 0);

    LLVMValueRef new_first = llvm_build_copy_out(first, builder, module); // copia dell'elemento sinistro
    LLVMValueRef new_second = llvm_build_copy_out(second, builder, module); // copia dell'elemento destro
    LLVMValueRef struct_tmp = LLVMBuildInsertValue(builder, LLVMGetUndef(type), new_first, 0, "");
    return LLVMBuildInsertValue(builder, struct_tmp, new_second, 1, "");
  } else return toCopy; // caso i1 e i32
}

void llvm_build_free(LLVMValueRef toFree, LLVMBuilderRef builder){
  // toFree deve essere un valore
  if (LLVMGetTypeKind(LLVMTypeOf(toFree)) == LLVMPointerTypeKind) { // caso i8*
    LLVMBuildFree(builder, toFree);
  }else if (LLVMGetTypeKind(LLVMTypeOf(toFree)) == LLVMStructTypeKind){ // caso struct
    LLVMValueRef first = LLVMBuildExtractValue(builder, toFree, 0, "");
    LLVMValueRef second = LLVMBuildExtractValue(builder, toFree, 1, "");
    llvm_build_free(first, builder);
    llvm_build_free(second, builder);
  }
}

void jit_eval(struct expr *expr) {
  LLVMModuleRef module = LLVMModuleCreateWithName("exe");
  LLVMBuilderRef builder = LLVMCreateBuilder();
  LLVMExecutionEngineRef engine;

  LLVMTypeRef print_i32_args[] = {LLVMInt32Type()};
  LLVMTypeRef print_string_args[] = {LLVMPointerType(LLVMInt8Type(), 0)};
  LLVMTypeRef strdup_args[] = {LLVMPointerType(LLVMInt8Type(), 0)};
  LLVMTypeRef strcat_args[] = {LLVMPointerType(LLVMInt8Type(), 0), LLVMPointerType(LLVMInt8Type(), 0)};
  LLVMTypeRef strlen_args[] = {LLVMPointerType(LLVMInt8Type(), 0)};
  LLVMTypeRef strcmp_args[] = {LLVMPointerType(LLVMInt8Type(), 0), LLVMPointerType(LLVMInt8Type(), 0)};
  LLVMAddFunction(module, "print_i32", LLVMFunctionType(LLVMVoidType(), print_i32_args, 1, 0));
  LLVMAddFunction(module, "print_string", LLVMFunctionType(LLVMVoidType(), print_string_args, 1, 0));
  LLVMAddFunction(module, "strdup", LLVMFunctionType(LLVMPointerType(LLVMInt8Type(), 0), strdup_args, 1, 0));
  LLVMAddFunction(module, "strcat_", LLVMFunctionType(LLVMPointerType(LLVMInt8Type(), 0), strcat_args, 2, 0));
  LLVMAddFunction(module, "strlen", LLVMFunctionType(LLVMInt32Type(), strlen_args, 1, 0));
  LLVMAddFunction(module, "strcmp", LLVMFunctionType(LLVMInt32Type(), strcmp_args, 2, 0));

  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();
  LLVMLinkInMCJIT();

  char *error;
  if (LLVMCreateExecutionEngineForModule(&engine, module, &error)) {
    fprintf(stderr, "%s\n", error);
    return;
  }

  LLVMTypeRef bad_f_type = LLVMFunctionType(LLVMVoidType(), NULL, 0, 0);
  LLVMValueRef typing_f = LLVMAddFunction(module, "typing_f", bad_f_type);
  LLVMBasicBlockRef typing_entry_bb = LLVMAppendBasicBlock(typing_f, "entry");
  LLVMPositionBuilderAtEnd(builder, typing_entry_bb);
  LLVMValueRef typing_ret = codegen_expr(expr, NULL, module, builder);
  LLVMBuildRetVoid(builder); // TODO
  LLVMTypeRef type = LLVMTypeOf(typing_ret);
  LLVMDeleteFunction(typing_f);


  LLVMTypeRef actual_f_type = LLVMFunctionType(type, NULL, 0, 0);
  LLVMValueRef f = LLVMAddFunction(module, "f", actual_f_type);
  LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(f, "entry");
  LLVMPositionBuilderAtEnd(builder, entry_bb);
  LLVMValueRef ret = codegen_expr(expr, NULL, module, builder);


  if (LLVMGetTypeKind(type) == LLVMVoidTypeKind) {
    LLVMBuildRetVoid(builder);
  } else {
    LLVMBuildRet(builder, ret);
  }

  LLVMDumpModule(module);
  LLVMVerifyModule(module, LLVMAbortProcessAction, NULL);

  fprintf(stderr, "running...\n");
  LLVMGenericValueRef result = LLVMRunFunction(engine, f, 0, NULL);

  if (LLVMGetTypeKind(type) == LLVMVoidTypeKind) {
    printf("-> done\n");
  } else {
    printf("-> %d\n", (int)LLVMGenericValueToInt(result, 0));
  }
  LLVMDisposeGenericValue(result);

  LLVMDisposeBuilder(builder);
  LLVMDisposeExecutionEngine(engine);
}
