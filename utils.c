#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_i32(int x) {
  printf("%d\n", x);
}

void print_string(char * s){
  printf("%s\n", s);
}

int read_i32(int defaultValue) {
  int x;
  if (scanf("%d", &x)) {
    return x;
  } else {
    return defaultValue;
  }
}

LLVMValueRef resolve(struct env *env, char *name) {
  if (env == NULL) {
    return NULL;
  } else if (strcmp(env->name, name) == 0) {
    return env->value;
  } else {
    return resolve(env->prev, name);
  }
}

struct env *push(struct env *env, char *name, LLVMValueRef value) {
  struct env *r = malloc(sizeof(struct env));

  r->name = name;
  r->prev = env;
  r->value = value;

  return r;
}

// assumes that env is NOT NULL
struct env *pop(struct env *env) {
  struct env *r = env->prev;
  free(env);
  return r;
}

// Funzione che concatena la stringa src in dest, riallocando dest se necessario
char * strcat_(char *dest, char *src){
  int len_dest = strlen(dest);
  int len_src = strlen(src);

  dest = realloc(dest, len_dest + len_src + 1);
  return strcat(dest, src);
}
 
char escaped(char c){
  switch (c) {
    case 't':
      return 0x09;
    case 'n':
      return 0x0A;
    case '\\':
      return 0x5C;
    case '\"':
      return 0x22;
    case '\'':
      return 0x27;
  }
  return 0;
}
