#include <string.h>
#include <stdlib.h>
#include <emacs-module.h>
#include <rime_api.h>

#define INTERN(val) env->intern(env, val)
#define GLOBAL_REF(val) env->make_global_ref(env, val)
#define REF(val) env->make_global_ref(env, env->intern(env, val))
#define STRING(val) env->make_string(env, val, strlen(val))
#define FUNCALL0(func) env->funcall(env, func, 0, NULL)
#define FUNCALL1(func, a) env->funcall(env, func, 1, (emacs_value[]){a})
#define FUNCALL2(func, a, b) env->funcall(env, func, 2, (emacs_value[]){a, b})

int plugin_is_GPL_compatible;

emacs_value rime_error, nil, t;

typedef struct _EmacsRime {
  RimeSessionId session_id;
  RimeApi *api;
} EmacsRime;

emacs_value
process_key(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  return nil;
}

emacs_value
get_context(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  return nil;
}

emacs_value
clear_composition(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  return nil;
}

emacs_value
get_input (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  return nil;
}

emacs_value
get_commit(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  return nil;
}

emacs_value
get_schema_list(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  return nil;
}

void
emacs_defun(emacs_env *env, EmacsRime *rime, void* cfunc, char* func_name, char* doc, size_t min, size_t max) {
  emacs_value func = env->make_function(env, min, max, cfunc, doc, rime);
  FUNCALL2(REF("defalias"), REF(func_name), func);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);

  /* Global emacs_value initialize */
  rime_error = FUNCALL2(REF("define-error"), REF("rime-error"), STRING("Rime error"));
  nil = REF("nil");
  t = REF("t");

  /* Get Rime API */
  EmacsRime *rime = (EmacsRime*) malloc(sizeof(EmacsRime));
  rime->api = rime_get_api();
  if (!rime->api) {
    free(rime);
    env->non_local_exit_signal(env, rime_error, STRING("lib init failed"));
  }

  /* Make functions */
  emacs_defun(env, rime, get_context, "rime-lib-get-context", "Get context.", 0, 0);
  emacs_defun(env, rime, get_input, "rime-lib-get-input", "Get input.", 0, 0);
  emacs_defun(env, rime, get_commit, "rime-lib-get-commit", "Get commit.", 0, 0);
  emacs_defun(env, rime, clear_composition, "rime-lib-clear-composition", "Clear composition.", 0, 0);
  emacs_defun(env, rime, process_key, "rime-lib-process-key", "Process key.", 2, 2);
  emacs_defun(env, rime, get_schema_list, "rime-lib-get-schema-list", "Get schema list.", 0, 0);

  if (ert->size < sizeof (*ert))
    return 1;
  else
    return 0;
}
