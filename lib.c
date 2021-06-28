// Author: Shi Tianshu
//
// This file is not part of GNU Emacs.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 3
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GNU Emacs; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <emacs-module.h>
#include <rime_api.h>

#define INTERN(val) env->intern(env, val)
#define GLOBAL_REF(val) env->make_global_ref(env, val)
#define REF(val) env->make_global_ref(env, env->intern(env, val))
#define STRING(val) env->make_string(env, val, strlen(val))
#define FUNCALL0(func) env->funcall(env, func, 0, NULL)
#define FUNCALL1(func, a) env->funcall(env, func, 1, (emacs_value[]){a})
#define FUNCALL2(func, a, b) env->funcall(env, func, 2, (emacs_value[]){a, b})
#define CONS(car, cdr) FUNCALL2(REF("cons"), car, cdr)
#define INT(val) env->make_integer(env, val)
#define LIST(len, array) env->funcall(env, REF("list"), len, array)

int plugin_is_GPL_compatible;

emacs_value nil, t;

typedef struct _EmacsRime {
  RimeSessionId session_id;
  RimeApi *api;
  bool first_run;
} EmacsRime;

static char *copy_string(char *str) {
  if (str) {
     size_t size = strlen(str);
     char *new_str = malloc((size + 1) * sizeof(char));
     strcpy(new_str, str);
     new_str[size] = '\0';
     return new_str;
  } else {
    return NULL;
  }
}

char *get_string(emacs_env *env, emacs_value arg)
{
  if (arg == NULL) {
    return NULL;
  } else {
     ptrdiff_t size;
    env->copy_string_contents(env, arg, NULL, &size);
    char *buf = (char*) malloc(size * sizeof(char));
    env->copy_string_contents(env, arg, buf, &size);
    return buf;
  }
}

void notification_handler(void *context,
                          RimeSessionId session_id,
                          const char *message_type,
                          const char *message_value) {
}


emacs_value
string_length(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  char* str = get_string(env, args[0]);
  int len = strlen(str);
  return INT(len);
}

emacs_value
finalize(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime*) data;
  if (rime->session_id) {
    rime->session_id = 0;
  }
  rime->api->finalize();
  return t;
}

emacs_value
get_sync_dir(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  const char *sync_dir = rime->api->get_sync_dir();
  return env->make_string(env, sync_dir, strlen(sync_dir));
}

emacs_value
sync_user_data(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  bool result = rime->api->sync_user_data();
  return result ? t : nil;
}

emacs_value
start(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  char *shared_data_dir = get_string(env,  args[0]);
  char *user_data_dir = get_string(env, args[1]);

  RIME_STRUCT(RimeTraits, emacs_rime_traits);

  emacs_rime_traits.shared_data_dir = shared_data_dir;
  emacs_rime_traits.app_name = "rime.emacs";
  emacs_rime_traits.user_data_dir = user_data_dir;
  emacs_rime_traits.distribution_name = "Rime";
  emacs_rime_traits.distribution_code_name = "emacs-rime";
  emacs_rime_traits.distribution_version = "1.0.1";
  if (rime->first_run) {
    rime->api->setup(&emacs_rime_traits);
    rime->first_run = false;
  }

  rime->api->initialize(&emacs_rime_traits);
  rime->api->set_notification_handler(notification_handler, rime);
  rime->api->start_maintenance(true);

  // wait for deploy
  rime->api->join_maintenance_thread();

  rime->session_id = rime->api->create_session();

  return t;
}


emacs_value
get_option (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  char *option = get_string(env, args[0]);
  if (rime->api->get_option(rime->session_id, option)) {
    return t;
  }
  return nil;
}


emacs_value
set_option (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  char *option = get_string(env, args[0]);
  bool value = env->is_not_nil(env, args[1]);

  rime->api->set_option(rime->session_id, option, value);

  return nil;
}

emacs_value
process_key(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  int keycode = env->extract_integer(env, args[0]);
  int mask = env->extract_integer(env, args[1]);

  if (rime->api->process_key(rime->session_id, keycode, mask)) {
    return t;
  }
  return nil;
}

emacs_value
get_context(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  RIME_STRUCT(RimeContext, context);
  if (!rime->api->get_context(rime->session_id, &context)){
    return nil;
  }

  size_t result_size = 4;
  emacs_value result_a[result_size];

  // 0. context.commit_text_preview
  char *ctp_str = copy_string(context.commit_text_preview);
  if (ctp_str)
    result_a[0] = CONS(INTERN("commit-text-preview"), STRING(ctp_str));
  else
    result_a[0] = CONS(INTERN("commit-text-preview"), nil);

  // 1. context.composition
  emacs_value composition_a[7];

  int length = context.composition.length;
  int cursor_pos = context.composition.cursor_pos;

  composition_a[0] = CONS(INTERN("length"), INT(length));
  composition_a[1] = CONS(INTERN("cursor-pos"), INT(cursor_pos));
  composition_a[2] = CONS(INTERN("sel-start"), INT(context.composition.sel_start));
  composition_a[3] = CONS(INTERN("sel-end"), INT(context.composition.sel_end));

  char *preedit_str = copy_string(context.composition.preedit);
  if (preedit_str) {
    composition_a[4] = CONS(INTERN("preedit"), STRING(preedit_str));

    int before_cursor_len = cursor_pos;
    int after_cursor_len = length - cursor_pos;

    char* before_cursor = malloc(before_cursor_len + 1);
    char* after_cursor = malloc(after_cursor_len + 1);

    strncpy(before_cursor, preedit_str, before_cursor_len);
    strncpy(after_cursor, preedit_str + before_cursor_len, after_cursor_len);

    before_cursor[before_cursor_len] = '\0';
    after_cursor[after_cursor_len] = '\0';

    composition_a[5] = CONS(INTERN("before-cursor"), STRING(before_cursor));
    composition_a[6] = CONS(INTERN("after-cursor"), STRING(after_cursor));

    free(before_cursor);
    free(after_cursor);

  } else {
    composition_a[4] = CONS(INTERN("preedit"), nil);
    composition_a[5] = CONS(INTERN("before-cursor"), nil);
    composition_a[6] = CONS(INTERN("after-cursor"), nil);
  }

  emacs_value composition_value = LIST(7, composition_a);
  result_a[1] = CONS(INTERN("composition"), composition_value);

  // 2. context.menu
  if (context.menu.num_candidates) {
    emacs_value menu_a[7];
    menu_a[0] = CONS(INTERN("highlighted-candidate-index"), INT(context.menu.highlighted_candidate_index));
    menu_a[1] = CONS(INTERN("last-page-p"), context.menu.is_last_page ? t : nil);
    menu_a[2] = CONS(INTERN("num-candidates"), INT(context.menu.num_candidates));
    menu_a[3] = CONS(INTERN("page-no"), INT(context.menu.page_no));
    menu_a[4] = CONS(INTERN("page-size"), INT(context.menu.page_size));
    menu_a[5] = CONS(INTERN("highlighted-candidate-index"), INT(context.menu.highlighted_candidate_index));
    emacs_value carray[context.menu.num_candidates];
    // Build candidates
    for (int i = 0; i < context.menu.num_candidates; i++) {
      RimeCandidate c = context.menu.candidates[i];
      char *ctext = copy_string(c.text);
      char *comment_text = copy_string(c.comment);
      emacs_value comment;
      if (comment_text) {
        comment =  STRING(comment_text);
      } else {
        comment = nil;
      }
      carray[i] = CONS(STRING(ctext), comment);
    }
    emacs_value candidates = LIST(context.menu.num_candidates, carray);
    menu_a[6] = CONS(INTERN("candidates"), candidates);
    emacs_value menu = LIST(7, menu_a);
    result_a[2] = CONS(INTERN("menu"), menu);
  } else {
    result_a[2] = CONS(INTERN("menu"), nil);
  }

  // 3. context.select_labels
  size_t page_size = context.menu.page_size;
  if (context.select_labels && page_size > 0) {
    emacs_value select_labels_a[page_size];
    for (int i = 0; i < page_size; i++) {
      char* label = context.select_labels[i];
      select_labels_a[i] = STRING(label);
    }
    result_a[3] = CONS(INTERN("select-labels"), LIST(page_size, select_labels_a));
  } else {
    result_a[3] = CONS(INTERN("select-labels"), nil);
  }

  // build result
  emacs_value result = LIST(result_size, result_a);

  rime->api->free_context(&context);

  return result;
}

emacs_value
version(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  return STRING("1.0.5");
}


emacs_value
set_cursor_pos(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    EmacsRime *rime = (EmacsRime*) data;
    int pos = env->extract_integer(env, args[0]);

    rime->api->set_caret_pos(rime->session_id, pos);

    return nil;
}


emacs_value
clear_composition(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  rime->api->clear_composition(rime->session_id);
  return t;
}

emacs_value
get_input (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  const char* input = rime->api->get_input(rime->session_id);

  if (!input) {
    return nil;
  } else {
    return STRING(input);
  }
}

emacs_value
get_commit(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  RIME_STRUCT(RimeCommit, commit);
  if (rime->api->get_commit(rime->session_id, &commit)) {
    if (!commit.text) {
      return nil;
    }

    char *commit_str = copy_string(commit.text);
    rime->api->free_commit(&commit);

    return STRING(commit_str);
  }

  return nil;
}


emacs_value
select_schema(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime*) data;
  const char *schema_id = get_string(env, args[0]);
  if (rime->api->select_schema(rime->session_id, schema_id)) {
    return t;
  }
  return nil;
}


emacs_value
get_schema_list(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  RimeSchemaList schema_list;

  if (!rime->api->get_schema_list(&schema_list)) {
    return nil;
  }

  emacs_value flist = env->intern(env, "list");
  emacs_value array[schema_list.size];

  for (int i = 0; i < schema_list.size; i++) {
    RimeSchemaListItem item = schema_list.list[i];
    array[i] = FUNCALL2(INTERN("list"), STRING(item.schema_id), STRING(item.name));
  }

  emacs_value result = env->funcall(env, flist, schema_list.size, array);

  rime->api->free_schema_list(&schema_list);

  return result;
}

/*
 * Deprecated
 * Prefer user set how to toggle inline ascii.
 *
 *
 * Find which key is used for inline_ascii, and simulate.
 * This is a tricky implementation, it work only when:
 * - build/default.yaml is exists.
 * - one of Shift_L, Shift_R, Control_L, Control_R is used to trigger inline_ascii.
 *
 * TODO: Find a better way.
 */
emacs_value
inline_ascii(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  EmacsRime *rime = (EmacsRime*) data;

  RimeConfig *conf = malloc(sizeof(RimeConfig));

  if(!rime->api->user_config_open("build/default.yaml", conf)) {
    free(conf);
    return nil;
  }

  char *buf = malloc(128 * sizeof(char));
  emacs_value result = nil;

  if (rime->api->config_get_string(conf, "ascii_composer/switch_key/Shift_L", buf, 128)
      && (strcmp(buf, "inline_ascii") == 0)) {
    rime->api->process_key(rime->session_id, 65505, 0);
    rime->api->process_key(rime->session_id, 65505, 1073741824);
    result = STRING("inline_ascii");
  } else if (rime->api->config_get_string(conf, "ascii_composer/switch_key/Shift_R", buf, 128)
             && (strcmp(buf, "inline_ascii") == 0)) {
    rime->api->process_key(rime->session_id, 65506, 0);
    rime->api->process_key(rime->session_id, 65506, 1073741824);
    result = STRING("inline_ascii");
  } else if (rime->api->config_get_string(conf, "ascii_composer/switch_key/Control_L", buf, 128)
             && (strcmp(buf, "inline_ascii") == 0)) {
    rime->api->process_key(rime->session_id, 65507, 0);
    rime->api->process_key(rime->session_id, 65507, 1073741824);
    result = STRING("inline_ascii");
  } else if (rime->api->config_get_string(conf, "ascii_composer/switch_key/Control_R", buf, 128)
             && (strcmp(buf, "inline_ascii") == 0)) {
    rime->api->process_key(rime->session_id, 65508, 0);
    rime->api->process_key(rime->session_id, 65508, 1073741824);
    result = STRING("inline_ascii");
  } else {
    result = nil;
  }

  free(buf);

  rime->api->config_close(conf);

  return result;
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

  /* Get Rime API */
  EmacsRime *rime = (EmacsRime*) malloc(sizeof(EmacsRime));
  rime->api = rime_get_api();
  if (!rime->api) {
    free(rime);
    return 0;
  }

  /* Global emacs_value initialize */
  nil = REF("nil");
  t = REF("t");
  /* Make functions */

  emacs_defun(env, rime, version, "rime-lib-version", "Version", 0, 0);
  emacs_defun(env, rime, set_cursor_pos, "rime-lib-set-cursor-pos", "Set Cursor Pos", 1, 1);
  emacs_defun(env, rime, start, "rime-lib-start", "Start", 2, 2);
  emacs_defun(env, rime, finalize, "rime-lib-finalize", "Finalize", 0, 0);
  emacs_defun(env, rime, sync_user_data, "rime-lib-sync-user-data", "Sync user data.", 0, 0);
  emacs_defun(env, rime, get_sync_dir, "rime-lib-get-sync-dir", "Get sync directory.", 0, 0);
  emacs_defun(env, rime, get_context, "rime-lib-get-context", "Get context.", 0, 0);
  emacs_defun(env, rime, get_input, "rime-lib-get-input", "Get input.", 0, 0);
  emacs_defun(env, rime, get_commit, "rime-lib-get-commit", "Get commit.", 0, 0);
  emacs_defun(env, rime, clear_composition, "rime-lib-clear-composition", "Clear composition.", 0, 0);
  emacs_defun(env, rime, process_key, "rime-lib-process-key", "Process key.", 2, 2);
  emacs_defun(env, rime, select_schema, "rime-lib-select-schema", "Select schema", 1, 1);
  emacs_defun(env, rime, get_schema_list, "rime-lib-get-schema-list", "Get schema list.", 0, 0);
  emacs_defun(env, rime, string_length, "rime-lib-string-length", "Get length of string", 1, 1);
  emacs_defun(env, rime, get_option, "rime-lib-get-option", "Get option", 1, 1);
  emacs_defun(env, rime, set_option, "rime-lib-set-option", "Set option", 2, 2);
  emacs_defun(env, rime, inline_ascii, "rime-lib-inline-ascii", "Inline ascii", 0, 0);

  if (ert->size < sizeof (*ert))
    return 1;
  else
    return 0;
}
