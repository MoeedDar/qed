open Core

type t = {
  terms : Term_meta_context.t;
  levels : Level_meta_context.t;
  environment : Environment.t;
}
