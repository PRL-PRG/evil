
create_counters <- function(call_id, eval_env, eval_frame_depth) {
    list(call_id = call_id,

         eval_env = eval_env,

         direct_builtin = 0L,
         indirect_builtin = 0L,

         direct_special = 0L,
         indirect_special = 0L,

         direct_closure = 0L,
         indirect_closure = 0L,

         direct_interpreter_eval = 0L,
         indirect_interpreter_eval = 0L,

         direct_c_call = 0L,
         indirect_c_call = 0L,

         direct_allocation = 0L,
         indirect_allocation = 0L,

         direct_writes = 0L,
         indirect_writes = 0L,

         library_packages = "",

         require_packages = "",

         eval_frame_depth = as.integer(eval_frame_depth))
}

push_counters <- function(context_data, call_id, eval_env, eval_frame_depth) {
    context_data$counters[[length(context_data$counters) + 1]] <- create_counters(call_id, eval_env, eval_frame_depth)
}

pop_counters <- function(context_data) {
    counters <- context_data$counters[[length(context_data$counters)]]
    context_data$counters[[length(context_data$counters)]] <- NULL
    counters
}

